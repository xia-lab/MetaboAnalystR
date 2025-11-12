package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PreDestroy;
import jakarta.ejb.Singleton;
import jakarta.inject.Inject;
import org.postgresql.PGConnection;
import org.postgresql.PGNotification;
import org.postgresql.ds.PGSimpleDataSource;
import pro.metaboanalyst.controllers.general.ApplicationBean1;

import java.net.InetSocketAddress;
import java.net.Socket;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * PostgreSQL LISTEN/NOTIFY service for real-time workflow status updates via WebSocket.
 * Dedicated JDBC connection (not pooled), robust reconnect, periodic keepalive,
 * low-latency poller, and user-channel fanout.
 */
@Singleton
public class WorkflowNotificationService {

    private static final Logger LOGGER = Logger.getLogger(WorkflowNotificationService.class.getName());

    /** userId -> list of WebSocket sessions (allow multiple tabs/windows) */
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<jakarta.websocket.Session>> webSocketListeners = new ConcurrentHashMap<>();

    // DB connectivity
    private volatile Connection baseConn;   // JDBC base connection
    private volatile PGConnection pgConn;   // PG-specific interface
    private volatile ScheduledExecutorService notificationPoller;
    private volatile ScheduledExecutorService keepAliveExecutor;
    private volatile ScheduledExecutorService heartbeatExecutor;

    @Inject
    private ApplicationBean1 applicationBean1;

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    // config
    private String dbUrl;
    private String dbUser;
    private String dbPassword;

    private static final String REMOTE_NOTIFICATION_URL = "jdbc:postgresql://34.133.74.138:5432/xialabdb"
            + "?sslmode=prefer&connectTimeout=10&loginTimeout=10&tcpKeepAlive=true&gssEncMode=disable";
    private static final String LOCAL_TUNNEL_NOTIFICATION_URL = "jdbc:postgresql://127.0.0.1:5433/xialabdb"
            + "?sslmode=prefer&connectTimeout=10&loginTimeout=10&tcpKeepAlive=true&gssEncMode=disable";

    private boolean shouldUseLocalTunnel() {
        if (applicationBean1 != null && "localhost".equalsIgnoreCase(applicationBean1.getToolLocation())) {
            return true;
        }
        String fallback = System.getProperty("metaboanalyst.tool.location",
                System.getenv().getOrDefault("METABOANALYST_TOOL_LOCATION", "pro"));
        return "localhost".equalsIgnoreCase(fallback);
    }

    // state
    private final AtomicBoolean initialized = new AtomicBoolean(false);
    private final Object initLock = new Object();
    private final ConcurrentHashMap<Integer, String> lastSentStatus = new ConcurrentHashMap<>();

    // ---------- lifecycle ----------
    private void ensureInitialized() {
        if (initialized.get()) return;
        synchronized (initLock) {
            if (initialized.get()) return;
            try {
                startService();
                initialized.set(true);
                LOGGER.info("PostgreSQL notification service initialized successfully");
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE,
                        "Failed to initialize notification service. Real-time updates disabled until recovery.", e);
            }
        }
    }

    private void startService() throws Exception {
        String fallbackUrl = shouldUseLocalTunnel() ? LOCAL_TUNNEL_NOTIFICATION_URL : REMOTE_NOTIFICATION_URL;

        dbUrl = System.getProperty("postgres.notification.url",
                System.getenv().getOrDefault("POSTGRES_NOTIFICATION_URL", fallbackUrl));
        dbUser = System.getProperty("postgres.notification.user",
                System.getenv().getOrDefault("POSTGRES_NOTIFICATION_USER", "glassfish"));
        dbPassword = System.getProperty("postgres.notification.password",
                System.getenv().getOrDefault("POSTGRES_NOTIFICATION_PASSWORD", "1qazxDR%"));

        LOGGER.info(() -> "Initializing PostgreSQL notification service with URL: " + dbUrl);

        final int maxRetries = 5;
        for (int attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                initializeNotificationConnection();
                LOGGER.info("PostgreSQL notification connection established successfully");
                break;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "Attempt " + attempt + "/" + maxRetries
                        + " failed to connect to PostgreSQL: " + e.getMessage());
                if (attempt == maxRetries) throw e;
                Thread.sleep(5000L * attempt);
            }
        }

        // DB keep-alive
        keepAliveExecutor = Executors.newSingleThreadScheduledExecutor(r -> daemon("PG-Notification-KeepAlive", r));
        keepAliveExecutor.scheduleAtFixedRate(this::keepAlive, 30, 30, TimeUnit.SECONDS);

        // SSE heartbeat (lightweight comment every 2s to prevent Jersey from closing idle streams)
        // Must be more frequent than the ~6 second timeout we're experiencing
        heartbeatExecutor = Executors.newSingleThreadScheduledExecutor(r -> daemon("SSE-Heartbeat", r));
        heartbeatExecutor.scheduleAtFixedRate(this::sendHeartbeat, 0, 2, TimeUnit.SECONDS);
    }

    private static Thread daemon(String name, Runnable r) {
        Thread t = new Thread(r, name);
        t.setDaemon(true);
        return t;
    }

    @PreDestroy
    public void destroy() {
        LOGGER.info("Shutting down PostgreSQL notification service...");


        // Stop executors
        shutdownExec(notificationPoller, "poller");
        shutdownExec(keepAliveExecutor, "keepalive");
        shutdownExec(heartbeatExecutor, "heartbeat");

        // Close DB
        try {
            Connection c = baseConn;
            baseConn = null;
            pgConn = null;
            if (c != null && !c.isClosed()) {
                c.close();
                LOGGER.info("PostgreSQL notification connection closed");
            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error closing PostgreSQL connection", e);
        }

        LOGGER.info("PostgreSQL notification service shut down");
    }

    // ---------- WebSocket support ----------
    /** Register WebSocket session for a user. */
    public void subscribeWebSocket(String userId, jakarta.websocket.Session session) {
        try {
            webSocketListeners.computeIfAbsent(userId, k -> new CopyOnWriteArrayList<>()).add(session);

            String channel = channelFor(userId);
            ensureInitialized();
            ensureConnected();

            try (Statement st = baseConn.createStatement()) {
                st.execute("LISTEN " + channel);
            }
            LOGGER.info("WebSocket subscribed: " + userId + " (channel: " + channel + ")");

        } catch (SQLException e) {
            LOGGER.log(Level.SEVERE, "Failed to subscribe WebSocket user: " + userId, e);
            throw new RuntimeException("Failed to subscribe to workflow notifications", e);
        }
    }

    /** Unregister WebSocket session for a user. */
    public void unsubscribeWebSocket(String userId, jakarta.websocket.Session session) {
        CopyOnWriteArrayList<jakarta.websocket.Session> list = webSocketListeners.get(userId);
        if (list != null) {
            list.removeIf(s -> s.getId().equals(session.getId()));
            if (list.isEmpty()) {
                webSocketListeners.remove(userId);
                String channel = channelFor(userId);
                try (Statement st = (baseConn != null && !baseConn.isClosed()) ? baseConn.createStatement() : null) {
                    if (st != null) {
                        st.execute("UNLISTEN " + channel);
                    }
                    LOGGER.info("WebSocket unsubscribed: " + userId + " (channel: " + channel + ")");
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, "Failed to UNLISTEN " + channel, e);
                }
            }
        }
    }

    public int getActiveConnectionCount() {
        return webSocketListeners.values().stream().mapToInt(CopyOnWriteArrayList::size).sum();
    }

    public int getActiveUserCount() {
        return webSocketListeners.size();
    }

    // ---------- core DB / LISTEN logic ----------
    private void initializeNotificationConnection() throws SQLException {
        String url = ensureParams(dbUrl);
        tryTcp(url);

        PGSimpleDataSource ds = new PGSimpleDataSource();
        ds.setUrl(url);
        ds.setUser(dbUser);
        ds.setPassword(dbPassword);
        ds.setLoginTimeout(10);

        baseConn = ds.getConnection();
        pgConn = baseConn.unwrap(PGConnection.class);

        startNotificationPoller();

        LOGGER.info("Notification connection established: " + url);
    }

    private void startNotificationPoller() {
        shutdownExec(notificationPoller, "poller");
        notificationPoller = Executors.newSingleThreadScheduledExecutor(r -> daemon("PG-Notification-Poller", r));

        // poll every 100ms for low-latency delivery
        notificationPoller.scheduleAtFixedRate(() -> {
            try {
                if (pgConn == null) return;
                PGNotification[] arr = pgConn.getNotifications(100); // timeout ms
                if (arr != null) {
                    for (PGNotification n : arr) {
                        handleNotification(n.getName(), n.getParameter());
                    }
                }
            } catch (SQLException e) {
                LOGGER.log(Level.WARNING, "Error polling for notifications; will reconnect", e);
                reconnect();
            } catch (Throwable t) {
                LOGGER.log(Level.SEVERE, "Unexpected error in notification poller", t);
            }
        }, 100, 100, TimeUnit.MILLISECONDS);
    }

    private void keepAlive() {
        try {
            if (baseConn == null || baseConn.isClosed()) {
                LOGGER.warning("Keep-alive: connection not available; reconnecting");
                reconnect();
                return;
            }
            try (Statement st = baseConn.createStatement()) {
                st.execute("SELECT 1");
            }
        } catch (SQLException e) {
            LOGGER.log(Level.WARNING, "Keep-alive failed; reconnecting", e);
            reconnect();
        }
    }

    /** Send periodic heartbeat (ping message) to all connected WebSocket clients. */
    private void sendHeartbeat() {
        int totalSent = 0, totalFailed = 0, totalClosed = 0;

        // Send to WebSocket clients
        for (Map.Entry<String, CopyOnWriteArrayList<jakarta.websocket.Session>> entry : webSocketListeners.entrySet()) {
            CopyOnWriteArrayList<jakarta.websocket.Session> wsSessions = entry.getValue();

            for (jakarta.websocket.Session wsSession : wsSessions) {
                try {
                    if (!wsSession.isOpen()) {
                        wsSessions.remove(wsSession);
                        totalClosed++;
                        continue;
                    }
                    wsSession.getBasicRemote().sendText("{\"type\":\"ping\"}");
                    totalSent++;
                } catch (Exception e) {
                    wsSessions.remove(wsSession);
                    totalFailed++;
                }
            }
        }

        if (totalFailed > 0 || totalClosed > 0) {
            LOGGER.info("[Heartbeat] Sent=" + totalSent + ", Failed=" + totalFailed + ", Pre-closed=" + totalClosed);
        }
    }

    private void reconnect() {
        final int maxRetries = 5;
        int delayMs = 5_000;
        for (int i = 1; i <= maxRetries; i++) {
            try {
                LOGGER.info("Reconnection attempt " + i + "/" + maxRetries);
                sleep(delayMs);

                // close old
                try { if (baseConn != null) baseConn.close(); } catch (Exception ignore) {}
                baseConn = null;
                pgConn = null;

                // re-open
                initializeNotificationConnection();

                LOGGER.info("Reconnection successful");
                return;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "Reconnect attempt " + i + " failed", root(e));
                delayMs = (int) Math.min(delayMs * 2L, Duration.ofMinutes(1).toMillis());
            }
        }
        LOGGER.severe("Failed to reconnect after " + maxRetries + " attempts.");
    }

    private void handleNotification(String channelName, String payload) {
        if (channelName == null) return;
        if (!channelName.startsWith("workflow_status_")) return;

        LOGGER.info("[NOTIFY] Received notification on channel: " + channelName + ", payload: " + payload);

        JsonNode parsed = null;
        int runId = -1;
        String latestStatus = null;
        String userId = null;
        try {
            parsed = OBJECT_MAPPER.readTree(payload);
            runId = parsed.path("runId").asInt(-1);
            latestStatus = parsed.path("status").asText(null);
            userId = parsed.path("userId").asText(null);
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "Failed to parse workflow notification payload", e);
        }

        // Fallback: extract from channel name if not in payload (shouldn't happen)
        if (userId == null || userId.isEmpty()) {
            userId = channelName.substring("workflow_status_".length());
            LOGGER.warning("[NOTIFY] userId not found in payload, using sanitized from channel: " + userId);
        }

        LOGGER.info("[NOTIFY] Extracted userId: " + userId);

        if (runId > 0 && latestStatus != null) {
            String prior = lastSentStatus.get(runId);
            if (latestStatus.equals(prior)) {
                // de-dup: skip identical consecutive statuses per run
                LOGGER.info("[NOTIFY] Skipping duplicate status for runId=" + runId + ", status=" + latestStatus);
                return;
            }
            lastSentStatus.put(runId, latestStatus);
        }

        int sent = 0;

        // Send to WebSocket clients
        CopyOnWriteArrayList<jakarta.websocket.Session> wsSessions = webSocketListeners.get(userId);
        if (wsSessions != null && !wsSessions.isEmpty()) {
            LOGGER.info("[NOTIFY] Broadcasting to " + wsSessions.size() + " WebSocket client(s)");
            for (jakarta.websocket.Session wsSession : wsSessions) {
                try {
                    if (!wsSession.isOpen()) {
                        wsSessions.remove(wsSession);
                        continue;
                    }
                    // Send the JSON payload directly (it's already in the right format)
                    wsSession.getBasicRemote().sendText(payload);
                    sent++;
                    LOGGER.info("[NOTIFY] Sent to WebSocket session: " + wsSession.getId());
                } catch (Exception ex) {
                    LOGGER.log(Level.WARNING, "Failed to send WebSocket; removing session", ex);
                    wsSessions.remove(wsSession);
                }
            }
        }

        LOGGER.info("[NOTIFY] Delivered to " + sent + " total client(s) for user=" + userId);
    }

    private void ensureConnected() throws SQLException {
        if (baseConn == null || baseConn.isClosed() || pgConn == null) {
            throw new SQLException("PostgreSQL notification connection not available");
        }
    }

    private String channelFor(String userId) {
        return "workflow_status_" + sanitizeChannelName(userId);
    }

    /** Ensure default URL params if none were provided. */
    private static String ensureParams(String rawUrl) {
        if (rawUrl == null || rawUrl.isEmpty()) {
            throw new IllegalArgumentException("PostgreSQL URL is not set");
        }
        if (rawUrl.contains("?")) return rawUrl;
        return rawUrl + "?sslmode=prefer&loginTimeout=10&connectTimeout=10&socketTimeout=0&tcpKeepAlive=true";
    }

    private static void tryTcp(String jdbcUrl) {
        try {
            String withoutPrefix = jdbcUrl.substring("jdbc:postgresql://".length());
            String hostPort = withoutPrefix.split("/", 2)[0];
            String host = hostPort.contains(":") ? hostPort.split(":", 2)[0] : hostPort;
            int port = hostPort.contains(":") ? Integer.parseInt(hostPort.split(":", 2)[1]) : 5432;
            try (Socket s = new Socket()) {
                s.connect(new InetSocketAddress(host, port), 5_000);
            }
            LOGGER.info("TCP connectivity OK to " + host + ":" + port);
        } catch (Throwable t) {
            LOGGER.log(Level.WARNING, "Pre-connect TCP check failed (may be normal in some networks)", t);
        }
    }

    private static void shutdownExec(ExecutorService exec, String label) {
        if (exec == null) return;
        exec.shutdown();
        try {
            if (!exec.awaitTermination(5, TimeUnit.SECONDS)) {
                exec.shutdownNow();
            }
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            exec.shutdownNow();
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "Error shutting down " + label, e);
        }
    }

    private static void sleep(long ms) {
        try { Thread.sleep(ms); } catch (InterruptedException ie) { Thread.currentThread().interrupt(); }
    }

    private static Throwable root(Throwable t) {
        while (t.getCause() != null && t.getCause() != t) t = t.getCause();
        return t;
    }

    private static String sanitizeChannelName(String input) {
        if (input == null || input.isEmpty()) return "unknown";
        return input.toLowerCase().replaceAll("[^a-z0-9_]", "_");
    }

}
