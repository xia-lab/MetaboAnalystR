package pro.metaboanalyst.workflows;

import com.impossibl.postgres.api.jdbc.PGConnection;
import com.impossibl.postgres.api.jdbc.PGNotificationListener;
import com.impossibl.postgres.jdbc.PGDataSource;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import jakarta.ejb.Singleton;
import jakarta.ejb.Startup;
import jakarta.ws.rs.sse.OutboundSseEvent;
import jakarta.ws.rs.sse.Sse;
import jakarta.ws.rs.sse.SseEventSink;
import java.sql.SQLException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * PostgreSQL LISTEN/NOTIFY service for real-time workflow status updates.
 * This service maintains a dedicated database connection to receive notifications
 * and distributes them to connected SSE clients.
 *
 * Usage:
 * 1. PostgreSQL triggers call pg_notify() when workflow_runs table is updated
 * 2. This service receives notifications via LISTEN
 * 3. Notifications are pushed to browser clients via Server-Sent Events (SSE)
 *
 * Benefits:
 * - Real-time updates (< 500ms latency)
 * - No polling overhead
 * - Minimal network traffic (only on status change)
 * - Automatic reconnection handling
 */
@Singleton
@Startup
public class WorkflowNotificationService {

    private static final Logger LOGGER = Logger.getLogger(WorkflowNotificationService.class.getName());

    // Map: userId -> list of SSE event sinks
    private final ConcurrentHashMap<String, CopyOnWriteArrayList<EventSinkHolder>> listeners =
        new ConcurrentHashMap<>();

    private PGConnection pgConnection;
    private ScheduledExecutorService keepAliveExecutor;

    // Database connection config - read from environment or system properties
    private String dbUrl;
    private String dbUser;
    private String dbPassword;

    @PostConstruct
    public void init() {
        try {
            // Read database configuration from system properties or environment variables
            // Priority: System property > Environment variable > Default
            dbUrl = System.getProperty("postgres.notification.url",
                    System.getenv().getOrDefault("POSTGRES_NOTIFICATION_URL",
                    "jdbc:pgsql://localhost:5432/metaboanalyst"));

            dbUser = System.getProperty("postgres.notification.user",
                     System.getenv().getOrDefault("POSTGRES_NOTIFICATION_USER", "postgres"));

            dbPassword = System.getProperty("postgres.notification.password",
                         System.getenv().getOrDefault("POSTGRES_NOTIFICATION_PASSWORD", ""));

            LOGGER.info("Initializing PostgreSQL notification service with URL: " + dbUrl);

            // Create dedicated connection for LISTEN/NOTIFY
            initializeNotificationConnection();

            // Start keep-alive scheduler (every 30 seconds)
            keepAliveExecutor = Executors.newSingleThreadScheduledExecutor(r -> {
                Thread t = new Thread(r, "PG-Notification-KeepAlive");
                t.setDaemon(true);
                return t;
            });
            keepAliveExecutor.scheduleAtFixedRate(
                this::keepAlive, 30, 30, TimeUnit.SECONDS);

            LOGGER.info("PostgreSQL notification service initialized successfully");

        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Failed to initialize notification service. " +
                      "Workflow status updates will not work in real-time.", e);
        }
    }

    private void initializeNotificationConnection() throws SQLException {
        PGDataSource dataSource = new PGDataSource();
        dataSource.setUrl(dbUrl);
        dataSource.setUser(dbUser);
        dataSource.setPassword(dbPassword);

        // Get dedicated connection (not from pool!)
        pgConnection = (PGConnection) dataSource.getConnection();

        // Add global notification listener
        pgConnection.addNotificationListener(new PGNotificationListener() {
            @Override
            public void notification(int processId, String channelName, String payload) {
                handleNotification(channelName, payload);
            }

            @Override
            public void closed() {
                LOGGER.warning("Notification connection closed unexpectedly. Attempting reconnect...");
                reconnect();
            }
        });

        LOGGER.info("PostgreSQL notification connection established");
    }

    private void reconnect() {
        int maxRetries = 5;
        int retryDelay = 5000; // 5 seconds

        for (int i = 0; i < maxRetries; i++) {
            try {
                LOGGER.info("Reconnection attempt " + (i + 1) + "/" + maxRetries);
                Thread.sleep(retryDelay);

                // Close old connection if still open
                if (pgConnection != null) {
                    try {
                        pgConnection.close();
                    } catch (Exception e) {
                        // Ignore
                    }
                }

                // Establish new connection
                initializeNotificationConnection();

                // Re-subscribe to all active channels
                resubscribeAll();

                LOGGER.info("Reconnection successful");
                return;

            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "Reconnection attempt " + (i + 1) + " failed", e);
                retryDelay *= 2; // Exponential backoff
            }
        }

        LOGGER.severe("Failed to reconnect after " + maxRetries + " attempts. " +
                     "Manual restart may be required.");
    }

    private void resubscribeAll() {
        LOGGER.info("Re-subscribing to " + listeners.size() + " active channels");

        for (String userId : listeners.keySet()) {
            try {
                String channelName = "workflow_status_" + sanitizeChannelName(userId);
                pgConnection.createStatement().execute("LISTEN " + channelName);
                LOGGER.fine("Re-subscribed to channel: " + channelName);
            } catch (SQLException e) {
                LOGGER.log(Level.WARNING, "Failed to re-subscribe to channel for user: " + userId, e);
            }
        }
    }

    private void keepAlive() {
        try {
            if (pgConnection != null && !pgConnection.isClosed()) {
                pgConnection.createStatement().execute("SELECT 1");
                LOGGER.fine("Keep-alive ping successful");
            } else {
                LOGGER.warning("Connection appears closed during keep-alive check");
                reconnect();
            }
        } catch (SQLException e) {
            LOGGER.log(Level.WARNING, "Keep-alive query failed, connection may be dead", e);
            reconnect();
        }
    }

    private void handleNotification(String channelName, String payload) {
        LOGGER.fine("Received notification on channel: " + channelName + ", payload: " + payload);

        // Extract userId from channel name (workflow_status_{email})
        if (channelName.startsWith("workflow_status_")) {
            String userId = channelName.substring("workflow_status_".length());

            // Send to all SSE listeners for this user
            CopyOnWriteArrayList<EventSinkHolder> userListeners = listeners.get(userId);
            if (userListeners != null && !userListeners.isEmpty()) {
                int sentCount = 0;
                for (EventSinkHolder holder : userListeners) {
                    try {
                        OutboundSseEvent event = holder.sse.newEventBuilder()
                            .name("workflow-status")
                            .data(payload)
                            .mediaType(jakarta.ws.rs.core.MediaType.APPLICATION_JSON_TYPE)
                            .build();

                        holder.sink.send(event);
                        sentCount++;

                    } catch (Exception e) {
                        LOGGER.log(Level.WARNING, "Failed to send SSE event, removing dead connection", e);
                        userListeners.remove(holder);
                        try {
                            holder.sink.close();
                        } catch (Exception closeEx) {
                            // Ignore close errors
                        }
                    }
                }
                LOGGER.fine("Sent notification to " + sentCount + " SSE client(s) for user: " + userId);
            }
        }
    }

    /**
     * Register SSE listener for a user.
     *
     * @param userId User email or identifier
     * @param sink SSE event sink to send events to
     * @param sse Sse instance for creating events
     */
    public void subscribe(String userId, SseEventSink sink, Sse sse) {
        try {
            // Add to listeners map
            EventSinkHolder holder = new EventSinkHolder(sink, sse);
            listeners.computeIfAbsent(userId, k -> new CopyOnWriteArrayList<>()).add(holder);

            // Subscribe to PostgreSQL channel for this user
            String channelName = "workflow_status_" + sanitizeChannelName(userId);

            if (pgConnection != null && !pgConnection.isClosed()) {
                pgConnection.createStatement().execute("LISTEN " + channelName);
                LOGGER.info("User subscribed: " + userId + " to channel: " + channelName);
            } else {
                LOGGER.warning("Cannot subscribe - PostgreSQL connection is closed");
                throw new SQLException("PostgreSQL notification connection not available");
            }

        } catch (SQLException e) {
            LOGGER.log(Level.SEVERE, "Failed to subscribe user: " + userId, e);
            throw new RuntimeException("Failed to subscribe to workflow notifications", e);
        }
    }

    /**
     * Unregister SSE listener.
     *
     * @param userId User email or identifier
     * @param sink SSE event sink to remove
     */
    public void unsubscribe(String userId, SseEventSink sink) {
        CopyOnWriteArrayList<EventSinkHolder> userListeners = listeners.get(userId);
        if (userListeners != null) {
            userListeners.removeIf(h -> h.sink.equals(sink));

            // If no more listeners for this user, UNLISTEN from PostgreSQL
            if (userListeners.isEmpty()) {
                listeners.remove(userId);
                String channelName = "workflow_status_" + sanitizeChannelName(userId);
                try {
                    if (pgConnection != null && !pgConnection.isClosed()) {
                        pgConnection.createStatement().execute("UNLISTEN " + channelName);
                        LOGGER.info("User unsubscribed: " + userId + " from channel: " + channelName);
                    }
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, "Failed to UNLISTEN from channel: " + channelName, e);
                }
            }
        }

        // Close the SSE sink
        try {
            sink.close();
        } catch (Exception e) {
            // Ignore close errors
        }
    }

    /**
     * Sanitize channel name to prevent SQL injection and ensure valid PostgreSQL identifier.
     * PostgreSQL identifiers must be lowercase alphanumeric + underscore.
     *
     * @param input User email or identifier
     * @return Sanitized channel name suffix
     */
    private String sanitizeChannelName(String input) {
        if (input == null || input.isEmpty()) {
            return "unknown";
        }
        // Convert to lowercase and replace non-alphanumeric characters with underscore
        return input.toLowerCase().replaceAll("[^a-z0-9_]", "_");
    }

    /**
     * Get count of active SSE connections.
     *
     * @return Total number of active SSE connections across all users
     */
    public int getActiveConnectionCount() {
        return listeners.values().stream()
            .mapToInt(CopyOnWriteArrayList::size)
            .sum();
    }

    /**
     * Get count of users with active connections.
     *
     * @return Number of unique users with at least one SSE connection
     */
    public int getActiveUserCount() {
        return listeners.size();
    }

    @PreDestroy
    public void destroy() {
        LOGGER.info("Shutting down PostgreSQL notification service...");

        try {
            // Close all SSE connections
            int closedCount = 0;
            for (CopyOnWriteArrayList<EventSinkHolder> sinks : listeners.values()) {
                for (EventSinkHolder holder : sinks) {
                    try {
                        holder.sink.close();
                        closedCount++;
                    } catch (Exception e) {
                        // Ignore close errors
                    }
                }
            }
            LOGGER.info("Closed " + closedCount + " SSE connection(s)");
            listeners.clear();

            // Shutdown keep-alive executor
            if (keepAliveExecutor != null) {
                keepAliveExecutor.shutdown();
                if (!keepAliveExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
                    keepAliveExecutor.shutdownNow();
                }
            }

            // Close PostgreSQL connection
            if (pgConnection != null && !pgConnection.isClosed()) {
                pgConnection.close();
                LOGGER.info("PostgreSQL notification connection closed");
            }

            LOGGER.info("PostgreSQL notification service shut down successfully");

        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error during notification service shutdown", e);
        }
    }

    /**
     * Holder for SSE sink and Sse instance.
     */
    private static class EventSinkHolder {
        final SseEventSink sink;
        final Sse sse;

        EventSinkHolder(SseEventSink sink, Sse sse) {
            this.sink = sink;
            this.sse = sse;
        }
    }
}
