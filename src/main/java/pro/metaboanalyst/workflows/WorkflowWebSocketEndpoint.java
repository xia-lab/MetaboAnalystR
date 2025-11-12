package pro.metaboanalyst.workflows;

import jakarta.inject.Inject;
import jakarta.websocket.*;
import jakarta.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * WebSocket endpoint for real-time workflow status updates.
 * This is an alternative to SSE that works more reliably with Payara Micro.
 *
 * Client usage:
 * const ws = new WebSocket('ws://localhost:8080/MetaboAnalyst/workflow/ws');
 * ws.onmessage = (event) => console.log('Update:', JSON.parse(event.data));
 */
@ServerEndpoint(
    value = "/workflow/ws/{userId}",
    configurator = WorkflowWebSocketConfigurator.class
)
public class WorkflowWebSocketEndpoint {

    private static final Logger LOGGER = Logger.getLogger(WorkflowWebSocketEndpoint.class.getName());

    // Static initializer to verify class is loaded and endpoint is discovered
    static {
        LOGGER.info("========================================================");
        LOGGER.info("[WS Endpoint] CLASS LOADED - WorkflowWebSocketEndpoint");
        LOGGER.info("[WS Endpoint] Endpoint path: /workflow/ws/{userId}");
        LOGGER.info("========================================================");
    }

    @Inject
    private WorkflowNotificationService notificationService;

    private String userId;
    private Session session;

    @OnOpen
    public void onOpen(Session session, EndpointConfig config, @jakarta.websocket.server.PathParam("userId") String userIdParam) {
        this.session = session;
        this.userId = userIdParam;

        LOGGER.info("[WS] Connection attempt from: " + session.getId() + " for userId: " + userId);

        // Set no timeout for idle connections (we'll use heartbeats)
        session.setMaxIdleTimeout(0);

        // Validate userId
        if (userId == null || userId.isBlank()) {
            LOGGER.warning("[WS] Unauthorized connection attempt - no userId provided");
            try {
                session.close(new CloseReason(CloseReason.CloseCodes.VIOLATED_POLICY, "Unauthorized"));
            } catch (IOException e) {
                // Ignore
            }
            return;
        }

        // Try to get injected beans, fallback to JNDI if needed
        if (notificationService == null) {
            LOGGER.warning("[WS] notificationService not injected, attempting JNDI lookup");
            try {
                javax.naming.InitialContext ctx = new javax.naming.InitialContext();
                notificationService = (WorkflowNotificationService) ctx.lookup(
                    "java:global/MetaboAnalyst-Pro-4.12/WorkflowNotificationService");
                LOGGER.info("[WS] Successfully looked up notificationService via JNDI");
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "[WS] Failed to lookup notificationService", e);
                try {
                    session.close(new CloseReason(CloseReason.CloseCodes.UNEXPECTED_CONDITION,
                        "Service unavailable"));
                } catch (IOException ex) {}
                return;
            }
        }

        LOGGER.info("[WS] Connection opened for user: " + userId + " (sessionId=" + session.getId() + ")");

        try {
            // Send initial connection confirmation
            session.getBasicRemote().sendText(
                "{\"type\":\"connected\",\"message\":\"Workflow status stream connected\",\"userId\":\"" +
                escapeJson(userId) + "\"}"
            );

            // Subscribe to PostgreSQL notifications using WebSocket handler
            notificationService.subscribeWebSocket(userId, session);

            LOGGER.info("[WS] Subscription established for user: " + userId +
                " (Total connections: " + notificationService.getActiveConnectionCount() + ")");

        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "[WS] Error establishing connection for user: " + userId, e);
            try {
                session.close(new CloseReason(CloseReason.CloseCodes.UNEXPECTED_CONDITION, "Setup failed"));
            } catch (IOException ex) {
                // Ignore
            }
        }
    }

    @OnMessage
    public void onMessage(String message, Session session) {
        LOGGER.info("[WS] >>> Received message from client: '" + message + "' (session: " + session.getId() + ", open: " + session.isOpen() + ")");
    }

    @OnClose
    public void onClose(Session session, CloseReason reason) {
        LOGGER.info("[WS] Connection closed for user: " + userId +
            " (reason: " + reason.getReasonPhrase() + ", code: " + reason.getCloseCode() + ")");

        if (userId != null) {
            notificationService.unsubscribeWebSocket(userId, session);
        }
    }

    @OnError
    public void onError(Session session, Throwable error) {
        LOGGER.log(Level.WARNING, "[WS] Error on connection for user: " + userId, error);
    }

    private static String escapeJson(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}
