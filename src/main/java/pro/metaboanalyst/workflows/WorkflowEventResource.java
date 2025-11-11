package pro.metaboanalyst.workflows;

import pro.metaboanalyst.controllers.general.SessionBean1;
import jakarta.inject.Inject;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.sse.Sse;
import jakarta.ws.rs.sse.SseEventSink;
import java.util.logging.Logger;

/**
 * JAX-RS resource for Server-Sent Events (SSE) workflow status updates.
 *
 * This endpoint streams real-time workflow status updates to browser clients.
 * When a workflow's status changes in the database, PostgreSQL triggers a
 * notification that is received by WorkflowNotificationService and pushed
 * to connected clients through this SSE endpoint.
 *
 * Usage from browser:
 *   var eventSource = new EventSource('/MetaboAnalyst/api/workflow/events');
 *   eventSource.addEventListener('workflow-status', function(e) {
 *     var data = JSON.parse(e.data);
 *     console.log('Workflow status:', data);
 *   });
 *
 * Benefits:
 * - Real-time updates (< 500ms latency)
 * - Automatic reconnection (built into EventSource API)
 * - Works through corporate firewalls (uses HTTP/HTTPS)
 * - Server push (no polling needed)
 */
@Path("/workflow")
public class WorkflowEventResource {

    private static final Logger LOGGER = Logger.getLogger(WorkflowEventResource.class.getName());

    @Inject
    private WorkflowNotificationService notificationService;

    @Inject
    private SessionBean1 sessionBean;

    /**
     * SSE endpoint for workflow status updates.
     *
     * This endpoint keeps the connection open and pushes events to the client
     * whenever workflow status changes occur.
     *
     * @param eventSink The SSE event sink to send events to
     * @param sse The Sse instance for creating events
     */
    @GET
    @Path("/events")
    @Produces(MediaType.SERVER_SENT_EVENTS)
    public void streamEvents(@Context SseEventSink eventSink, @Context Sse sse) {

        // Validate user is authenticated
        if (sessionBean.getCurrentUser() == null) {
            LOGGER.warning("Unauthorized SSE connection attempt - no current user");
            try {
                eventSink.send(sse.newEvent("error",
                    "{\"error\":\"Unauthorized\",\"message\":\"Please log in first\"}"));
            } catch (Exception e) {
                // Ignore send error
            }
            try {
                eventSink.close();
            } catch (Exception e) {
                // Ignore close error
            }
            return;
        }

        String userId = sessionBean.getCurrentUser().getName();

        try {
            // Send initial connection confirmation
            eventSink.send(sse.newEvent("connected",
                "{\"message\":\"Workflow status stream connected\",\"userId\":\"" + userId + "\"}"));

            // Register this SSE sink with notification service
            // This will start receiving PostgreSQL notifications
            notificationService.subscribe(userId, eventSink, sse);

            LOGGER.info("SSE connection established for user: " + userId +
                       " (Total connections: " + notificationService.getActiveConnectionCount() + ")");

        } catch (Exception e) {
            LOGGER.severe("Error establishing SSE connection for user " + userId + ": " + e.getMessage());
            try {
                eventSink.send(sse.newEvent("error",
                    "{\"error\":\"ConnectionFailed\",\"message\":\"" + e.getMessage() + "\"}"));
            } catch (Exception sendEx) {
                // Ignore send error
            }
            try {
                eventSink.close();
            } catch (Exception closeEx) {
                // Ignore close error
            }
        }
    }

    /**
     * Health check endpoint to verify the notification service is running.
     *
     * @return JSON with service status
     */
    @GET
    @Path("/status")
    @Produces(MediaType.APPLICATION_JSON)
    public String getStatus() {
        try {
            int activeConnections = notificationService.getActiveConnectionCount();
            int activeUsers = notificationService.getActiveUserCount();

            return String.format(
                "{\"status\":\"running\",\"activeConnections\":%d,\"activeUsers\":%d}",
                activeConnections, activeUsers
            );
        } catch (Exception e) {
            return "{\"status\":\"error\",\"message\":\"" + e.getMessage() + "\"}";
        }
    }
}
