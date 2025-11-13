package pro.metaboanalyst.workflows;

import jakarta.ejb.Singleton;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Workflow notification service.
 * Currently uses client-side polling for status updates.
 * This service is kept for future notification enhancements.
 */
@Singleton
public class WorkflowNotificationService {

    private static final Logger LOGGER = Logger.getLogger(WorkflowNotificationService.class.getName());

    /**
     * Called when workflow status changes.
     * With polling-based updates, this is a no-op.
     * Client polls the database every 10 seconds for current status.
     *
     * @param runData workflow run data
     * @param updates status updates
     */
    public void broadcastWorkflowUpdate(Map<String, Object> runData, Map<String, Object> updates) {
        // No-op: Client uses polling to fetch current workflow status
        if (LOGGER.isLoggable(java.util.logging.Level.FINE)) {
            int runId = extractRunId(runData, updates);
            String status = extractStatus(runData, updates);
            LOGGER.fine("Workflow update: runId=" + runId + ", status=" + status + " (polling-based)");
        }
    }

    private int extractRunId(Map<String, Object> runData, Map<String, Object> updates) {
        Object id = coalesce(
                runData != null ? coalesce(runData.get("runId"), runData.get("id")) : null,
                updates != null ? coalesce(updates.get("runId"), updates.get("id")) : null
        );
        return asInt(id);
    }

    private String extractStatus(Map<String, Object> runData, Map<String, Object> updates) {
        Object status = coalesce(
                updates != null ? updates.get("status") : null,
                runData != null ? runData.get("status") : null
        );
        return status != null ? status.toString() : "unknown";
    }

    private static Object coalesce(Object... values) {
        if (values == null) return null;
        for (Object value : values) {
            if (value != null) {
                if (value instanceof String && ((String) value).isBlank()) {
                    continue;
                }
                return value;
            }
        }
        return null;
    }

    private static int asInt(Object value) {
        if (value == null) return -1;
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        try {
            return Integer.parseInt(value.toString());
        } catch (NumberFormatException ignored) {
            return -1;
        }
    }
}
