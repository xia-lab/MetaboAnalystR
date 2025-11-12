package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import jakarta.ejb.Singleton;
import jakarta.websocket.Session;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

@Singleton
public class WorkflowNotificationService {

    private static final Logger LOGGER = Logger.getLogger(WorkflowNotificationService.class.getName());
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final ConcurrentHashMap<String, CopyOnWriteArrayList<Session>> webSocketListeners = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<Integer, String> lastSentStatus = new ConcurrentHashMap<>();
    private final AtomicBoolean initialized = new AtomicBoolean(false);

    public void subscribeWebSocket(String userId, jakarta.websocket.Session session) {
        if (userId == null || session == null) return;
        webSocketListeners.computeIfAbsent(userId, unused -> new CopyOnWriteArrayList<>()).add(session);
        if (initialized.compareAndSet(false, true)) {
            LOGGER.info("WorkflowNotificationService initialized (direct broadcast mode).");
        }
        LOGGER.info("WebSocket subscribed: " + userId + " (total sessions: "
                + webSocketListeners.getOrDefault(userId, new CopyOnWriteArrayList<>()).size() + ")");
    }

    public void unsubscribeWebSocket(String userId, jakarta.websocket.Session session) {
        CopyOnWriteArrayList<Session> sessions = webSocketListeners.get(userId);
        if (sessions == null) return;
        sessions.removeIf(s -> s.getId().equals(session.getId()));
        if (sessions.isEmpty()) {
            webSocketListeners.remove(userId);
            LOGGER.info("WebSocket unsubscribed: " + userId);
        }
    }

    public int getActiveConnectionCount() {
        return webSocketListeners.values().stream().mapToInt(CopyOnWriteArrayList::size).sum();
    }

    public void broadcastWorkflowUpdate(Map<String, Object> runData, Map<String, Object> updates) {
        if (!initialized.get()) {
            initialized.set(true);
            LOGGER.info("WorkflowNotificationService initialized (direct broadcast only).");
        }

        if ((runData == null || runData.isEmpty()) && (updates == null || updates.isEmpty())) {
            LOGGER.fine("[NOTIFY] Skipping broadcast: no data provided.");
            return;
        }

        int runId = asInt(coalesce(
                runData != null ? coalesce(runData.get("runId"), runData.get("id")) : null,
                updates != null ? coalesce(updates.get("runId"), updates.get("id")) : null
        ));

        String userId = asString(coalesce(
                runData != null ? coalesce(runData.get("userId"), runData.get("email")) : null,
                updates != null ? updates.get("userId") : null
        ));

        if (userId == null || userId.isBlank()) {
            LOGGER.fine("[NOTIFY] Skipping broadcast for runId=" + runId + " (missing userId)");
            return;
        }

        String status = asString(coalesce(
                updates != null ? updates.get("status") : null,
                runData != null ? runData.get("status") : null
        ));
        if (status == null) {
            status = "updated";
        }

        String projectId = asString(coalesce(
                updates != null ? coalesce(updates.get("projectId"), updates.get("project_id")) : null,
                runData != null ? coalesce(runData.get("projectId"), runData.get("project_id")) : null
        ));

        ObjectNode node = OBJECT_MAPPER.createObjectNode();
        node.put("type", "workflow-status");
        if (runId > 0) {
            node.put("runId", runId);
        }
        node.put("userId", userId);
        node.put("status", status);
        if (projectId != null) {
            node.put("projectId", projectId);
        }

        if (runData != null) {
            putIfPresent(node, "lastUpdated", coalesce(runData.get("lastUpdated"), runData.get("last_updated")));
            putIfPresent(node, "startDate", coalesce(runData.get("startDate"), runData.get("start_date")));
            putIfPresent(node, "finishDate", coalesce(runData.get("finishDate"), runData.get("finish_date")));
        }

        dispatchNotification(runId, status, userId, node.toString());
    }

    private void dispatchNotification(int runId, String status, String userId, String payload) {
        if (runId > 0) {
            String prior = lastSentStatus.get(runId);
            if (status.equals(prior)) {
                LOGGER.info("[NOTIFY] Skipping duplicate status for runId=" + runId + ", status=" + status);
                return;
            }
            lastSentStatus.put(runId, status);
        }

        int sent = 0;
        CopyOnWriteArrayList<Session> sessions = webSocketListeners.get(userId);
        if (sessions != null && !sessions.isEmpty()) {
            LOGGER.info("[NOTIFY] Broadcasting to " + sessions.size() + " WebSocket client(s)");
            for (jakarta.websocket.Session session : sessions) {
                try {
                    if (!session.isOpen()) {
                        sessions.remove(session);
                        continue;
                    }
                    session.getBasicRemote().sendText(payload);
                    sent++;
                    LOGGER.info("[NOTIFY] Sent to WebSocket session: " + session.getId());
                } catch (Exception ex) {
                    LOGGER.log(Level.WARNING, "Failed to send WebSocket; removing session", ex);
                    sessions.remove(session);
                }
            }
        }
        LOGGER.info("[NOTIFY] Delivered to " + sent + " total client(s) for user=" + userId);
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

    private static String asString(Object value) {
        if (value == null) return null;
        String s = value.toString();
        return s.isBlank() ? null : s;
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

    private static void putIfPresent(ObjectNode node, String field, Object value) {
        if (node == null || value == null) {
            return;
        }

        if (value instanceof Integer) {
            node.put(field, (Integer) value);
        } else if (value instanceof Long) {
            node.put(field, (Long) value);
        } else if (value instanceof Float) {
            node.put(field, (Float) value);
        } else if (value instanceof Double) {
            node.put(field, (Double) value);
        } else if (value instanceof Number) {
            node.put(field, ((Number) value).doubleValue());
        } else if (value instanceof Boolean) {
            node.put(field, (Boolean) value);
        } else {
            node.put(field, value.toString());
        }
    }
}
