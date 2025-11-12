package pro.metaboanalyst.workflows;

import jakarta.enterprise.inject.spi.CDI;
import jakarta.websocket.server.ServerEndpointConfig;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Configurator to enable CDI injection in WebSocket endpoints.
 * This allows @Inject to work in WorkflowWebSocketEndpoint.
 */
public class WorkflowWebSocketConfigurator extends ServerEndpointConfig.Configurator {

    private static final Logger LOGGER = Logger.getLogger(WorkflowWebSocketConfigurator.class.getName());

    // Static initializer to verify class is loaded
    static {
        LOGGER.info("[WS Configurator] ========== CLASS LOADED ==========");
    }

    public WorkflowWebSocketConfigurator() {
        super();
        LOGGER.info("[WS Configurator] ========== CONSTRUCTOR CALLED ==========");
    }

    @Override
    public <T> T getEndpointInstance(Class<T> endpointClass) throws InstantiationException {
        LOGGER.info("[WS Configurator] ========== CREATING ENDPOINT INSTANCE ==========");
        LOGGER.info("[WS Configurator] Endpoint class: " + endpointClass.getName());
        LOGGER.info("[WS Configurator] ClassLoader: " + endpointClass.getClassLoader());

        // Try CDI first
        try {
            LOGGER.info("[WS Configurator] Attempting CDI lookup...");
            T instance = CDI.current().select(endpointClass).get();
            LOGGER.info("[WS Configurator] ✓ Successfully created instance via CDI");
            return instance;
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "[WS Configurator] ✗ CDI instantiation failed: " + e.getMessage(), e);

            // Fallback to manual instantiation
            try {
                LOGGER.info("[WS Configurator] Attempting manual instantiation...");
                T instance = endpointClass.getDeclaredConstructor().newInstance();
                LOGGER.info("[WS Configurator] ✓ Successfully created instance via constructor");
                return instance;
            } catch (Exception ex) {
                LOGGER.log(Level.SEVERE, "[WS Configurator] ✗ Failed to instantiate endpoint", ex);
                throw new InstantiationException("Failed to instantiate endpoint: " + ex.getMessage());
            }
        }
    }
}
