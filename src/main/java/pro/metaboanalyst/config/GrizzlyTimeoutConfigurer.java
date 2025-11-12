package pro.metaboanalyst.config;

import jakarta.annotation.PostConstruct;
import jakarta.ejb.Singleton;
import jakarta.ejb.Startup;
import java.util.logging.Logger;

/**
 * Configures Payara Micro HTTP timeouts at application startup to support long-lived SSE connections.
 * Sets system properties that Payara's internal Grizzly HTTP server should respect.
 */
@Singleton
@Startup
public class GrizzlyTimeoutConfigurer {

    private static final Logger LOGGER = Logger.getLogger(GrizzlyTimeoutConfigurer.class.getName());

    @PostConstruct
    public void configurePayaraTimeouts() {
        LOGGER.info("[PayaraConfig] Setting HTTP timeout system properties for SSE support...");

        // Set all possible Grizzly/Payara timeout properties
        // Payara Micro uses Grizzly internally, so we set properties it may check
        String[][] properties = {
            {"org.glassfish.grizzly.IDLE_TIMEOUT", "900000"},
            {"org.glassfish.grizzly.http.IDLE_TIMEOUT", "900000"},
            {"grizzly.http.idle-timeout-delay", "900000"},
            {"fish.payara.http.idleTimeoutSeconds", "900"},
            {"payara.http.idle.timeout", "900000"},
            {"fish.payara.requestTimeoutSeconds", "-1"},
            {"org.glassfish.grizzly.Transport.IDLE_TIMEOUT", "900000"},
            {"grizzly.selector.timeout", "900000"}
        };

        for (String[] prop : properties) {
            String existing = System.getProperty(prop[0]);
            System.setProperty(prop[0], prop[1]);
            LOGGER.info(String.format("[PayaraConfig] Set %s=%s (was: %s)",
                prop[0], prop[1], existing == null ? "null" : existing));
        }

        LOGGER.info("[PayaraConfig] HTTP timeout properties configured");
    }
}
