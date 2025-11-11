package pro.metaboanalyst.workflows;

import jakarta.ws.rs.ApplicationPath;
import jakarta.ws.rs.core.Application;

/**
 * JAX-RS application configuration for workflow REST endpoints.
 *
 * This class activates JAX-RS for the /api/* URL path.
 * All @Path annotated resources will be automatically discovered
 * and made available under this context path.
 *
 * Example endpoints:
 * - /MetaboAnalyst/api/workflow/events (SSE stream)
 * - /MetaboAnalyst/api/workflow/status (health check)
 */
@ApplicationPath("/api")
public class RestApplication extends Application {
    // JAX-RS will auto-discover all @Path annotated classes in the application
    // No additional configuration needed
}
