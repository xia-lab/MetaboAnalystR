package pro.metaboanalyst.workflows;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Set;

/**
 * Simple CORS filter for the workflow REST endpoints.
 * <p>
 * It locks down permitted origins, adds the required Access-Control-* headers,
 * and handles OPTIONS preflight requests while remaining async-safe for SSE.
 */
@WebFilter(urlPatterns = {"/api/*"}, asyncSupported = true)
public class ApiCorsFilter implements Filter {

    // TODO: replace with your actual UI host(s)
    private static final Set<String> ALLOWED_ORIGINS = Set.of(
        "https://pro.metaboanalyst.ca",
        "https://eu.metaboanalyst.ca",
        "https://as.metaboanalyst.ca",
        "https://vip.metaboanalyst.ca",
        "https://vip2.metaboanalyst.ca"
    );

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) req;
        HttpServletResponse response = (HttpServletResponse) res;

        String origin = request.getHeader("Origin");
        if (origin == null || origin.isBlank()) {
            origin = getServerOrigin(request);
        }
        System.out.println("[ApiCorsFilter] Incoming Origin=" + origin + " for " + request.getRequestURI());
        if (origin != null && isAllowedOrigin(origin, request)) {
            System.out.println("[ApiCorsFilter] Allowing Origin=" + origin);
            response.setHeader("Access-Control-Allow-Origin", origin);
            response.setHeader("Vary", "Origin");
            response.setHeader("Access-Control-Allow-Credentials", "true");
            response.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
            response.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
        }

        if ("OPTIONS".equalsIgnoreCase(request.getMethod())) {
            System.out.println("[ApiCorsFilter] Responding to preflight for " + origin);
            response.setStatus(HttpServletResponse.SC_NO_CONTENT);
            return;
        }

        chain.doFilter(req, res);
    }

    private String getServerOrigin(HttpServletRequest request) {
        String scheme = request.getScheme();
        String host = request.getServerName();
        int port = request.getServerPort();
        boolean isDefaultPort = (scheme.equals("http") && port == 80) || (scheme.equals("https") && port == 443);
        return scheme + "://" + host + (isDefaultPort ? "" : ":" + port);
    }

    private boolean isAllowedOrigin(String origin, HttpServletRequest request) {
        if (ALLOWED_ORIGINS.contains(origin)) {
            return true;
        }
        if (origin.startsWith("http://localhost:")) {
            return true;
        }
        if (origin.equalsIgnoreCase(getServerOrigin(request))) {
            return true;
        }
        return origin.startsWith("http://zgy-OptiPlex-7040.ht.home:");
    }
}
