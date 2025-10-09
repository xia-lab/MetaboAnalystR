package pro.metaboanalyst.api;

import jakarta.inject.Inject;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;
import java.io.IOException;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.utils.DataUtils; // if you have messaging helpers

@WebServlet(name = "DatasetDeleteServlet", urlPatterns = {"/dataset/delete"}, loadOnStartup = 1)
public class DatasetDeleteServlet extends HttpServlet {

    @Inject
    private FireBase fb;

    @Override
    public void init() throws ServletException {
        super.init();
        getServletContext().log("[DatasetDeleteServlet] init — ready at /dataset/delete");
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        // Expected params:
        // email=<owner>  datasetId=<uuid>  file=<filename or empty>  hard=<true|false>
        String email = req.getParameter("email");
        String datasetId = req.getParameter("datasetId");
        String file = req.getParameter("file");        // optional; if absent -> delete whole dataset folder
        boolean hard = Boolean.parseBoolean(req.getParameter("hard")); // optional, default soft delete

        if (isBlank(email) || isBlank(datasetId)) {
            sendError(resp, 400, "Missing email or datasetId");
            return;
        }

        // (Optional) Authorization guard (owner or admin). Replace with your own session check.
        /*
        HttpSession session = req.getSession(false);
        String sessionEmail = (session != null) ? (String) session.getAttribute("userEmail") : null;
        if (sessionEmail == null || (!sessionEmail.equalsIgnoreCase(email) && !isAdmin(session))) {
            sendError(resp, 403, "Forbidden");
            return;
        }
        */
        
        Path root = Paths.get(fb.getProjectPath(), "user_folders").normalize();
        Path dsFolder = root.resolve(email).resolve(datasetId).normalize();
        Path dsZip = root.resolve(email).resolve(datasetId + ".zip").normalize();

        if (!dsFolder.startsWith(root) || !dsZip.startsWith(root)) {
            sendError(resp, 400, "Illegal path");
            return;
        }
        if (!Files.exists(dsFolder)) {
            sendError(resp, 404, "Dataset folder not found");
            return;
        }

        // Invalidate any cached ZIP so downloads won’t serve stale content
        try { Files.deleteIfExists(dsZip); } catch (Exception ignore) {}

        // Soft delete moves to .trash; hard delete removes immediately
        if (isBlank(file)) {
            // Delete entire dataset folder
            if (hard) {
                deleteRecursively(dsFolder);
            } else {
                moveToTrash(root, dsFolder);
            }
            writeJson(resp, 200, "{\"status\":\"ok\",\"deleted\":\"dataset\"}");
            return;
        }

        // Single file delete (ensure filename is safe)
        if (!isSafeFilename(file)) {
            sendError(resp, 400, "Illegal filename");
            return;
        }
        Path target = dsFolder.resolve(file).normalize();
        if (!target.startsWith(dsFolder) || !Files.exists(target) || Files.isDirectory(target)) {
            sendError(resp, 404, "File not found");
            return;
        }

        if (hard) {
            Files.delete(target);
        } else {
            Path trashBase = root.resolve(".trash").resolve(email).resolve(datasetId);
            Files.createDirectories(trashBase);
            String stamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"));
            Path trashTarget = trashBase.resolve(file + "." + stamp + ".deleted");
            // same filesystem move preferred
            try {
                Files.move(target, trashTarget, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            } catch (AtomicMoveNotSupportedException amnse) {
                Files.move(target, trashTarget, StandardCopyOption.REPLACE_EXISTING);
            }
        }

        // (Optional) DB bookkeeping: remove from DatasetFile table
        // db.deleteDatasetFile(UUID.fromString(datasetId), file);

        writeJson(resp, 200, "{\"status\":\"ok\",\"deleted\":\"file\",\"name\":\"" + escJson(file) + "\"}");
    }

    // ---------- helpers ----------

    private static boolean isBlank(String s) { return s == null || s.isBlank(); }
    private static boolean isAdmin(HttpSession s) {
        Object role = (s != null) ? s.getAttribute("role") : null;
        return role != null && "admin".equals(role.toString());
    }

    private static boolean isSafeFilename(String name) {
        // Disallow path traversal and separators
        return name != null
            && !name.contains("..")
            && !name.contains("/") && !name.contains("\\")
            && name.chars().noneMatch(ch -> ch <= 31);
    }

    private void moveToTrash(Path root, Path dsFolder) throws IOException {
        String email = dsFolder.getParent().getFileName().toString();
        String datasetId = dsFolder.getFileName().toString();
        Path trashBase = root.resolve(".trash").resolve(email);
        Files.createDirectories(trashBase);
        String stamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"));
        Path trashTarget = trashBase.resolve(datasetId + "." + stamp);
        try {
            Files.move(dsFolder, trashTarget, StandardCopyOption.ATOMIC_MOVE);
        } catch (AtomicMoveNotSupportedException amnse) {
            // fallback: copy then delete
            copyRecursively(dsFolder, trashTarget);
            deleteRecursively(dsFolder);
        }
    }

    private static void deleteRecursively(Path p) throws IOException {
        if (!Files.exists(p)) return;
        try (var walk = Files.walk(p)) {
            walk.sorted((a,b) -> b.getNameCount() - a.getNameCount()).forEach(path -> {
                try { Files.deleteIfExists(path); } catch (IOException ignored) {}
            });
        }
    }

    private static void copyRecursively(Path src, Path dst) throws IOException {
        try (var walk = Files.walk(src)) {
            for (Path p : (Iterable<Path>) walk::iterator) {
                Path rel = src.relativize(p);
                Path q = dst.resolve(rel);
                if (Files.isDirectory(p)) {
                    Files.createDirectories(q);
                } else {
                    Files.createDirectories(q.getParent());
                    Files.copy(p, q, StandardCopyOption.REPLACE_EXISTING);
                }
            }
        }
    }

    private static void writeJson(HttpServletResponse resp, int code, String json) throws IOException {
        resp.setStatus(code);
        resp.setContentType("application/json");
        resp.setCharacterEncoding("UTF-8");
        resp.getWriter().write(json);
    }

    private static void sendError(HttpServletResponse resp, int code, String msg) throws IOException {
        if (!resp.isCommitted()) {
            resp.reset();
            writeJson(resp, code, "{\"status\":\"error\",\"message\":\"" + escJson(msg) + "\"}");
        }
    }

    private static String escJson(String s) { return s == null ? "" : s.replace("\\", "\\\\").replace("\"", "\\\""); }
}
