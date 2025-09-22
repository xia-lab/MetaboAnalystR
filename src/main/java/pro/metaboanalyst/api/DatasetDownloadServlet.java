/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.api;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.inject.Inject;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.*;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import pro.metaboanalyst.lts.FireBase;

/**
 * Download a dataset as a ZIP stream.
 *
 * GET /download/dataset?email=<email>&datasetId=<uuid-or-folder>
 *
 * Looks under: <projectPath>/user_folders/<email>/<datasetId>[/ or .zip] - If
 * <datasetId>.zip exists, streams it directly - Else, zips the folder
 * <datasetId>/ on the fly
 */
@WebServlet(
        name = "DatasetDownloadServlet",
        urlPatterns = {"/download/*"}, // <— ROBUST PATH MAPPING
        loadOnStartup = 1 // log init at startup
)
public class DatasetDownloadServlet extends HttpServlet {

    @Override
    public void init() throws ServletException {
        super.init();
        getServletContext().log("[DatasetDownloadServlet] init — ready at /download/*");
    }

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        getServletContext().log("[DatasetDownloadServlet] GET " + req.getRequestURI()
                + (req.getQueryString() != null ? ("?" + req.getQueryString()) : "")
                + " ctx=" + req.getContextPath() + " pathInfo=" + req.getPathInfo());

        // Optional health check: /download/ping
        if ("/ping".equals(req.getPathInfo())) {
            resp.setContentType("text/plain");
            resp.getWriter().println("OK " + System.currentTimeMillis());
            return;
        }

        String email = req.getParameter("email");
        String datasetId = req.getParameter("datasetId");
        if (email == null || email.isBlank() || datasetId == null || datasetId.isBlank()) {
            resp.sendError(400, "Missing email or datasetId");
            return;
        }

        Path userFolders = Paths.get("/mnt/disks/launchpad/user_folders").normalize();
        Path dsFolder = userFolders.resolve(email).resolve(datasetId).normalize();
        Path dsZip = userFolders.resolve(email).resolve(datasetId + ".zip").normalize();

        if (!dsFolder.startsWith(userFolders) || !dsZip.startsWith(userFolders)) {
            resp.sendError(400, "Illegal path");
            return;
        }

        if (Files.isRegularFile(dsZip)) {
            streamExistingZip(dsZip, resp);
            return;
        }

        if (!Files.isDirectory(dsFolder)) {
            resp.sendError(404, "Dataset not found");
            return;
        }

        String outName = datasetId + ".zip";
        try {
            resp.setBufferSize(64 * 1024);
        } catch (IllegalStateException ignore) {
        }
        resp.setContentType("application/zip");
        resp.setHeader("Content-Disposition",
                "attachment; filename=\"" + outName + "\"; filename*=UTF-8''" + URLEncoder.encode(outName, StandardCharsets.UTF_8));

        byte[] buf = new byte[64 * 1024];
        int files = 0;
        long bytes = 0;

        try (ZipOutputStream zos = new ZipOutputStream(new BufferedOutputStream(resp.getOutputStream()))) {
            try (var walk = Files.walk(dsFolder)) {
                for (Path p : (Iterable<Path>) walk::iterator) {
                    if (Files.isDirectory(p)) {
                        continue;
                    }
                    String name = p.getFileName().toString();
                    if (name.equals(".DS_Store") || name.startsWith("._")) {
                        continue;
                    }

                    Path rel = dsFolder.relativize(p);
                    String entryName = rel.toString().replace(File.separatorChar, '/');

                    zos.putNextEntry(new ZipEntry(entryName));
                    try (InputStream in = new BufferedInputStream(Files.newInputStream(p))) {
                        int r;
                        while ((r = in.read(buf)) != -1) {
                            zos.write(buf, 0, r);
                            bytes += r;
                        }
                    }
                    zos.closeEntry();
                    files++;
                }
            }
            zos.finish();
            zos.flush();
        }
        getServletContext().log("[DatasetDownloadServlet] streamed " + files + " files (" + bytes + " bytes)");
        try {
            resp.flushBuffer();
        } catch (Exception ignore) {
        }
    }

    private static void streamExistingZip(Path zip, HttpServletResponse resp) throws IOException {
        String fn = zip.getFileName().toString();
        try {
            resp.setBufferSize(64 * 1024);
        } catch (IllegalStateException ignore) {
        }
        resp.setContentType("application/zip");
        resp.setHeader("Content-Disposition",
                "attachment; filename=\"" + fn + "\"; filename*=UTF-8''" + URLEncoder.encode(fn, StandardCharsets.UTF_8));

        long len = Files.size(zip);
        if (len <= Integer.MAX_VALUE) {
            resp.setContentLength((int) len);
        }

        try (InputStream in = new BufferedInputStream(Files.newInputStream(zip)); OutputStream out = resp.getOutputStream()) {
            byte[] buf = new byte[64 * 1024];
            int r;
            while ((r = in.read(buf)) != -1) {
                out.write(buf, 0, r);
            }
            out.flush();
        }
        try {
            resp.flushBuffer();
        } catch (Exception ignore) {
        }
    }

    private static String urlEncode(String s) {
        return URLEncoder.encode(s, StandardCharsets.UTF_8);
    }

    private static void sendError(HttpServletResponse resp, int code, String msg) throws IOException {
        if (!resp.isCommitted()) {
            resp.reset();
            resp.sendError(code, msg);
        }
    }

}
