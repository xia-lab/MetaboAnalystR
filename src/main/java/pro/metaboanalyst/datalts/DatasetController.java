/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.datalts;

import org.primefaces.model.StreamedContent;
import org.primefaces.model.DefaultStreamedContent;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.lts.FireProjectBean;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.project.ProjectModel;
import pro.metaboanalyst.utils.DataUtils;
import java.io.*;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.*;
import org.primefaces.model.file.UploadedFile;
import static pro.metaboanalyst.lts.FireBaseController.saveJsonStringToFile;
import pro.metaboanalyst.lts.HistoryBean;
import pro.metaboanalyst.lts.StateSaver;
import pro.metaboanalyst.rwrappers.RDataUtils;

/**
 *
 * @author zgy
 */
@SessionScoped
@Named("datasetController")
public class DatasetController implements Serializable {

    @Inject
    private DatabaseClient db;
    @Inject
    private SessionBean1 sb;
    @Inject
    private FireUserBean fub;
    @Inject
    private FireBaseController fc;
    @Inject
    private FireBase fb;
    @Inject
    private ApplicationBean1 ab;
    @Inject
    private FireProjectBean pb;

    private DatasetRow currentDatasetRow;

    public void initUserDatasets() {

        fc.reloadUserInfo();
        if (fub.getEmail() == null || fub.getEmail().equals("")) {// on local do not need to login
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "error", "Please login first!");
        } else {
            pb.setActiveTabIndex(0);
            //DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/xialabpro/ProjectView.xhtml");
            fc.setupProjectTable();
        }

    }

    public boolean setupDataTable() {

        try {
            ArrayList<HashMap<String, Object>> res = db.getProjectsFromPostgres(fub.getEmail(), ab.getAppName(), ab.getToolLocation());
            pb.setProjectTable(new ArrayList());
            if (res == null) {
                return false;
            }

            boolean loadProjectBool = false;
            for (HashMap<String, Object> myHashMap : res) {
                Map<String, Object> myMap = myHashMap;

                Object projectTypeObject = myMap.get("projecttype");
                if (projectTypeObject != null && "workflow".equals(projectTypeObject.toString())) {
                    continue; // Skip this project and move to the next iteration
                }
                System.out.println(projectTypeObject.toString() + "===projecttype");
                ProjectModel project = fc.createProjectFromMap(myMap);
                pb.getProjectTable().add(project);
            }
            if (loadProjectBool) {
                pb.settingProceedType("load");
            }
            return true;
        } catch (Exception e) {
            sb.addMessage("Error", "Failed to load project information!");
            e.printStackTrace();  // Logging the exception
            return false;
        }
    }

    public UUID insertAndSaveDataset(String title,
            int sampleNum,
            List<org.primefaces.model.file.UploadedFile> uploaded,
            List<String> roles) {
        try {
            // ---- guards ----
            if (uploaded == null || uploaded.isEmpty()) {
                sb.addMessage("Error", "No files were uploaded.");
                return null;
            }
            if (roles == null || roles.size() != uploaded.size()) {
                sb.addMessage("Error", "Roles list must match the number of files.");
                return null;
            }

            final String email = fub.getEmail();
            final String node = ab.getToolLocation();
            final java.time.OffsetDateTime now = java.time.OffsetDateTime.now();

            // ---- build file metadata (names & sizes BEFORE saving) ----
            List<DatasetFile> files = new ArrayList<>(uploaded.size());
            for (int i = 0; i < uploaded.size(); i++) {
                var uf = uploaded.get(i);
                if (uf == null || uf.getFileName() == null) {
                    continue;
                }

                String safe = uf.getFileName();
                String ext = extOf(safe);

                DatasetFile df = new DatasetFile();
                df.setRole(roles.get(i));                 // use roles exactly as provided
                df.setFilename(safe);                     // same name will be used when saving to disk
                df.setType(ext.isEmpty() ? "bin" : ext);
                df.setSizeBytes(Math.max(0L, uf.getSize()));
                df.setUploadedAt(now);
                files.add(df);
            }
            if (files.isEmpty()) {
                sb.addMessage("Error", "No valid uploaded files.");
                return null;
            }

            // ---- title fallback to first filename if blank ----
            String resolvedTitle = (title != null && !title.isBlank())
                    ? title.trim()
                    : files.get(0).getFilename();

            // ---- CALL YOUR SPECIALIZED INSERT FUNCTION (DB or API) ----
            String resp = db.insertDataset(email, node, resolvedTitle, sb.getAnalType(), sb.getDataType(),"metaboanalyst", sampleNum, files);

            UUID datasetId = extractUUID(resp);
            if (datasetId == null) {
                sb.addMessage("Error", "Insert failed: " + resp);
                return null;
            }

            saveDatasetFiles(email, datasetId, uploaded, roles);

            sb.addMessage("info", "Dataset created: " + resolvedTitle);
            return datasetId;

        } catch (Exception e) {
            sb.addMessage("Error", "Insert/save failed: " + e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    private static java.util.UUID extractUUID(String msg) {
        if (msg == null) {
            return null;
        }
        java.util.regex.Matcher m = java.util.regex.Pattern
                .compile("([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})")
                .matcher(msg);
        return m.find() ? java.util.UUID.fromString(m.group(1)) : null;
    }

    private static Path uniquePath(Path dir, String fileName) throws IOException {
        Path p = dir.resolve(fileName);
        if (!Files.exists(p)) {
            return p;
        }

        String ext = extOf(fileName);
        String base = ext.isEmpty() ? fileName : fileName.substring(0, fileName.length() - ext.length() - 1);
        for (int i = 2; i < 10_000; i++) {
            String candidate = ext.isEmpty() ? (base + " (" + i + ")") : (base + " (" + i + ")." + ext);
            Path q = dir.resolve(candidate);
            if (!Files.exists(q)) {
                return q;
            }
        }
        throw new IOException("Cannot create unique filename for " + fileName);
    }

    private static String extOf(String filename) {
        int dot = (filename == null) ? -1 : filename.lastIndexOf('.');
        return (dot > 0 && dot < filename.length() - 1)
                ? filename.substring(dot + 1).toLowerCase(Locale.ROOT)
                : "";
    }

    public List<DatasetFile> saveDatasetFiles(String userEmail,
            UUID dataSetID,
            List<UploadedFile> uploadedFiles,
            List<String> roles /* same size as uploadedFiles */) throws IOException {
        Objects.requireNonNull(userEmail, "userEmail is required");
        Objects.requireNonNull(dataSetID, "dataSetID is required");
        if (uploadedFiles == null || uploadedFiles.isEmpty()) {
            return Collections.emptyList();
        }
        if (roles == null || roles.size() != uploadedFiles.size()) {
            throw new IllegalArgumentException("roles must be provided and match uploadedFiles size");
        }

        Path projSubFolder = Paths.get(fb.getProjectPath(), "user_folders", userEmail, dataSetID.toString());
        Files.createDirectories(projSubFolder);

        List<DatasetFile> out = new ArrayList<>();
        for (int i = 0; i < uploadedFiles.size(); i++) {
            UploadedFile uf = uploadedFiles.get(i);
            String role = roles.get(i); // you control the role value

            if (uf == null || uf.getFileName() == null) {
                continue;
            }

            String original = uf.getFileName();
            String safeName = original;
            String ext = extOf(safeName);
            Path target = uniquePath(projSubFolder, safeName);

            try (InputStream in = uf.getInputStream()) {
                Files.copy(in, target);
            }

            DatasetFile df = new DatasetFile();
            df.setDatasetId(dataSetID);
            df.setRole(role); // <- no inference
            df.setFilename(target.getFileName().toString()); // saved name (may have suffix)
            df.setType(ext.isEmpty() ? "bin" : ext);
            df.setSizeBytes(Files.size(target));
            df.setUploadedAt(OffsetDateTime.now());
            out.add(df);
        }
        return out;
    }

    private List<DatasetRow> datasetTable = new ArrayList<>();

    private List<DatasetRow> selectedDatasets = new ArrayList<>();
    private DatasetRow selected;

    // optional cache for row-expansion
    private final Map<UUID, List<DatasetFile>> fileCache = new HashMap<>();

    /**
     * Called by Refresh button and can be wired to preRenderView.
     */
    public void refresh() {
        fc.reloadUserInfo();

        String email = fub.getEmail();
        String node = ab.getToolLocation();
        if (email == null || email.isBlank()) {
            datasetTable = new ArrayList<>();
            return;
        }
        datasetTable = db.getDatasetsForEmail(email, "metaboanalyst", true);
    }

    public void syncSelectedDatasets() {
        if (datasetTable == null) {
            selectedDatasets = java.util.Collections.emptyList();
            return;
        }
        selectedDatasets = datasetTable.stream()
                .filter(DatasetRow::isSelected)
                .collect(java.util.stream.Collectors.toList());
    }

    // -------- getters/setters used by the page --------
    public List<DatasetRow> getDatasetTable() {
        return datasetTable;
    }

    public void setDatasetTable(List<DatasetRow> t) {
        this.datasetTable = t;
    }

    public List<DatasetRow> getSelectedDatasets() {
        return selectedDatasets;
    }

    public void setSelectedDatasets(List<DatasetRow> s) {
        this.selectedDatasets = s;
    }

    public DatasetRow getSelected() {
        return selected;
    }

    public void setSelected(DatasetRow s) {
        this.selected = s;
    }

    /*
    DatasetFile data = new DatasetFile();
data.setRole("data");
data.setFilename("cohort_a.csv");
data.setType("csv");
data.setSizeBytes(1_234_567);

DatasetFile meta = new DatasetFile();
meta.setRole("metadata");
meta.setFilename("cohort_a_meta.json");
meta.setType("json");
meta.setSizeBytes(2_345);

String res = insertDataset("guangyan.zhou@mcgill.ca", "ca-east-1",
                           "Cohort_A", 48, java.util.List.of(data, meta));
     */
    // --- Staging store (session memory) ---
    private DatasetRow stagedDataset;                 // metadata summary
    private java.util.List<DatasetFile> stagedFiles = new java.util.ArrayList<>();

    public DatasetRow getStagedDataset() {
        return stagedDataset;
    }

    public java.util.List<DatasetFile> getStagedFiles() {
        return stagedFiles;
    }

    public boolean hasStagedDataset() {
        return stagedDataset != null && !stagedFiles.isEmpty();
    }

    public void clearStaged() {
        stagedDataset = null;
        stagedFiles.clear();
    }

    /**
     * Stage the dataset in memory only (no DB insert, no file save). Roles are
     * used exactly as provided.
     */
    public void stageDataset(String title,
            int sampleNum,
            java.util.List<org.primefaces.model.file.UploadedFile> uploaded,
            java.util.List<String> roles) {
        // Guards
        if (uploaded == null || uploaded.isEmpty()) {
            sb.addMessage("Error", "No files were uploaded.");
            return;
        }
        if (roles == null || roles.size() != uploaded.size()) {
            sb.addMessage("Error", "Roles list must match the number of files.");
            return;
        }

        final String email = fub.getEmail();
        final String node = ab.getToolLocation();
        final java.time.OffsetDateTime now = java.time.OffsetDateTime.now();

        // Build file metadata (from upload headers only; no disk write)
        java.util.List<DatasetFile> files = new java.util.ArrayList<>(uploaded.size());
        for (int i = 0; i < uploaded.size(); i++) {
            var uf = uploaded.get(i);
            if (uf == null || uf.getFileName() == null) {
                continue;
            }

            String safe = uf.getFileName();
            String ext = extOf(safe);

            DatasetFile df = new DatasetFile();
            df.setRole(roles.get(i));                         // use roles exactly as passed
            df.setFilename(safe);                             // intended saved name
            df.setType(ext.isEmpty() ? "bin" : ext);
            df.setSizeBytes(Math.max(0L, uf.getSize()));
            df.setUploadedAt(now);
            files.add(df);
        }
        if (files.isEmpty()) {
            sb.addMessage("Error", "No valid uploaded files.");
            return;
        }

        // Choose primary file for dataset-level summary (prefer role=data)
        DatasetFile primary = files.stream()
                .filter(f -> "data".equalsIgnoreCase(f.getRole()))
                .findFirst()
                .orElse(files.get(0));

        // Resolve title
        String resolvedTitle = (title != null && !title.isBlank()) ? title.trim() : primary.getFilename();

        // Fill DatasetRow (id stays null until actual DB insert)
        DatasetRow ds = new DatasetRow();
        ds.setId(null);
        ds.setEmail(email);
        ds.setNode(node);
        ds.setTitle(resolvedTitle);
        ds.setFilename(primary.getFilename());
        ds.setType(primary.getType());
        ds.setSizeBytes(primary.getSizeBytes());
        ds.setSamplenum(Math.max(0, sampleNum));
        ds.setUploadedAt(now);

        // Optional convenience fields if present in your model
        try {
            ds.setFileCount(files.size());
        } catch (Throwable ignore) {
        }
        try {
            ds.setHasMetadata(files.stream().anyMatch(f -> "metadata".equalsIgnoreCase(f.getRole())));
        } catch (Throwable ignore) {
        }
        try {
            ds.setFiles(files);
        } catch (Throwable ignore) {
        }

        // Store in session (single instance)
        this.stagedDataset = ds;
        this.stagedFiles.clear();
        this.stagedFiles.addAll(files);
        this.currentDatasetRow = ds; // if you already use this pointer elsewhere

        sb.addMessage("info", "Dataset staged in memory.");
    }

    public void stageDatasetFromStrings(String title, int sampleNum,
            java.util.List<String> fileNames,
            java.util.List<String> roles) {
        // ...guards unchanged...

        final String email = fub.getEmail();
        final String node = ab.getToolLocation();
        final java.time.OffsetDateTime now = java.time.OffsetDateTime.now();
        final java.nio.file.Path home = java.nio.file.Paths.get(sb.getCurrentUser().getHomeDir());

        java.util.List<DatasetFile> files = new java.util.ArrayList<>(fileNames.size());
        for (int i = 0; i < fileNames.size(); i++) {
            String fname = fileNames.get(i);
            if (fname == null || fname.isBlank()) {
                continue;
            }

            // strip any directories just in case
            String base = java.nio.file.Paths.get(fname).getFileName().toString();
            String ext = extOf(base);

            long size = 0L;
            try {
                java.nio.file.Path p = home.resolve(base);
                if (java.nio.file.Files.exists(p)) {
                    size = java.nio.file.Files.size(p);
                }
            } catch (Exception ignore) {
                // keep size 0 if not found; commit will still succeed
            }

            DatasetFile df = new DatasetFile();
            df.setRole(roles.get(i));
            df.setFilename(base);
            df.setType(ext.isEmpty() ? "bin" : ext);
            df.setSizeBytes(Math.max(0L, size));
            df.setUploadedAt(now);
            files.add(df);
        }

        if (files.isEmpty()) {
            sb.addMessage("Error", "No valid filenames.");
            return;
        }

        // prefer data → list → first
        DatasetFile primary = files.stream()
                .filter(f -> "data".equalsIgnoreCase(f.getRole()))
                .findFirst()
                .orElseGet(() -> files.stream()
                .filter(f -> "list".equalsIgnoreCase(f.getRole()))
                .findFirst()
                .orElse(files.get(0)));

        String resolvedTitle = (title != null && !title.isBlank()) ? title.trim() : primary.getFilename();

        DatasetRow ds = new DatasetRow();
        ds.setId(null);
        ds.setEmail(email);
        ds.setNode(node);
        ds.setTitle(resolvedTitle);
        ds.setFilename(primary.getFilename());
        ds.setType(primary.getType());
        ds.setSizeBytes(primary.getSizeBytes());  // now non-zero when file is in home
        ds.setSamplenum(Math.max(0, sampleNum));
        ds.setUploadedAt(now);

        try {
            ds.setFileCount(files.size());
        } catch (Throwable ignore) {
        }
        try {
            ds.setHasMetadata(files.stream().anyMatch(f -> "metadata".equalsIgnoreCase(f.getRole())));
        } catch (Throwable ignore) {
        }
        try {
            ds.setFiles(files);
        } catch (Throwable ignore) {
        }

        this.stagedDataset = ds;
        this.stagedFiles.clear();
        this.stagedFiles.addAll(files);
        this.currentDatasetRow = ds;

        sb.addMessage("info", "Dataset staged in memory.");
    }

    /**
     * Commit the staged dataset: perform DB insert (via your wrapper) and save
     * files to disk. NOTE: do NOT try to keep UploadedFile in session; pass it
     * again here.
     */
// Commit without needing UploadedFile again
    @Inject
    StateSaver stateSaver;

    @Inject
    private HistoryBean hb;

    public java.util.UUID commitStagedDataset() {
        if (!hasStagedDataset()) {
            sb.addMessage("Error", "No staged dataset to commit.");
            return null;
        }

        try {
            Path home = Paths.get(sb.getCurrentUser().getHomeDir());

            // --- Ensure/attach mSetObj_after_sanity.qs ---
            Path msetFile = home.resolve("mSetObj_after_sanity.qs");
            if (!Files.exists(msetFile)) {
                // Ask R to save; then re-resolve size
                RDataUtils.saveMsetObject(sb.getRConnection());
            }
            if (Files.exists(msetFile)) {
                DatasetFile df = new DatasetFile();
                df.setRole("supplement");
                df.setFilename("mSetObj_after_sanity.qs");
                df.setType("qs");
                df.setSizeBytes(Files.size(msetFile));
                df.setUploadedAt(OffsetDateTime.now());
                stagedFiles.add(df);
            } else {
                sb.addMessage("warn", "mSetObj_after_sanity.qs was not found after save request; continuing.");
            }

            stateSaver.saveState();

            // --- Ensure/attach java_history.json ---
            fc.saveJavaHistory();
            String jh = hb.getJavaHistoryString();
            saveJsonStringToFile(jh, sb.getCurrentUser().getOrigHomeDir() + File.separator + "java_history.json");
            Path javaHistoryFile = home.resolve("java_history.json");
            if (Files.exists(javaHistoryFile)) {
                DatasetFile df = new DatasetFile();
                df.setRole("javahistory");
                df.setFilename("java_history.json");
                df.setType("json");
                df.setSizeBytes(Files.size(javaHistoryFile));
                df.setUploadedAt(OffsetDateTime.now());
                stagedFiles.add(df);
            } else {
                sb.addMessage("warn", "java_history.json not found; continuing.");
            }

            // ---------- IMPORTANT: Insert ALL staged files (including supplement & javahistory) ----------
            java.util.List<DatasetFile> dbFiles = new java.util.ArrayList<>(stagedFiles);

            String resp = db.insertDataset(
                    stagedDataset.getEmail(),
                    stagedDataset.getNode(),
                    stagedDataset.getTitle(),
                    sb.getAnalType(),
                    sb.getDataType(),
                    "metaboanalyst",
                    stagedDataset.getSamplenum(),
                    dbFiles
            );
            java.util.UUID datasetId = extractUUID(resp);
            if (datasetId == null) {
                sb.addMessage("Error", "Insert failed: " + resp);
                return null;
            }

            // 2) Copy physical files into dataset folder (ALL staged files)
            java.util.List<Path> sources = resolveSourcePathsForStaged();
            if (sources.size() != stagedFiles.size()) {
                sb.addMessage("Error", "Cannot resolve source files for all staged entries.");
                return null;
            }
            copyStagedFilesToDatasetFolder(stagedDataset.getEmail(), datasetId, stagedFiles, sources);

            // Optional: if you display a "file count" in UI, compute a NON-counting view here
            // (just for UI state; DB already has all files).
            int displayCount = (int) stagedFiles.stream()
                    .map(DatasetFile::getRole)
                    .map(r -> r == null ? "" : r.toLowerCase())
                    .filter(r -> !r.equals("supplement") && !r.equals("javahistory"))
                    .count();
            try {
                stagedDataset.setFileCount(displayCount);
            } catch (Throwable ignore) {
            }

            sb.addMessage("info", "Dataset created: " + stagedDataset.getTitle());
            clearStaged();
            return datasetId;

        } catch (Exception e) {
            sb.addMessage("Error", "Commit failed: " + e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    private List<java.nio.file.Path> resolveSourcePathsForStaged() {
        List<java.nio.file.Path> guessed = new ArrayList<>();
        java.nio.file.Path home = java.nio.file.Paths.get(sb.getCurrentUser().getHomeDir());
        for (DatasetFile f : stagedFiles) {
            java.nio.file.Path p = home.resolve(f.getFilename());
            guessed.add(p);
        }
        return guessed;
    }

    private void copyStagedFilesToDatasetFolder(String email,
            java.util.UUID datasetId,
            List<DatasetFile> files,
            List<java.nio.file.Path> sources) throws java.io.IOException {
        java.nio.file.Path outDir = java.nio.file.Paths.get(
                fb.getProjectPath(), "user_folders", email, datasetId.toString()
        );
        java.nio.file.Files.createDirectories(outDir);

        for (int i = 0; i < files.size(); i++) {
            java.nio.file.Path src = sources.get(i);
            DatasetFile f = files.get(i);
            java.nio.file.Path dst = outDir.resolve(f.getFilename());
            if (!java.nio.file.Files.exists(src)) {
                throw new java.io.IOException("Source file missing: " + src);
            }
            java.nio.file.Files.copy(src, dst, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        }
    }

    public void stageDatasetFromPaths(String title,
            int sampleNum,
            java.util.List<DatasetFile> files,
            java.util.List<java.nio.file.Path> sourcePaths) {
        if (files == null || files.isEmpty() || sourcePaths == null || sourcePaths.size() != files.size()) {
            sb.addMessage("Error", "Staging failed: files/paths mismatch.");
            return;
        }
        final String email = fub.getEmail();
        final String node = ab.getToolLocation();
        final java.time.OffsetDateTime now = java.time.OffsetDateTime.now();

        DatasetFile primary = files.stream()
                .filter(f -> "data".equalsIgnoreCase(f.getRole()))
                .findFirst().orElse(files.get(0));

        DatasetRow ds = new DatasetRow();
        ds.setId(null);
        ds.setEmail(email);
        ds.setNode(node);
        ds.setTitle((title != null && !title.isBlank()) ? title.trim() : primary.getFilename());
        ds.setFilename(primary.getFilename());
        ds.setType(primary.getType());
        ds.setSizeBytes(primary.getSizeBytes());
        ds.setSamplenum(Math.max(0, sampleNum));
        ds.setUploadedAt(now);
        try {
            ds.setFileCount(files.size());
        } catch (Throwable ignore) {
        }
        try {
            ds.setHasMetadata(files.stream().anyMatch(f -> "metadata".equalsIgnoreCase(f.getRole())));
        } catch (Throwable ignore) {
        }
        try {
            ds.setFiles(files);
        } catch (Throwable ignore) {
        }

        this.stagedDataset = ds;
        this.stagedFiles.clear();
        this.stagedFiles.addAll(files);

        this.currentDatasetRow = ds;
    }

    public StreamedContent getDownloadSelected() {
        try {
            if (selected == null || selected.getId() == null || selected.getFilename() == null) {
                sb.addMessage("Error", "No dataset/file selected to download.");
                return null;
            }

            Path file = Paths.get(
                    fb.getProjectPath(),
                    "user_folders",
                    fub.getEmail(),
                    selected.getId().toString(),
                    selected.getFilename()
            );

            if (!Files.exists(file)) {
                sb.addMessage("Error", "File not found: " + selected.getFilename());
                return null;
            }

            String contentType = Files.probeContentType(file);
            if (contentType == null || contentType.isBlank()) {
                contentType = guessMimeByExt(selected.getFilename()); // fallback
            }

            final Path f = file;
            return DefaultStreamedContent.builder()
                    .name(selected.getFilename())
                    .contentType(contentType)
                    .stream(() -> {
                        try {
                            return Files.newInputStream(f); // <-- RETURN the InputStream
                        } catch (IOException ex) {
                            throw new UncheckedIOException(ex);
                        }
                    })
                    .build();

        } catch (Exception e) {
            sb.addMessage("Error", "Download failed: " + e.getMessage());
            return null;
        }
    }

    /**
     * very small fallback mapper when probeContentType() returns null
     */
    private static String guessMimeByExt(String name) {
        String n = name == null ? "" : name.toLowerCase();
        if (n.endsWith(".csv")) {
            return "text/csv";
        }
        if (n.endsWith(".tsv") || n.endsWith(".txt")) {
            return "text/plain";
        }
        if (n.endsWith(".zip")) {
            return "application/zip";
        }
        if (n.endsWith(".json")) {
            return "application/json";
        }
        if (n.endsWith(".xlsx")) {
            return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
        }
        if (n.endsWith(".xls")) {
            return "application/vnd.ms-excel";
        }
        if (n.endsWith(".mztab")) {
            return "text/plain";
        }
        return "application/octet-stream";
    }

    public void load(DatasetRow ds) {
        if (ds == null) {
            sb.addMessage("Error", "No dataset selected to load.");
            return;
        }

        try {
            // 0) Remember selection
            this.selected = ds;

            // 1) Login -> (re)creates a fresh working folder for the session
            sb.doLogin(ds.getDataType(), ds.getModule(), false, false);

            // 2) Resolve source (dataset storage) and destination (fresh work folder) paths
            Path srcDir = Paths.get(
                    fb.getProjectPath(), "user_folders",
                    ds.getEmail(), ds.getId().toString()
            ).normalize();

            Path srcZip = Paths.get(
                    fb.getProjectPath(), "user_folders",
                    ds.getEmail(), ds.getId().toString() + ".zip"
            ).normalize();

            Path dstDir = Paths.get(sb.getCurrentUser().getHomeDir()).normalize();
            Files.createDirectories(dstDir);

            int copied = 0;
            boolean restoredFromZip = false;

            // 3) Always copy whole dataset folder when it exists; ignore ds.getFiles()
            if (Files.isDirectory(srcDir)) {
                // 4a) Copy everything (recursively)
                copied = copyDirectoryRecursive(srcDir, dstDir);

            } else if (Files.isRegularFile(srcZip)) {
                // 4b) Fallback: local ZIP exists -> unzip into workspace
                unzipInto(srcZip, dstDir);
                restoredFromZip = true;
                copied = countRegularFiles(dstDir);

            } else {
                // 4c) Last resort: download from SAME server endpoint, then unzip
                // Build https://<node>.metaboanalyst.ca/<AppName> from ds.getNode()
                String node = ds.getNode();
                if (node == null || node.isBlank()) {
                    throw new IllegalArgumentException("Dataset node is missing.");
                }
                node = node.trim().replaceAll("\\.+$", ""); // strip trailing dots

                String baseUrl = "https://" + node + ".metaboanalyst.ca/";

                Path tempZip = Files.createTempFile(dstDir, "dataset-", ".zip");
                try {
                    downloadDatasetObjectFromBaseUrl(
                            baseUrl,
                            ds.getEmail(),
                            ds.getId().toString(),
                            tempZip.toString(),
                            null // bearer token if/when you secure the endpoint
                    );
                    unzipInto(tempZip, dstDir);
                    restoredFromZip = true;
                    copied = countRegularFiles(dstDir);
                } finally {
                    try {
                        Files.deleteIfExists(tempZip);
                    } catch (Exception ignore) {
                    }
                }
            }

            // 5) Load any java_history.json if present
            File histFile = new File(sb.getCurrentUser().getHomeDir(), "java_history.json");
            if (histFile.exists()) {
                try {
                    String javaHistory = fc.readJsonStringFromFile(histFile.getAbsolutePath());
                    int res1 = fc.loadJavaHistory(javaHistory);
                    sb.getNaviTrack().clear();
                    sb.setNaviType("NA");
                } catch (Exception hx) {
                    System.err.println("Failed to load java_history.json: " + hx.getMessage());
                }
            }

            // 6) Notify + redirect to module selection
            String mode = restoredFromZip ? "restored" : "loaded";
            sb.addMessage("info", "Workspace " + mode + " (" + copied + " file" + (copied == 1 ? "" : "s") + ").");
            DataUtils.doRedirectWithGrowl(
                    sb,
                    "/" + ab.getAppName() + "/Secure/ModuleSelectionView.xhtml",
                    "info",
                    "Dataset loaded, please select a module to start analysis."
            );

        } catch (Exception e) {
            sb.addMessage("Error", "Load failed: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /* ---------------- helpers ---------------- */
    private int copyDirectoryRecursive(Path srcDir, Path dstDir) throws IOException {
        final int[] count = {0};
        try (var walk = Files.walk(srcDir)) {
            for (Path p : (Iterable<Path>) walk::iterator) {
                if (Files.isDirectory(p)) {
                    continue;
                }
                Path rel = srcDir.relativize(p);
                Path dst = dstDir.resolve(rel).normalize();
                Files.createDirectories(dst.getParent());
                Files.copy(p, dst, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
                count[0]++;
            }
        }
        return count[0];
    }

    private void unzipInto(Path zipFile, Path dstDir) throws IOException {
        try (java.util.zip.ZipInputStream zis = new java.util.zip.ZipInputStream(new BufferedInputStream(Files.newInputStream(zipFile)))) {
            java.util.zip.ZipEntry entry;
            byte[] buffer = new byte[64 * 1024];
            while ((entry = zis.getNextEntry()) != null) {
                // Skip directories
                if (entry.isDirectory()) {
                    Path dir = dstDir.resolve(entry.getName()).normalize();
                    if (!dir.startsWith(dstDir)) {
                        zis.closeEntry();
                        continue; // guard
                    }
                    Files.createDirectories(dir);
                    zis.closeEntry();
                    continue;
                }
                Path out = dstDir.resolve(entry.getName()).normalize();
                if (!out.startsWith(dstDir)) { // guard against traversal
                    zis.closeEntry();
                    continue;
                }
                Files.createDirectories(out.getParent());
                try (OutputStream os = new BufferedOutputStream(Files.newOutputStream(out, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))) {
                    int r;
                    while ((r = zis.read(buffer)) != -1) {
                        os.write(buffer, 0, r);
                    }
                }
                zis.closeEntry();
            }
        }
    }

    private int countRegularFiles(Path root) throws IOException {
        try (var walk = Files.walk(root)) {
            int cnt = 0;
            for (Path p : (Iterable<Path>) walk::iterator) {
                if (Files.isRegularFile(p)) {
                    cnt++;
                }
            }
            return cnt;
        }
    }

    public void deleteSelected() {
        boolean res = db.deleteDatasetFilesFolderOk(selected.getId(), fub.getEmail());
        if (res) {
            sb.addMessage("info", "Successfully deleted!");
        } else {
            sb.addMessage("warn", "Failed to delete!");
        }
    }

    public String stageListDataset(String niceTitleHint) {
        try {
            final String home = sb.getCurrentUser().getHomeDir();
            final java.nio.file.Path lp = java.nio.file.Paths.get(home, "datalist.csv");
            if (!java.nio.file.Files.exists(lp)) {
                sb.addMessage("Error", "datalist.csv not found in your home directory.");
                return null;
            }

            String fname = "datalist.csv";
            String niceTitle = (niceTitleHint == null || niceTitleHint.isBlank())
                    ? "List_" + java.time.LocalDate.now()
                    : niceTitleHint;

            pro.metaboanalyst.datalts.DatasetFile lf = new pro.metaboanalyst.datalts.DatasetFile();
            lf.setRole("data");
            lf.setFilename(fname);
            lf.setType("csv");
            lf.setSizeBytes(java.nio.file.Files.size(lp));
            lf.setUploadedAt(java.time.OffsetDateTime.now());

            java.util.List<pro.metaboanalyst.datalts.DatasetFile> files = java.util.List.of(lf);
            java.util.List<java.nio.file.Path> src = java.util.List.of(lp);

            stageDatasetFromPaths(niceTitle, /*samples*/ 0, files, src);
            sb.addMessage("info", "List staged in memory. You can review and commit later.");
            return "Name check";
        } catch (Exception e) {
            sb.addMessage("Error", "Failed to stage list: " + e.getMessage());
            return null;
        }
    }

    public List<DatasetRow> getDatasetTableExample() {
        List<DatasetRow> datasetTableExample = new ArrayList<>();

        /*
            UUID.fromString("11111111-2222-4aaa-8bbb-cccccccccccc"),
    UUID.fromString("11111111-3333-4ddd-9eee-ffffffffffff"),
    UUID.fromString("11111111-4444-4111-abcd-1234567890ab"),
    UUID.fromString("11111111-5555-4f0f-b999-aaaaaaaaaaaa"),
    UUID.fromString("11111111-6666-4abc-8def-bbbbbbbbbbbb")
         */
        // Example 1
        DatasetRow ds1 = new DatasetRow();
        ds1.setId(UUID.fromString("11111111-00e3-4838-95ab-fec9d1d149e1"));
        ds1.setTitle("Concentration Table");
        ds1.setFilename("human_cachexia.csv");
        ds1.setType("csv");
        ds1.setSizeBytes(32461L);
        ds1.setUploadedAt(OffsetDateTime.now().minusDays(2));
        ds1.setEmail("example");
        ds1.setNode("vip2");
        ds1.setSamplenum(77);
        ds1.setModule("stat");
        ds1.setDataType("conc");
        ds1.setDescription("Urinary metabolite concentrations from 77 cancer patients measured by 1H NMR.");
        ds1.setTags(List.of("metabolomics", "experimentA"));
        ds1.setFileCount(1);
        ds1.setHasMetadata(true);

        List<DatasetFile> ds1Files = new ArrayList<>();
        {
            DatasetFile f1 = new DatasetFile();
            f1.setRole("data");
            f1.setFilename("human_cachexia.csv"); // main table
            f1.setType("csv");
            f1.setSizeBytes(32461L);
            ds1Files.add(f1);

        }
        ds1.setFiles(ds1Files);
        ds1.setFileCount(ds1Files.size());

        datasetTableExample.add(ds1);

        /////////////////////////////////////
        ds1 = new DatasetRow();
        ds1.setId(UUID.fromString("2292bf17-3ce9-43f1-81ea-6735b4cb7b96"));
        ds1.setTitle("Peaks with metadata table");
        ds1.setFilename("covid_metabolomics_data.csv");
        ds1.setType("csv");
        ds1.setSizeBytes(1468006L);
        ds1.setUploadedAt(OffsetDateTime.now().minusDays(2));
        ds1.setEmail("example");
        ds1.setNode("vip2");
        ds1.setSamplenum(59);
        ds1.setModule("mf");
        ds1.setDataType("pktable");
        ds1.setDescription("LC-MS peak intensity data table and meta-data of 20 healthy and 39 COVID-19 patient samples; ");
        ds1.setTags(List.of("metabolomics", "experimentA"));
        ds1.setFileCount(1);
        ds1.setHasMetadata(true);

        ds1Files = new ArrayList<>();
        {
            DatasetFile f1 = new DatasetFile();
            f1.setRole("data");
            f1.setFilename("covid_metabolomics_data.csv"); // main table
            f1.setType("csv");
            f1.setSizeBytes(1468006L);
            ds1Files.add(f1);

            DatasetFile f2 = new DatasetFile();
            f2.setRole("metadata");
            f2.setFilename("covid_metadata_multiclass.csv"); // main table
            f2.setType("csv");
            f2.setSizeBytes(1468006L);
            ds1Files.add(f2);
        }
        ds1.setFiles(ds1Files);
        ds1.setFileCount(ds1Files.size());

        datasetTableExample.add(ds1);

        ////////////////////////////////////////////////////////
        ds1 = new DatasetRow();
        ds1.setId(UUID.fromString("9a105202-0de6-4e24-b42e-dcf10b6ca562"));
        ds1.setTitle("List of Metabolites");
        ds1.setFilename("datalist.csv");
        ds1.setType("csv");
        ds1.setSizeBytes(226L);
        ds1.setUploadedAt(OffsetDateTime.now().minusDays(2));
        ds1.setEmail("example");
        ds1.setNode("vip2");
        ds1.setSamplenum(0);
        ds1.setModule("pathora");
        ds1.setDataType("list");
        ds1.setDescription("An example of metabolite list for testing purpose");
        ds1.setTags(List.of("metabolomics", "experimentA"));
        ds1.setFileCount(1);
        ds1.setHasMetadata(true);

        ds1Files = new ArrayList<>();
        {
            DatasetFile f1 = new DatasetFile();
            f1.setRole("data");
            f1.setFilename("datalist.csv"); // main table
            f1.setType("csv");
            f1.setSizeBytes(226L);
            ds1Files.add(f1);

        }
        ds1.setFiles(ds1Files);
        ds1.setFileCount(ds1Files.size());

        datasetTableExample.add(ds1);
        return datasetTableExample;
    }

    public void downloadDatasetObject(String serverLocation, String email, String datasetId, String destFilePath) {
        // serverLocation examples:
        //   "www"  -> https://www.metaboanalyst.ca/MetaboAnalyst/...
        //   "asia" -> https://asia.metaboanalyst.ca/MetaboAnalyst/...
        // If you want to pass a full base URL, see the overloaded method below.
        String baseUrl = "https://" + serverLocation + ".metaboanalyst.ca/";
        downloadDatasetObjectFromBaseUrl(baseUrl, email, datasetId, destFilePath, null);
    }

    /**
     * Overload that allows a full base URL (useful for dev/staging or
     * non-standard hosts), and an optional Bearer token for internal auth (pass
     * null if not used).
     *
     * Example baseUrl: https://www.metaboanalyst.ca/MetaboAnalyst
     * https://10.0.2.17:8443/MetaboAnalyst
     */
    public void downloadDatasetObjectFromBaseUrl(String baseUrl, String email, String datasetId, String destFilePath, String bearerToken) {
        File tempFile = null;
        try {
            // Ensure baseUrl has no trailing slash. It should already include your app context (e.g., https://www.metaboanalyst.ca/MetaboAnalyst)
            String base = baseUrl.replaceAll("/+$", "");
            // New servlet endpoint:
            String urlString = base + "/download/dataset"
                    + "?email=" + URLEncoder.encode(email, StandardCharsets.UTF_8)
                    + "&datasetId=" + URLEncoder.encode(datasetId, StandardCharsets.UTF_8);

            System.out.println("[downloadDataset] URL: " + urlString);

            HttpClient client = HttpClient.newBuilder()
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .connectTimeout(Duration.ofSeconds(20))
                    .build();

            HttpRequest.Builder reqBuilder = HttpRequest.newBuilder()
                    .uri(URI.create(urlString))
                    .timeout(Duration.ofMinutes(45)) // allow large on-the-fly zips
                    .header("Accept", "application/zip")
                    .header("Accept-Encoding", "identity") // avoid gzip of binary stream
                    .GET();

            if (bearerToken != null && !bearerToken.isBlank()) {
                reqBuilder.header("Authorization", "Bearer " + bearerToken);
            }

            HttpRequest request = reqBuilder.build();

            // Prepare destination (temp file alongside final for atomic move)
            Path destPath = Paths.get(destFilePath);
            Path destDir = destPath.getParent();
            if (destDir != null) {
                Files.createDirectories(destDir);
            }

            tempFile = File.createTempFile("dataset-download-", ".part",
                    destDir != null ? destDir.toFile() : null);
            tempFile.deleteOnExit();

            HttpResponse<InputStream> resp = client.send(request, HttpResponse.BodyHandlers.ofInputStream());
            int code = resp.statusCode();

            if (code != 200) {
                String err = "HTTP " + code;
                try (InputStream es = resp.body()) {
                    byte[] buf = es.readNBytes(4096);
                    if (buf != null && buf.length > 0) {
                        err += " - " + new String(buf, StandardCharsets.UTF_8);
                    }
                } catch (Exception ignore) {
                }
                throw new IOException("Remote server returned non-200 status: " + err);
            }

            try (InputStream in = resp.body()) {
                Files.copy(in, tempFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
            }

            long size = tempFile.length();
            if (size <= 0) {
                throw new IOException("Downloaded file is empty.");
            }

            try {
                Files.move(tempFile.toPath(), destPath, StandardCopyOption.ATOMIC_MOVE);
            } catch (AtomicMoveNotSupportedException ex) {
                Files.move(tempFile.toPath(), destPath, StandardCopyOption.REPLACE_EXISTING);
            }

            System.out.println("[downloadDataset] Saved to: " + destPath + " (" + size + " bytes)");

        } catch (Exception e) {
            System.err.println("[downloadDataset] Failed: " + e.getMessage());
            e.printStackTrace();
            if (tempFile != null && tempFile.exists()) {
                try {
                    Files.deleteIfExists(tempFile.toPath());
                } catch (IOException ignore) {
                }
            }
        }
    }

    // In DatasetController
// --- UI model for the dialog ---
    private List<DatasetFile> filesModel = new ArrayList<>();
    private long filesTotalBytes = 0;

    public List<DatasetFile> getFilesModel() {
        return filesModel;
    }

    public long getFilesTotalBytes() {
        return filesTotalBytes;
    }

    public String getFilesTotalHuman() {
        return DatasetRow.humanReadable(filesTotalBytes);
    }

    /**
     * Populate the Files dialog table for the given dataset. Call this before
     * showing PF('filesDialog').show().
     */
    public void populateFilesForDialog(DatasetRow ds) {
        if (ds == null) {
            sb.addMessage("Error", "No dataset selected.");
            return;
        }
        this.selected = ds;

        filesModel = new ArrayList<>();
        filesTotalBytes = 0L;

        try {
            // 1) If DatasetRow already has files, use them (and enrich size/mtime if missing)
            List<DatasetFile> predefined = ds.getFiles();
            if (predefined != null && !predefined.isEmpty()) {
                // Enrich from disk if possible (size/mtime), otherwise keep as-is
                Path root = Paths.get(fb.getProjectPath(), "user_folders", ds.getEmail(), ds.getId().toString()).normalize();
                if (Files.isDirectory(root) && root.startsWith(Paths.get(fb.getProjectPath(), "user_folders").normalize())) {
                    for (DatasetFile f : predefined) {
                        if (f == null || f.getFilename() == null || f.getFilename().isBlank()) {
                            continue;
                        }
                        Path p = root.resolve(f.getFilename()).normalize();
                        if (!p.startsWith(root)) {
                            continue; // guard
                        }
                        try {
                            if (Files.isRegularFile(p)) {
                                long sz = Files.size(p);
                                f.setSizeBytes(sz);
                                filesTotalBytes += sz;
                            }
                        } catch (Exception ignore) {
                        }
                        filesModel.add(f);
                    }
                } else {
                    // No disk access; just use the predefined list
                    for (DatasetFile f : predefined) {
                        if (f == null || f.getFilename() == null || f.getFilename().isBlank()) {
                            continue;
                        }
                        if (f.getSizeBytes() > 0) {
                            filesTotalBytes += f.getSizeBytes();
                        }
                        filesModel.add(f);
                    }
                }
            } else {
                // 2) No predefined list -> scan the dataset folder on disk
                Path userFolders = Paths.get(fb.getProjectPath(), "user_folders").normalize();
                Path root = userFolders.resolve(ds.getEmail()).resolve(ds.getId().toString()).normalize();

                if (!root.startsWith(userFolders)) {
                    sb.addMessage("Error", "Illegal dataset path.");
                    return;
                }

                if (Files.isDirectory(root)) {
                    try (var walk = Files.walk(root)) {
                        for (Path p : (Iterable<Path>) walk::iterator) {
                            if (Files.isDirectory(p)) {
                                continue;
                            }

                            String name = p.getFileName().toString();
                            if (name.equals(".DS_Store") || name.startsWith("._")) {
                                continue;
                            }

                            Path rel = root.relativize(p);
                            long sz = Files.size(p);
                            long lm = Files.getLastModifiedTime(p).toMillis();

                            DatasetFile f = new DatasetFile();
                            f.setFilename(rel.toString().replace(File.separatorChar, '/'));
                            f.setSizeBytes(sz);

                            filesModel.add(f);
                            filesTotalBytes += sz;
                        }
                    }
                    // keep UI tidy: sort by folder/name
                    filesModel.sort(java.util.Comparator.comparing(DatasetFile::getFilename, String.CASE_INSENSITIVE_ORDER));
                    // update derived count on the row, if you want
                    ds.setFileCount(filesModel.size());
                } else {
                    sb.addMessage("Warn", "Dataset folder not found on server.");
                }
            }

        } catch (Exception e) {
            sb.addMessage("Error", "Failed to list files: " + e.getMessage());
            e.printStackTrace();
        }
    }

    // in DatasetController (@ViewScoped or @SessionScoped)
    public void onFilesClick() {
        if (selected == null) {
            sb.addMessage("Error", "No dataset selected.");
            return;
        }
        populateFilesForDialog(selected);   // fills filesModel and totals
    }

}
