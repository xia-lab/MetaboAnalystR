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
            String resp = db.insertDataset(email, node, resolvedTitle, sb.getAnalType(), sb.getDataType(), sampleNum, files);

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
        datasetTable = db.getDatasetsForEmail(email, true);
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

    /**
     * Used by the rowExpansion table:
     * <p:dataTable value="#{dataManagerBean.getFiles(ds.id)}" ...>
     */
    public List<DatasetFile> getFiles(UUID datasetId) {
        return fileCache.computeIfAbsent(datasetId, db::getDatasetFiles);
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

            // 1) Login -> this typically (re)creates a fresh working folder for the session
            sb.doLogin(ds.getDataType(), ds.getModule(), false, false);

            // 2) Resolve source (dataset storage) and destination (fresh work folder) paths
            //    Source convention: <project>/user_folders/<email>/<datasetId>/
            java.nio.file.Path srcDir = java.nio.file.Paths.get(
                    fb.getProjectPath(), "user_folders",
                    ds.getEmail(), ds.getId().toString()
            );

            //    Destination: whatever folder doLogin prepared for this session/user
            //    (if your API differs, adjust accordingly)
            java.nio.file.Path dstDir = java.nio.file.Paths.get(sb.getCurrentUser().getHomeDir());
            java.nio.file.Files.createDirectories(dstDir);

            // 3) Determine which files to copy
            java.util.List<DatasetFile> filesToCopy = ds.getFiles();

            int copied = 0;

            if (filesToCopy != null && !filesToCopy.isEmpty()) {
                // 4a) Copy only the registered files
                for (DatasetFile f : filesToCopy) {
                    if (f == null || f.getFilename() == null || f.getFilename().isBlank()) {
                        continue;
                    }
                    java.nio.file.Path src = srcDir.resolve(f.getFilename());
                    java.nio.file.Path dst = dstDir.resolve(f.getFilename());
                    if (java.nio.file.Files.exists(src)) {
                        java.nio.file.Files.createDirectories(dst.getParent());
                        java.nio.file.Files.copy(src, dst, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                        copied++;
                    }
                }
            } else {

                if (java.nio.file.Files.isDirectory(srcDir)) {
                    try (java.nio.file.DirectoryStream<java.nio.file.Path> stream
                            = java.nio.file.Files.newDirectoryStream(srcDir)) {
                        for (java.nio.file.Path p : stream) {
                            if (java.nio.file.Files.isRegularFile(p)) {
                                java.nio.file.Path dst = dstDir.resolve(p.getFileName().toString());
                                java.nio.file.Files.copy(p, dst, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                                copied++;
                            }
                        }
                    }
                }
            }

            File histFile = new File(sb.getCurrentUser().getHomeDir(), "java_history.json");
            if (histFile.exists()) {
                String javaHistory = fc.readJsonStringFromFile(histFile.getAbsolutePath());
                int res1 = fc.loadJavaHistory(javaHistory);
                System.out.println("javahistoryloaded=======");
            }

            // 6) Notify + redirect to module selection
            sb.addMessage("info", "Loaded dataset files into workspace (" + copied + " file" + (copied == 1 ? "" : "s") + ").");
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
}
