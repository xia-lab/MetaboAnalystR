/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.datalts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.annotation.PostConstruct;
import org.primefaces.model.StreamedContent;
import org.primefaces.model.DefaultStreamedContent;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
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
import pro.metaboanalyst.models.SheetRow;
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
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.*;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import static pro.metaboanalyst.lts.FireBaseController.saveJsonStringToFile;
import pro.metaboanalyst.lts.HistoryBean;
import pro.metaboanalyst.lts.StateSaver;
import pro.metaboanalyst.models.StickyDTO;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.extensions.component.sheet.Sheet;
import org.primefaces.extensions.event.SheetEvent;
import org.primefaces.extensions.model.sheet.SheetUpdate;
import pro.metaboanalyst.controllers.mummichog.PeakUploadBean;
import pro.metaboanalyst.rwrappers.SearchUtils;

/**
 *
 * @author zgy
 */
@SessionScoped
@Named("datasetController")
public class DatasetController implements Serializable {

    private DatasetRow selected;
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
    @Inject
    private HistoryBean hb;
    @Inject
    StateSaver stateSaver;
    @Inject
    PeakUploadBean pu;

    // In DatasetController.java
    private List<DatasetRow> datasetTableAll = new ArrayList<>();

    public List<DatasetRow> getDatasetTableAll() {
        return datasetTableAll;
    }

    private List<DatasetRow> datasetTableExample = new ArrayList<>();

    public void initUserDatasets() {

        fc.reloadUserInfo();
        if (fub.getEmail() == null || fub.getEmail().equals("")) {// on local do not need to login
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "error", "Please login first!");
        } else {
            pb.setActiveTabIndex(0);
            //DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/xialabpro/ProjectView.xhtml");
            fc.setupProjectTable("project");
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
            String resp = db.insertDataset(email, node, resolvedTitle, sb.getAnalType(), sb.getDataType(), "metaboanalyst", sampleNum, files);

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

    // optional cache for row-expansion
    private final Map<UUID, List<DatasetFile>> fileCache = new HashMap<>();

    /**
     * Called by Refresh button and can be wired to preRenderView.
     */
    public void refresh() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }
        fc.reloadUserInfo();
        reloadTable();

    }

    public void reloadTable() {
        final String email = fub.getEmail();
        final String node = ab.getToolLocation();

        if (email == null || email.isBlank()) {
            datasetTable = new ArrayList<>();
            datasetTableAll = new ArrayList<>(getDatasetTableExample()); // show examples if no login
            return;
        }

        // Load user datasets
        datasetTable = db.getDatasetsForEmail(email, "metaboanalyst", true);
        if (datasetTable == null) {
            datasetTable = new ArrayList<>();
        }

        // In refresh() after loading user datasets:
        for (DatasetRow ds : datasetTable) {
            ds.setOrigin("Custom"); // user datasets show no badge/label
        }

        // Merge: Examples + Custom
        final List<DatasetRow> examples = getDatasetTableExample(); // already cached by your guard

        datasetTableAll = new ArrayList<>(examples.size() + datasetTable.size());
        datasetTableAll.addAll(examples);
        datasetTableAll.addAll(datasetTable);

        System.out.println("[refresh] merged datasets: total=" + datasetTableAll.size()
                + ", examples=" + examples.size() + ", custom=" + datasetTable.size());
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

        sb.addMessage("info", "Dataset staged in memory.");
    }

    public void stageDatasetFromStrings(String title, int sampleNum,
            java.util.List<String> fileNames,
            java.util.List<String> roles) {

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

        sb.addMessage("info", "Dataset staged in memory.");
    }

    /**
     * Commit the staged dataset: perform DB insert (via your wrapper) and save
     * files to disk. NOTE: do NOT try to keep UploadedFile in session; pass it
     * again here.
     */
// Commit without needing UploadedFile again
    public java.util.UUID commitStagedDataset() {
        if (!hasStagedDataset()) {
            sb.addMessage("Error", "No staged dataset to commit.");
            return null;
        }

        try {
            Path home = Paths.get(sb.getCurrentUser().getHomeDir());

            // --- Ensure/attach mSetObj_after_sanity.qs ---
            Path msetFile = home.resolve("RloadSanity.RData");
            if (!Files.exists(msetFile)) {
                // Ask R to save; then re-resolve size
                RCenter.saveRLoadImgCustom(sb.getRConnection(), "RloadSanity.RData");
            }
            if (Files.exists(msetFile)) {
                DatasetFile df = new DatasetFile();
                df.setRole("supplement");
                df.setFilename("RloadSanity.RData");
                df.setType("qs");
                df.setSizeBytes(Files.size(msetFile));
                df.setUploadedAt(OffsetDateTime.now());
                stagedFiles.add(df);
            } else {
                sb.addMessage("warn", "RloadSanity.RData was not found after save request; continuing.");
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
            //clearStaged();
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
        System.out.println("staged========");
        System.out.println("stagedFiles========" + stagedFiles.size());

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

    public void loadDefault(DatasetRow ds) {
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

    public boolean handleDataset(DatasetRow ds) {
        try {
            if (ds == null) {
                sb.addMessage("Error", "No dataset selected.");
                return false;
            }
            if (ds.getFiles() == null || ds.getFiles().isEmpty()) {
                sb.addMessage("Error", "Selected dataset has no files.");
                return false;
            }

            // --- Gather filenames by role ---
            String dataName = null, data2Name = null, metaName = null, listName = null, listGeneName = null, ms2Name = null, rawName = null;
            for (DatasetFile f : ds.getFiles()) {
                String role = f.getRole() == null ? "" : f.getRole().toLowerCase();
                switch (role) {
                    case "data" ->
                        dataName = f.getFilename();
                    case "data2" ->
                        data2Name = f.getFilename();
                    case "metadata" ->
                        metaName = f.getFilename();
                    case "list" ->
                        listName = f.getFilename();
                    case "listgene" ->
                        listGeneName = f.getFilename();
                    case "ms2" ->
                        ms2Name = f.getFilename();
                    case "raw" ->
                        rawName = f.getFilename();
                }
            }

            // --- Decide analysis type from dataset/module (fallback to existing) ---
            String analType = (ds.getModule() != null && !ds.getModule().isBlank())
                    ? ds.getModule()
                    : (sb.getAnalType() != null ? sb.getAnalType() : "stat");
            sb.setAnalType(analType);

            final String dataType = sb.getDataType() == null ? "" : sb.getDataType().toLowerCase();

            // --- Init R session for this analysis ---
            RConnection RC = sb.getRConnection();
            RDataUtils.initDataObjects(RC, sb.getDataType(), analType, sb.isPaired());

            // --- Smart loader: choose mzTab vs text by filename/type ---
            java.util.function.BiFunction<RConnection, String, Boolean> loadSmart = (rc, fname) -> {
                if (fname == null) {
                    return false;
                }
                String lower = fname.toLowerCase();
                if (lower.endsWith(".mztab") || "mztab".equalsIgnoreCase(ds.getType())) {
                    return RDataUtils.readMzTabDataReload(rc, fname);
                } else {
                    return RDataUtils.readTextDataReload(rc, fname);
                }
            };

            boolean ok = true;

            // --- Module-specific behavior (aligned with openModuleRC) ---
            switch (analType) {

                // Data-only modules; metadata optional (will be read if present)
                case "stat":
                case "roc":
                case "power":
                case "dose":
                case "mnet": {
                    if (dataName == null) {
                        sb.addMessage("Error", "No data file (role=data).");
                        return false;
                    }
                    ok = loadSmart.apply(RC, dataName);
                    if (!ok) {
                        break;
                    }

                    // optional metadata for these
                    if (metaName != null && !metaName.isBlank()) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    }
                    break;
                }

                case "mummichog": {
                    if (sb.getUploadType().equals("table")) {
                        if (dataName == null) {
                            sb.addMessage("Error", "No data file (role=data).");
                            return false;
                        }
                        ok = loadSmart.apply(RC, dataName);
                        if (!ok) {
                            break;
                        }

                        // optional metadata for these
                        if (metaName != null && !metaName.isBlank()) {
                            ok = RDataUtils.readMetaData(RC, metaName) && ok;
                        }
                    } else {
                        pu.setPeakListParams();
                        RDataUtils.readPeakListData(RC, dataName);

                    }
                    break;

                }
                case "pathway":
                case "pathqea":
                case "msetqea": {
                    if (dataName == null) {
                        sb.addMessage("Error", "No data file (role=data) for pathway/QEA.");
                        return false;
                    }
                    ok = loadSmart.apply(RC, dataName);
                    if (!ok) {
                        break;
                    }

                    if (metaName != null && !metaName.isBlank()) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    }
                    break;
                }

                // List-only ORA variants
                case "msetora":
                case "pathora": {
                    // Prefer explicit list file if provided; otherwise datalist.csv in home
                    String listFile = (listName != null && !listName.isBlank()) ? listName : "datalist.csv";
                    String home = sb.getCurrentUser().getHomeDir();
                    java.nio.file.Path lp = java.nio.file.Paths.get(home, java.nio.file.Paths.get(listFile).getFileName().toString());
                    if (!java.nio.file.Files.exists(lp)) {
                        sb.addMessage("Error", "List file not found in workspace: " + listFile);
                        return false;
                    }
                    String[] qVec = pro.metaboanalyst.utils.DataUtils.readListFileToNames(home, lp.getFileName().toString());
                    if (qVec == null || qVec.length == 0) {
                        sb.addMessage("Error", "The list file appears to be empty: " + lp.getFileName());
                        return false;
                    }
                    RDataUtils.setMapData(RC, qVec);
                    if (sb.getFeatType().equals("lipid")) {
                        SearchUtils.crossReferenceExactLipid(sb, sb.getCmpdIDType());
                    } else {
                        SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());
                    }
                    ok = true;
                    break;
                }

                // Joint two-table integration (e.g., pathinteg)
                case "pathinteg": {
                    if (dataName == null || data2Name == null) {
                        sb.addMessage("Error", "Expected two data files (role=data and role=data2) for integration.");
                        return false;
                    }
                    ok = loadSmart.apply(RC, dataName) && loadSmart.apply(RC, data2Name);
                    if (!ok) {
                        break;
                    }

                    if (metaName != null && !metaName.isBlank()) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    }
                    break;
                }

                // Time / multifactor module expects data; metadata is recommended
                case "mf": {
                    if (dataName == null) {
                        sb.addMessage("Error", "No data file (role=data) for multifactor analysis.");
                        return false;
                    }
                    ok = loadSmart.apply(RC, dataName);
                    if (!ok) {
                        break;
                    }

                    if (metaName != null && !metaName.isBlank()) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    } else {
                        sb.addMessage("Warn", "No metadata found; multifactor/time analyses may be limited.");
                    }
                    break;
                }

                // Metadata-focused module: load data if present; read metadata if available
                case "metadata": {
                    if (dataName != null) {
                        ok = loadSmart.apply(RC, dataName);
                        if (!ok) {
                            break;
                        }
                    } else {
                        ok = true; // allow metadata-only workflows
                    }
                    if (metaName != null && !metaName.isBlank()) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    }
                    break;
                }

                // MS/MS only (or fallback to data)
                case "ms2": {
                    String toLoad = (ms2Name != null && !ms2Name.isBlank()) ? ms2Name : dataName;
                    if (toLoad == null) {
                        sb.addMessage("Error", "Expected an MS/MS or data table to load.");
                        return false;
                    }
                    ok = RDataUtils.readTextDataReload(RC, toLoad);
                    break;
                }

                // Raw mode expects a raw file
                case "raw": {
                    if (rawName == null) {
                        sb.addMessage("Error", "No raw file (role=raw).");
                        return false;
                    }
                    ok = RDataUtils.readTextDataReload(RC, rawName);
                    break;
                }

                // Default: treat like generic data-only + optional meta
                default: {
                    if (dataName == null) {
                        sb.addMessage("Error", "No data file (role=data).");
                        return false;
                    }
                    ok = loadSmart.apply(RC, dataName);
                    if (!ok) {
                        break;
                    }

                    if (metaName != null && !metaName.isBlank()) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    }
                    break;
                }
            }

            if (!ok) {
                sb.addMessage("Error", "Failed to load the selected dataset for this module.");
                return false;
            }

            // Load scripts for this analysis and return
            RDataUtils.loadRscriptsOnDemand(RC, sb.getAnalType());
            return true;

        } catch (Exception e) {
            sb.addMessage("Error", "Unable to open module: " + e.getMessage());
            e.printStackTrace();
            return false;
        }
    }

    public void loadWorkflow(DatasetRow ds, boolean login) {
        boolean res = load(ds, login);

        if (res) {
            boolean res2 = handleDataset(ds);

            DataUtils.doRedirectWithGrowl(
                    sb,
                    "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml",
                    "info",
                    "Dataset loaded, please select a workflow and start analysis."
            );
        }
    }

    public boolean load(DatasetRow ds, boolean login) {
        System.out.println("selected====" + ds.getFilename());
        if (ds == null) {
            sb.addMessage("Error", "No dataset selected to load.");
            return false;
        }

        try {
            // 0) Remember selection
            this.selected = ds;

            if (login) {
                sb.doLogin(ds.getDataType(), ds.getModule(), false, false);
            }
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
                    //sb.setNaviType("NA");
                } catch (Exception hx) {
                    System.err.println("Failed to load java_history.json: " + hx.getMessage());
                }
            }

            File histFile2 = new File(sb.getCurrentUser().getHomeDir(), "RloadSanity.RData");
            if (histFile2.exists()) {
                RCenter.LoadRLoadImg(sb.getRConnection(), "RloadSanity.RData");
            }
            // 6) Notify + redirect to module selection
            String mode = restoredFromZip ? "restored" : "loaded";
            sb.addMessage("info", "Workspace " + mode + " (" + copied + " file" + (copied == 1 ? "" : "s") + ").");
        } catch (Exception e) {
            sb.addMessage("Error", "Load failed: " + e.getMessage());
            e.printStackTrace();
            return false;

        }
        return true;

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
        System.out.println("stageListDataset");
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

            DatasetFile lf = new DatasetFile();
            lf.setRole("data");
            lf.setFilename(fname);
            lf.setType("txt");
            lf.setSizeBytes(java.nio.file.Files.size(lp));
            lf.setUploadedAt(java.time.OffsetDateTime.now());

            java.util.List<DatasetFile> files = java.util.List.of(lf);
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
        if (!datasetTableExample.isEmpty()) {
            return datasetTableExample;
        }

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
        ds1.setOrigin("Example"); // <-- NEW

        List<DatasetFile> ds1Files = new ArrayList<>();
        {
            DatasetFile f1 = new DatasetFile();
            f1.setRole("data");
            f1.setFilename("human_cachexia.csv");
            f1.setType("csv");
            f1.setSizeBytes(32461L);
            ds1Files.add(f1);
        }
        ds1.setFiles(ds1Files);
        ds1.setFileCount(ds1Files.size());

        datasetTableExample.add(ds1);

        // Example 2
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
        ds1.setOrigin("Example"); // <-- NEW

        ds1Files = new ArrayList<>();
        {
            DatasetFile f1 = new DatasetFile();
            f1.setRole("data");
            f1.setFilename("covid_metabolomics_data.csv");
            f1.setType("csv");
            f1.setSizeBytes(1468006L);
            ds1Files.add(f1);

            DatasetFile f2 = new DatasetFile();
            f2.setRole("metadata");
            f2.setFilename("covid_metadata_multiclass.csv");
            f2.setType("csv");
            f2.setSizeBytes(2598L);
            ds1Files.add(f2);
        }
        ds1.setFiles(ds1Files);
        ds1.setFileCount(ds1Files.size());

        datasetTableExample.add(ds1);

        // Example 3
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
        ds1.setOrigin("Example"); // <-- NEW

        ds1Files = new ArrayList<>();
        {
            DatasetFile f1 = new DatasetFile();
            f1.setRole("data");
            f1.setFilename("datalist.csv");
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

    public void stageExampleDataset(String title, int sampleNum,
            java.util.List<String> filePaths,
            java.util.List<String> roles) {
        if (filePaths == null || roles == null || filePaths.size() != roles.size()) {
            sb.addMessage("Error", "File paths and roles must be provided in equal length.");
            return;
        }

        final java.time.OffsetDateTime now = java.time.OffsetDateTime.now();
        final java.nio.file.Path home = java.nio.file.Paths.get(sb.getCurrentUser().getHomeDir());

        java.util.List<DatasetFile> files = new java.util.ArrayList<>(filePaths.size());
        java.util.List<java.nio.file.Path> paths = new java.util.ArrayList<>(filePaths.size());

        for (int i = 0; i < filePaths.size(); i++) {
            String fname = java.nio.file.Paths.get(filePaths.get(i)).getFileName().toString();
            String role = roles.get(i);

            java.nio.file.Path p = home.resolve(fname);
            long size = 0L;
            try {
                if (java.nio.file.Files.exists(p)) {
                    size = java.nio.file.Files.size(p);
                }
            } catch (Exception ignore) {
            }

            DatasetFile df = new DatasetFile();
            df.setRole(role);
            df.setFilename(fname);
            df.setType(extOf(fname));
            df.setSizeBytes(Math.max(0L, size));
            df.setUploadedAt(now);

            files.add(df);
            paths.add(p);
        }

        if (files.isEmpty()) {
            sb.addMessage("Error", "No valid files for staging.");
            return;
        }

        // Prefer data → list → first
        DatasetFile primary = files.stream()
                .filter(f -> "data".equalsIgnoreCase(f.getRole()))
                .findFirst()
                .orElseGet(() -> files.stream()
                .filter(f -> "list".equalsIgnoreCase(f.getRole()))
                .findFirst()
                .orElse(files.get(0)));

        String resolvedTitle = (title != null && !title.isBlank())
                ? title.trim()
                : primary.getFilename();

        DatasetRow ds = new DatasetRow();
        ds.setEmail(fub.getEmail());
        ds.setNode(ab.getToolLocation());
        ds.setTitle(resolvedTitle);
        ds.setFilename(primary.getFilename());
        ds.setType(primary.getType());
        ds.setSizeBytes(primary.getSizeBytes());
        ds.setSamplenum(Math.max(0, sampleNum));
        ds.setUploadedAt(now);
        ds.setFileCount(files.size());
        ds.setHasMetadata(files.stream().anyMatch(f -> "metadata".equalsIgnoreCase(f.getRole())));
        ds.setFiles(files);

        this.stagedDataset = ds;
        this.stagedFiles.clear();
        this.stagedFiles.addAll(files);

        sb.addMessage("info", "Example dataset staged in memory.");
    }

    private String editorMode;                 // "table" or "text"
    private String editorTitle;                // file display name
    private String editorText;                 // for text/metadata files
    private java.util.List<String> editorCols; // for table files
    private java.util.List<java.util.List<String>> editorRows;

    public String getEditorMode() {
        return editorMode;
    }

    public String getEditorTitle() {
        return editorTitle;
    }

    public String getEditorText() {
        return editorText;
    }

    public java.util.List<String> getEditorCols() {
        return editorCols;
    }

    public java.util.List<java.util.List<String>> getEditorRows() {
        return editorRows;
    }

    /**
     * Load dataset contents into the session workspace (same logic as load()),
     * then open a chosen file for the editor dialog.
     *
     * @param ds dataset to load
     * @param preferRole optional: "metadata" or "data" to pick which file to
     * open; null = best effort
     * @return true if prepared successfully
     */
    public boolean loadForEditor(DatasetRow ds, String preferRole) {
        try {
            this.selected = ds;

            // 1) Ensure working folder
            String module = (ds.getModule() != null && !ds.getModule().isBlank())
                    ? ds.getModule() : sb.getAnalType();
            sb.doLogin(ds.getDataType(), ds.getModule(), false, false);

            // 2) Resolve storage (src) and workspace (dst)
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

            boolean restoredFromZip = false;

            // 3) Restore workspace
            if (Files.isDirectory(srcDir)) {
                copyDirectoryRecursive(srcDir, dstDir);
            } else if (Files.isRegularFile(srcZip)) {
                unzipInto(srcZip, dstDir);
                restoredFromZip = true;
            } else {
                // Remote fallback
                String node = ds.getNode();
                if (node == null || node.isBlank()) {
                    throw new IllegalArgumentException("Dataset node is missing.");
                }
                node = node.trim().replaceAll("\\.+$", "");
                String baseUrl = "https://" + node + ".metaboanalyst.ca/";

                Path tempZip = Files.createTempFile(dstDir, "dataset-", ".zip");
                try {
                    downloadDatasetObjectFromBaseUrl(
                            baseUrl,
                            ds.getEmail(),
                            ds.getId().toString(),
                            tempZip.toString(),
                            null
                    );
                    unzipInto(tempZip, dstDir);
                    restoredFromZip = true;
                } finally {
                    try {
                        Files.deleteIfExists(tempZip);
                    } catch (Exception ignore) {
                    }
                }
            }

            // =========================
            // 4) Resolve Data & Metadata files
            // =========================
            DatasetFile dataDf = chooseByRole(ds, "data");
            DatasetFile metaDf = chooseByRole(ds, "metadata");

            // Fallback: if no explicit "data" file, use ds.filename as data
            String dataDisplay = (dataDf != null && dataDf.getFilename() != null && !dataDf.getFilename().isBlank())
                    ? dataDf.getFilename()
                    : ds.getFilename();

            if (dataDisplay == null || dataDisplay.isBlank()) {
                sb.addMessage("Error", "No data file name available for the editor.");
                return false;
            }

            Path dataPath = resolveLocal(dstDir, dataDisplay);
            if (!Files.exists(dataPath) || !Files.isRegularFile(dataPath)) {
                sb.addMessage("Error", "Data file not found in workspace: " + dataDisplay);
                return false;
            }

            Path metaPath = null;
            if (metaDf != null && metaDf.getFilename() != null && !metaDf.getFilename().isBlank()) {
                Path p = resolveLocal(dstDir, metaDf.getFilename());
                if (Files.exists(p) && Files.isRegularFile(p)) {
                    metaPath = p; // enable metadata tab
                }
            }

            // =========================
            // 5) Prepare editor models
            // =========================
            // Reset (Data tab)
            dataCols = new java.util.ArrayList<>();
            dataRows = new java.util.ArrayList<>();

            // Detect data file mode
            String dataExt = extOf(dataPath.getFileName().toString());
            boolean dataIsTabular = dataExt.matches("(?i)csv|tsv|txt");

            if (dataIsTabular) {
                final int maxRows = 10;
                char sep = dataExt.equalsIgnoreCase("tsv") ? '\t' : detectCsvSeparator(dataPath);
                readTablePreview(dataPath, sep, maxRows, dataCols, dataRows);
            } else {
                // treat as text but still show in text area only if you add a Data text area
                // for sheet demo, make it one-column "preview"
                dataCols.add("Content");
                java.util.List<String> r = new java.util.ArrayList<>();
                r.add(readTextWithCap(dataPath, 2 * 1024 * 1024));
                dataRows.add(r);
            }
            buildDataSheetModel(); // pads, creates SheetRow list + map

            // Reset (Metadata tab)
            hasMetadataTab = false;
            metaMode = null;
            metaCols = new java.util.ArrayList<>();
            metaRows = new java.util.ArrayList<>();
            editorMetaText = null;

            if (metaPath != null) {
                hasMetadataTab = true;
                String metaExt = extOf(metaPath.getFileName().toString());
                boolean metaIsTabular = metaExt.matches("(?i)csv|tsv|txt");
                boolean metaIsText = metaExt.matches("(?i)json|yaml|yml|toml|ini|txt");

                if (metaIsTabular) {
                    metaMode = "table";
                    char msep = metaExt.equalsIgnoreCase("tsv") ? '\t' : detectCsvSeparator(metaPath);
                    readTableAll(metaPath, msep, metaCols, metaRows);
                    buildMetaSheetModel();
                } else if (metaIsText) {
                    metaMode = "text";
                    editorMetaText = readTextWithCap(metaPath, 2 * 1024 * 1024);
                } else {
                    // Unknown type: fall back to text
                    metaMode = "text";
                    editorMetaText = readTextWithCap(metaPath, 2 * 1024 * 1024);
                }
            }

            // Optional: pick which tab opens by default
            editorActiveIndex = 0; // 0 = Data, 1 = Metadata
            // If preferRole explicitly asks for metadata and we have it, switch:
            if ("metadata".equalsIgnoreCase(String.valueOf(preferRole)) && hasMetadataTab) {
                editorActiveIndex = 1;
            }

            String mode = restoredFromZip ? "restored" : "loaded";
            sb.addMessage("info", "Workspace " + mode + ". Opening Data: “" + dataPath.getFileName() + "”"
                    + (hasMetadataTab ? ("; Metadata: “" + metaPath.getFileName() + "”.") : "."));
            return true;

        } catch (Exception e) {
            sb.addMessage("Error", "Open in editor failed: " + e.getMessage());
            e.printStackTrace();
            return false;
        }
    }

    /**
     * Read ALL rows from a CSV/TSV into (outCols, outRows).
     */
    private void readTableAll(Path p, char sep,
            java.util.List<String> outCols,
            java.util.List<java.util.List<String>> outRows) throws IOException {
        try (java.io.BufferedReader br = Files.newBufferedReader(p)) {
            String header = br.readLine();
            if (header == null) {
                return;
            }
            String[] cols = splitLine(header, sep);
            java.util.Collections.addAll(outCols, cols);

            String line;
            while ((line = br.readLine()) != null) {
                String[] cells = splitLine(line, sep);
                java.util.List<String> row = new java.util.ArrayList<>(cols.length);
                for (int i = 0; i < cols.length; i++) {
                    row.add(i < cells.length ? cells[i] : "");
                }
                outRows.add(row);
            }
        }
    }

    private String readTextWithCap(Path p, int capBytes) throws IOException {
        long size = Files.size(p);
        if (size <= capBytes) {
            return Files.readString(p);
        }
        try (InputStream in = Files.newInputStream(p); java.io.Reader r = new java.io.InputStreamReader(in); java.io.BufferedReader br = new java.io.BufferedReader(r)) {
            StringBuilder sbuf = new StringBuilder(capBytes + 1024);
            int read, total = 0;
            char[] buf = new char[8192];
            while ((read = br.read(buf)) != -1 && total < capBytes) {
                int toAppend = Math.min(read, capBytes - total);
                sbuf.append(buf, 0, toAppend);
                total += toAppend;
            }
            sbuf.append("\n\n… (truncated)");
            return sbuf.toString();
        }
    }

    private char detectCsvSeparator(Path p) throws IOException {
        // simple heuristic: count commas vs tabs in first non-empty line
        try (java.io.BufferedReader br = Files.newBufferedReader(p)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    continue;
                }
                int commas = countChar(line, ',');
                int tabs = countChar(line, '\t');
                if (tabs > commas) {
                    return '\t';
                }
                return ',';
            }
        }
        return ','; // default
    }

    private static int countChar(String s, char c) {
        int n = 0;
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == c) {
                n++;
            }
        }
        return n;
    }

    /**
     * Minimal CSV/TSV preview reader
     */
    private void readTablePreview(Path p, char sep, int maxRows,
            java.util.List<String> outCols,
            java.util.List<java.util.List<String>> outRows) throws IOException {
        try (java.io.BufferedReader br = Files.newBufferedReader(p)) {
            String header = br.readLine();
            if (header == null) {
                return;
            }
            String[] cols = splitLine(header, sep);
            java.util.Collections.addAll(outCols, cols);

            String line;
            int rows = 0;
            while (rows < maxRows && (line = br.readLine()) != null) {
                String[] cells = splitLine(line, sep);
                java.util.List<String> row = new java.util.ArrayList<>(cols.length);
                for (int i = 0; i < cols.length; i++) {
                    row.add(i < cells.length ? cells[i] : "");
                }
                outRows.add(row);
                rows++;
            }
        }
    }

    private List<SheetRow> sheetRows;                // exposed to <pe:sheet>

    public List<SheetRow> getSheetRows() {
        return sheetRows;
    }

    public void onSheetChangeData(SheetEvent event) {
        Sheet sheet = event.getSheet();
        int applied = 0;
        for (SheetUpdate u : sheet.getUpdates()) {
            Integer r = dataRowIndexById.get(String.valueOf(u.getRowKey()));
            int c = u.getColIndex(); // from your SheetUpdate class
            if (r != null && r >= 0 && r < dataRows.size()) {
                List<String> row = dataRows.get(r);
                while (c >= row.size()) {
                    row.add("");
                }
                row.set(c, u.getNewValue() != null ? u.getNewValue().toString() : "");
                applied++;
            }
        }
        sheet.commitUpdates();
        sb.addMessage("info", applied + " cell(s) updated in Data.");
    }

    public void onSheetChangeMeta(SheetEvent event) {
        if (!"table".equals(metaMode)) {
            return;
        }
        Sheet sheet = event.getSheet();
        int applied = 0;
        for (SheetUpdate u : sheet.getUpdates()) {
            Integer r = metaRowIndexById.get(String.valueOf(u.getRowKey()));
            int c = u.getColIndex();
            if (r != null && r >= 0 && r < metaRows.size()) {
                List<String> row = metaRows.get(r);
                while (c >= row.size()) {
                    row.add("");
                }
                row.set(c, u.getNewValue() != null ? u.getNewValue().toString() : "");
                applied++;
            }
        }
        sheet.commitUpdates();
        sb.addMessage("info", applied + " cell(s) updated in Metadata.");
    }

    // Tabs
    private int editorActiveIndex = 0; // 0=Data, 1=Metadata
    private boolean hasMetadataTab;
    private String metaMode;           // "table" or "text"

// Data table model
    private List<String> dataCols = new ArrayList<>();
    private List<List<String>> dataRows = new ArrayList<>();
    private List<SheetRow> dataSheetRows = new ArrayList<>();
    private Map<String, Integer> dataRowIndexById = new HashMap<>();

// Metadata models
    private List<String> metaCols = new ArrayList<>();
    private List<List<String>> metaRows = new ArrayList<>();
    private List<SheetRow> metaSheetRows = new ArrayList<>();
    private Map<String, Integer> metaRowIndexById = new HashMap<>();
    private String editorMetaText; // for JSON/YAML/TXT

    public int getEditorActiveIndex() {
        return editorActiveIndex;
    }

    public boolean isHasMetadataTab() {
        return hasMetadataTab;
    }

    public String getMetaMode() {
        return metaMode;
    }

    public List<String> getDataCols() {
        return dataCols;
    }

    public List<SheetRow> getDataSheetRows() {
        return dataSheetRows;
    }

    public List<String> getMetaCols() {
        return metaCols;
    }

    public List<SheetRow> getMetaSheetRows() {
        return metaSheetRows;
    }

    public String getEditorMetaText() {
        return editorMetaText;
    }

    public void setEditorMetaText(String s) {
        this.editorMetaText = s;
    }

    /**
     * Choose first file with given role (case-insensitive).
     */
    private DatasetFile chooseByRole(DatasetRow ds, String role) {
        java.util.List<DatasetFile> files = ds.getFiles();
        if (files == null) {
            return null;
        }
        for (DatasetFile f : files) {
            if (f != null && f.getRole() != null && role.equalsIgnoreCase(f.getRole())) {
                return f;
            }
        }
        return null;
    }

    /**
     * Resolve a local path inside dstDir; allow basename fallback.
     */
    private Path resolveLocal(Path dstDir, String displayName) {
        Path p = dstDir.resolve(displayName).normalize();
        if (!Files.exists(p)) {
            p = dstDir.resolve(Paths.get(displayName).getFileName().toString()).normalize();
        }
        return p;
    }

    private void buildDataSheetModel() {
        int cols = dataCols.size();
        dataSheetRows = new ArrayList<>();
        dataRowIndexById = new HashMap<>();
        for (int i = 0; i < dataRows.size(); i++) {
            List<String> row = dataRows.get(i);
            while (row.size() < cols) {
                row.add("");
            }
            var sr = new SheetRow(String.valueOf(i), row);
            dataSheetRows.add(sr);
            dataRowIndexById.put(sr.getId(), i);
        }
    }

    private void buildMetaSheetModel() {
        if (!"table".equals(metaMode)) {
            return;
        }
        int cols = metaCols.size();
        metaSheetRows = new ArrayList<>();
        metaRowIndexById = new HashMap<>();
        for (int i = 0; i < metaRows.size(); i++) {
            List<String> row = metaRows.get(i);
            while (row.size() < cols) {
                row.add("");
            }
            var sr = new SheetRow(String.valueOf(i), row);
            metaSheetRows.add(sr);
            metaRowIndexById.put(sr.getId(), i);
        }
    }

    public void saveEditedFile() {
        try {
            if (selected == null) {
                sb.addMessage("Error", "No dataset is loaded.");
                return;
            }

            // Ensure account + node
            String email = (selected.getEmail() == null || selected.getEmail().isBlank())
                    ? fub.getEmail() : selected.getEmail();
            selected.setEmail(email);

            String currentNode = ab.getToolLocation();
            selected.setNode(currentNode);

            UUID datasetId = selected.getId();
            if (datasetId == null) {
                sb.addMessage("warn", "Edits saved locally, but dataset has no DB id; skipping DB update.");
                return;
            }

            // Resolve names (Data is read-only; we won't write it)
            String metaName = resolveRoleFilename(selected, "metadata"); // may be null
            if (!hasMetadataTab || metaName == null || metaName.isBlank()) {
                sb.addMessage("info", "No metadata to save (data table is read-only).");
                return;
            }

            // Workspace
            Path home = Paths.get(sb.getCurrentUser().getHomeDir());
            Files.createDirectories(home);

            // --- METADATA ONLY ---
            Path metaPath = home.resolve(Paths.get(metaName).getFileName().toString());
            if ("table".equals(metaMode) && metaRows != null) {
                char metaSep = pickSeparatorByExt(metaName, home, metaPath);
                // write header + rows so header isn't lost next load
                writeTableWithHeader(metaPath, metaSep, metaCols, metaRows);
            } else if ("text".equals(metaMode) && editorMetaText != null) {
                writeTextToFile(editorMetaText, metaPath);
            } else {
                sb.addMessage("warn", "Unknown metadata mode; nothing saved.");
                return;
            }

            // Copy only metadata file into canonical storage
            Path outDir = Paths.get(fb.getProjectPath(), "user_folders", email, datasetId.toString());
            Files.createDirectories(outDir);

            Path src = metaPath;
            if (!Files.exists(src)) {
                sb.addMessage("Error", "Metadata file was not written to workspace.");
                return;
            }
            Path dst = outDir.resolve(src.getFileName().toString());
            Files.copy(src, dst, StandardCopyOption.REPLACE_EXISTING);

            // Prepare DB update (metadata only)
            List<DatasetFile> editedFilesForDb = new ArrayList<>(1);
            DatasetFile mf = new DatasetFile();
            mf.setDatasetId(datasetId);
            mf.setRole("metadata");
            mf.setFilename(dst.getFileName().toString());
            mf.setType(extOf(mf.getFilename()));
            mf.setSizeBytes(Files.size(dst));
            mf.setUploadedAt(OffsetDateTime.now());
            editedFilesForDb.add(mf);

            String title = (selected.getTitle() != null) ? selected.getTitle() : "";
            String module = (selected.getModule() != null) ? selected.getModule() : sb.getAnalType();
            String dataType = (selected.getDataType() != null) ? selected.getDataType() : sb.getDataType();
            int sampleNum = selected.getSamplenum(); // Data tab is read-only, leave as-is

            // Update DB (incremental upsert) and node
            String result = db.updateDataset(
                    datasetId,
                    email,
                    currentNode,
                    title,
                    module,
                    dataType,
                    "metaboanalyst",
                    sampleNum,
                    editedFilesForDb
            );

            if (result != null && result.toLowerCase(Locale.ROOT).startsWith("error")) {
                sb.addMessage("warn", "Metadata saved, but DB update failed: " + result);
            } else {
                sb.addMessage("info", "Metadata saved and database updated (node '" + currentNode + "').");
            }

        } catch (Exception ex) {
            sb.addMessage("Error", "Save failed: " + ex.getMessage());
            ex.printStackTrace();
        }
    }

    /**
     * Write a CSV/TSV table with header (cols) + rows.
     */
    private void writeTableWithHeader(Path out, char sep,
            List<String> cols,
            List<List<String>> rows) throws IOException {
        try (BufferedWriter bw = Files.newBufferedWriter(out)) {
            // header
            for (int i = 0; i < cols.size(); i++) {
                if (i > 0) {
                    bw.write(sep);
                }
                String cell = cols.get(i) == null ? "" : cols.get(i);
                boolean mustQuote = cell.indexOf(sep) >= 0 || cell.indexOf('"') >= 0 || cell.indexOf('\n') >= 0 || cell.indexOf('\r') >= 0;
                if (mustQuote) {
                    bw.write('"');
                    bw.write(cell.replace("\"", "\"\""));
                    bw.write('"');
                } else {
                    bw.write(cell);
                }
            }
            bw.newLine();
            // data rows
            for (List<String> row : rows) {
                for (int i = 0; i < cols.size(); i++) {
                    if (i > 0) {
                        bw.write(sep);
                    }
                    String cell = (row != null && i < row.size() && row.get(i) != null) ? row.get(i) : "";
                    boolean mustQuote = cell.indexOf(sep) >= 0 || cell.indexOf('"') >= 0 || cell.indexOf('\n') >= 0 || cell.indexOf('\r') >= 0;
                    if (mustQuote) {
                        bw.write('"');
                        bw.write(cell.replace("\"", "\"\""));
                        bw.write('"');
                    } else {
                        bw.write(cell);
                    }
                }
                bw.newLine();
            }
        }
    }

    /**
     * Find the first file in ds.files with the given role (case-insensitive).
     */
    public String resolveRoleFilename(DatasetRow ds, String role) {
        try {
            List<DatasetFile> fs = ds.getFiles();
            if (fs == null) {
                return null;
            }
            for (DatasetFile f : fs) {
                System.out.println("f.getRole()====" + f.getRole());
                if (f != null && f.getRole() != null && role.equalsIgnoreCase(f.getRole())) {
                    return f.getFilename();
                }
            }
        } catch (Throwable ignore) {
        }
        return null;
    }

    /**
     * Write rows with proper CSV/TSV quoting.
     */
    private void writeRowsToDelimited(Path out, char sep, List<List<String>> rows) throws IOException {
        try (java.io.BufferedWriter bw = java.nio.file.Files.newBufferedWriter(out)) {
            for (List<String> row : rows) {
                if (row == null) {
                    bw.newLine();
                    continue;
                }
                StringBuilder sbuf = new StringBuilder();
                for (int i = 0; i < row.size(); i++) {
                    if (i > 0) {
                        sbuf.append(sep);
                    }
                    String cell = row.get(i);
                    if (cell == null) {
                        cell = "";
                    }
                    boolean mustQuote = cell.indexOf(sep) >= 0 || cell.indexOf('"') >= 0 || cell.indexOf('\n') >= 0 || cell.indexOf('\r') >= 0;
                    if (mustQuote) {
                        sbuf.append('"').append(cell.replace("\"", "\"\"")).append('"');
                    } else {
                        sbuf.append(cell);
                    }
                }
                bw.write(sbuf.toString());
                bw.newLine();
            }
        }
    }

    /**
     * Write plain text (UTF-8).
     */
    private void writeTextToFile(String text, Path out) throws IOException {
        Files.writeString(out, text != null ? text : "", java.nio.charset.StandardCharsets.UTF_8);
    }

    public static boolean deleteDatasetObjectAtBaseUrl(
            String baseUrl,
            String email,
            String datasetId,
            String fileOrNull,
            boolean hard,
            String bearerToken
    ) throws IOException, InterruptedException {

        Objects.requireNonNull(baseUrl, "baseUrl");
        Objects.requireNonNull(email, "email");
        Objects.requireNonNull(datasetId, "datasetId");

        String base = baseUrl.replaceAll("/+$", "");
        String url = base + "/dataset/delete";

        // Build x-www-form-urlencoded body
        StringBuilder form = new StringBuilder()
                .append("email=").append(enc(email))
                .append("&datasetId=").append(enc(datasetId))
                .append("&hard=").append(hard);
        if (fileOrNull != null && !fileOrNull.isBlank()) {
            form.append("&file=").append(enc(fileOrNull));
        }

        HttpClient client = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.NORMAL)
                .connectTimeout(Duration.ofSeconds(15))
                .build();

        HttpRequest.Builder rb = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(30))
                .header("Content-Type", "application/x-www-form-urlencoded")
                .POST(HttpRequest.BodyPublishers.ofString(form.toString()));

        if (bearerToken != null && !bearerToken.isBlank()) {
            rb.header("Authorization", "Bearer " + bearerToken);
        }

        HttpRequest req = rb.build();
        HttpResponse<InputStream> resp = client.send(req, HttpResponse.BodyHandlers.ofInputStream());
        int code = resp.statusCode();
        String body = readSmallBody(resp.body());

        if (code != 200) {
            throw new IOException("DELETE failed: HTTP " + code + (body.isEmpty() ? "" : (" - " + body)));
        }

        // Simple check (no JSON lib): {"status":"ok", ...}
        boolean ok = body.contains("\"status\"") && body.contains("\"ok\"");
        if (!ok) {
            throw new IOException("DELETE returned non-ok payload: " + body);
        }
        return true;
    }

    // -------- convenience wrappers --------
    public static boolean deleteWholeDatasetSoft(String baseUrl, String email, String datasetId, String bearerToken)
            throws IOException, InterruptedException {
        return deleteDatasetObjectAtBaseUrl(baseUrl, email, datasetId, null, false, bearerToken);
    }

    public static boolean deleteWholeDatasetHard(String baseUrl, String email, String datasetId, String bearerToken)
            throws IOException, InterruptedException {
        return deleteDatasetObjectAtBaseUrl(baseUrl, email, datasetId, null, true, bearerToken);
    }

    private static String enc(String s) {
        return URLEncoder.encode(s, StandardCharsets.UTF_8);
    }

    private static String readSmallBody(InputStream in) throws IOException {
        if (in == null) {
            return "";
        }
        byte[] buf = in.readAllBytes(); // fine for small JSON replies
        return new String(buf, StandardCharsets.UTF_8).trim();
    }

    public void applyHeaderEdits() {
        buildDataSheetModel();
    }

    public void applyMetaHeaderEdits() {
        if ("table".equals(metaMode)) {
            buildMetaSheetModel();
        }
    }

    // Consider anything with email "example" as an example dataset
    public boolean isSelectedExample() {
        return selected != null
                && selected.getEmail() != null
                && "example".equalsIgnoreCase(selected.getEmail());
    }

// Save is allowed only if metadata tab exists AND it's not an example dataset
    public boolean isCanSave() {
        return hasMetadataTab && !isSelectedExample();
    }

    private DatasetFile selectedFile;

    public DatasetFile getSelectedFile() {
        return selectedFile;
    }

    public void setSelectedFile(DatasetFile selectedFile) {
        this.selectedFile = selectedFile;
    }

    /**
     * Used by
     * <p:fileDownload value="#{datasetController.downloadSelectedFile}"/>.
     * Tries the user_folders/<email>/<datasetId>/ path first, then falls back
     * to ab.getResourceDir() + "/pro/" + filename.
     */
    public org.primefaces.model.StreamedContent getDownloadSelectedFile() {
        try {
            // --- Guards ---
            if (selected == null || selected.getId() == null) {
                sb.addMessage("Error", "No dataset is selected.");
                return null;
            }
            if (selectedFile == null || selectedFile.getFilename() == null || selectedFile.getFilename().isBlank()) {
                sb.addMessage("Error", "No file is selected to download.");
                return null;
            }

            final String ownerEmail = selected.getEmail();
            if (ownerEmail == null || ownerEmail.isBlank()) {
                sb.addMessage("Error", "Dataset owner email is missing.");
                return null;
            }

            final String fileName = selectedFile.getFilename();

            // --- 1) User dataset folder ---
            java.nio.file.Path primaryPath = java.nio.file.Paths.get(
                    fb.getProjectPath(), "user_folders", ownerEmail,
                    selected.getId().toString(), fileName
            ).normalize();

            if (java.nio.file.Files.exists(primaryPath)) {
                return buildFileStreamedContent(primaryPath, fileName);
            }

            // --- 2) Local example resource: <resourceDir>/pro/<fileName> ---
            java.nio.file.Path examplePath = java.nio.file.Paths.get(
                    ab.getResourceDir(), "pro", fileName
            ).normalize();

            if (java.nio.file.Files.exists(examplePath)) {
                return buildFileStreamedContent(examplePath, fileName);
            }

            // --- 3) Remote via API: ab.getResourceByAPI(fileName) ---
            String apiUrl = null;
            try {
                apiUrl = ab.getResourceByAPI(fileName); // e.g., https://www.xialab.ca/api/download/metaboanalyst/human_cachexia.csv
            } catch (Throwable t) {
                // swallow and treat as missing
            }

            if (apiUrl != null && !apiUrl.isBlank()) {
                final String finalApiUrl = apiUrl;
                return org.primefaces.model.DefaultStreamedContent.builder()
                        .name(fileName)
                        .contentType(guessMimeByExt(fileName)) // fallback; will refine from connection below
                        .stream(() -> {
                            try {
                                java.net.URL url = new java.net.URL(finalApiUrl);
                                java.net.URLConnection conn = url.openConnection();
                                conn.setConnectTimeout(10_000);
                                conn.setReadTimeout(60_000);

                                // Try to use server-declared content type if available
                                String ct = conn.getContentType();
                                if (ct != null && !ct.isBlank()) {
                                    // PrimeFaces ignores changes after build(), so this is informational only.
                                    // If you want to strictly use server CT, duplicate builder with known CT.
                                }

                                return new java.io.BufferedInputStream(conn.getInputStream());
                            } catch (java.io.IOException ex) {
                                throw new java.io.UncheckedIOException(ex);
                            }
                        })
                        .build();
            }

            // If all fail:
            sb.addMessage("Error", "File not found in dataset, example resources, or API: " + fileName);
            return null;

        } catch (Exception e) {
            sb.addMessage("Error", "Download failed: " + e.getMessage());
            return null;
        }
    }

    private org.primefaces.model.StreamedContent buildFileStreamedContent(java.nio.file.Path path, String fileName)
            throws java.io.IOException {

        String contentType = java.nio.file.Files.probeContentType(path);
        if (contentType == null || contentType.isBlank()) {
            contentType = guessMimeByExt(fileName);
        }
        final java.nio.file.Path f = path;

        return org.primefaces.model.DefaultStreamedContent.builder()
                .name(fileName)
                .contentType(contentType)
                .stream(() -> {
                    try {
                        return java.nio.file.Files.newInputStream(f);
                    } catch (java.io.IOException ex) {
                        throw new java.io.UncheckedIOException(ex);
                    }
                })
                .build();
    }

    /* =========================
 * 1) Robust line parser
 * ========================= */
    private static List<String> parseDelimitedLine(String line, char sep) {
        List<String> out = new ArrayList<>();
        if (line == null) {
            out.add("");
            return out;
        }
        StringBuilder cell = new StringBuilder();
        boolean inQuotes = false;
        int len = line.length();

        for (int i = 0; i < len; i++) {
            char ch = line.charAt(i);

            if (inQuotes) {
                if (ch == '"') {
                    // Lookahead for escaped quote
                    if (i + 1 < len && line.charAt(i + 1) == '"') {
                        cell.append('"'); // escaped quote
                        i++;              // skip the second quote
                    } else {
                        inQuotes = false; // closing quote
                    }
                } else {
                    cell.append(ch);
                }
            } else {
                if (ch == '"') {
                    inQuotes = true; // opening quote
                } else if (ch == sep) {
                    out.add(cell.toString());
                    cell.setLength(0);
                } else {
                    cell.append(ch);
                }
            }
        }
        out.add(cell.toString());
        return out;
    }

    /* ==========================================
 * 3) Replace splitLine(...) everywhere
 * ========================================== */
    private static String[] splitLine(String line, char sep) {
        // Use robust parser above (handles quotes & escapes)
        List<String> cells = parseDelimitedLine(line, sep);
        return cells.toArray(new String[0]);
    }

    private char pickSeparatorByExt(String fileName, Path home, Path targetPath) {
        String ext = extOf(fileName);
        if ("tsv".equalsIgnoreCase(ext)) {
            return '\t';
        }
        if ("csv".equalsIgnoreCase(ext)) {
            return ',';
        }

        // Try to sniff from an existing file
        try {
            Path p = (targetPath != null && Files.exists(targetPath))
                    ? targetPath
                    : (home != null ? home.resolve(Paths.get(fileName).getFileName().toString()) : null);

            if (p != null && Files.exists(p)) {
                return detectCsvSeparator(p);  // uses the robust detector you added
            }
        } catch (Exception ignore) {
            // fall through to default
        }

        // Default
        return ',';
    }

    // ---- Export just the identifier (safer & smaller)
    public StickyDTO exportSticky() {
        StickyDTO dto = new StickyDTO();
        if (selected != null && selected.getId() != null) {
            System.out.println("Export===============================" + dto.getSelectedDatasetId());

            dto.setSelectedDatasetId(selected.getId());
        }
        return dto;
    }
    private boolean resetFlag = false;

    public boolean isResetFlag() {
        return resetFlag;
    }

    public void setResetFlag(boolean resetFlag) {
        this.resetFlag = resetFlag;
    }

    // ---- Import from DTO (called after new session is created)
    @PostConstruct
    public void init() {
        // pick up sticky that LogoutHelper put into the new session
        Object o = FacesContext.getCurrentInstance().getExternalContext()
                .getSessionMap().remove("STICKY_AFTER_LOGOUT");
        if (o instanceof StickyDTO dto && dto.getSelectedDatasetId() != null) {
            refresh();
            this.selected = findById(dto.getSelectedDatasetId());
            System.out.println("Selected===============================" + dto.getSelectedDatasetId());
            this.resetFlag = true;                 // boolean with getter

        }
    }

    // Called by remoteCommand on next request
    public void afterResetInit() {
        if (selected != null) {
            boolean ok = load(selected, true);          // your existing method (no streaming)
            if (ok) {
                handleDataset(selected);      // your existing method (no streaming)
            }
        }
        resetFlag = false;
    }

    // In DatasetController
    public DatasetRow findById(java.util.UUID id) {
        if (id == null) {
            return null;
        }

        // 0) Fast path: current selection
        if (selected != null && id.equals(selected.getId())) {
            return selected;
        }

        // 1) Search the main loaded table
        if (datasetTable != null && !datasetTable.isEmpty()) {
            for (DatasetRow r : datasetTable) {
                if (r != null && id.equals(r.getId())) {
                    return r;
                }
            }
        }

        // 2) Also check the "selectedDatasets" convenience list if you use it
        if (getDatasetTableExample() != null && !datasetTableExample.isEmpty()) {
            for (DatasetRow r : datasetTableExample) {
                if (r != null && id.equals(r.getId())) {
                    return r;
                }
            }
        }

        // 3) As a last resort, try a one-time refresh if user is logged in,
        // then search again (kept very local—no DB service needed).
        try {
            String email = (fub != null) ? fub.getEmail() : null;
            if (email != null && !email.isBlank()) {
                refresh(); // repopulates datasetTable from db.getDatasetsForEmail(...)
                if (datasetTable != null && !datasetTable.isEmpty()) {
                    for (DatasetRow r : datasetTable) {
                        if (r != null && id.equals(r.getId())) {
                            return r;
                        }
                    }
                }
            }
        } catch (Exception ignore) {
            // keep it silent; just return null if not found
        }

        // Not found locally
        return null;
    }
   

}
