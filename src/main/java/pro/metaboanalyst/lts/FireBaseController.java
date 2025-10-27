/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.enrich.IntegProcessBean;
import pro.metaboanalyst.controllers.enrich.IntegResBean;
import pro.metaboanalyst.controllers.enrich.MsetBean;
import pro.metaboanalyst.controllers.enrich.PathBean;
import pro.metaboanalyst.models.User;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.zeroturnaround.zip.ZipUtil;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.SecureRandom;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletResponse;
import pro.metaboanalyst.utils.UtilsBean;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.mnet.MnetResBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.controllers.stats.PowerAnalBean;
import pro.metaboanalyst.project.ProjectModel;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.omnifaces.util.Faces;
import org.primefaces.PrimeFaces;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.spectra.SpectraParamBean;
import pro.metaboanalyst.utils.NaviUtils;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.file.Path;
import java.util.UUID;
import org.postgresql.util.PGobject;
import pro.metaboanalyst.api.ApiClient;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.meta.MetaLoadBean;
import pro.metaboanalyst.controllers.meta.MetaResBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.mummichog.PeakUploadBean;
import pro.metaboanalyst.datalts.DatasetController;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.spectra.TandemMSBean;
import pro.metaboanalyst.workflows.DiagramView;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.workflows.WorkflowBean;
import pro.metaboanalyst.workflows.WorkflowRunModel;
import pro.metaboanalyst.workflows.WorkflowView;

/**
 * @author zgy
 */
@SessionScoped
@Named("fireBaseController")
public class FireBaseController implements Serializable {

    private static final org.apache.logging.log4j.Logger LOGGER = LogManager.getLogger(FireBaseController.class);
    @Inject
    private DatabaseClient db;
    @Inject
    private SessionBean1 sb;
    @Inject
    private FireUserBean fub;

    @Inject
    private FireBase fb;
    @Inject
    private HistoryBean hb;
    @Inject
    private FireProjectBean pb;

    @Inject
    private ApplicationBean1 ab;

    @Inject
    private WorkflowBean wb;

    @Inject
    private MultifacBean mfb;

    @Inject
    private MsetBean mstb;
    @Inject
    private UtilsBean utb;

    @Inject
    private DiagramView dv;

    @Inject
    private MetaResBean mrb;

    @Inject
    private DatasetController dc;

    @Inject
    private PeakUploadBean pub;

    private String shareableLink = "";

    private String naviUrlAfterLogin = "";
    private String directlyLogout = "stay"; //home,omicsquare,stay
    private String passwordSaved = "";

    private String fireDocName = "";
    private String fireDocDescription = "";

    private boolean saveDataBoolean = true;
    private boolean saveWorkflowBoolean = false;

    private final ApiClient apiClient = new ApiClient();

    public boolean isSaveWorkflowBoolean() {
        return saveWorkflowBoolean;
    }

    public void setSaveWorkflowBoolean(boolean saveWorkflowBoolean) {
        this.saveWorkflowBoolean = saveWorkflowBoolean;
    }

    public boolean isSaveDataBoolean() {
        return saveDataBoolean;
    }

    public void setSaveDataBoolean(boolean saveDataBoolean) {
        this.saveDataBoolean = saveDataBoolean;
    }

    public String getFireDocName() {
        return fireDocName;
    }

    public void setFireDocName(String nm) {
        this.fireDocName = nm;
    }

    public String getFireDocDescription() {
        return fireDocDescription;
    }

    public void setFireDocDescription(String description) {
        this.fireDocDescription = description;
    }

    public String getPasswordSaved() {
        return passwordSaved;
    }

    public void setPasswordSaved(String passwordSaved) {
        //System.out.println("setPasswordSaved==" + passwordSaved);
        this.passwordSaved = passwordSaved;
    }

    public void toggleOnDirectQuit(String destination) {
        directlyLogout = destination;
    }

    public String getNaviUrlAfterLogin() {
        return naviUrlAfterLogin;
    }

    public void setNaviUrlAfterLogin(String naviUrlAfterLogin) {
        this.naviUrlAfterLogin = naviUrlAfterLogin;
    }

    public boolean saveProject(String type) throws Exception {
        if (sb.getCurrentUser() == null) {
            sb.addMessage("Error", "Please start analysis before saving!");
            return false;
        }

        if (sb.getRConnection() == null) {
            sb.getRConnection("stat");
        }
        RCenter.saveRLoadImg(sb.getRConnection());

        //save user folder to bucket
        User user = sb.getCurrentUser();
        String myDir = user.getOrigHomeDir();
        File folder = new File(myDir);
        String zipName = user.getName() + ".zip";
        File[] zipFilesToDelete = folder.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".zip");
                //return true;
            }
        });

        if (!sb.getDataType().equals("spec")) {
            assert zipFilesToDelete != null;
            for (File file : zipFilesToDelete) {
                DataUtils.deleteFile(user, file.getName());
            }
        }

        //save/loading java state test
        //realPaht = Resource folder
        String folderName;
        switch (type) {
            case "share", "workflow" ->
                folderName = "guest";
            case "assist" ->
                folderName = "assist";
            default -> {
                folderName = fub.getEmail();
            }
        }

        /////////////////////////////
        //save info to 'project' table in 'xialabdb' database
        /////////////////////////////
        writeDoc(user.getName(), type);

        File projSubFolder = new File(fb.getProjectPath() + "/user_folders/" + folderName);
        if (!projSubFolder.exists()) {
            projSubFolder.mkdirs();
        }
        ZipUtil.pack(new File(myDir), new File(fb.getProjectPath() + "/user_folders/" + folderName + "/" + zipName));
        System.out.println(fb.getProjectPath() + "/user_folders/" + folderName + "/" + zipName + "======savedZip");
        //copy batch template outside zip file
        File oribtfile = new File(myDir + "/RBatchTemplate.qs");
        if (oribtfile.exists()) {
            File destDir = new File(fb.getProjectPath() + "/BatchProcess/MetaboAnalyst/");
            if (!destDir.exists()) {
                destDir.mkdirs(); // This will create the directory path if it doesn't exist
            }
            File destbtfile = new File(fb.getProjectPath() + "/BatchProcess/MetaboAnalyst/" + user.getName() + "_RBatchTemplate.qs");
            DataUtils.copyFile(oribtfile, destbtfile);
        }

        if (!type.equals("share")) {
            sb.addMessage("info", "Project saved sucessfully!");
        }

        setFireDocName(pb.getSelectedProject().getTitle());
        setFireDocDescription(pb.getSelectedProject().getDescription());

        //save workflow
        //WorkflowView wf = (WorkflowView) DataUtils.findBean2("workflowView");
        //wf.generateWorkflowJson(pb.getSelectedProject().getTitle(), pb.getSelectedProject().getDescription(), false);
        switch (directlyLogout) {
            case "home" ->
                doXialabLogout();
            case "module" -> {
                DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/ModuleView.xhtml", ab);
            }
            case "omicsquare" -> {
                sb.doLogout(0);
                DataUtils.doRedirect("https://www.xialab.ca/translation.xhtml", ab);
            }
            default -> {
            }
        }
        System.out.println("saving project...");
        return true;
    }

    public boolean saveBatchProject(String type, String template_token) {

        if (sb.getCurrentUser() == null) {
            sb.addMessage("Error", "Please start analysis before saving!");
            return false;
        }

        if (sb.getRConnection() != null) {
            RCenter.saveRLoadImg(sb.getRConnection());
        }
        System.out.print("Now we are trying to save a batch project ... " + type);
        String folderName;
        switch (type) {
            //case "share" ->
            //    folderName = "guest";
            case "assist" ->
                folderName = "assist";
            default -> {
                folderName = fub.getEmail();

            }
        }

        System.out.print("Now we are trying to save a batch project ... " + type + " | " + folderName);
        User user = sb.getCurrentUser();
        try {
            writeDoc(user.getName(), type);
            // once the project is saved, let's transfer and update the JSF history to adapt to this current batch project
            RCenter.transferJSFHistory(
                    sb.getRConnection(),
                    fb.getProjectDBPath(),
                    template_token,
                    sb.getShareToken());
        } catch (Exception ex) {
            System.out.print("Now we are experiencing an error for project saving... ");
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        }

        return false;
    }
    @Inject
    StateSaver stateSaver;

    public int writeDoc(String folderName, String type) throws InterruptedException, ExecutionException, JsonProcessingException {

        long idNum = 0;
        String token = DataUtils.generateToken(folderName);
        sb.setShareToken(token);

        String uid = fub.getEmail();
        //System.out.println("Saver injected? " + (stateSaver != null));
        stateSaver.saveState();
        String date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date());

        if (fireDocName == null || fireDocName.isEmpty()) {
            fireDocName = "Autosaved Project";
        }

        if (fireDocDescription.isEmpty()) {
            fireDocDescription = "None";
        }
        saveJavaHistory();
        String imgMapStr = saveImgMap();

        String jh = hb.getJavaHistoryString();

        jh = jh.replace(":\"[{\"", ":[{\\\"");
        // Create a Map to store the data we want to set
        Map<String, Object> docData = new HashMap<>();
        docData.put("userid", uid);
        docData.put("name", fireDocName);
        docData.put("description", fireDocDescription);
        docData.put("module", sb.getAnalType());
        docData.put("modulereadable", getAnalTypeReadable());
        docData.put("imgMap", imgMapStr);
        docData.put("datatype", sb.getDataType());
        docData.put("date", date);
        docData.put("foldername", folderName);
        docData.put("javahistory", "");
        docData.put("navitrack", "");
        docData.put("navistring", sb.getCurrentNaviUrl());
        docData.put("id", idNum);
        docData.put("org", sb.getOrg());
        docData.put("partialtoken", token);
        docData.put("toolname", "MetaboAnalyst");
        docData.put("toolcode", "MET");
        docData.put("paired", sb.isPaired());
        docData.put("regression", sb.isRegression());
        docData.put("location", ab.getToolLocation());

        //System.out.println("javahistory==============================================");
        saveJsonStringToFile(jh, sb.getCurrentUser().getOrigHomeDir() + File.separator + "java_history.json");
        String cmdHistory = convertObjToJson(pb.getFunctionInfos());
        //System.out.println(cmdHistory);
        saveJsonStringToFile(cmdHistory, sb.getCurrentUser().getOrigHomeDir() + File.separator + "java_fun_history.json");

        String app_url = ab.getApp_url();
        if (!ab.isOnLocalServer() & ab.isInDocker()) {
            app_url = app_url.replace("http:", "https:");
        }

        shareableLink = app_url + "/MetaboAnalyst/faces/AjaxHandler.xhtml?funcNm=ShareLink&tokenId=" + token;

        if (ab.isInDocker()) {
            DatabaseController.writeProjectToPostgres(docData, type);
        } else {
            try {
                Map<String, Object> payload = new HashMap<>(docData);
                payload.put("projectType", type);
                payload.put("tableName", "project_history");
                String response = apiClient.post("/database/projects", toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                System.out.println("Error writing project to Postgres: " + e);
                return -1;
            }
        }

        idNum = db.checkMatchingFolderNameProject(sb.getCurrentUser().getName());
        docData.put("id", idNum);

        ProjectModel selectedProject = createProjectFromMap(docData);
        pb.setSelectedProject(selectedProject);

        if (dc.hasStagedDataset() && saveDataBoolean) {
            dc.getStagedDataset().setSamplenum(RDataUtils.getSampleNum(sb.getRConnection()));
            java.util.UUID dsId = dc.commitStagedDataset();
            if (dsId != null) {
                System.out.println("Committed staged dataset: " + dsId);
            }
        }

        try {
            wb.setName(getFireDocName());
            wb.setDescription(getFireDocDescription());
            wfv.generateWorkflowJson(type, saveWorkflowBoolean);

        } catch (IOException ex) {
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        }
        return 0;
    }

    private String toJson(Map<String, ?> map) {
        return new Gson().toJson(map);
    }

    public boolean saveDataAndGoToWorkflow() {
        if (dc.hasStagedDataset()) {
            dc.getStagedDataset().setSamplenum(RDataUtils.getSampleNum(sb.getRConnection()));
            java.util.UUID dsId = dc.commitStagedDataset();
            if (dsId != null) {
                System.out.println("Committed staged dataset: " + dsId);
            }
            dc.loadWorkflow(dc.findById(dsId), false);
            return true;
        } else {
            sb.addMessage("warn", "Failed to load the current dataset.");
            return false;
        }
    }

    @Inject
    private SpectraProcessBean sppb;
    @Inject
    private SpectraControlBean spcb;
    @Inject
    private SpectraParamBean spmb;
    @Inject
    private IntegProcessBean itpb;
    @Inject
    private IntegResBean itrb;
    @Inject
    private PathBean patb;

    @Inject
    private MetaPathStatBean mpsb;
    @Inject
    private MetaPathLoadBean mplb;
    @Inject
    private MetaLoadBean mlb;
    @Inject
    private DoseResponseBean drb;

    @Inject
    private RocAnalBean rab;

    @Inject
    private PowerAnalBean pab;

    @Inject
    private MnetResBean mnrb;

    @Inject
    private MummiAnalBean mab;

    @Inject
    private TandemMSBean tmsb;
    @Inject
    private WorkflowView wfv;

    public void saveJavaHistory() {
        ObjectMapper mapper = new ObjectMapper();
        try {
            String jsonInJavaHistory = mapper.writeValueAsString(hb.getJavaHistory());
            if (jsonInJavaHistory != null) {
                hb.setJavaHistoryString(jsonInJavaHistory);
            }
        } catch (JsonProcessingException ex) {

        }
    }

    private String saveImgMap() {
        ObjectMapper mapper = new ObjectMapper();
        try {
            String json = mapper.writeValueAsString(sb.getImgMap());
            if (json != null) {
                return (json);
            }
        } catch (JsonProcessingException ex) {

        }
        return "";
    }

    public String convertObjToJson(Object obj) {
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            // Write the map of function information to a JSON file
            String json = objectMapper.writeValueAsString(obj);
            return json;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return "";
    }

    //analType: stat, pathora, pathqea, msetora, msetssp, msetqea, msetview, cmpdmap, peaksearch, smpmap
    public String getAnalTypeReadable() {
        String readable;
        readable = switch (sb.getAnalType()) {
            case "stat" ->
                "Statistical Analysis";
            case "pathora" ->
                "Pathway Analysis";
            case "pathqea" ->
                "Meta-analysis";
            case "msetora" ->
                "Enrichment Analysis";
            case "raw" ->
                "LC-MS Raw Spectral Processing";
            default ->
                sb.getAnalType();
        };
        return readable;
    }

    public boolean loadProject(String projectId, String type) {
        try {
            return (loadProject(projectId, type, false));
        } catch (SQLException ex) {
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ExecutionException ex) {
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ParseException ex) {
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    public boolean loadProject(String projectId, String type, boolean forceReload) throws SQLException, InterruptedException, ExecutionException, ParseException {
        //TODO: give error message if no files are ready for download
        //  Maybe, call setupDownloadTable() of downloader from here
        Map<String, Object> docData;

        docData = db.loadProject(projectId);

        //String naviStr = docData.get("naviString").toString();
        String dateStr = docData.get("date").toString();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd X");
        String email = docData.get("userid").toString();

        ProjectModel selectedProject = createProjectFromMap(docData);
        String folderName = selectedProject.getFolderName();
        String javaHistory = selectedProject.getJavaHistory();
        String org = selectedProject.getOrg();
        String analType = selectedProject.getType();
        String dataType = selectedProject.getDataType();
        System.out.println("loadProject====" + dataType);
        String shareToken = selectedProject.getShareToken();
        boolean paired = selectedProject.isPaired();
        boolean regression = selectedProject.isRegression();
        if (!forceReload) {
            if (!type.equals("workflow")) {
                if (sb.isLoggedIn()) {
                    if (sb.getCurrentUser().getName().equals(folderName)) {
                        return true;
                    }
                }
            }
        }
        pb.setSelectedProject(selectedProject);
        sb.setAnalType(analType);
        sb.setDataType(dataType);
        sb.setShareToken(shareToken);
        boolean check = sb.doLogin(dataType, analType, regression, paired, folderName, true);
        if (check) {
            //System.out.println(org + "=====org");
            if (!org.equals("NA")) {
                sb.setOrg(org);
                RDataUtils.setOrganism(sb, org);
            }
        }
        String destDirPath = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";

        String userFolderName;
        switch (type) {
            case "share", "workflow" ->
                userFolderName = "guest";
            case "assist" ->
                userFolderName = "assist";
            default -> {
                if (fub.getEmail() == null || fub.getEmail().isEmpty()) {
                    userFolderName = email;
                } else {
                    userFolderName = fub.getEmail();
                }
            }
        }

        String bucketObjectName = "user_folders/" + userFolderName + "/" + folderName + ".zip";
        String localFilePath = fb.getProjectPath() + bucketObjectName;
        File f = new File(localFilePath);
        //System.out.println(localFilePath + "========================================abc");
        if (f.exists()) {
            DataUtils.extract(localFilePath, destDirPath);
        } else {
            downloadObject(selectedProject.getHostname(), userFolderName, folderName, destDirPath + folderName + ".zip");
            DataUtils.extract(destDirPath + folderName + ".zip", destDirPath);
        }
        System.out.println("sb.getCurrentUser().getHomeDir()====" + sb.getCurrentUser().getHomeDir());
        RCenter.setWd(sb.getRConnection(), sb.getCurrentUser().getHomeDir());

        RCenter.loadHistory(sb.getRConnection());

        if (javaHistory.isEmpty()) {
            javaHistory = readJsonStringFromFile(sb.getCurrentUser().getOrigHomeDir() + File.separator + "java_history.json");
        }
        loadJavaHistory(javaHistory);
        System.out.println("wb.isReloadingWorkflow()===" + wb.isReloadingWorkflow());
        if (wb.isReloadingWorkflow() || type.equals("workflow")) {
            File ftest = new File(sb.getCurrentUser().getOrigHomeDir() + File.separator + "workflow.json");
            if (ftest.exists()) {
                Map<String, FunctionInfo> functionInfos = FunctionInvoker.loadFunctionInfosFromFile(destDirPath + "workflow.json");
                wb.setFunctionInfos(functionInfos);
            }
        }
        for (String module : wb.getModuleNames()) {
            RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), module);
        }

        String naviType = sb.getNaviType();
        sb.setNaviTree(NaviUtils.createNaviTree(naviType));
        return true;
    }

    public void downloadObject(String serverLocation, String email, String folderName, String destFilePath) {
        try {

            File tempFile = File.createTempFile("download-", ".tmp");
            tempFile.deleteOnExit();

            String urlString = "https://" + serverLocation + ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml?funcNm=fetchProject&email="
                    + URLEncoder.encode(email, StandardCharsets.UTF_8)
                    + "&folderName=" + URLEncoder.encode(folderName, StandardCharsets.UTF_8);

            HttpClient client = HttpClient.newHttpClient();
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(urlString))
                    .GET()
                    .build();

            try (InputStream in = client.send(request, HttpResponse.BodyHandlers.ofInputStream()).body(); FileOutputStream out = new FileOutputStream(tempFile)) {

                byte[] buffer = new byte[4096];
                int bytesRead;
                while ((bytesRead = in.read(buffer)) != -1) {
                    out.write(buffer, 0, bytesRead);
                }
            }

            // Now move the downloaded temp file to the desired destination
            File destFile = new File(destFilePath);
            if (!destFile.exists()) {
                destFile.getParentFile().mkdirs(); // Create parent directories if they don't exist
            }
            Files.move(tempFile.toPath(), Paths.get(destFilePath), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        } catch (Exception e) {
            // Handle the exception here
            System.err.println("An error occurred during file download or moving: " + e.getMessage());
            e.printStackTrace();
        }
    }

    public int loadJavaHistory(String jh) {
        ObjectMapper mapper = JsonMapper.builder()
                .enable(JsonReadFeature.ALLOW_BACKSLASH_ESCAPING_ANY_CHARACTER)
                .build();
        LinkedHashMap<String, String> javaHistory = new LinkedHashMap();
        try {
            javaHistory = mapper.readValue(jh, LinkedHashMap.class);
            if (javaHistory != null) {
                hb.setJavaHistory(javaHistory);
            }
        } catch (IOException ex) {
            LOGGER.error("loadJavaHistory", ex);
        }

        String myClass, myMethod, myParamType, myParam;
        // setAnalType should be called first
        for (Map.Entry<String, String> entry : javaHistory.entrySet()) {
            String[] splitString = entry.getKey().split("\\.");
            myClass = splitString[0];
            myMethod = splitString[1];
            myParamType = splitString[2];
            myParam = entry.getValue();
            // now work with key and value...

            runJavaMethod(myClass, myMethod, myParamType, myParam);
        }
        hb.setJavaHistory(javaHistory);

        return (1);
    }

    public void loadImgMap(String imgMapStr) {
        ObjectMapper mapper = new ObjectMapper();
        try {
            //TODO: handle traceTrack when null
            if (imgMapStr != null) {
                sb.setImgMap(mapper.readValue(imgMapStr, HashMap.class));
            }
        } catch (IOException ex) {
            LOGGER.error("loadImgMap", ex);
        }
    }

    public void runJavaMethod(String myClassType, String myMethod, String myParamType, String myParam) {
        // Initialize the map to hold all beans
        Map<String, Object> beanClassMap = new HashMap<>();
        beanClassMap.put("DiagramView", dv);

        beanClassMap.put("SessionBean1", sb);
        beanClassMap.put("SpectraProcessBean", sppb);
        beanClassMap.put("SpectraControlBean", spcb);
        beanClassMap.put("SpectraParamBean", spmb);
        beanClassMap.put("WorkflowBean", wb);
        beanClassMap.put("PathBean", patb);
        beanClassMap.put("IntegProcessBean", itpb);
        beanClassMap.put("IntegResBean", itrb);
        beanClassMap.put("MsetBean", mstb);
        beanClassMap.put("UtilsBean", utb);
        beanClassMap.put("MetaLoadBean", mlb);
        beanClassMap.put("MetaPathLoadBean", mplb);
        beanClassMap.put("TandemMSBean", tmsb);
        beanClassMap.put("MetaPathStatBean", mpsb);
        beanClassMap.put("MnetResBean", mnrb);
        beanClassMap.put("MultifacBean", mfb);
        beanClassMap.put("MummiAnalBean", mab);
        beanClassMap.put("PowerAnalBean", pab);
        beanClassMap.put("RocAnalBean", rab);
        beanClassMap.put("PeakUploadBean", pub);

        // Print debug information
        //System.out.println("myClassType: " + myClassType + ", myMethod: " + myMethod + ", myParamType: " + myParamType + ", myParam: " + myParam);
        // Retrieve the bean from the map
        Object obj = beanClassMap.get(myParamType);
        if (obj == null) {
            LOGGER.error("Bean not found: " + myParamType);
            return;
        }

        Class<?> cls = obj.getClass();
        Method method = null;

        if (myParam == null) {
            return;
        }

        try {
            Class<?>[] paramString = new Class<?>[1];
            switch (myParamType) {
                case "noparam" -> {
                    method = cls.getDeclaredMethod(myMethod, new Class<?>[0]);
                    method.invoke(obj);
                }
                case "stringparam" -> {
                    paramString[0] = String.class;
                    method = cls.getDeclaredMethod(myMethod, paramString);
                    method.invoke(obj, myParam);
                }
                case "intparam" -> {
                    paramString[0] = int.class;
                    method = cls.getDeclaredMethod(myMethod, paramString);
                    method.invoke(obj, Integer.valueOf(myParam));
                }
                case "doubleparam" -> {
                    paramString[0] = double.class;
                    method = cls.getDeclaredMethod(myMethod, paramString);
                    method.invoke(obj, Double.valueOf(myParam));
                }
                case "booleanparam" -> {
                    paramString[0] = boolean.class;
                    method = cls.getDeclaredMethod(myMethod, paramString);
                    method.invoke(obj, Boolean.valueOf(myParam));
                }
                case "omicsModelList" -> {
                    paramString[0] = ArrayList.class;
                    method = cls.getDeclaredMethod(myMethod, paramString);
                    // ArrayList<OmicsModel> dataSets = (ArrayList<OmicsModel>) DataUtils.convertJsonToObj(myParam, myParamType);
                    // method.invoke(obj, dataSets);
                }
                default -> {
                    paramString[0] = ArrayList.class;
                    DataUtils.convertJsonToObj(obj, myParam, myParamType);
                }
            }
        } catch (Exception ex) {
            LOGGER.error("Error executing method: " + myMethod + " on class: " + myClassType, ex);
        }
    }

    public void navToProject() {

        String navitype = sb.getNaviType();
        sb.setNaviType("NA");
        //sb.buildCustomNaviTree();
        sb.initNaviTree(navitype);
        DataUtils.doRedirect("/MetaboAnalyst/" + sb.getCurrentNaviUrl(), ab);
    }

    public void navToProject(String naviString) {

        String navitype = sb.getNaviType();
        sb.setNaviType("NA");
        //sb.buildCustomNaviTree();

        sb.initNaviTree(navitype);

        if (naviString.startsWith("/")) {
            naviString = naviString.substring(1);
        }
        if (naviString.equals("")) {
            naviString = pb.getSelectedProject().getNaviStr();
        }

        DataUtils.doRedirect("/MetaboAnalyst/" + naviString, ab);
    }

    public String getShareableLink() {
        return shareableLink;
    }

    public void setShareableLink(String shareableLink) {
        this.shareableLink = shareableLink;
    }

    public void showShareMsg() {
        sb.addMessage("info", "Link has been copied!");
    }

    private boolean userInit = false;

    public boolean reloadUserInfo() {
        String value = Faces.getRequestCookie("user");
        userInit = false;

        if (value != null) {
            FireUserBean stored = fb.getUserMap().get(value);
            if (stored == null) {
                sb.addMessage("Error", "Failed to reload info!");
                return false;
            }

            fub.setFireUserBean(stored);          // see below
            sb.setRegisteredLogin(true);
            userInit = true;
        }

        return userInit;
    }

    private static final SecureRandom secureRandom = new SecureRandom(); //threadsafe
    private static final Base64.Encoder base64Encoder = Base64.getUrlEncoder(); //threadsafe

    public static String generateNewToken() {
        byte[] randomBytes = new byte[24];
        secureRandom.nextBytes(randomBytes);
        return base64Encoder.encodeToString(randomBytes);
    }

    private boolean projInit = false;

    public void initUserProjects() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }

        reloadUserInfo();

        if (fub.getEmail() == null || fub.getEmail().equals("")) {// on local do not need to login
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "error", "Please login first!");
        } else {
            pb.setActiveTabIndex(0);
            setupProjectTable("project");
            //setupProjectTable("workflow");
            projInit = true;
        }

    }

    private static String readSecret(Path path) {
        try {
            // Read whole file, trim trailing newlines/spaces
            return Files.readString(path, StandardCharsets.UTF_8).trim();
        } catch (Exception e) {
            // Don't log the secret; just explain what's wrong
            LOGGER.warn("Unable to read password from {}", path, e);
            return "";
        }
    }

    public void toModuleView() throws IOException {
        if (ab.isOnZgyPc()) {
            fub.setEmail("guangyan.zhou@xialab.ca");
            fub.setPassword(readSecret(Path.of("/home/zgy/NetBeansProjects/password.txt")));
            fub.doUserLogin();
            DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/ModuleView.xhtml", ab);
            return;
        }

        if (ab.isOnLocalServer()) {
            //need to access Secure folder
            FacesContext.getCurrentInstance().getExternalContext().getSessionMap().put("MA6_PRO_user", true);
            DataUtils.doRedirect("/MetaboAnalyst/Secure/ModuleView.xhtml", ab);
        } else {
            PrimeFaces.current().executeScript("PF('notLoginDialog').show()");
        }
    }

    public boolean isDisabledSave() {

        List aList = new ArrayList();

        aList.add("Upload");
        aList.add("Mode Selection");

        return aList.contains(sb.getCurrentPageID());

    }

    private String projectToLoadId = "";

    public String getProjectToLoadId() {
        return projectToLoadId;
    }

    public void setProjectToLoadId(String projectToLoadId) {
        this.projectToLoadId = projectToLoadId;
    }

    public boolean setupProjectTable(String type) {

        try {
            ArrayList<HashMap<String, Object>> res = db.getProjectsFromPostgres(fub.getEmail(), ab.getAppName(), ab.getToolLocation());
            if (type.equals("project")) {
                pb.setProjectTable(new ArrayList());
            } else {
                pb.setWorkflowProjectTable(new ArrayList());

            }
            if (res == null) {
                return false;
            }

            boolean loadProjectBool = false;
            for (HashMap<String, Object> myHashMap : res) {
                Map<String, Object> myMap = myHashMap;

                Object projectTypeObject = myMap.get("projecttype");
                if (projectTypeObject != null && !type.equals(projectTypeObject.toString())) {
                    continue; // Skip this project and move to the next iteration
                }
                ProjectModel project = createProjectFromMap(myMap);
                if (type.equals("project")) {
                    pb.getProjectTable().add(project);
                } else {
                    pb.getWorkflowProjectTable().add(project);
                }
                if (!projectToLoadId.isEmpty()) {
                    Object idObject = myMap.get("id"); // This will get the value associated with 'Id' key
                    if (idObject != null) { // Check if the 'Id' exists
                        String idString = idObject + "";
                        if (idString.equals(projectToLoadId)) {
                            pb.setProjectToLoad(project);
                            loadProjectBool = true;
                            projectToLoadId = "";
                        }
                    }
                }
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

    private String safeGet(Map<String, Object> map, String key, String defaultValue) {
        Object value = map.get(key);
        if (value != null) {
            String valueString = value.toString();
            try {
                // Try to parse the value as a double and cast to int if it contains a decimal point
                if (valueString.contains(".")) {
                    return Integer.toString((int) Double.parseDouble(valueString));
                } else {
                    // Try to parse the value directly as an integer
                    return Integer.toString(Integer.parseInt(valueString));
                }
            } catch (NumberFormatException e) {
                // If parsing fails, return the original string value
                return valueString;
            }
        }
        return defaultValue;
    }

    public ProjectModel createProjectFromMap(Map<String, Object> docData) {
        try {
            ProjectModel project = new ProjectModel();

            // Apply safeGet for each expected value
            String projectName = safeGet(docData, "name", "NA");
            String description = safeGet(docData, "description", "NA");
            String org = safeGet(docData, "org", "NA");
            String analType = safeGet(docData, "module", "NA");
            String dataType = safeGet(docData, "datatype", "NA");
            String folderName = safeGet(docData, "foldername", "NA");
            String javaHistory = safeGet(docData, "javahistory", "NA");
            String naviStr = safeGet(docData, "navistring", "NA");
            String dateStr = safeGet(docData, "date", "NA");
            String shareToken = safeGet(docData, "partialtoken", "NA");
            String hostname = safeGet(docData, "location", "NA");

            Date date = null;
            if (!dateStr.equals("NA")) {
                date = new SimpleDateFormat("yyyy-MM-dd").parse(dateStr);
            }

            project.setTitle(projectName);
            project.setDescription(description);
            project.setType(analType);
            project.setDataType(dataType);
            project.setProjectType(safeGet(docData, "projecttype", "NA"));

            project.setOrg(org);
            project.setCreationDate(date); // This will be null if dateStr was "NA"
            project.setFolderName(folderName);
            project.setJavaHistory(javaHistory);
            project.setNaviStr(naviStr);
            project.setId(Integer.parseInt(safeGet(docData, "id", "0")));
            project.setPaired(Boolean.parseBoolean(safeGet(docData, "paired", "false")));
            project.setRegression(Boolean.parseBoolean(safeGet(docData, "regression", "false")));
            project.setShareToken(shareToken);
            project.setHostname(hostname);

            return project;
        } catch (Exception e) {
            // Logging the exception (replace with your own logging mechanism if needed)
            e.printStackTrace();
            return null;
        }
    }

    public boolean prepareProject(boolean naviBool) {
        ProjectModel selectedProject = pb.getSelectedProject();
        String folderName = selectedProject.getFolderName();
        String javaHistory = selectedProject.getJavaHistory();
        sb.setCurrentNaviUrl(selectedProject.getNaviStr());
        //creater user folder with same name
        boolean success = sb.doLogin(selectedProject.getDataType(), selectedProject.getType(), selectedProject.isRegression(), selectedProject.isPaired(), folderName, true);
        if (!success) {
            sb.addMessage("Error", "Loading project failed, can not login!");
            return false;
        }
        sb.setOrg(selectedProject.getOrg());
        sb.setShareToken(selectedProject.getShareToken());
        String destDirPath = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";
        String userFolderName = fub.getEmail();

        String bucketObjectName = "/user_folders/" + userFolderName + "/" + folderName + ".zip";
        String localFilePath = fb.getProjectPath() + bucketObjectName;
        File f = new File(localFilePath);
        if (f.exists()) {
            DataUtils.extract(localFilePath, destDirPath);
        } else {
            bucketObjectName = "/user_folders/" + "guest" + "/" + folderName + ".zip";
            localFilePath = fb.getProjectPath() + bucketObjectName;
            f = new File(localFilePath);
            if (!f.exists()) {
                downloadObject(selectedProject.getHostname(), userFolderName, folderName, destDirPath + folderName + ".zip");
                DataUtils.extract(destDirPath + folderName + ".zip", destDirPath);
            }
        }
        RCenter.loadHistory(sb.getRConnection());

        if (javaHistory.isEmpty()) {
            javaHistory = readJsonStringFromFile(sb.getCurrentUser().getOrigHomeDir() + File.separator + "java_history.json");
        }
        int res = loadJavaHistory(javaHistory);

        for (String module : wb.getModuleNames()) {
            RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), module);
        }
        String naviType = sb.getNaviType();
        sb.setNaviTree(NaviUtils.createNaviTree(naviType));
        if (res == 0) {
            sb.addMessage("Error", "Loading project failed, can not restore java state!");
            return false;
        }

        setFireDocName(selectedProject.getTitle());
        setFireDocDescription(selectedProject.getDescription());
        //System.out.println("navibool====222" + sb.getNaviTrack().size());

        if (naviBool) {
            if (sb.getCurrentNaviUrl() != null) {
                String modifiedPath = sb.getCurrentNaviUrl().replace("StaticHeatmapView", "HeatmapView");
                sb.setCurrentNaviUrl(modifiedPath);
            }
            //System.out.println("navibool====" + sb.getCurrentNaviUrl());
            navToProject(sb.getCurrentNaviUrl());
        }
        return true;
    }

    public boolean prepareRawProject(boolean naviBool) {

        ProjectModel selectedProject = pb.getSelectedProject();
        String folderName = selectedProject.getFolderName();
        String javaHistory = selectedProject.getJavaHistory();
        sb.setNaviString(selectedProject.getNaviStr());
        //creater user folder with same name

        boolean success = sb.doSpecLogin(selectedProject.getDataType(), selectedProject.getType(),
                selectedProject.isRegression(), selectedProject.isPaired(), folderName, false);

        //boolean success = sb.doLogin(selectedProject.getDataType(), selectedProject.getType(), selectedProject.isRegression(), selectedProject.isPaired(), folderName, true);
        if (!success) {
            sb.addMessage("Error", "Loading project failed, can not login!");
            return false;
        }
        sb.setOrg(selectedProject.getOrg());
        sb.setShareToken(selectedProject.getShareToken());

        String userFolderName = fub.getEmail();

        long jobid = db.extractRawJobStatus(folderName, userFolderName);
        spcb.setCurrentJobId((long) jobid);
        spcb.setJobSubmitted(true);
        RCenter.loadHistory(sb.getRConnection());

        if (javaHistory.isEmpty()) {
            javaHistory = readJsonStringFromFile(sb.getCurrentUser().getOrigHomeDir() + File.separator + "java_history.json");
        }
        int res = loadJavaHistory(javaHistory);

        for (String module : wb.getModuleNames()) {
            RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), module);
        }
        String naviType = sb.getNaviType();
        sb.setNaviTree(NaviUtils.createNaviTree(naviType));
        if (res == 0) {
            sb.addMessage("Error", "Loading project failed, can not restore java state!");
            return false;
        }
        if (naviBool) {
            navToProject(sb.getCurrentNaviUrl());
        }
        return true;
    }

    public void generateHighDef(HttpServletResponse response) {
        String key = sb.getImageSource();
        String mydpi = "150";
        String formatOpt = sb.getFormatOpt();
        int dpiOpt = sb.getDpiOpt();
        if (formatOpt.equals("png") || formatOpt.equals("tiff")) {
            mydpi = dpiOpt + "";
        }
        String rcmd = sb.getGraphicsMap().get(key);
        if (rcmd == null) {
            sb.addMessage("Error", "No command found for plotting the image!");
            return;
        }
        rcmd = rcmd.replace("png", formatOpt);
        rcmd = rcmd.replace("150", mydpi + "");
        String imgName = key + "_" + sb.getImgMap().get(key) + "_dpi150.png";
        imgName = imgName.replace("png", formatOpt);
        imgName = imgName.replace("150", mydpi + "");

        try {
            // Execute the R command
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rcmd);
            RC.voidEval(rcmd);

            // Read the file and write to response output stream
            response.setContentType("application/octet-stream");
            response.setHeader("Content-Disposition", "attachment; filename=" + imgName);

            try (InputStream inStream = new FileInputStream(sb.getCurrentUser().getHomeDir() + File.separator + imgName); OutputStream outStream = response.getOutputStream()) {

                byte[] buffer = new byte[4096];
                int bytesRead;
                while ((bytesRead = inStream.read(buffer)) != -1) {
                    outStream.write(buffer, 0, bytesRead);
                }
                outStream.flush();
            }
            sb.doLogout(0);
        } catch (Exception e) {
            LOGGER.error("generateHighDef", e);
        }
    }

    public void generateHighDef2() {
        String key = sb.getImageSource();
        String mydpi = "150";
        String formatOpt = sb.getFormatOpt();
        int dpiOpt = sb.getDpiOpt();
        if (formatOpt.equals("png") || formatOpt.equals("tiff")) {
            mydpi = dpiOpt + "";
        }

        String rcmd = sb.getGraphicsMap().get(key);

        if (key.startsWith("plotms2Mirror") || key.startsWith("plotrawms2Mirror")) {
            if (key.startsWith("plotms2Mirror")) {
                // this function is used to plot mirror plot for ms2 module
                String[] rcmds = key.split("__");
                rcmd = "plotms2Mirror(NA, \"" + rcmds[1] + "\", " + rcmds[2] + ")";
                key = key.replaceAll("plotms2Mirror__", "dyn_mplot_");
                key = key.replaceAll("__", "_");
            } else if (key.startsWith("plotrawms2Mirror")) {
                // this function is used to plot mirror plot for raw spec module
                String[] rcmds = key.split("__");
                rcmd = "plotrawms2Mirror(NA, \"" + rcmds[1] + "\", " + rcmds[2] + ")";
                key = key.replaceAll("plotrawms2Mirror__", "dyn_mplot_");
                key = key.replaceAll("__", "_");
            }
        }

        if (rcmd == null) {
            sb.addMessage("Error", "No command found for plotting the image!");
            return;
        }
        rcmd = rcmd.replace("png", formatOpt);
        rcmd = rcmd.replace("150", mydpi + "");

        String imgName = key + "_" + sb.getImgMap().get(key) + "_dpi150.png";

        if (sb.getImgMap().get(key) == null) {
            imgName = key + ".png";
        } else if (rcmd.startsWith("PlotKEGGPath")) {
            String[] sts = rcmd.split("\"");
            String pathway_nm = sts[1];
            pathway_nm = pathway_nm.replaceAll("\\s", "_");
            pathway_nm = pathway_nm.replaceAll(",", "");
            imgName = pathway_nm + "_dpi150.png";
        }

        imgName = imgName.replace("png", formatOpt);
        if (imgName.startsWith("meta_bubble_")) {
            mydpi = "300";
        } else if (imgName.startsWith("raw_spec_stic_")) {
            imgName = imgName.replace("raw_spec_stic_", "");
        } else if (imgName.startsWith("raw_spec_msf_")) {
            imgName = imgName.replace("raw_spec_msf_", "");
        }

        imgName = imgName.replace("150", mydpi + "");

        try {
            // Execute the R command
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rcmd);
            RC.voidEval(rcmd);

            // Read the file and write to response output stream, Construct the URL to the image
            String imageURL = sb.getCurrentUser().getRelativeDir() + File.separator + imgName;
            //System.out.println("imagepath==========" + imageURL);

            // another condition for raw seq data processing module
            if (sb.getAnalType().equals("raw")) {
                //imgName
                String userNM = sb.getCurrentUser().getName();
                imageURL = "resources/users/" + userNM + File.separator + imgName;
                String newImgPath = ab.getRealUserHomePath() + "/" + userNM + "/";
                //DataUtils.internalizeFile(imgName, sb.getCurrentUser().getRelativeDir(), newImgPath);
            }

            // Redirect to the image URL
            DataUtils.doRedirect("/" + ab.getAppName() + "/" + imageURL, ab);

        } catch (Exception e) {
            LOGGER.error("generateHighDef", e);
        }
    }

    public boolean loadReport(String projectId, String analNavi, String cmpd) {
        try {
            loadProject(projectId, "project");
        } catch (Exception ex) {
            Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
        }
        sb.setReloadReportImage(true);
        handleNaviCases(analNavi, cmpd);
        // optional: handle other cases here or do nothing
        System.out.println("currentnaviurl========" + sb.getCurrentNaviUrl());
        navToProject(sb.getCurrentNaviUrl());

        return true;
    }

    public boolean handleNaviCases(String analNavi, String cmpd) {
        boolean res = true;
        switch (analNavi) {
            case "upset" ->
                sb.setCurrentNaviUrl("/Secure/metastat/UpsetDiagramView.xhtml");
            case "pca_3d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PCAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=4");
            case "plsda_3d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=4");
            case "splsda_3d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SparsePLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=3");
            case "ipca_3d" ->
                sb.setCurrentNaviUrl("/Secure/multifac/LivePCAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=1");
            case "heatmap2" ->
                sb.setCurrentNaviUrl("/Secure/multifac/Heatmap2View.xhtml");
            case "opls_score2d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/OrthoPLSDAView.xhtml");
            case "opls_splot" ->
                sb.setCurrentNaviUrl("/Secure/analysis/OrthoPLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=1");
            case "opls_mdl" ->
                sb.setCurrentNaviUrl("/Secure/analysis/OrthoPLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=3");
            case "opls_perm" ->
                sb.setCurrentNaviUrl("/Secure/analysis/OrthoPLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=4");
            case "pls_pair" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml");
            case "spls_pair" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SparsePLSDAView.xhtml");
            case "spls_score2d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SparsePLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=1");
            case "spls_loading" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SparsePLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=2");
            case "spls_cv" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SparsePLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=4");
            case "pls_score2d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=1");
            case "pls_loading" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=2");
            case "pls_imp" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=3");
            case "pls_cv" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=5");
            case "pls_perm" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PLSDAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=6");
            case "pca_pair" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PCAView.xhtml");
            case "pca_scree" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PCAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=1");
            case "pca_score2d" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PCAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=2");
            case "pca_biplot" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PCAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=5");
            case "pca_score2d_meta" ->
                sb.setCurrentNaviUrl("/Secure/multifac/LivePCAView.xhtml");
            case "pca_score_meta" ->
                sb.setCurrentNaviUrl("/Secure/analysis/LivePCAView.xhtml?faces-redirect=true&tabWidgetId=tabWidgetVar&activeTab=1");
            case "heatmap_mummichog" -> {
                if (sb.getReportJsonMap().containsKey("heatmap_mummichog")) {
                    mab.setHeatmapName(sb.getReportJsonMap().get("heatmap_mummichog"));
                }
                sb.setCurrentNaviUrl("/Secure/viewer/HeatmapView.xhtml");
            }
            case "heatmap_pathway" -> {
                if (sb.getReportJsonMap().containsKey("heatmap_pathway")) {
                    patb.setHeatmapName(sb.getReportJsonMap().get("heatmap_pathway"));
                }
                sb.setCurrentNaviUrl("/Secure/viewer/HeatmapView.xhtml");
            }
            case "network_gsea", "network_integ", "network_mummichog" -> {
                switch (analNavi) {
                    case "network_gsea" -> {
                        String[] stringArray = new String[1];
                        stringArray[0] = "gsea";
                        mab.setAlgOpts(stringArray);
                    }
                    case "network_integ" -> {
                        String[] stringArray = new String[2];
                        stringArray[0] = "mum";
                        stringArray[1] = "gsea";
                        mab.setAlgOpts(stringArray);
                    }
                    default -> {
                        String[] stringArray = new String[1];
                        stringArray[0] = "mum";
                        mab.setAlgOpts(stringArray);
                    }
                }
                sb.setCurrentNaviUrl("/Secure/mummichog/KeggNetView.xhtml");
            }
            case "norm" ->
                sb.setCurrentNaviUrl("/Secure/process/NormalizationView.xhtml?funcNm=OpenDialog&cmd=normResDialog");
            case "correlation_heatmap" ->
                sb.setCurrentNaviUrl("/Secure/analysis/CorrelationView.xhtml");
            case "volcano" ->
                sb.setCurrentNaviUrl("/Secure/analysis/VolcanoView.xhtml");
            case "fc" ->
                sb.setCurrentNaviUrl("/Secure/analysis/FoldChangeView.xhtml");
            case "tt" ->
                sb.setCurrentNaviUrl("/Secure/analysis/TtestView.xhtml");
            case "ptn" ->
                sb.setCurrentNaviUrl("/Secure/analysis/PatternView.xhtml");
            case "ptn_multifac" ->
                sb.setCurrentNaviUrl("/Secure/multifac/PartialCorrView.xhtml");
            case "sam" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SAMView.xhtml");
            case "ebam" ->
                sb.setCurrentNaviUrl("/Secure/analysis/EBAMView.xhtml");
            case "tree" ->
                sb.setCurrentNaviUrl("/Secure/analysis/TreeView.xhtml");
            case "heatmap" ->
                sb.setCurrentNaviUrl("/Secure/analysis/HeatmapView.xhtml");
            case "som" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SOMView.xhtml");
            case "rf_cls" ->
                sb.setCurrentNaviUrl("/Secure/analysis/RFView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=0");
            case "rf_imp" ->
                sb.setCurrentNaviUrl("/Secure/analysis/RFView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "rf_outlier" ->
                sb.setCurrentNaviUrl("/Secure/analysis/RFView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=2");
            case "rf_cls_multifac" ->
                sb.setCurrentNaviUrl("/Secure/multifac/MultifacRFView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=0");
            case "rf_imp_multifac" ->
                sb.setCurrentNaviUrl("/Secure/multifac/MultifacRFView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "rf_outlier_multifac" ->
                sb.setCurrentNaviUrl("/Secure/multifac/MultifacRFView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=2");
            case "km" ->
                sb.setCurrentNaviUrl("/Secure/analysis/KMView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "km_pca" ->
                sb.setCurrentNaviUrl("/Secure/analysis/KMView.xhtml");
            case "svm_imp" ->
                sb.setCurrentNaviUrl("/Secure/analysis/RSVMView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "svm_cls" ->
                sb.setCurrentNaviUrl("/Secure/analysis/RSVMView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=0");
            case "roc_univ", "roc_boxplot" -> {
                rab.setCurrentCmpd(null);
                rab.performDefaultDetailRocAnalysis(cmpd);
                sb.setCurrentNaviUrl("/Secure/roc/RocDetailView.xhtml");
            }
            case "tt_table" -> {
                sb.detailsLnk_action("tt");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "fc_table" -> {
                sb.detailsLnk_action("fc");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "volcano_table" -> {
                sb.detailsLnk_action("volcano");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "ptn_table", "ptn_table_multifac" -> {
                sb.detailsLnk_action("template");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "sam_table" -> {
                sb.detailsLnk_action("sam");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "ebam_table" -> {
                sb.detailsLnk_action("ebam");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "som_table" ->
                sb.setCurrentNaviUrl("/Secure/analysis/SOMView.xhtml?funcNm=OpenDialog&cmd=detailDialog");
            case "uniroc_table" ->
                sb.setCurrentNaviUrl("/Secure/roc/UnivRocView.xhtml");
            case "cls_roc" ->
                sb.setCurrentNaviUrl("/Secure/roc/MultiRocView.xhtml");
            case "cls_prob" ->
                sb.setCurrentNaviUrl("/Secure/roc/MultiRocView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "cls_accu" ->
                sb.setCurrentNaviUrl("/Secure/roc/MultiRocView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=2");
            case "cls_imp" ->
                sb.setCurrentNaviUrl("/Secure/roc/MultiRocView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=3");
            case "cls_test_roc" ->
                sb.setCurrentNaviUrl("/Secure/roc/RocTestView.xhtml");
            case "cls_test_prob" ->
                sb.setCurrentNaviUrl("/Secure/roc/RocTestView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "cls_test_accu" ->
                sb.setCurrentNaviUrl("/Secure/roc/RocTestView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=2");
            case "roc_perm" ->
                sb.setCurrentNaviUrl("/Secure/roc/RocTestView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=3");
            case "roc_new_samples" ->
                sb.setCurrentNaviUrl("/Secure/roc/RocTestView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=4");
            case "power_stat" ->
                sb.setCurrentNaviUrl("/Secure/utils/PowerParamView.xhtml");
            case "power_profile" ->
                sb.setCurrentNaviUrl("/Secure/utils/PowerCurveView.xhtml");
            case "ora" ->
                sb.setCurrentNaviUrl("/Secure/enrichment/OraView.xhtml");
            case "ora_dot" ->
                sb.setCurrentNaviUrl("/Secure/enrichment/OraView.xhtml?faces-redirect=true&scrollTo=ora_dotForm");
            case "ora_resTbl" ->
                sb.setCurrentNaviUrl("/Secure/enrichment/OraView.xhtml?faces-redirect=true&scrollTo=resTblForm");
            case "qea" ->
                sb.setCurrentNaviUrl("/Secure/enrichment/QeaView.xhtml");
            case "qea_dot" ->
                sb.setCurrentNaviUrl("/Secure/enrichment/QeaView.xhtml?faces-redirect=true&scrollTo=qea_dotForm");
            case "qea_resTbl" ->
                sb.setCurrentNaviUrl("/Secure/enrichment/QeaView.xhtml?faces-redirect=true&scrollTo=resTblForm");
            case "name_map_network" ->
                sb.setCurrentNaviUrl("/Secure/network/MnetMapView.xhtml");
            case "name_map_network_gene" ->
                sb.setCurrentNaviUrl("/Secure/network/MnetMapView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "name_map_pathinteg" ->
                sb.setCurrentNaviUrl("/Secure/pathinteg/IntegMapView.xhtml");
            case "name_map_pathinteg_cmpd" ->
                sb.setCurrentNaviUrl("/Secure/pathinteg/IntegMapView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "name_map" ->
                sb.setCurrentNaviUrl("/Secure/process/NameMapView.xhtml");
            case "meta_peaks_to_paths", "meta_peaks_to_paths_gsea" ->
                sb.setCurrentNaviUrl("/Secure/metapath/MetaPathResultView.xhtml");
            case "peaks_to_paths" ->
                sb.setCurrentNaviUrl("/Secure/mummichog/MummiResultView.xhtml");
            case "integ_peaks" ->
                sb.setCurrentNaviUrl("/Secure/mummichog/IntegMumResultView.xhtml");
            case "integ_resTbl" ->
                sb.setCurrentNaviUrl("/Secure/mummichog/IntegMumResultView.xhtml?faces-redirect=true&scrollTo=resTblForm");
            case "mum_resTbl" ->
                sb.setCurrentNaviUrl("/Secure/mummichog/MummiResultView.xhtml?faces-redirect=true&scrollTo=resTblForm");
            case "gsea_resTbl" ->
                sb.setCurrentNaviUrl("/Secure/mummichog/GseaResultView.xhtml?faces-redirect=true&scrollTo=resTblForm");
            case "peaks_to_paths_gsea" ->
                sb.setCurrentNaviUrl("/Secure/mummichog/GseaResultView.xhtml");
            case "network_MetaboNet" ->
                sb.setCurrentNaviUrl("/Secure/network/MetaboNetView.xhtml");
            case "metastat_restbl" ->
                sb.setCurrentNaviUrl("/Secure/metastat/MetaResultView.xhtml");
            case "path_view" ->
                sb.setCurrentNaviUrl("/Secure/pathway/PathResultView.xhtml");
            case "path_view_integ" ->
                sb.setCurrentNaviUrl("/Secure/pathinteg/IntegResultView.xhtml");
            case "path_view_restbl" ->
                sb.setCurrentNaviUrl("/Secure/pathway/PathResultView.xhtml?faces-redirect=true&scrollTo=resTblForm");
            case "metainfo" ->
                sb.setCurrentNaviUrl("/Secure/process/MetaDataCheck.xhtml");
            case "filter" ->
                sb.setCurrentNaviUrl("/Secure/process/FilterView.xhtml");
            case "meba" ->
                sb.setCurrentNaviUrl("/Secure/multifac/TimeCourseView.xhtml");
            case "aov2" ->
                sb.setCurrentNaviUrl("/Secure/multifac/Anova2View.xhtml");
            case "aov2_table" -> {
                sb.detailsLnk_action("anova2");
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "asca_scree" ->
                sb.setCurrentNaviUrl("/Secure/multifac/AscaView.xhtml");
            case "asca_perm" ->
                sb.setCurrentNaviUrl("/Secure/multifac/AscaView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=3");
            case "asca_fa", "asca_fab", "asca_fb" ->
                sb.setCurrentNaviUrl("/Secure/multifac/AscaView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "asca_impa", "asca_impab", "asca_impb" ->
                sb.setCurrentNaviUrl("/Secure/multifac/AscaView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=2");
            case "asca.sigA", "asca.sigB", "asca.sigAB" -> {
                sb.detailsLnk_action(analNavi);
                sb.setCurrentNaviUrl("/Secure/FeatureDetailsView.xhtml");
            }
            case "covariate_plot" ->
                sb.setCurrentNaviUrl("/Secure/multifac/LinearModelView.xhtml");
            case "metaHeatmap" ->
                sb.setCurrentNaviUrl("/Secure/multifac/MetaDataView.xhtml");
            case "metaCorrHeatmap" ->
                sb.setCurrentNaviUrl("/Secure/multifac/MetaDataView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeTab=1");
            case "network", "gene_metabolites", "metabo_phenotypes", "metabo_metabolites", "global", "dspc" -> {
                //MnetResBean mnb = (MnetResBean) DataUtils.findBean("mnetResBean");

                sb.setVisMode(analNavi);
                sb.setCurrentNaviUrl("/Secure/network/MphenoNetView.xhtml");
            }
            case "enrichment_network" -> {
                sb.setCurrentNaviUrl("/Secure/enrichment/OraView.xhtml");
            }
            case "dose_volcano" -> {
                sb.setCurrentNaviUrl("/Secure/dose/SigFeatureView.xhtml");
            }

            case "PlotDRHistogram" -> {
                sb.setCurrentNaviUrl("/Secure/dose/ModelFitView.xhtml");
            }

            case "PlotDRModelBars" -> {
                sb.setCurrentNaviUrl("/Secure/dose/FitResultView.xhtml");
            }

            case "mr_results_merge" -> {
                sb.setCurrentNaviUrl("/Secure/mgwas/ResultView.xhtml");
            }
            case "harmonized_dat" -> {
                sb.setCurrentNaviUrl("/Secure/mgwas/ParamView.xhtml");
            }
            case "mr_scatter_plot", "mr_forest_plot", "mr_leaveoneout_plot", "mr_funnel_plot" -> {
                sb.setCurrentNaviUrl("/Secure/mgwas/ResultView.xhtml");
            }
            default -> {
                res = false;
            }
        }
        return res;
    }

    public boolean isDisplayDefaultReportButton() {
        Set<String> urls = new HashSet<>();
        //PCAs
        urls.add("/" + ab.getAppName() + "/Secure/analysis/PCAView.xhtml");
        urls.add("/" + ab.getAppName() + "/Secure/analysis/PLSDAView.xhtml");
        urls.add("/" + ab.getAppName() + "/Secure/analysis/SparsePLSDAView.xhtml");
        urls.add("/" + ab.getAppName() + "/Secure/multifac/LivePCAView.xhtml");
        //
        urls.add("/" + ab.getAppName() + "/Secure/viewer/HeatmapView.xhtml");
        urls.add("/" + ab.getAppName() + "/Secure/spectra/SpectraResult.xhtml");

        /*
        urls.add("/Secure/mummichog/GseaResultView.xhtml");
        urls.add("/Secure/mummichog/IntegMumResultView.xhtml");
        urls.add("/Secure/mummichog/MummiResultView.xhtml");
         */
        urls.add("/" + ab.getAppName() + "/Secure/mummichog/KeggNetView.xhtml");
        urls.add("/" + ab.getAppName() + "/Secure/network/MetaboNetView.xhtml");
        urls.add("/" + ab.getAppName() + "/Secure/network/MphenoNetView.xhtml");

        urls.add("/" + ab.getAppName() + "/Secure/enrichment/OraView.xhtml");

        FacesContext context = FacesContext.getCurrentInstance();
        String viewId = context.getViewRoot().getViewId();
        String baseUrl = context.getExternalContext().getRequestContextPath();
        String url = baseUrl + viewId;

        return !urls.contains(url);
    }

    public String FindRhist2BatchTemplate() {

        ProjectModel selectedProject = pb.getSelectedProject();
        String folderName = selectedProject.getFolderName();

        if (checkBatchTemplateDir()) {
            //System.out.println("now the folderName in extractRhist2BatchTemplate is ===> " + folderName);
            String Batch_temp_path_this_proj = fb.getProjectPath();
            Batch_temp_path_this_proj = Batch_temp_path_this_proj + "BatchProcess/" + ab.getAppName() + "/" + folderName + "_RBatchTemplate.qs";
            return Batch_temp_path_this_proj;
        } else {
            sb.addMessage("Error", "Your project home is not writable! Please report this to admin!");
        }

        return null;
    }

    private boolean checkBatchTemplateDir() {
        String home_dir = fb.getProjectPath();
        String appNm = ab.getAppName();
        //File dir exits?
        File BatchDir = new File(home_dir + "/BatchProcess/" + appNm);
        if (!BatchDir.exists()) {
            BatchDir.mkdirs();
        }
        return BatchDir.exists();
    }

    public void checkLogout() {
        //System.out.println("logoutcheck========");
        if (sb.getCurrentUser() == null) {
            doXialabLogout();
        } else {
            PrimeFaces.current().executeScript("PF('saveWarningDialog').show()");
        }
    }

    public void checkLogoutModule() {
        if (sb.getCurrentUser() == null) {
            DataUtils.doRedirect("/MetaboAnalyst/Secure/ModuleView.xhtml", ab);
        } else {
            PrimeFaces.current().executeScript("PF('saveWarningDialogModule').show()");
        }
    }

    public void doXialabLogout() {
        // Invalidate the cookie
        Faces.addResponseCookie("user", "", "/", 0);

        // Assuming you have access to the FireBase instance (fb) and the user's token
        if (fb != null && fub != null) {
            String userToken = fub.getOmicsquareToken();
            if (fb.getUserMap().containsKey(userToken)) {
                fb.getUserMap().remove(userToken);
            }
        }

        sb.setRegisteredLogin(false);
        sb.doLogout(1);
    }

    // Function to save JSON String to a file
    public static void saveJsonStringToFile(String jsonString, String filePath) {
        try (FileWriter file = new FileWriter(filePath)) {
            file.write(jsonString);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Function to read JSON String from a file
    public static String readJsonStringFromFile(String filePath) {
        StringBuilder jsonString = new StringBuilder();
        try (Reader reader = new FileReader(filePath)) {
            int data = reader.read();
            while (data != -1) {
                jsonString.append((char) data);
                data = reader.read();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return jsonString.toString();
    }

    public String goToSpectraUpload() {
        String myURL;
        //
        if (ab.isOnLocalServer()) { // local
            myURL = ab.getDomainURL() + "/Secure/upload/SpectraUpload.xhtml";
        } else if (ab.isInDocker()) {
            myURL = ab.getDomainURL() + "/Secure/upload/SpectraUpload.xhtml";
        } else {
            if (ab.getVersionCode().equals("2025R2")) {
                myURL = "https://vip2.metaboanalyst.ca/MetaboAnalyst/Secure/upload/SpectraUpload.xhtml";
            } else {
                myURL = "https://vip.metaboanalyst.ca/MetaboAnalyst/Secure/upload/SpectraUpload.xhtml";
            }

            if (!ab.isOnVipServer() & ab.getVersionCode().equals("2025R2")) {
                fub.sendPostRequest("vip2", "spec");
                return myURL;
            }

            if (!ab.isOnVipServer()) {
                fub.sendPostRequest("vip", "spec");
                return myURL;
            }
        }

        try {
            FacesContext.getCurrentInstance().getExternalContext().redirect(myURL);
            return null;
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("setUtilOpt", e);
        }
        return myURL;

    }

    public String goToSpectraWorkflowUpload() {

        String myURL = "https://vip.metaboanalyst.ca/MetaboAnalyst/Secure/workflow/upload/SpecGoogleUploadView.xhtml";
        if (!ab.isOnVipServer()) {
            fub.sendPostRequest("vip", "WfSpecGoogleUploadView");
            return myURL;
        }

        return myURL;
    }

    public void addFeatureToReport() {
        RDataUtils.addFeatureToReport(sb.getRConnection(), sb.getCurrentCmpdName(), sb.getCmpdSummaryNm());
        sb.addMessage("info", "This feature has been added to report!");
    }

    public void addMetaFeatureToReport() {
        RDataUtils.addFeatureToReport(sb.getRConnection(), mrb.getSelectedFeature().getName(), mrb.getCurrentFeatureImg());
        sb.addMessage("info", "This feature has been added to report!");
    }

    public void addDoseFeatureToReport() {
        RDataUtils.addDoseFeatureToReport(sb.getRConnection(), drb.getSelectedFeature().getName(), drb.getCurrentFeatureImg());
        sb.addMessage("info", "This feature has been added to report!");
    }

    private String runToLoadId = "";

    public String getRunToLoadId() {
        return runToLoadId;
    }

    public void setRunToLoadId(String runToLoadId) {
        this.runToLoadId = runToLoadId;
    }

    private static final double EPS = 1e-9;

    public static int safeIntFromDoubleLike(Object v) {
        if (v == null) {
            System.out.println("[safeIntFromDoubleLike] null -> 0");
            return 0;
        }
        try {
            // Fast path: integral Number types
            if (v instanceof Integer || v instanceof Short || v instanceof Byte) {
                return ((Number) v).intValue();
            }
            if (v instanceof Long) {
                long x = (Long) v;
                if (x < Integer.MIN_VALUE || x > Integer.MAX_VALUE) {
                    System.out.println("[safeIntFromDoubleLike] WARN: long out of int range: " + x);
                    return 0;
                }
                return (int) x;
            }

            // Double/Float → accept only if effectively whole
            if (v instanceof Double || v instanceof Float) {
                double d = ((Number) v).doubleValue();
                if (!Double.isFinite(d)) {
                    System.out.println("[safeIntFromDoubleLike] WARN: non-finite: " + d + " -> 0");
                    return 0;
                }
                double rounded = Math.rint(d);
                if (Math.abs(d - rounded) > EPS) {
                    System.out.println("[safeIntFromDoubleLike] WARN: fractional " + d + " -> 0");
                    return 0;
                }
                if (rounded < Integer.MIN_VALUE || rounded > Integer.MAX_VALUE) {
                    System.out.println("[safeIntFromDoubleLike] WARN: out of int range: " + rounded);
                    return 0;
                }
                return (int) rounded;
            }

            // Fallback for other Number impls
            if (v instanceof Number) {
                double d = ((Number) v).doubleValue();
                double rounded = Math.rint(d);
                if (!Double.isFinite(d) || Math.abs(d - rounded) > EPS
                        || rounded < Integer.MIN_VALUE || rounded > Integer.MAX_VALUE) {
                    System.out.println("[safeIntFromDoubleLike] WARN: unsupported/frac number " + v + " -> 0");
                    return 0;
                }
                return (int) rounded;
            }

            System.out.println("[safeIntFromDoubleLike] WARN: unsupported type "
                    + v.getClass().getName() + " -> 0");
            return 0;

        } catch (Exception ex) {
            System.out.println("[safeIntFromDoubleLike] ERROR parsing '" + v + "': " + ex.getMessage());
            return 0;
        }
    }

}
