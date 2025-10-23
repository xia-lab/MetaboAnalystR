/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.lts;

import java.io.File;
import pro.metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import org.primefaces.PrimeFaces;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.DownloadBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.project.ProjectModel;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.io.IOException;
import java.util.logging.Level;
import java.time.Duration;
import pro.metaboanalyst.workflows.WorkflowRunModel;

/**
 *
 * @author soufanom
 */
@SessionScoped
@Named("fireProjectBean")
public class FireProjectBean implements Serializable {

    @Inject
    private DatabaseClient db;

    private final static int PROJECT_LIMITS = 10;
    @Inject
    private ApplicationBean1 ab;

    @Inject
    private FireBase fb;

    @Inject
    private FireBaseController fbc;

    @Inject
    private FireUserBean fub;

    @Inject
    private SessionBean1 sb;

    @Inject
    private DownloadBean dwb;
    //Project details
    private String title;
    private String description;
    private String projectType = "project";
    //Project data table
    private List<ProjectModel> projectTable = new ArrayList<>();
    private List<ProjectModel> workflowProjectTable = new ArrayList<>();

    private HashMap<Long, ProjectModel> projectMap = null;
    private ProjectModel selectedProject = null;
    private ProjectModel projectToLoad = null;
    private ProjectModel myProject = null;
    private int activeTabIndex = 0;
// In your ProjectBean (or whatever pb is)
    private List<WorkflowRunModel> workflowRunsTable = new ArrayList<>();
    private WorkflowRunModel workflowRunToLoad; // optional, if you auto-select one
 private WorkflowRunModel selectedWorkflowRun;

    public WorkflowRunModel getSelectedWorkflowRun() {
        return selectedWorkflowRun;
    }

    public void setSelectedWorkflowRun(WorkflowRunModel selectedWorkflowRun) {
        this.selectedWorkflowRun = selectedWorkflowRun;
    }
 
 
    public List<WorkflowRunModel> getWorkflowRunsTable() {
        // ensure it's never null for the UI
        if (workflowRunsTable == null) {
            workflowRunsTable = new ArrayList<>();
        }
        return workflowRunsTable;
    }

    public void setWorkflowRunsTable(List<WorkflowRunModel> workflowRunsTable) {
        this.workflowRunsTable = (workflowRunsTable != null) ? workflowRunsTable : new ArrayList<>();
    }

// Optional helpers if you’re auto-picking a row to load
    public WorkflowRunModel getWorkflowRunToLoad() {
        return workflowRunToLoad;
    }

    public void setWorkflowRunToLoad(WorkflowRunModel run) {
        this.workflowRunToLoad = run;
    }

    public ProjectModel getProjectToLoad() {
        return projectToLoad;
    }

    public void setProjectToLoad(ProjectModel projectToLoad) {
        this.projectToLoad = projectToLoad;
    }

    public List<ProjectModel> getWorkflowProjectTable() {
        return workflowProjectTable;
    }

    public void setWorkflowProjectTable(List<ProjectModel> workflowProjectTable) {
        this.workflowProjectTable = workflowProjectTable;
    }

    public String checkProjectInfo() {
        if (selectedProject == null || sb.getCurrentUser() == null || !sb.getCurrentUser().getName().equals(selectedProject.getFolderName())) {
            return "Current Project: <i>" + "No project loaded.</i>";
        } else {
            return "Current Project: <i>" + selectedProject.getTitle() + " (Project " + selectedProject.getId() + ")</i>";

        }
    }

    public boolean selectionFlag(ProjectModel project) {
        if (selectedProject == null | sb.getCurrentUser() == null || !sb.getCurrentUser().getName().equals(selectedProject.getFolderName())) {
            return false;
        }
        return selectedProject.getId() == project.getId();
    }

    public boolean isbatchProj(ProjectModel project) {
        String jsh = project.getJavaHistory();
        return jsh.contains("\\\"isBatchProject\\\":true");
        //return false;
    }

    public int getActiveTabIndex() {
        return activeTabIndex;
    }

    public void setActiveTabIndex(int activeTabIndex) {
        this.activeTabIndex = activeTabIndex;
    }

    public int getProjectLimit() {
        return PROJECT_LIMITS;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getProjectType() {
        return projectType;
    }

    public void setProjectType(String projectType) {
        this.projectType = projectType;
    }

    public HashMap<Long, ProjectModel> getProjectMap() {
        return projectMap;
    }

    public void setProjectMap(HashMap<Long, ProjectModel> projectMap) {
        this.projectMap = projectMap;
    }

    public ProjectModel getSelectedProject() {
        return selectedProject;
    }

    public void setSelectedProject(ProjectModel selectedProject) {
        this.selectedProject = selectedProject;
    }

    public List<ProjectModel> getProjectTable() {
        return projectTable;
    }

    public void setProjectTable(List<ProjectModel> projectTable) {
        this.projectTable = projectTable;
    }

    public void viewEditDialog() {
        ProjectModel currentProject = this.getSelectedProject();
        if (currentProject == null) {
            sb.addMessage("Èrror", "Please select a project to upload.");
        } else {
            PrimeFaces.current().executeScript("PF('projectEditDialog').show();");
        }
    }

    boolean batchModeEnabled = false;

    String batchTemplatePath = null;

    public boolean isBatchModeEnabled() {
        return batchModeEnabled;
    }

    public void setBatchModeEnabled(boolean batchModeEnabled) {
        this.batchModeEnabled = batchModeEnabled;
    }

    public String getBatchTemplatePath() {
        return batchTemplatePath;
    }

    public String getTemplateToken() {
        return selectedProject.getShareToken();
    }

    public ProjectModel getMyProject() {
        return myProject;
    }

    public void setMyProject(ProjectModel myProject) {
        this.myProject = myProject;
    }

    public void callProjectDelete() {
        try {
            //FireBase fb = (FireBase) DataUtils.findBean("fireBase");
            //FireUserBean ub = (FireUserBean) DataUtils.findBean("fireUserBean");

            String folderNm = fub.getEmail();
            db.deleteProjectById((long) selectedProject.getId());

            DataUtils.deleteFile(fb.getProjectPath() + "/user_folders/" + folderNm + "/", selectedProject.getFolderName() + ".zip");
            //System.out.println("Document successfully deleted!");
            fbc.setupProjectTable("project");
            sb.addMessage("info", "Project " + selectedProject.getId() + " successfully deleted!");
            selectedProject = null;
        } catch (Exception e) {
            System.err.println("Error deleting document: " + e.getMessage());
        }
    }
    private String proceedType = "";

    public void loadProjectOrShowDialog() {
        if (!projectToLoad.getHostname().equals(ab.getToolLocation())) {
            PrimeFaces.current().executeScript("PF('switchServerDialog').show();");
        } else {
            if (sb.getCurrentUser() == null || sb.getCurrentUser().getName().equals(projectToLoad.getFolderName())) {
                settingProceedType("load");
            } else {
                PrimeFaces.current().executeScript("PF('switchProjDialog').show();");
            }
        }
    }

    public void settingProceedType(String type) {
        proceedType = type;
        if (type.equals("load")) {
            //System.out.println("projectToLoad=====" + projectToLoad.getId());
            selectedProject = projectToLoad;
        }
        System.out.println("proceedType ===" + type);
        proceedToAnotherProject();
    }

    public void proceedToAnotherProject() {
        switch (proceedType) {
            case "report" ->
                loadReport();
            case "view" -> {
                sb.goToResultPage();
            }
            case "load" -> {
                loadMainFolder(proceedType);
            }
            case "resume" -> {
                String this_datatype = selectedProject.getDataType();
                //System.out.println(this_datatype + "=======rawproject");
                if (this_datatype.equals("spec")) {
                    fbc.prepareRawProject(true);
                } else {
                    fbc.prepareProject(true);
                }
            }
            default -> {
            }
        }

    }

    public boolean loadReport() {

        boolean resOk = fbc.prepareProject(false);
        if (!resOk) {
            return false;
        }

        File newFile = new File(sb.getCurrentUser().getHomeDir() + "/Analysis_Report.html");
        //System.out.println("/" + ab.getAppName() + sb.getCurrentUser().getRelativeDir() + "/Analysis_Report.html" + "====" + newFile.exists());

        if (!newFile.exists()) {
            dwb.generateReport("html");
        }
        DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", ab);
        return true;
    }

    public boolean loadMainFolder(String nav) {

        String this_datatype = selectedProject.getDataType();

        if (!selectedProject.getHostname().equals(ab.getToolLocation()) && selectedProject.getDataType().equals("spec")) {
            fub.sendPostRequestLoadProject(selectedProject.getHostname(), selectedProject.getId() + "");
            return false;
        }

        boolean projOk;
        if (this_datatype.equals("spec")) {
            projOk = fbc.prepareRawProject(false);
        } else {
            projOk = fbc.prepareProject(false);
        }
        if (!projOk) {
            sb.addMessage("Error", "Failed to load Project: " + selectedProject.getId() + "! Make sure Rserve is started with right permission.");
            return false;
        }
        if (ab.isInDocker()) {
            if (sb.getCurrentUser() == null) {
                sb.addMessage("error", "Your docker session is invalid, please re-deploy it or renew your authentication key!");
                return false;
            }
        }

        //File newFile = new File(sb.getCurrentUser().getHomeDir() + "/Analysis_Report.html");
        if (nav.equals("view")) {//load specific project folder, the other option is to stay on ProjectView page
            sb.goToResultPage();
        } else {
            sb.addMessage("info", "Project " + selectedProject.getId() + " successfully loaded!");
        }

        return true;
    }

    public void updateProjectInfo() {
        try {
            db.updateProjectTitleDescription(selectedProject.getTitle(), selectedProject.getDescription(), (int) selectedProject.getId());
        } catch (Exception ex) {
            java.util.logging.Logger.getLogger(FireProjectBean.class.getName()).log(Level.SEVERE, null, ex);
            return;
        }
        sb.addMessage("info", "Project " + selectedProject.getId() + " successfully edited");

    }

    public void deleteAccount() {

        String res = db.deleteUserAndProjects(fub.getEmail());

        if (res.equals("Successfully deleted user and associated projects.")) {
            boolean deletedDirectoryFlag = DataUtils.deleteDir(fb.getProjectPath() + "/user_folders/" + fub.getEmail() + "/");
            if (deletedDirectoryFlag) {
                fb.getUserMap().remove(fub.getEmail());  // Remove the entry from the userMap
                DataUtils.removeCookie("user");
                fub.setEmail("");
                sb.doLogout(0);
                DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/home.xhtml", "info", "Successfully deleted user and associated projects!");
            } else {
                sb.addMessage("error", "Successfully deleted user and associated projects info from database but unable to delete the project files!");
            }
        } else {
            sb.addMessage("error", res);
        }
    }

    private String shareToken = "";

    public String getShareToken() {
        return shareToken;
    }

    public void setShareToken(String shareToken) {
        this.shareToken = shareToken;
    }

    private String authenticationKey = "";

    public String getAuthenticationKey() {
        return authenticationKey;
    }

    public void setAuthenticationKey(String authenticationKey) {
        this.authenticationKey = authenticationKey;
    }

    public void authenticateDocker() {

        String urlString = "https://pro.xialab.ca/rest/authenticate/" + authenticationKey;
        String res = "";
        try {
            // Create HTTP client with timeouts
            HttpClient client = HttpClient.newBuilder()
                    .connectTimeout(Duration.ofSeconds(30)) // Connection timeout
                    .build();

            // Build the request with response timeout
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(urlString))
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .timeout(Duration.ofSeconds(60)) // Response timeout
                    .GET()
                    .build();

            // Send request and get response
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

            // Check response status code and get the first line if successful
            if (response.statusCode() == 200) {
                res = response.body().lines().findFirst().orElse("");
            }
        } catch (IOException | InterruptedException ex) {
            java.util.logging.Logger.getLogger(FireProjectBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        /**
         * try { URL url = new URL(urlString); HttpURLConnection conn; conn =
         * (HttpURLConnection) url.openConnection();
         * conn.setRequestMethod("GET"); conn.setRequestProperty("Content-Type",
         * "application/x-www-form-urlencoded"); conn.setDoOutput(true);
         *
         * int responseCode = conn.getResponseCode(); if (responseCode == 200) {
         * try (BufferedReader reader = new BufferedReader(new
         * InputStreamReader(conn.getInputStream()))) { res = reader.readLine();
         * // Read only one line from the response } catch (IOException e) { //
         * Handle the exception (e.g., logging) } } } catch (Exception ex) {
         * java.util.logging.Logger.getLogger(FireProjectBean.class.getName()).log(Level.SEVERE,
         * null, ex); }
         *
         */
        String contactMsg = "Please contact XiaLab Analytics Team (support@xialab.ca)!";
        if (res.contains("invalid")) {
            sb.addMessage("Error", "Your authentication key is invalid. " + contactMsg);
        } else if (res.contains("valid")) {
            res = res.replace("valid; ", "");
            String[] all_res = res.split("; ");
            String res_num = all_res[0];
            String res_expDate = all_res[1];

            ab.setBaseExpDate(res_expDate);
            ab.setDockerAuthed(true);
            int res_int = Integer.parseInt(res_num);
            if (res_int < 0) {
                sb.addMessage("Error", "Your authentication key is valid, but you have no authentication time left. " + contactMsg);
            }
            sb.addMessage("Info", "Authentication succeeded! You have " + res_num + " authentication times left!");
            sb.addMessage("Info", "Welcome! The key will be valid until " + res_expDate + " !");
        } else {
            if (res.contains("expired")) {
                sb.addMessage("Error", "Your authentication key is expired. " + contactMsg);
            } else {
                sb.addMessage("Error", "Your authentication key is invalid. " + contactMsg);
            }
            ab.setDockerAuthed(false);
        }

    }

    private String destEmail = "";

    public String getDestEmail() {
        return destEmail;
    }

    public void setDestEmail(String destEmail) {
        this.destEmail = destEmail;
    }

    public void transferProject() {
        if (!DataUtils.isValidEmail(destEmail)) {
            sb.addMessage("error", "Please provide a valid email address!");
            return;
        }

        String destDirPath = fb.getProjectPath() + "user_folders/" + destEmail + "/";
        int res = db.transferProject((int) selectedProject.getId(), destEmail, "_copy", ab.getToolLocation());
        if (res != 1) {
            sb.addMessage("error", "Error in modifying project table!");
            return;
        }
        //System.out.println(destDirPath + selectedProject.getFolderName() + "_copy.zip" + "======destPath");
        fbc.downloadObject(selectedProject.getHostname(), fub.getEmail(), selectedProject.getFolderName(), destDirPath + selectedProject.getFolderName() + "_copy.zip");
        sb.addMessage("info", "Transfer finished!");

    }

    private final Map<String, FunctionInfo> functionInfos = new LinkedHashMap<>();

    public void addFunctionInfo(String functionName, FunctionInfo info) {
        functionInfos.put(functionName, info);
    }

    public FunctionInfo getFunctionInfo(String functionName) {
        return functionInfos.get(functionName);
    }

    public Map<String, FunctionInfo> getFunctionInfos() {
        return functionInfos;
    }
    
    
}
