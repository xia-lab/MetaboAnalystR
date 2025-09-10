/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.UUID;
import java.util.logging.Level;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.faces.context.ExternalContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.PhaseEvent;
import jakarta.faces.event.PhaseId;
import jakarta.faces.event.PhaseListener;
import jakarta.inject.Named;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Optional;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.project.ProjectBean;
import pro.metaboanalyst.project.UserLoginBean;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.spectra.SpectraControlBean;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.lts.MailService;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.workflows.DiagramView;
import pro.metaboanalyst.workflows.QuartzDbUtils;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author Jeff
 */
@ApplicationScoped
@Named("phaseListener")
public class MyPhaseListener implements PhaseListener {

    /*
    * Do not inject narrower scoped bean into application scoped bean
    * PhaseListeners are typically instantiated and managed by the 
    * JSF implementation itself (e.g., Mojarra or MyFaces). They are usually application-scoped singletons.
    * CDI  is a separate specification that manages the lifecycle and dependencies of CDI beans.
    * For CDI to @Inject a dependency into a class, CDI must be the one instantiating and managing that class. 
    * Since JSF instantiates PhaseListeners, CDI doesn't get a chance to perform the injection.
    @Inject
    private ApplicationBean1 ab;
    @Inject
    private MailService ms;
    @Inject
    private SessionBean1 sb;
    @Inject
    private  ProjectBean prjb;
    @Inject
    private  UserLoginBean ulb;
    @Inject
    private FireBase fb;
    @Inject
    private  FireUserBean fub;
    @Inject
    private  FireBaseController fbc;
    @Inject
    private SpectraControlBean spcb;
    @Inject
    private DiagramView dv;
    @Inject
    private WorkflowBean wfb;
    @Inject
    private DatabaseClient dbc;
     */
    public static final String USER_SESSION_KEY = "MA6_PRO_user";
    private static final String PartialPersistence = "Share"; //ok
    private static final String LoadingProject = "LoadProject"; //ok
    private static final String JobManager = "JobManager"; //ok
    private static final String resetToken = "ResetView"; //OK
    //private static final String redirToken = "upload"; //ok

    //LTS
    private static final String OPEN_DIALOG = "OpenDialog";// ok
    private static final String ACTIVATE_REQUEST = "ActivateView";//ok
    private static final String LoadPartialLink = "ShareLink"; //ok
    private static final String LoginVerified = "verified"; //ok
    private static final String LOGIN_EXTERNAL = "loginExternal";//ok
    private static final String DOWNLOAD_PROJECT = "fetchProject";//ok
    private static final String RESUME_RAW = "resumeRawProject";//ok
    private static final String FINISH_RAW = "finishRawProject";//ok

    private static final String DELETE_FILE = "deleteProject";//ok
    private static final String EXECUTE_WORKFLOWJOB = "executeWorkflowJob";
    private static final String FINISH_WORKFLOWJOB = "finishWorkflowJob";

    private static final Logger LOGGER = LogManager.getLogger(MyPhaseListener.class);

    private <T> T getBeanByName(String name, Class<T> clazz) {
        FacesContext context = FacesContext.getCurrentInstance();
        return context.getApplication().evaluateExpressionGet(context, "#{" + name + "}", clazz);
    }

    @Override
    public void afterPhase(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        if (context == null) {
            System.out.println("FacesContext is null");
            return;
        }

        if (context.getResponseComplete()) {
            System.out.println("Response is already complete, skipping further processing.");
            return;
        }

        ExternalContext extContext = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest) extContext.getRequest();
        String ajaxFunction = request.getParameter("function");
        String path = request.getRequestURI();

        if (ajaxFunction != null) {
            System.out.println("AJAX request for function: " + ajaxFunction);
            context.responseComplete();
            return;
        }

        String funcNm = request.getParameter("funcNm");
        String rootId = (context.getViewRoot() != null) ? context.getViewRoot().getViewId() : request.getRequestURL().toString();

        //if (event.getPhaseId() == PhaseId.RESTORE_VIEW && context.getViewRoot() == null) {
        //context.setViewRoot(context.getApplication().getViewHandler().createView(context, "/home.xhtml"));
        //System.out.println("Setting default view /home.xhtml");
        //}
        if (funcNm != null) {
            switch (funcNm) {
                case LOGIN_EXTERNAL ->
                    handleLoginExternalRequest(event);
                case OPEN_DIALOG ->
                    handleDialogRequest(event);
                case LoadPartialLink ->
                    handleFirePartialRequest(event);
                case DOWNLOAD_PROJECT ->
                    handleProjectDownloadRequest(event);
                case PartialPersistence ->
                    handlePartialRequest(event);
                case LoadingProject ->
                    handleLoadingProjectRequest(event);
                case LoginVerified ->
                    handleVerifiedRequest(event);
                case ACTIVATE_REQUEST ->
                    handleActivationRequest(event);
                case RESUME_RAW ->
                    handleResumeRawRequest(event);
                case FINISH_RAW ->
                    handleFinishRawRequest(event);
                case EXECUTE_WORKFLOWJOB ->
                    handleWorkflowStartRequest(event);
                case FINISH_WORKFLOWJOB ->
                    handleWorkflowFinishRequest(event);
                default -> {
                }
            }
        }

        if (rootId.contains(JobManager)) {
            FacesContext facesContext = event.getFacesContext();

            SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
            UserLoginBean ulb = getBeanByName("userLoginBean", UserLoginBean.class);

            if (!(sb.isRegisteredLogin() && ulb.isJobManager())) {
                facesContext.getApplication().getNavigationHandler().handleNavigation(facesContext, "*", "Exit");
                facesContext.responseComplete();
            }
        } else if (rootId.contains(resetToken)) {
            handleResetRequest(event);
            //} else if (rootId.contains(redirToken)) {
            //    handleRedirRequest(event);
        } else if (!extContext.getSessionMap().containsKey(USER_SESSION_KEY)) {
            if (path.startsWith("/MetaboAnalyst/Secure")) {
                context.getApplication().getNavigationHandler().handleNavigation(context, "*", "Exit");
                context.responseComplete();
            }
        }
        /*    
        } else if (!userExists(context)) {
            if (requestingSecureView(context)) {
                context.getApplication().getNavigationHandler().handleNavigation(context, "*", "Exit");
                context.responseComplete();
                return; // Ensure no further processing
            }
        }**/
    }

    @Override
    public PhaseId getPhaseId() {
        return PhaseId.RESTORE_VIEW;
    }

    @Override
    public void beforePhase(PhaseEvent event) {
    }

    private void handlePartialRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        ApplicationBean1 ab = getBeanByName("applicationBean1", ApplicationBean1.class);
        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        ProjectBean prjb = getBeanByName("projectBean", ProjectBean.class);
        SpectraControlBean spcb = getBeanByName("spectraController", SpectraControlBean.class);

        try {
            String partialId = request.getParameter("ID");
            sb.setPartialId(partialId);

            boolean res;
            if (ab.isInDocker()) {
                res = prjb.checkLinkinDocker();
            } else {
                res = prjb.checkLink();
            }

            sb.setPartialLinkValide(res);
            if (res) {
                // Temporary, NEED TO FIX NAVITRACK SAVING/LOADING LATER
                sb.initNaviTree("spec");
                sb.addNaviTrack("Upload", null);
                sb.addNaviTrack("Spectra check", null);
                sb.addNaviTrack("Spectra processing", null);
                sb.addNaviTrack("Job status", null);

                spcb.setCreatedShareLink(true);
                sb.setPartialId(partialId);
            }

            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "sharelink");

        } catch (Exception e) {
            LOGGER.error("handlePartialRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");
        }
    }

    private void handleResumeRawRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        DiagramView dv = getBeanByName("diagramView", DiagramView.class);
        FireBaseController fbc = getBeanByName("fireBaseController", FireBaseController.class);
        MailService ms = getBeanByName("mailService", MailService.class);

        try {
            String folderName = request.getParameter("folderName");
            String jobId = request.getParameter("jobId");
            String email = request.getParameter("email");
            System.out.println("handleResumeRawRequest_folderName===" + folderName);

            boolean res = dv.resumeRawProject(folderName, jobId, email);
            if (res) {
                FireUserBean fu = getBeanByName("fireUserBean", FireUserBean.class);
                fu.setEmail(email);

                RDataUtils.updateRawJobStatusByFolder(sb.getRConnection(), folderName, "WORKFLOW_FINISHED");

                fbc.setFireDocName("Workflow Project");
                boolean saveRes = fbc.saveProject("project");
                if (saveRes) {
                    RCenter.recordMessage(sb.getRConnection(), "Saving Project for Spectra Processing Workflow ------ <b>Finished!</b>");
                    if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))
                            && Files.isRegularFile(Paths.get("/home/glassfish/payara6_micro/useVIP_2025R2"))) {
                        DataUtils.sendRawFinishEmail(ms, "vip2", email, jobId, folderName);
                    } else if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))) {
                        DataUtils.sendRawFinishEmail(ms, "vip", email, jobId, folderName);
                    }
                }
            }
        } catch (Exception e) {
            LOGGER.error("handleResumeRawRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context, "*", "logout");
        }
    }

    private void handleResetRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);

        String token = request.getParameter("token");
        if (token != null) {
            sb.setResetToken(token);
        }
    }

    private void handleLoadingProjectRequest(PhaseEvent event) {
        PrimeFaces.current().executeScript("PF('statusDialog').show();");

        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        ProjectBean prjb = getBeanByName("projectBean", ProjectBean.class);
        UserLoginBean ulb = getBeanByName("userLoginBean", UserLoginBean.class);

        try {
            String projectLoadingCode = request.getParameter("ID");
            String userId = projectLoadingCode.split("_")[0];
            long UserNM = Long.parseLong(userId);
            String guestFolder = projectLoadingCode.split("_")[1];

            String pageFlag = "";
            boolean ready;
            boolean ready0;

            // 1. keep user signed in
            ulb.setUserNM(UserNM);
            ulb.doLoginKeep(guestFolder);

            // 2. Restore selected projects
            String initRes = prjb.initializeProject(guestFolder);
            ready0 = initRes != null;

            // 3. Load project based on folder type
            if (guestFolder.startsWith("nv")) {
                String loadRes = prjb.loadnewProject();
                ready = loadRes != null;
                if (ready) {
                    pageFlag = "spec";
                }
            } else {
                String loadRes = prjb.loadProject();
                ready = loadRes != null;
                if (ready) {
                    pageFlag = loadRes;
                }
            }

            PrimeFaces.current().executeScript("PF('statusDialog').hide();");
            if (ready0 && ready) {
                context.getApplication().getNavigationHandler().handleNavigation(context,
                        "*", pageFlag);
            }

        } catch (Exception e) {
            LOGGER.error("handleLoadingProjectRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");
        }
    }

    private void handleFirePartialRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        ApplicationBean1 ab = getBeanByName("applicationBean1", ApplicationBean1.class);
        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        FireBaseController fbc = getBeanByName("fireBaseController", FireBaseController.class);

        try {
            String tokenId = request.getParameter("tokenId");
            String analNavi = request.getParameter("analNavi");
            String imgCmd = request.getParameter("imgCmd");
            String imgType = request.getParameter("format");
            String downloadPath = request.getParameter("download");
            String returnNavi = request.getParameter("returnNavi");

            boolean res;
            if (analNavi != null && !analNavi.isEmpty()) {
                String cmpd = request.getParameter("cmpd");
                fbc.loadReport(tokenId, analNavi, cmpd);
            } else if (imgCmd != null && !imgCmd.isEmpty()) {
                fbc.loadProject(tokenId, "share");
                sb.graphicsLnk_action(imgCmd);
                sb.setFormatOpt(imgType);
                fbc.generateHighDef2();
            } else if (downloadPath != null && !downloadPath.isEmpty()) {
                fbc.loadProject(tokenId, "share");
                DataUtils.doRedirect("/MetaboAnalyst/" + sb.getCurrentUser().getRelativeDir() + File.separator + downloadPath, ab);
            } else {
                res = fbc.loadProject(tokenId, "share");
                if (res) {
                    if (returnNavi == null) {
                        DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/ProjectLanding.xhtml", ab);
                    } else {
                        switch (returnNavi) {
                            case "result" ->
                                sb.goToResultPage();
                            case "workflow" ->
                                DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml", ab);
                        }
                    }
                } else {
                    DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/home.xhtml", "error", "Failed to load project!");
                }
            }
        } catch (Exception e) {
            LOGGER.error("handleFireBaseProjectLoad", e);
        }
    }

    private void handleVerifiedRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        FireUserBean fub = getBeanByName("fireUserBean", FireUserBean.class);
        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        FireBaseController fbc = getBeanByName("fireBaseController", FireBaseController.class);

        try {
            String tokenId = request.getParameter("token");
            String projectId = request.getParameter("projectId");
            String navi = request.getParameter("navi");
            boolean success = fub.loginHandshake(tokenId);

            if (success) {
                if (navi != null) {
                    context.getApplication().getNavigationHandler().handleNavigation(context, "*", navi);
                } else if (projectId != null) {
                    fbc.setProjectToLoadId(projectId);
                    context.getApplication().getNavigationHandler().handleNavigation(context, "*", "ProjectView");
                } else {
                    sb.addMessage("info", "Login successful!");
                    context.getApplication().getNavigationHandler().handleNavigation(context, "*", "Upload");
                }
            } else {
                sb.addMessage("error", "Login failed, please try again!");
                context.getApplication().getNavigationHandler().handleNavigation(context, "*", "logout");
            }
        } catch (Exception e) {
            LOGGER.error("handleVerifiedRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context, "*", "logout");
        }
    }

    private void handleActivationRequest(PhaseEvent event) {

        FacesContext context = event.getFacesContext();
        FireUserBean fub = getBeanByName("fireUserBean", FireUserBean.class);
        try {
            //System.out.println("activateView===========");
            String activationCode = context.getExternalContext().getRequestParameterMap().get("code");
            String email = context.getExternalContext().getRequestParameterMap().get("mail");
            fub.setActivationCode(activationCode);
            fub.setEmail(email);

        } catch (Exception e) {
            LOGGER.error("handleActivationRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");
        }

    }

    private void handleDialogRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        try {
            String cmd = context.getExternalContext().getRequestParameterMap().get("cmd");
            PrimeFaces.current().executeScript("PF('" + cmd + "').show()");
        } catch (Exception e) {
            LOGGER.error("handleActivationRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context, "*", "logout");
        }
    }

    private void handleLoginExternalRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();

        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();

        FireUserBean fub = getBeanByName("fireUserBean", FireUserBean.class);
        FireBase fb = getBeanByName("fireBase", FireBase.class);

        try {
            String username = request.getParameter("username");
            String password = request.getParameter("password");

            fub.setEmail(username);
            fub.setPassword(password);
            boolean res = fub.doUserLoginLocal();

            if (res) {
                String uid = UUID.randomUUID().toString();
                fb.getLoginUserMap().put(uid, fub);
                response.setStatus(HttpServletResponse.SC_OK);
                response.setContentType("text/plain");
                response.getWriter().write(uid);
            } else {
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Login failed");
            }
            context.responseComplete();
        } catch (Exception exception) {
            exception.printStackTrace();
            try {
                response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Login failed");
                context.responseComplete();
            } catch (IOException ex) {
                // Logger.getLogger(MyPhaseListener.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    public void handleProjectDownloadRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();

        FireBase fb = getBeanByName("fireBase", FireBase.class);

        try {
            String email = request.getParameter("email");
            String folderName = request.getParameter("folderName");
            String sourceFilePath = fb.getProjectPath() + "user_folders/" + email + "/" + folderName + ".zip";
            File file = new File(sourceFilePath);

            if (file.exists()) {
                response.setContentType("application/zip");
                response.setHeader("Content-Disposition", "attachment; filename=\"" + file.getName() + "\"");
                response.setContentLength((int) file.length());

                try (BufferedInputStream input = new BufferedInputStream(new FileInputStream(file)); OutputStream output = response.getOutputStream()) {

                    byte[] buffer = new byte[4096];
                    int length;
                    while ((length = input.read(buffer)) > 0) {
                        output.write(buffer, 0, length);
                    }
                    output.flush();
                }

                context.responseComplete();
            } else {
                response.sendError(HttpServletResponse.SC_NOT_FOUND, "File not found");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void handleWorkflowStartRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        ApplicationBean1 ab = getBeanByName("applicationBean1", ApplicationBean1.class);
        FireUserBean fub = getBeanByName("fireUserBean", FireUserBean.class);
        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        FireBaseController fbc = getBeanByName("fireBaseController", FireBaseController.class);
        DiagramView dv = getBeanByName("diagramView", DiagramView.class);

        try {
            String tokenId = request.getParameter("tokenId");
            String email = request.getParameter("email");
            String jobId = request.getParameter("jobId");

            boolean res = fbc.loadProject(tokenId, "workflow");
            if (res) {
                boolean wfRes = dv.startWorkflow();
                if (wfRes) {
                    sb.setCurrentNaviUrl("/Secure/xialabpro/DashboardView.xhtml");
                    fub.setEmail(email);

                    String shareLink = ab.getApp_url() + "/" + ab.getAppName()
                            + "/faces/AjaxHandler.xhtml?funcNm=finishWorkflowJob&tokenId=" + tokenId;

                    sb.addMessage("info", DataUtils.obtainTimestampText());
                    sb.addMessage("info", "Your workflow processing job (ID: " + jobId + ") status has become <b>COMPLETED</b>.");
                    sb.addMessage("info", "You can access the following link to resume your project: "
                            + "<a href=\"" + shareLink + "\">click here</a>.\n");

                    dv.setShowNotif(true);
                    sb.setDataNormed();
                    boolean saveRes = fbc.saveProject("workflow");
                    if (saveRes) {
                        QuartzDbUtils.updateJobStatus(jobId, "COMPLETED");
                        dv.sendRawResume(email, jobId, shareLink);
                    }
                }
            } else {
                DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/home.xhtml", "error", "Failed to load project!");
            }
        } catch (Exception e) {
            LOGGER.error("handleFireBaseProjectLoad", e);
        }
    }

    private void handleWorkflowFinishRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        ApplicationBean1 ab = getBeanByName("applicationBean1", ApplicationBean1.class);
        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        FireBaseController fbc = getBeanByName("fireBaseController", FireBaseController.class);
        DiagramView dv = getBeanByName("diagramView", DiagramView.class);
        WorkflowBean wfb = getBeanByName("workflowBean", WorkflowBean.class);

        try {
            String tokenId = request.getParameter("tokenId");
            boolean res = fbc.loadProject(tokenId, "workflow");

            if (res) {
                dv.setStatusMsg("<b style='color: green'>Workflow Completed.</b>");
                if (wfb.getWorkflowOptions() != null && !wfb.getWorkflowOptions().isEmpty()) {
                    DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml", "info", "Workflow completed!");
                } else {
                    DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?callFunc=checkPagesToVisit", ab);
                }
            } else {
                dv.setStatusMsg("<b style='color: green'>Workflow Failed.</b>");
                DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/home.xhtml", "error", "Failed to load project!");
            }
        } catch (Exception e) {
            LOGGER.error("handleFireBaseProjectLoad", e);
        }
    }

    private void handleFinishRawRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        ApplicationBean1 ab = getBeanByName("applicationBean1", ApplicationBean1.class);
        SessionBean1 sb = getBeanByName("sessionBean1", SessionBean1.class);
        FireBaseController fbc = getBeanByName("fireBaseController", FireBaseController.class);
        DiagramView dv = getBeanByName("diagramView", DiagramView.class);
        FireUserBean fub = getBeanByName("fireUserBean", FireUserBean.class);
        DatabaseClient dbc = getBeanByName("databaseClient", DatabaseClient.class);

        try {
            String folderName = request.getParameter("folderName");
            String email = request.getParameter("email");

            Map<String, Object> obj = dbc.obtainFolderNameProject(folderName);
            Optional<Map.Entry<String, Object>> matchingEntry = obj.entrySet().stream()
                    .filter(entry -> entry.getKey().equals("partialtoken") || entry.getKey().contains("partialtoken"))
                    .findFirst();

            if (matchingEntry.isPresent()) {
                Object token2 = matchingEntry.get().getValue();
                try {
                    boolean res1 = fbc.loadProject(token2 + "", "project", true);
                    if (res1) {
                        fub.setEmail(email);
                        dv.setWorkflowFinished(true);
                        
                        DataUtils.copyFile(
                                new File("/data/glassfish/projects/metaboanalyst/" + folderName + "/mSet.rda"),
                                new File(sb.getCurrentUser().getHomeDir() + "/mSet.rda")
                        );
                        RCenter.loadHistory(sb.getRConnection());
                        //sb.setAnalType("roc");
                        DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/DashboardView.xhtml", ab);
                    }
                } catch (Exception ex) {
                    LOGGER.error("handlePartialRequest", ex);
                }
            }
        } catch (Exception e) {
            LOGGER.error("handlePartialRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context, "*", "logout");
        }
    }

}
