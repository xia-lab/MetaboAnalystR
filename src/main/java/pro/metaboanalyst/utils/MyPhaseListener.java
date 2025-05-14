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
import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.ValueExpression;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.PhaseEvent;
import jakarta.faces.event.PhaseId;
import jakarta.faces.event.PhaseListener;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
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
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.workflows.DiagramView;
import pro.metaboanalyst.workflows.QuartzDbUtils;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author Jeff
 */
public class MyPhaseListener implements PhaseListener {

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

        if (event.getPhaseId() == PhaseId.RESTORE_VIEW && context.getViewRoot() == null) {
            //context.setViewRoot(context.getApplication().getViewHandler().createView(context, "/home.xhtml"));
            //System.out.println("Setting default view /home.xhtml");
        }

        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();
        ValueExpression ve = ef.createValueExpression(elc, "#{sessionBean1}", SessionBean1.class);
        SessionBean1 sb = (SessionBean1) ve.getValue(elc);

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
            ValueExpression ve2 = ef.createValueExpression(elc, "#{userLoginBean}", UserLoginBean.class);
            UserLoginBean ub = (UserLoginBean) ve2.getValue(elc);

            if (!(sb.isRegisteredLogin() && ub.isJobManager())) {
                context.getApplication().getNavigationHandler().handleNavigation(context, "*", "Exit");
                context.responseComplete();
                return; // Ensure no further processing
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

        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{sessionBean1}", SessionBean1.class);
        SessionBean1 sb = (SessionBean1) ve.getValue(elc);

        ve = ef.createValueExpression(elc, "#{applicationBean1}", ApplicationBean1.class);
        ApplicationBean1 ab = (ApplicationBean1) ve.getValue(elc);

        ve = ef.createValueExpression(elc, "#{projectBean}", ProjectBean.class);
        ProjectBean pb = (ProjectBean) ve.getValue(elc);

        ve = ef.createValueExpression(elc, "#{spectraController}", SpectraControlBean.class);
        SpectraControlBean spcb = (SpectraControlBean) ve.getValue(elc);

        try {
            String partialId = request.getParameter("ID");
            sb.setPartialId(partialId);

            boolean res = false;
            if (ab.isInDocker()) {
                res = pb.checkLinkinDocker();
            } else {
                res = pb.checkLink();
            }

            sb.setPartialLinkValide(res);
            if (res) {
                //Temporary, NEED TO FIX NAVITRACK SAVING/LOADING LATER
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

        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{diagramView}", DiagramView.class);
        DiagramView dv = (DiagramView) ve.getValue(elc);

        ValueExpression ve2 = ef.createValueExpression(elc, "#{sessionBean1}", SessionBean1.class);
        SessionBean1 sb = (SessionBean1) ve2.getValue(elc);

        try {
            FireBaseController fb = (FireBaseController) DataUtils.findBean("fireBaseController");
            ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");

            String folderName = request.getParameter("folderName");
            String jobId = request.getParameter("jobId");
            String email = request.getParameter("email");
            System.out.println("handleResumeRawRequest_folderName===" + folderName);

            boolean res = dv.resumeRawProject(folderName, jobId, email);
            if (res) {
                FireUserBean fu = (FireUserBean) DataUtils.findBean("fireUserBean");

                RDataUtils.updateRawJobStatusByFolder(sb.getRConnection(), folderName, "WORKFLOW_FINISHED");

                String funcName = "finishRawProject";
                String shareLink = ab.getApp_url() + "/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?"
                        + "funcNm=" + URLEncoder.encode(funcName, StandardCharsets.UTF_8) + "&"
                        + "folderName=" + URLEncoder.encode(folderName, StandardCharsets.UTF_8) + "&"
                        + "jobId=" + URLEncoder.encode(jobId, StandardCharsets.UTF_8) + "&"
                        + "email=" + URLEncoder.encode(email, StandardCharsets.UTF_8);
                fu.setEmail(email);
                System.out.println("handleResumeRawRequest_EMAIL===" + email);
                System.out.println("handleResumeRawRequest_ANAL===" + sb.getAnalType());
                fb.setFireDocName("Workflow Project");
                boolean saveRes = fb.saveProject("project");
                if (saveRes) {
                    RCenter.recordMessage(sb.getRConnection(), "Saving Project for Spectra Processing Workflow ------ <b>Finished!</b>");
                    if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))
                            & Files.isRegularFile(Paths.get("/home/glassfish/payara6_micro/useVIP2"))) {
                        boolean res2 = DataUtils.sendRawFinishEmail("vip2", email, jobId, folderName);
                    } else if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))) {
                        boolean res2 = DataUtils.sendRawFinishEmail("vip", email, jobId, folderName);
                    }
                }
            }
        } catch (Exception e) {
            LOGGER.error("handleResumeRawRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");

        }
    }

    private void handleResetRequest(PhaseEvent event) {

        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        String token = request.getParameter("token");

        SessionBean1 sb = context.getApplication().evaluateExpressionGet(context, "#{sessionBean1}", SessionBean1.class);
        if (token != null) {
            sb.setResetToken(token);
        }

    }

    private void handleLoadingProjectRequest(PhaseEvent event) {
        PrimeFaces.current().executeScript("PF('statusDialog').show();");
        FacesContext context = event.getFacesContext();

        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{projectBean}", ProjectBean.class);
        ProjectBean pb = (ProjectBean) ve.getValue(elc);

        ve = ef.createValueExpression(elc, "#{userLoginBean}", UserLoginBean.class);
        UserLoginBean ulb = (UserLoginBean) ve.getValue(elc);

        try {
            String projectLoadingCode = request.getParameter("ID");
            String userId = projectLoadingCode.split("_")[0];
            long UserNM = Long.parseLong(userId);

            String guestFolder = projectLoadingCode.split("_")[1];
            String pageFlag = "";
            boolean ready = false;
            boolean ready0 = false;

            if (guestFolder.startsWith("nv")) {

                //Several steps need to be done for this case:
                // 1. keep user signned in
                ulb.setUserNM(UserNM);
                ulb.doLoginKeep(guestFolder);

                // 2. Restore selected projects
                String initRes = pb.initializeProject(guestFolder);
                ready0 = initRes != null;

                // 3. load new project
                String loadRes = pb.loadnewProject();
                if (loadRes == null) {
                    ready = false;
                } else {
                    ready = true;
                    pageFlag = "spec";
                }

            } else {

                // 1. keep user signned in
                ulb.setUserNM(UserNM);
                ulb.doLoginKeep(guestFolder);

                // 2. Restore selected projects
                String initRes = pb.initializeProject(guestFolder);
                ready0 = initRes != null;

                // 3. load project
                String loadRes = pb.loadProject();
                if (loadRes == null) {
                    ready = false;
                } else {
                    ready = true;
                    pageFlag = loadRes;
                }
            }

            // 4. If everything ready (both readys equals true), jump into the corresponsing page
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
        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{sessionBean1}", SessionBean1.class);
        SessionBean1 sb = (SessionBean1) ve.getValue(elc);

        try {
            FireBaseController fu = (FireBaseController) DataUtils.findBean("fireBaseController");
            String tokenId = request.getParameter("tokenId");
            String analNavi = request.getParameter("analNavi");
            String imgCmd = request.getParameter("imgCmd");
            String imgType = request.getParameter("format");
            String downloadPath = request.getParameter("download");
            String returnNavi = request.getParameter("returnNavi");

            boolean res;
            if (analNavi != null && !analNavi.isEmpty()) {
                String cmpd = request.getParameter("cmpd");
                fu.loadReport(tokenId, analNavi, cmpd);
                // DataUtils.doRedirect("/MetaboAnalys/docs/Format.xhtml");

            } else if (imgCmd != null && !imgCmd.isEmpty()) {

                fu.loadProject(tokenId, "share");
                System.out.println("loadProject ==> ");
                sb.graphicsLnk_action(imgCmd);
                sb.setFormatOpt(imgType);
                fu.generateHighDef2();

            } else if (downloadPath != null && !downloadPath.isEmpty()) {
                fu.loadProject(tokenId, "share");
                DataUtils.doRedirect("/MetaboAnalyst/" + sb.getCurrentUser().getRelativeDir() + File.separator + downloadPath);
            } else {
                res = fu.loadProject(tokenId, "share");
                if (res) {
                    if (null == returnNavi) {
                        DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/ProjectLanding.xhtml");
                    } else {
                        switch (returnNavi) {
                            case "result" ->
                                sb.goToResultPage();
                            case "workflow" ->
                                DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml");
                            default -> {
                            }
                        }
                    }
                } else {
                    DataUtils.doRedirectWithGrowl("/MetaboAnalyst/home.xhtml", "error", "Failed to load project!");
                }
            }

        } catch (Exception e) {
            LOGGER.error("handleFireBaseProjectLoad", e);
        }
    }

    private void handleVerifiedRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        try {
            FireUserBean fu = (FireUserBean) DataUtils.findBean("fireUserBean");
            //boolean success = fu.reloadUserInfo();
            String tokenId = (String) request.getParameter("token");
            String projectId = (String) request.getParameter("projectId");
            String navi = (String) request.getParameter("navi");
            SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
            boolean success = fu.loginHandshake(tokenId);
            if (success) {
                if (navi != null) {
                    context.getApplication().getNavigationHandler().handleNavigation(context,
                            "*", navi);
                } else if (projectId != null) {
                    FireBaseController fb = (FireBaseController) DataUtils.findBean("fireBaseController");
                    fb.setProjectToLoadId(projectId);
                    context.getApplication().getNavigationHandler().handleNavigation(context,
                            "*", "ProjectView");
                } else {

                    sb.addMessage("info", "Login successful!");
                    context.getApplication().getNavigationHandler().handleNavigation(context,
                            "*", "Upload");
                }

            } else {
                sb.addMessage("error", "Login failed, please try again!");
                context.getApplication().getNavigationHandler().handleNavigation(context,
                        "*", "logout");
            }

        } catch (Exception e) {
            LOGGER.error("handleVerifiedRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");
        }
    }

    private void handleActivationRequest(PhaseEvent event) {

        FacesContext context = event.getFacesContext();
        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();
        ValueExpression ve = ef.createValueExpression(elc, "#{fireUserBean}", FireUserBean.class
        );
        FireUserBean fu = (FireUserBean) ve.getValue(elc);

        try {
            System.out.println("activateView===========");
            String activationCode = context.getExternalContext().getRequestParameterMap().get("code");
            String email = context.getExternalContext().getRequestParameterMap().get("mail");

            fu.setActivationCode(activationCode);
            fu.setEmail(email);

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
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");
        }

    }

    private void handleLoginExternalRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();

        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();

        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{fireUserBean}", FireUserBean.class
        );
        FireUserBean fu = (FireUserBean) ve.getValue(elc);

        String username = request.getParameter("username");
        String password = request.getParameter("password");
        //If no parameters are passed, then, try to parse the JSON body of the API request
        try {

            fu.setEmail(username);
            fu.setPassword(password);
            boolean res = fu.doUserLoginLocal();

            if (res) {

                FireBase fb = (FireBase) DataUtils.findBean("fireBase");
                String uid = UUID.randomUUID().toString();
                fb.getLoginUserMap().put(uid, fu);

                response.setStatus(HttpServletResponse.SC_OK);
                // Set the content type, for example, text/plain or application/json
                response.setContentType("text/plain");

                // Set the body of the response
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
                java.util.logging.Logger.getLogger(MyPhaseListener.class
                        .getName()).log(Level.SEVERE, null, ex);
            }

            //context.getApplication().getNavigationHandler().handleNavigation(context,"*", "logout");
        }

    }

    public void handleProjectDownloadRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();

        try {
            FireBase fb = (FireBase) DataUtils.findBean("fireBase");
            String email = request.getParameter("email");
            String folderName = request.getParameter("folderName");

            String sourceFilePath = fb.getProjectPath() + "user_folders/" + email + "/" + folderName + ".zip";
            File file = new File(sourceFilePath);
            // Serve the downloaded file to the client

            if (file.exists()) {
                response.setContentType("application/zip");
                String headerKey = "Content-Disposition";
                String headerValue = String.format("attachment; filename=\"%s\"", file.getName());
                response.setHeader(headerKey, headerValue);
                response.setContentLength((int) file.length());

                try (BufferedInputStream input = new BufferedInputStream(new FileInputStream(file)); OutputStream output = response.getOutputStream()) {

                    byte[] buffer = new byte[4096];
                    int length;
                    while ((length = input.read(buffer)) > 0) {
                        output.write(buffer, 0, length);
                    }
                    output.flush();
                } catch (IOException e) {
                    e.printStackTrace();
                }

                context.responseComplete(); // Prevent JSF navigation
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
        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{sessionBean1}", SessionBean1.class);
        SessionBean1 sb = (SessionBean1) ve.getValue(elc);

        ValueExpression ve2 = ef.createValueExpression(elc, "#{diagramView}", DiagramView.class);
        DiagramView dv = (DiagramView) ve2.getValue(elc);

        ValueExpression ve3 = ef.createValueExpression(elc, "#{applicationBean1}", ApplicationBean1.class);
        ApplicationBean1 ab = (ApplicationBean1) ve3.getValue(elc);

        ValueExpression ve4 = ef.createValueExpression(elc, "#{fireUserBean}", FireUserBean.class
        );
        FireUserBean fu = (FireUserBean) ve4.getValue(elc);

        try {
            FireBaseController fb = (FireBaseController) DataUtils.findBean("fireBaseController");
            String tokenId = request.getParameter("tokenId");
            String email = request.getParameter("email");
            String jobId = request.getParameter("jobId");

            boolean res;

            res = fb.loadProject(tokenId, "workflow");
            
            if (res) {
                boolean wfRes = dv.startWorkflow();
                if (wfRes) {
                    sb.setCurrentNaviUrl("/Secure/xialabpro/ResultView.xhtml");
                    fu.setEmail(email);
                    String shareLink = ab.getApp_url() + "/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=finishWorkflowJob&tokenId=" + tokenId;
                    sb.addMessage("info", DataUtils.obtainTimestampText());
                    sb.addMessage("info", "Your workflow processing job (ID: " + jobId + ") status has become <b>COMPLETED</b>.");
                    sb.addMessage("info", "You can access the following link to resume your project: "
                            + "<a href=\"" + shareLink + "\">click here</a>.\n");
                    dv.setShowNotif(true);
                    boolean saveRes = fb.saveProject("workflow");
                    if (saveRes) {
                        QuartzDbUtils.updateJobStatus(jobId, "COMPLETED");
                        dv.sendRawResume(email, jobId, shareLink);
                    }
                }
            } else {

                DataUtils.doRedirectWithGrowl("/ExpressAnalyst/home.xhtml", "error", "Failed to load project!");
            }
        } catch (Exception e) {
            LOGGER.error("handleFireBaseProjectLoad", e);
        }
    }

    private void handleWorkflowFinishRequest(PhaseEvent event) {

        FacesContext context = event.getFacesContext();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{diagramView}", DiagramView.class);
        DiagramView dv = (DiagramView) ve.getValue(elc);

        ve = ef.createValueExpression(elc, "#{workflowBean}", WorkflowBean.class);
        WorkflowBean wb = (WorkflowBean) ve.getValue(elc);
        try {
            FireBaseController fu = (FireBaseController) DataUtils.findBean("fireBaseController");
            String tokenId = request.getParameter("tokenId");

            boolean res;

            res = fu.loadProject(tokenId, "workflow");
            if (res) {
                dv.setStatusMsg("<b style='color: green'>Workflow Completed.</b>");
                if (wb.getWorkflowOptions() != null && !wb.getWorkflowOptions().isEmpty()) {
                    DataUtils.doRedirectWithGrowl("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml", "info", "Workflow completed!");
                    //DataUtils.doRedirectWithGrowl("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?callFunc=checkPagesToVisit", "info", "Workflow completed!");

                } else {
                    DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?callFunc=checkPagesToVisit");
                }
                //DataUtils.doRedirectWithGrowl("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml", "info", "Workflow completed!");
            } else {
                dv.setStatusMsg("<b style='color: green'>Workflow Failed.</b>");
                DataUtils.doRedirectWithGrowl("/MetaboAnalyst/home.xhtml", "error", "Failed to load project!");
            }

        } catch (Exception e) {
            LOGGER.error("handleFireBaseProjectLoad", e);
        }
    }

    private void handleFinishRawRequest(PhaseEvent event) {

        FacesContext context = event.getFacesContext();

        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();

        FacesContext fc = FacesContext.getCurrentInstance();
        ELContext elc = fc.getELContext();
        ExpressionFactory ef = fc.getApplication().getExpressionFactory();

        ValueExpression ve = ef.createValueExpression(elc, "#{diagramView}", DiagramView.class);
        DiagramView dv = (DiagramView) ve.getValue(elc);

        try {
            DatabaseClient db = (DatabaseClient) DataUtils.findBean("databaseClient");
            FireBaseController fb = (FireBaseController) DataUtils.findBean("fireBaseController");
            SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

            String folderName = request.getParameter("folderName");
            String jobId = request.getParameter("jobId");
            String email = request.getParameter("email");
            Map<String, Object> obj = db.obtainFolderNameProject(folderName);
            Optional<Map.Entry<String, Object>> matchingEntry = obj.entrySet().stream()
                    .filter(entry -> entry.getKey().equals("partialtoken") || entry.getKey().contains("partialtoken"))
                    .findFirst();

            if (matchingEntry.isPresent()) {
                Object token2 = matchingEntry.get().getValue();
                try {
                    boolean res1 = fb.loadProject(token2 + "", "project", true);
                    if (res1) {
                        FireUserBean fu = (FireUserBean) DataUtils.findBean("fireUserBean");
                        fu.setEmail(email);
                        dv.setWorkflowFinished(true);
                        //SpectraProcessBean spb = (SpectraProcessBean) DataUtils.findBean("spectraProcessor");
                        //spb.internalizeRes(0);
                        DataUtils.copyFile(new File("/data/glassfish/projects/metaboanalyst/" + folderName + "/mSet.rda"), new File(sb.getCurrentUser().getHomeDir() + "/mSet.rda"));
                        RCenter.loadHistory(sb.getRConnection());
                        sb.setAnalType("roc");
                        DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/DashboardView.xhtml");
                    }
                } catch (Exception ex) {
                    java.util.logging.Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
                }
            }

        } catch (Exception e) {
            LOGGER.error("handlePartialRequest", e);
            context.getApplication().getNavigationHandler().handleNavigation(context,
                    "*", "logout");

        }
    }
}
