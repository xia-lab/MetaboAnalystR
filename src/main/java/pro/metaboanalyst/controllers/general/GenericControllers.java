/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Named;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Arrays;
import org.primefaces.event.NodeSelectEvent;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.menu.DefaultMenuItem;
import org.primefaces.model.menu.DefaultMenuModel;
import org.primefaces.model.menu.MenuModel;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.models.RcmdBean;
import pro.metaboanalyst.project.UserLoginBean;
import pro.metaboanalyst.project.UserLoginModel;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RNetworkUtils;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.utils.NaviUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author xia
 */
@RequestScoped
@Named("ctl")
public class GenericControllers implements Serializable {

    @Inject
    private ApplicationBean1 ab;
    @Inject
    private SessionBean1 sb;
    private static final Logger LOGGER = LogManager.getLogger(GenericControllers.class);
    private static final String ORIG_STYLE = "color: inherit; text-decoration: underline";
    private static final String HIGHLIGHT_STYLE = "color: maroon; text-decoration: none";

    /*
     * navigation tree
     */
    public void onNodeSelect(NodeSelectEvent event) {
        NaviUtils.selectNaviTreeNode(sb, event.getTreeNode());
    }

    public ArrayList<String> getMsgVec() {
        ArrayList<String> myMsgs = new ArrayList<>();
        RConnection RC = sb.getRConnection();

        if (RC == null) {
            //LOGGER.error("RConnection is null in getCmdVec");
            return new ArrayList<>(List.of("No message available"));
        }

        try {
            /**
             * if (sb.isLoggedIn() && sb.getDataType().equals("spec") &&
             * ab.isInDocker()) { SpectraProcessBean spb = (SpectraProcessBean)
             * DataUtils.findBean("spectraProcessor"); if (spb.isRecordCMD()) {
             * spb.setRecordCMD(false); String[][] msgs =
             * RCenter.getSystemMessages(RC); if (msgs != null) { for (int i =
             * 0; i < msgs.length; i++) { myMsgs.add(new RcmdBean(msgs[i][0],
             * msgs[i][1])); } } } } else *
             */
            if (sb.isLoggedIn()) {
                String[] msgs = RCenter.getSysMessages(RC);
                if (msgs != null) {
                    myMsgs.addAll(Arrays.asList(msgs));
                }
            }
        } catch (Exception e) {
            LOGGER.error("Error in getMsgVec", e);
        }
        return myMsgs;
    }

    public ArrayList<RcmdBean> getCmdVec() {
        ArrayList<RcmdBean> myCmds = new ArrayList<>();
        RConnection RC = sb.getRConnection();

        if (RC == null || !isRConnectionValid(RC)) {
            LOGGER.error("RConnection is null or invalid in getCmdVec");
            return myCmds;
        }
        
        try {
            if (sb.isLoggedIn() && sb.getDataType().equals("spec") && ab.isInDocker()) {
                SpectraProcessBean spb = (SpectraProcessBean) DataUtils.findBean("spectraProcessor");
                if (spb.isRecordCMD()) {
                    spb.setRecordCMD(false);
                    String[] cmds = RCenter.getRCommandHistory(RC);
                    if (cmds != null) {
                        for (int i = 0; i < cmds.length; i++) {
                            myCmds.add(new RcmdBean(i + 1 + "", cmds[i]));
                        }
                    }
                }
            } else if (sb.isLoggedIn()) {
                String[] cmds = RCenter.getRCommandHistory(RC);
                if (cmds != null) {
                    for (int i = 0; i < cmds.length; i++) {
                        myCmds.add(new RcmdBean(i + 1 + "", cmds[i]));
                    }
                }
            }
        } catch (Exception e) {
            LOGGER.error("Error in getCmdVec", e);
        }
        return myCmds;
    }

    /**
     * Checks if the given RConnection is valid.
     */
    private boolean isRConnectionValid(RConnection RC) {
        try {
            return RC.isConnected() && RC.eval("R.version.string") != null; // Simple R command to validate connection
        } catch (Exception e) {
            LOGGER.error("RConnection is invalid or lost", e);
            return false;
        }
    }

    public MenuModel getSimpleMenuModel() {
        LinkedHashMap<String, String> traceTrack = sb.getNaviTrack();
        String currentPageID = sb.getCurrentPageID();
        List<String> keyVec = new ArrayList(traceTrack.keySet());
        //String domainURL = "/MetaboAnalyst";

        //see if we need to trim the navi bar if too long
        // max 10 (exclude home and download)
        int maxNum = 12;
        int leftInx = 0;
        int rightInx = keyVec.size() - 1;
        boolean maxFlag = false;
        if (keyVec.size() > maxNum) {
            maxFlag = true;
            // the idea is to only show the pages "around" current page 
            int myInx = keyVec.indexOf(currentPageID);

            if (myInx < 2) {
                leftInx = 0;
                rightInx = maxNum;
            } else if (myInx > maxNum / 2) {
                leftInx = myInx - maxNum / 2; //half max
                rightInx = myInx - maxNum / 2 + maxNum;
                if (rightInx > keyVec.size() - 1) {
                    rightInx = keyVec.size() - 1;
                }
            } else {
                rightInx = myInx + maxNum / 2;
                leftInx = myInx - maxNum / 2; //half max
                if (leftInx < 0) {
                    leftInx = 0;
                }
            }
            //System.out.println("=========left:" + leftInx + "=======right:" + rightInx + "===========");
        }

        MenuModel simpleMenuModel = new DefaultMenuModel();
        DefaultMenuItem menuItem = new DefaultMenuItem();

        //set the first URL
        menuItem.setValue("Home");
        menuItem.setOnclick("document.getElementById('safeLogoutBtn').click()");
        //menuItem.setUrl("/MetaboAnalyst/home.xhtml");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //set the 2nd URL
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Modules");
        menuItem.setOnclick("document.getElementById('safeLogoutBtnModule').click()");
        //menuItem.setUrl("/MetaboAnalyst/Secure/ModuleView.xhtml");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //note this keeps order; using myKeys could lose order
        String naviURL;
        int count = 0;
        for (String key : keyVec) {
            count++;

            //should we skip some menu item
            if (maxFlag) {
                if (count < leftInx + 1) {
                    if (!key.equals("Statistics")) { // spare the anchor page
                        continue;
                    }
                }

                if (count > rightInx + 1) {
                    continue;
                }
            }

            naviURL = traceTrack.get(key);
            menuItem = new DefaultMenuItem();
            menuItem.setValue(key);

            //System.out.println("=========domainURL:" + domainURL + "=======naviURL:" + naviURL + "===========");  
            //menuItem.setUrl(domainURL + naviURL);
            menuItem.setUrl(naviURL);
            if (key.equals(currentPageID)) {
                menuItem.setStyle(HIGHLIGHT_STYLE);
            } else {
                menuItem.setStyle(ORIG_STYLE);
            }
            simpleMenuModel.getElements().add(menuItem);
        }

        return simpleMenuModel;
    }

    //Only for 4 main pages involved in workflow and projects
    public MenuModel getWorkflowMenuModel() {

        MenuModel simpleMenuModel = new DefaultMenuModel();
        DefaultMenuItem menuItem = new DefaultMenuItem();

        //set the first URL
        menuItem.setValue("Home");
        menuItem.setOnclick("document.getElementById('safeLogoutBtn').click()");
        //menuItem.setUrl("/MetaboAnalyst/home.xhtml");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //set the 2nd URL
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Modules");
        //menuItem.setOnclick("doLogoutModule();");
        menuItem.setOnclick("document.getElementById('safeLogoutBtnModule').click()");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //set the 3nd URL
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Projects");
        //menuItem.setOnclick("doLogoutModule();");
        menuItem.setUrl("/MetaboAnalyst/Secure/xialabpro/ProjectView.xhtml");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //set the 4th URL
        WorkflowBean wb = (WorkflowBean) DataUtils.findBean("workflowBean");
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Data Prep.");
        //menuItem.setOnclick("doLogoutModule();");
        if (wb.getDataPreparationUrl().isEmpty()) {
            menuItem.setOnclick("document.getElementById('checkWorkflowInput').click()");
        } else {
            menuItem.setUrl(wb.getDataPreparationUrl());
        }
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //set the 5th URL
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Workflows");
        //menuItem.setOnclick("doLogoutModule();");
        menuItem.setUrl("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        //set the 6th URL
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Results");
        //menuItem.setOnclick("doLogoutModule();");
        menuItem.setUrl("/MetaboAnalyst/Secure/xialabpro/DashboardView.xhtml");
        menuItem.setStyle(ORIG_STYLE);
        simpleMenuModel.getElements().add(menuItem);

        return simpleMenuModel;
    }

    public DefaultStreamedContent getRCmdFile() {
        try {
            RDataUtils.saveRCommands(sb.getRConnection());
            return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/Rhistory.R");
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("getRCmdFile", e);
        }
        return null;
    }

    public DefaultStreamedContent getSysMsgFile() {
        try {
            RDataUtils.saveRCommands(sb.getRConnection());
            return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/Project.log");
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("getSysMsgFile", e);
        }
        return null;
    }

    public String toModuleView() {
        return ((ApplicationBean1) DataUtils.findBean("applicationBean1")).getDomainURL() + "/Secure/ModuleView.xhtml";
        //return ((ApplicationBean1) DataUtils.findBean("applicationBean1")).getDomainURL() + "ModuleView.xhtml";
    }

    private String userType = "guest";

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    private String computingNode = "new";

    public String getComputingNode() {
        return computingNode;
    }

    public void setComputingNode(String computingNode) {
        this.computingNode = computingNode;
    }

    private final String vipURL = "https://vip.metaboanalyst.ca/MetaboAnalyst/Secure/upload/SpectraUpload.xhtml";
    private final String vip2URL = "https://vip2.metaboanalyst.ca/MetaboAnalyst/Secure/upload/SpectraUpload.xhtml";

    public String goToSpectraUpload() {
        String myURL;
        //
        if (ab.isOnLocalServer()) { // local
            myURL = ab.getDomainURL() + "/Secure/upload/SpectraUpload.xhtml";
        } else if (ab.isInDocker()) {
            myURL = ab.getDomainURL() + "/Secure/upload/SpectraUpload.xhtml";
        } else if (ab.isOnVipServer2()) {
            myURL = vip2URL;
        } else {
            myURL = vipURL;
        }

        return myURL;
    }

    public void redirectToLogin() {
        System.out.println("redirectToLogin===> " + "/" + ab.getAppName() + "/users/LoginView.xhtml");
        PrimeFaces.current().executeScript("PF('notLoginDialog').hide()");
        DataUtils.doRedirect("/" + ab.getAppName() + "/users/LoginView.xhtml");
    }

    public String setUtilOpt(String utilOpt) {
        // if (doLogin("NA", "utils", false, false)) {
        switch (utilOpt) {
            case "convert" -> {
                return "ID Upload";
            }
            case "lipid" -> {
                return "Lipidomics Upload";
            }
            case "batch" -> {
                // STRONG WARNING: should not use DataUtils.redirect here!
                if (ab.shouldUseScheduler()) {
                    return "Batch Upload";
                } else if (ab.isOnProServer()) {
                    try {
                        FacesContext.getCurrentInstance().getExternalContext().redirect("https://www.metaboanalyst.ca/MetaboAnalyst/Secure/upload/BatchUpload.xhtml");
                        return null;
                    } catch (Exception e) {
                        //e.printStackTrace();
                        LOGGER.error("setUtilOpt", e);
                    }
                } else { //local
                    return "Batch Upload";
                }
            }
            case "replicates" -> {
                return "Duplicates Upload";
            }
        }
        return null;
    }

    public String enterModule() {
        String analType = sb.getAnalType();
        switch (analType) {
            case "stat" -> {
                return "Statistics";
            }
            case "msetqea" -> {
                return "enrichparam";
            }
            case "pathqea" -> {
                return "pathparam";
            }
            case "network" -> {
                if (!sb.getCmpdIDType().equals("pklist")) {
                    RConnection RC = sb.getRConnection();
                    int res = RNetworkUtils.prepareNetworkData(RC); // for concentration table input
                    if (res == 0) {
                        sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                        return null;
                    }
                }
                // since called from Normalization page
                sb.setDspcNet(true);
                return "MnetParam";
            }
            case "mf" -> {
                MultifacBean tb = (MultifacBean) DataUtils.findBean("multifacBean");
                tb.setAscaInit(false);
                return "Multi-factors";
            }
            case "power" -> {
                return "powerparam";
            }
            case "roc" -> {
                return "ROC Analysis";
            }
            case "mummichog" -> {
                return "mzlibview";
            }

            case "dose" -> {
                return "Dose Analysis";
            }
        }
        return null;
    }

    public String getModuleURL(int num) {
        String moduleURL = sb.getCurrentModelURL();
        if (moduleURL == null) {
            moduleURL = ab.getModuleURL();
            sb.setCurrentModelURL(moduleURL);
        }
        String url1;

        url1 = switch (num) {
            case 0 ->
                "/Secure/upload/PeakUploadView.xhtml";
            case 1 ->
                "/Secure/upload/MetaPathLoadView.xhtml";
            case 2 ->
                "/Secure/upload/EnrichUploadView.xhtml";
            case 3 ->
                "/Secure/upload/PathUploadView.xhtml";
            case 4 ->
                "/Secure/upload/JointUploadView.xhtml";
            case 5 ->
                "/Secure/upload/MnetUploadView.xhtml";
            case 6 ->
                "/Secure/upload/StatUploadView.xhtml";
            case 7 ->
                "/Secure/upload/MultifacUploadView.xhtml";
            case 8 ->
                "/Secure/upload/RocUploadView.xhtml";
            case 9 ->
                "/Secure/upload/MetaLoadView.xhtml";
            case 10 ->
                "/Secure/upload/PowerUploadView.xhtml";
            case 11 ->
                "/Secure/upload/DoseUploadView.xhtml";
            case 12 ->
                "/Secure/upload/MgwasUploadView.xhtml";
            case 14 ->
                "/Secure/upload/MS2UploadView.xhtml";
            default ->
                "/Secure/upload/StatUploadView.xhtml";
        };

        /*
        if (sb.isRegisteredLogin()) {
            UserLoginBean ulb = (UserLoginBean) DataUtils.findBean("userLoginBean");
            UserLoginModel currentLoginUser = sb.getCurrentLoginUser();
            String sessionToken = sb.getSessionToken();
            if (ulb.isJustloggedin()) {
                int id = (int) currentLoginUser.getId();
                sessionToken = ulb.keepLoggedinToken(id);
                ulb.setJustloggedin(false);
                //System.out.println("sessionToken --> " + sessionToken);
            } else if (sessionToken == null) {
                int id = (int) currentLoginUser.getId();
                sessionToken = ulb.keepLoggedinToken(id);
                ulb.setJustloggedin(false);
                //System.out.println("new sessionToken --> " + sessionToken);
            }
            //System.out.println("NOW sessionToken is --> " + sessionToken);
            String url2 = "/faces" + url1 + "?redirToken=" + sessionToken;
            //return moduleURL + url2;
            return "https://www.metaboanalyst.ca/MetaboAnalyst" + url2; //temp fix
        }

        return moduleURL + url1;
         */
        FireUserBean ub = (FireUserBean) DataUtils.findBean("fireUserBean");
        if (ub.isOmicsquareVerified()) {

        } else if (sb.getCurrentLoginUser() != null) {
            UserLoginBean ulb = (UserLoginBean) DataUtils.findBean("userLoginBean");
            UserLoginModel currentLoginUser = sb.getCurrentLoginUser();
            String sessionToken = sb.getSessionToken();
            if (ulb.isJustloggedin()) {
                int id = (int) currentLoginUser.getId();
                sessionToken = ulb.keepLoggedinToken(id);
                ulb.setJustloggedin(false);
                //System.out.println("sessionToken --> " + sessionToken);
            } else if (sessionToken == null) {
                int id = (int) currentLoginUser.getId();
                sessionToken = ulb.keepLoggedinToken(id);
                ulb.setJustloggedin(false);
                //System.out.println("new sessionToken --> " + sessionToken);
            }
            //System.out.println("NOW sessionToken is --> " + sessionToken);
            String url2 = "/faces" + url1 + "?redirToken=" + sessionToken;
            //return moduleURL + url2;
            return "https://www.metaboanalyst.ca/MetaboAnalyst" + url2; //temp fix
        }

        return moduleURL + url1;
    }

    public void doExit() {
        sb.doLogout(1);
    }

    public void redirectToPage(String url) {
        FacesContext context = FacesContext.getCurrentInstance();
        if (context != null) {
            try {
                System.out.println("Redirecting to: " + url);
                context.getExternalContext().redirect(url);
            } catch (IOException e) {
                e.printStackTrace();
                System.err.println("Redirection failed: " + e.getMessage());
            }
        } else {
            System.err.println("FacesContext is null. Cannot redirect.");
        }
    }
}
