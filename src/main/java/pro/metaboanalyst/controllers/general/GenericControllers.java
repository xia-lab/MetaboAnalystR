/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
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
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import org.primefaces.model.TreeNode;
import pro.metaboanalyst.controllers.enrich.IntegProcessBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.mnet.MnetResBean;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import static pro.metaboanalyst.utils.NaviUtils.selectNode;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author xia
 */
@RequestScoped
@Named("ctl")
public class GenericControllers implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private SpectraProcessBean spb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private UserLoginBean ulb;

    @JsonIgnore
    @Inject
    private IntegProcessBean ipb;

    @JsonIgnore
    @Inject
    private MetaPathStatBean pmb;

    @JsonIgnore
    @Inject
    private SpectraControlBean pfb;

    @JsonIgnore
    @Inject
    private MetaPathLoadBean plb;

    private static final Logger LOGGER = LogManager.getLogger(GenericControllers.class);
    private static final String ORIG_STYLE = "color: inherit; text-decoration: underline";
    private static final String HIGHLIGHT_STYLE = "color: maroon; text-decoration: none";
    private static final List<String> twoGrpsMethods = Arrays.asList(new String[]{"T-test", "Volcano plot", "Fold change", "EBAM", "SVM", "OrthoPLSDA"});

    /*
     * navigation tree
     */
    public void onNodeSelect(NodeSelectEvent event) {

        TreeNode node = event.getTreeNode();
        String naviKey = node.getData().toString();
        String analType = sb.getAnalType();

        //make sure to load right analType
        String currentAnalType = sb.getNaviTrackAnalType().get(naviKey);
        if (currentAnalType != null && !currentAnalType.equals(sb.getAnalType())) {
            analType = currentAnalType;
            RDataUtils.setAnalType(sb.getRConnection(), analType);
        }

        if (analType.equals("utils")) {
            FacesContext.getCurrentInstance().getApplication().getNavigationHandler()
                    .handleNavigation(FacesContext.getCurrentInstance(), "null", naviKey);
            return;
        }

        if (!naviKey.equals("Upload") && !naviKey.equals("Exit")) {
            if (!sb.isDataUploaded()) {
                sb.addMessage("Error", "You need to upload a dataset first!");
                PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                return;
            }
        }
        selectNode(node);
        String dataType = sb.getDataType();
        switch (naviKey) {
            case "Pre-process":
                if (dataType.equals("conc") || dataType.equals("specbin") || dataType.equals("pktable")) {
                    sb.addMessage("Error", "Your data type does not need this procedure!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Data check":
                if (dataType.equals("conc") || dataType.equals("specbin") || dataType.equals("pktable") || dataType.equals("mass_all") || dataType.equals("mass_table")) {
                    break;
                } else if (!sb.isDataPreprocessed()) {
                    sb.addMessage("Error", "Your need to pre-process your data first!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Name check":
                if (!dataType.equals("conc")) {
                    sb.addMessage("Error", "The procedure is only applicable to compound concentration data!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Conc. check":
                if (!analType.equals("msetssp")) {
                    sb.addMessage("Error", "The procedure is only applicable to single sample profiling!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                } else {
                    naviKey = "msetssp";
                }
                break;
            case "Data editor":
                if (analType.startsWith("mummichog")) {
                    if (!sb.getDataType().equals("mass_table")) {
                        sb.addMessage("Error", "The option is only applicable for peak table, not peak list!");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                }

                if (!sb.isAnalInit("Normalization")) {
                    sb.addMessage("Error", "The data need to be further processed till normalization page for this procedure! ");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Missing value":
            case "Metadata check":
            case "Data filter":
            case "Normalization":
                if (analType.startsWith("mummichog")) {
                    if (!sb.getDataType().equals("mass_table")) {
                        sb.addMessage("Error", "The option is only applicable for peak table, not peak list!");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                } else if (!sb.isIntegChecked()) {
                    sb.addMessage("Error", "The data need to pass integrity check first!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "GSEA result":
            case "Mummi. result":
            case "Integ. result":
                String naviPath = sb.getNaviTrack().get(naviKey);
                if (naviPath != null) {
                    DataUtils.doRedirect(naviPath, ab);
                    return;
                } else {
                    sb.addMessage("Error", "Please set parameters first and use the 'Proceed' button to access this page.");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                }
                break;
            case "Metabolic network":
                if (sb.getNaviTrack().get("Mummi. result") != null
                        || sb.getNaviTrack().get("GSEA result") != null
                        || sb.getNaviTrack().get("Integ. result") != null) {
                    break;
                } else {
                    sb.addMessage("Error", "Please perform enrichment analysis first before viewing them in the network.");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
            case "DSPC network":
                MnetResBean mn = (MnetResBean) DataUtils.findBean("mnetResBean");
                naviKey = mn.computeDspcNet();
            case "Set parameter":
                //need to work out unspecific case
                if (analType.startsWith("mset")) {
                    naviKey = "enrichparam";
                } else if (analType.startsWith("pathinteg")) {

                    if (ipb.getDatatype().equals("peak")) {
                        naviKey = "IntegAnalPeak";
                    } else {
                        naviKey = "IntegAnal";
                    }
                } else if (analType.startsWith("path")) {
                    naviKey = "pathparam";
                } else if (analType.startsWith("power")) {
                    naviKey = "powerparam";
                } else if (analType.startsWith("network")) {
                    naviKey = "MnetParam";
                } else if (analType.startsWith("roc")) {
                    naviKey = "Multivariate";
                } else if (analType.startsWith("mummichog")) {
                    if (sb.getDataType().equals("mass_table")) {
                        naviKey = "mzlibview";
                    } else {
                        naviKey = "mzlibview";
                    }
                } else if (analType.startsWith("metapaths")) {
                    naviKey = "Meta-Analysis Params";
                    pmb.setResOK(false);
                    pmb.setResOK2(false);
                }
                break;
            case "View result":
                //need to work out unspecific case
                if (analType.equals("msetora") || analType.equals("msetssp")) {
                    naviKey = "oraview";
                } else if (analType.equals("msetqea")) {
                    naviKey = "qeaview";
                } else if (analType.startsWith("pathinteg")) {
                    naviKey = "integview";
                } else if (analType.startsWith("path")) {
                    naviKey = "pathview";
                } else if (analType.startsWith("power")) {
                    naviKey = "powerview";
                } else if (analType.startsWith("mummichog")) {
                    naviKey = "mummires";
                    node.setExpanded(true);
                } else if (analType.equals("gsea_peaks")) {
                    naviKey = "gseapkview";
                } else if (analType.equals("integ_peaks")) {
                    naviKey = "peakintegview";
                } else if (analType.startsWith("metapaths")) {
                    sb.addMessage("Error", "Select the result page ('Meta path' or 'Pooling peaks') you want to reach from the sub-nodes!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Upload":
                if (sb.isSwitchMode()) {
                    sb.addMessage("Error", "The upload page is undefined after you switched to a new module. Please exit to restart again.");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                } else {
                    String naviPath1 = sb.getNaviTrack().get("Upload");
                    if (naviPath1 != null) {
                        DataUtils.doRedirect(naviPath1, ab);
                        return;
                    }
                }
                break;
            case "Correlations":
                if (analType.equals("mf")) {
                    naviKey = "CorrelationTest";
                }
                break;
            case "RandomForest":
                if (analType.equals("mf")) {
                    naviKey = "RandomForest2";
                }
                break;
            case "ID map":
            case "Download":
                break;
            case "Spectra check":
            case "Spectra processing":

                if (!pfb.isDataConfirmed() || pfb.getIncludedFileNamesString().equals("")) {
                    sb.addMessage("Error", "No right spectra files selected or confirmed!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }

                if (pfb.isJobSubmitted() && (pfb.getCurrentJobStatus().contains("Pending") || pfb.getCurrentJobStatus().contains("Running"))) {
                    sb.addMessage("Error", "Job is running, can not modify parameters again!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                } else {
                    pfb.setStopStatusCheck(true);
                    pfb.setJobSubmitted(false);
                    pfb.setPerformedPlan(false);
                    pfb.setCurrentJobId(0);
                    pfb.setCurrentJobStatus("Submitting...");
                    pfb.setProgress2(0.0);
                }
                break;
            case "Spectra result":

                if (!pfb.isDataConfirmed() || pfb.getIncludedFileNamesString().equals("")) {
                    sb.addMessage("Error", "No right spectra files selected or confirmed!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }

                if (!pfb.getCurrentJobStatus().contains("Finished") || !pfb.getFinishedJobStatus().contains("Finished")) {
                    sb.addMessage("Error", "Please wait until the job is finished!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Job status":

                if (!pfb.isDataConfirmed() || pfb.getIncludedFileNamesString().equals("")) {
                    sb.addMessage("Error", "No right spectra files selected or confirmed!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }

                String currentPage = FacesContext.getCurrentInstance().getViewRoot().getViewId();
                if (currentPage.contains("SpectraProcess")) {
                    sb.addMessage("Error", "Please click on submit job button located at the bottom of the page instead!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                } else {
                    pfb.setCurrentJobId(pfb.getFinishedJobId());
                    pfb.setProgress2(pfb.getFinishedProgress2());
                    pfb.setCurrentJobStatus(pfb.getFinishedJobStatus());
                    pfb.setStopStatusCheck(false);
                    pfb.setJobSubmitted(true);
                    pfb.setPerformedPlan(true);
                }
                break;
            case "Meta-Analysis Params":
                break;
            case "Meta paths":
                naviKey = "Meta-Analysis Results";

                if (pmb.isResOK() && pmb.isPlotOK()) {
                    break;
                } else {
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    sb.addMessage("Error", "Your result is not ready! Click Submit button for processing from the 'Set parameter' page!");
                    return;
                }
            case "Pooling peaks":

                if (pmb.getPoolAlgOpt().equals("mummichog")) {
                    naviKey = "mummires";
                } else if (pmb.getPoolAlgOpt().equals("gsea_peaks")) {
                    naviKey = "gseapkview";
                }

                if (pmb.isResOK2()) {
                    break;
                } else {
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    sb.addMessage("Error", "Your result is not ready! Click Submit button for processing from the 'Set parameter' page!");
                    return;
                }
            case "Upset diagram":
                //MetaPathLoadBean plb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");
                if (!sb.getNaviTrack().containsKey("Upset diagram")) {
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    sb.addMessage("Error", "Please select datasets you wish to process by click 'UpSet diagram' button from Result page! ");
                    return;
                }
                break;
            case "Network viewer":
                if (sb.getAnalType().equals("metapaths")) {
                    if (plb.getSelectedData().getName() != null) {
                        sb.addMessage("Error", "Click 'Network Analysis' button from Results page to continue!");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                }
                break;
            case "Heatmap view":
                if (analType.startsWith("mummichog")) {
                    if (!sb.getDataType().equals("mass_table")) {
                        sb.addMessage("Error", "The option is only applicable for peak table, not peak list!");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                }
                break;
            default://all statisitcal 
                if (sb.getAnalType().equals("stat")) {
                    if (!sb.isDataNormed()) {
                        sb.addMessage("Error", "The data need to be normalized first!");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                }
                if (naviKey.equals("ASCA")) {
                    if (sb.isTimeOnly()) {
                        sb.addMessage("Error", "This method has not been tested for time-series only data!");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                } else if (naviKey.equals("MEBA")) {
                    if (!sb.isContainsTime()) {
                        sb.addMessage("Error", "This method only work on time-series data.");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                }
                if (sb.isMultiGroup()) {
                    if (twoGrpsMethods.contains(naviKey)) {
                        sb.addMessage("Error", "The method is only applicable for two-group data analysis! "
                                + "You can use <b>Data Editor</b> => <b>Edit Groups</b> to specify two groups of interest for analysis.");
                        PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                        return;
                    }
                } else if (naviKey.equals("ANOVA")) {
                    sb.addMessage("Error", "The method is only for multi-group data analysis!");
                    PrimeFaces.current().executeScript("PF('statusDialog').hide()");
                    return;
                }

        }
        //System.out.println("end==========" + naviKey);
        //test if we have entered URL in the track
        if (sb.getNaviTrack().keySet().contains(naviKey)) {
            String key = sb.getNaviTrack().get(naviKey);
            if (!key.equals("0")) {
                DataUtils.doRedirect(sb.getNaviTrack().get(naviKey), ab);
            }
        } else {
            FacesContext.getCurrentInstance().getApplication().getNavigationHandler()
                    .handleNavigation(FacesContext.getCurrentInstance(), "null", naviKey);
        }

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

        try {
            RConnection RC = sb.getRConnection();
            if (RC == null || !isRConnectionValid(RC)) {
                return myCmds; // empty, not null
            }
            if (!sb.isLoggedIn()) {
                return myCmds;
            }

            // Only control the record flag in the special case
            if (sb.getDataType().equals("spec") && ab.isInDocker() && spb.isRecordCMD()) {
                spb.setRecordCMD(false);
            }

            String[] cmds = RCenter.getRCommandHistory(RC);
            if (cmds == null || cmds.length == 0) {
                return myCmds;
            }

            // Newest first (optional); comment out if you prefer oldest first
            for (int i = cmds.length - 1, step = 1; i >= 0; i--, step++) {
                String c = cmds[i];
                if (c != null) {
                    c = c.trim();
                    if (!c.isEmpty()) {
                        myCmds.add(new RcmdBean(String.valueOf(step), c));
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
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Data");
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
        DataUtils.doRedirect("/" + ab.getAppName() + "/users/LoginView.xhtml", ab);
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
                mfb.setAscaInit(false);
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
        if (fub.isOmicsquareVerified()) {

        } else if (sb.getCurrentLoginUser() != null) {
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
