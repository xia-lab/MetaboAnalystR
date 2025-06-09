/*
 * SessionBean1.java
 *
 * Created on Oct 21, 2008, 9:37:17 AM
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.controllers.mnet.MnetResBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.models.ColorBean;
import pro.metaboanalyst.models.SampleBean;
import pro.metaboanalyst.models.User;
import pro.metaboanalyst.project.UserLoginModel;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.utils.NaviUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.model.DefaultTreeNode;
import org.primefaces.model.TreeNode;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.ActionEvent;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.Serializable;
import java.util.*;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.lts.FireUserBean;
import org.omnifaces.util.Faces;
import pro.metaboanalyst.workflows.WorkflowView;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.DiagramView;
import pro.metaboanalyst.workflows.WorkflowBean;

@SessionScoped
@Named("sessionBean1")
@JsonIgnoreProperties(ignoreUnknown = true)

public class SessionBean1 implements Serializable {

    @JsonIgnore
    private static final long serialVersionUID = 3520685098167757691L;
    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private WorkflowView wfv;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private FireBaseController fbc;

    @JsonIgnore
    @Inject
    private DiagramView dv;

    @JsonIgnore
    @Inject
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private FireBase fb;

    @JsonIgnore
    @Inject
    private MnetResBean mnb;

    @JsonIgnore
    @Inject
    private ResourceSemaphore resourceSemaphore;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    //****************user defined methods**************
    //USED TO BE FINAL, REMOVED FINAL AND ADDED SETTER FUNCTION FOR DESERIALIZATION FROM JSON
    //OTHERWISE, CAN NOT RESTORE STATE.
    private LinkedHashMap<String, String> naviTrack = new LinkedHashMap<>();
    private LinkedHashMap<String, String> naviTrackAnalType = new LinkedHashMap<>();//useful for workflow custom navitree, on navigating to page load analtype 

    /**
     * To record all commands that produce the images
     */
    private HashMap<String, String> graphicsMap = new HashMap<>();
    private HashMap<String, String> graphicsMapLink = new HashMap<>();

    private final int ACQUIRE_TIMEOUT = 10;
    /**
     * *****************************************
     * Methods for users managements *****************************************
     */
    private boolean reloadReportImage = false;
    private String cmpdSummaryType = "boxplot";
    private boolean loggedIn = false;
    @JsonIgnore
    private User currentUser = null;
    @JsonIgnore
    private RConnection RC = null;
    private String dataType = "unknown";
    private String analType = "unknown";
    private boolean paired = false;
    private boolean regression = false;
    private boolean keepClsOrder = true; //for multiple groups using PLS-DA and othogonal PLS-DA
    private boolean dataUploaded = false;
    private boolean dataProcessed = false;
    private boolean integChecked = false;
    private boolean dataNormed = false;
    @JsonIgnore
    private TreeNode naviTree = null;
    private String naviType = "NA";

    private String moduleURL = null;
    private boolean switchMode = false;
    private String sigSource = "";
    private String imgSource = null;
    private String currentPathName, currentCmpdName;
    //central place for user access control
    private boolean privileged = true;
    private String sessionToken;
    /**
     * To remember all image names in order to update immediately to avoid
     * caching problem. And remove images left previously
     */
    private HashMap<String, Integer> imgMap = new HashMap<>();
    private HashMap<String, String> reportImgMap = new HashMap<>();
    private HashMap<String, String> reportJsonMap = new HashMap<>();

    private int fileCount = 0;
    private String currentPageID = "";
    private String cmpdIDType = "na";
    private String featType = "none";
    private boolean multiGroup = false;
    private boolean smallSmplSize = false;
    private int defaultFilterCutoff = 0;
    private int filterMin = 0;
    //record whether ttests or anova give significant features
    private boolean anovaSig = false;
    private boolean lmSig = false;
    private boolean ttSig = false;
    private boolean covSig = false;
    //need to remember some states cross pages
    private boolean msPeakAligned = false;
    private boolean msSpecAligned = false;
    //For graph regeneration
    private String formatOpt = "png";
    private int dpiOpt = 300;
    private String sizeOpt = "NA";
    private String imgDownloadTxt = "";
    private boolean bigFeature = false;
    private int featureNum;
    private int sampleNum;
    //ROC analysis for one variable
    private boolean roc1Col = false;
    private String org = "NA";
    private String partialId = "";
    private String naviString;
    //project saving
    @JsonIgnore
    private UserLoginModel currentLoginUser = null;
    private boolean registeredLogin = false;
    private boolean saveEnabled = false;
    private String resetToken;
    private boolean dspcNet = false;
    @JsonIgnore
    private List<ColorBean> colorBeanLists;
    private SelectItem[] metaInfo = null;
    private String expFac;
    private String heatmapType = "NA";
    private List<SampleBean> sampleBeans = null;
    private String boxplotUrl;
    //temp session storage for some request bean
    private boolean jobDone = false;
    private boolean partialLinkValide = false;
    private String tsDesign = "multi";
    private int imgCount = 0;
    private String cmpdSummaryNm;
    private boolean showResumable = false;
    private String currentNaviUrl = "";
    private String shareToken = "";
    private int maxtopNum;
    private String visMode = "static"; //4 mode: ppi, chem, tf, drug
    private String uploadType = "table";

    public void setNoticeSize(int noticeSize) {
        this.noticeSize = noticeSize;
    }

    public String getVisMode() {
        return visMode;
    }

    public void setVisMode(String visMode) {
        this.visMode = visMode;
    }

    public LinkedHashMap<String, String> getNaviTrackAnalType() {
        return naviTrackAnalType;
    }

    public void setNaviTrackAnalType(LinkedHashMap<String, String> naviTrackAnalType) {
        this.naviTrackAnalType = naviTrackAnalType;
    }

    public HashMap<String, String> getGraphicsMapLink() {
        return graphicsMapLink;
    }

    public void setGraphicsMapLink(HashMap<String, String> graphicsMapLink) {
        this.graphicsMapLink = graphicsMapLink;
    }

    public void setGraphicsMap(HashMap<String, String> graphicsMap) {
        this.graphicsMap = graphicsMap;
    }

    public void setModuleURL(String moduleURL) {
        this.moduleURL = moduleURL;
    }

    public void setSigSource(String sigSource) {
        this.sigSource = sigSource;
    }

    public void setImgSource(String imgSource) {
        this.imgSource = imgSource;
    }

    public void setFileCount(int fileCount) {
        this.fileCount = fileCount;
    }

    public void setCurrentPageID(String currentPageID) {
        this.currentPageID = currentPageID;
    }

    public void setBigFeature(boolean bigFeature) {
        this.bigFeature = bigFeature;
    }

    public void setFeatureNum(int featureNum) {
        this.featureNum = featureNum;
    }

    public void setSampleNum(int sampleNum) {
        this.sampleNum = sampleNum;
    }

    public void setRoc1Col(boolean roc1Col) {
        this.roc1Col = roc1Col;
    }

    public void setMetaInfo(SelectItem[] metaInfo) {
        this.metaInfo = metaInfo;
    }

    public void setImgCount(int imgCount) {
        this.imgCount = imgCount;
    }

    public boolean isPrivileged() {
        return privileged;
    }

    public void setPrivileged(boolean privileged) {
        this.privileged = privileged;
    }

    public void setNaviType(String naviType) {
        this.naviType = naviType;
    }

    public boolean doLogin(String dataType, String analType, boolean isRegression, boolean paired) {
        return doLogin(dataType, analType, isRegression, paired, "", false);
    }

    public void setNaviTrack(LinkedHashMap<String, String> naviTrack) {
        this.naviTrack = naviTrack;
    }

    /*
     * Log in and out
     * dataType: list, conc, specbin, pktable, nmrpeak, mspeak, msspec
     * analType: stat, pathora, pathqea, msetora, msetssp, msetqea, msetview, cmpdmap, peaksearch, smpmap
     * */
    public boolean doLogin(String dataType, String analType, boolean isRegression, boolean paired, String previousFolderName, boolean reload) {

        if (!ab.validateExpiry() & ab.isInDocker()) {
            addMessage("Error", "Your license is expired! Please contact support@xialab.ca to renew it!");
            return false;
        }

        if (!ab.isCompiled()) {

            if (!ab.compileRScripts(analType)) {
                addMessage("error", "Cannot connect to Rserver! Please start your Rserver with the right permission!");
                return false;
            }
        }

        if (currentUser != null) {
            if (RC != null) {
                RC.close();
            }
            currentUser = null;
            if (analType.equals("roc")) {
                FacesContext.getCurrentInstance().getExternalContext().getSessionMap().remove("rocAnalBean");
            }
        }

        if (reload) {
            currentUser = DataUtils.createPreviousFolder(ab.getRealUserHomePath(), previousFolderName);
        } else {
            if (analType.equals("raw") && ab.shouldUseScheduler()) {
                currentUser = DataUtils.createRawSpecUser(ab.getRaw_spec_folder());
            } else {
                currentUser = DataUtils.createTempUser(ab.getRealUserHomePath());
            }
        }

        String myAnalType = analType;
        if (analType.equals("mummichog")) {
            myAnalType = dataType; //"mass_all" or "mass_table" for refined R function loading
        }
        //System.out.println("pro.metaboanalyst.controllers.general.SessionBean1.doLogin()" + analType);
        RC = getRConnection(myAnalType);
        System.out.println("CURRENTUSER===" + getCurrentUser().getOrigHomeDir());

        if (RC == null) {
            addMessage("Error", "Cannot connect to Rserve. Please make sure that your application has been authenticated, and start your Rserver with the right permission!");
            return false;
        }

        performCleaning();

        //setDefaultTableView();
        this.setDataType(dataType);
        this.setAnalType(analType);
        this.setPaired(paired);
        this.setRegression(isRegression);

        if (naviTrack.keySet().size() > 2) {
            int count = 0;
            Iterator<String> iterator = naviTrack.keySet().iterator();
            while (iterator.hasNext()) {
                count++;
                iterator.next();
                if (count > 2) {
                    iterator.remove();
                }
            }
        }

        RDataUtils.initDataObjects(RC, dataType, analType, paired);

        if ("spec".equals(dataType)) {
            //check optilcms version
            String checkres = RDataUtils.checkOptiLCMS(RC, ab.getOptiLCMSversion());
            if (!checkres.equals("TRUE")) {
//                System.out.println("MP Updating your local OPtiLCMS now....");
//                int upres = RDataUtils.updateOptiLCMS(RC);
//                System.out.println("MP Updating finished!" + upres);
            }

            //job scheduler init path for output files
            String path = "";
            if (ab.shouldUseScheduler()) {
                path = ab.getRaw_spec_folder();
            } else if (ab.isOnLocalServer()) {
                path = RDataUtils.getPathForScheduler(RC);
            }
            path = path + currentUser.getName() + "/";
            RDataUtils.setUserPathForScheduler(RC, path);
            RDataUtils.initDataPlan(RC); //record to R
        }

        this.setLoggedIn(true);
        addMessage("info", "Log in successful.");
        try {
            //WorkflowBean wb = (WorkflowBean) DataUtils.findBean("workflowBean");
            wb.setCalledWorkflowsError(new LinkedHashSet());
            wb.setCalledWorkflows(new LinkedHashSet());
        } catch (Exception e) {
            System.out.println("Failed to use find bean. Trying CDI way...");
            //WorkflowBean wb = CDI.current().select(WorkflowBean.class).get();
            wb.setCalledWorkflowsError(new LinkedHashSet());
            wb.setCalledWorkflows(new LinkedHashSet());
        }

        RDataUtils.setPrimaryUser(RC);
        try {
            PrimeFaces.current().executeScript("localStorage.clear();");
        } catch (Exception e) {
            System.out.println("Failed to use clear local storage...");
        }
        //PrimeFaces.current().executeScript("console.log('clear========');");
        //System.out.println("loginOK");
        return true;
    }

    public boolean doSpecLogin(String dataType, String analType, boolean isRegression, boolean paired, String previousFolderName, boolean reload) {
        if (!ab.isCompiled()) {

            if (!ab.compileRScripts(analType)) {
                addMessage("error", "Cannot connect to Rserver! Please start your Rserver with the right permission!");
                return false;
            }
        }

        if (currentUser != null) {
            if (RC != null) {
                RC.close();
            }
            currentUser = null;
            if (analType.equals("roc")) {
                FacesContext.getCurrentInstance().getExternalContext().getSessionMap().remove("rocAnalBean");
            }
        }

        if (reload) {
            currentUser = DataUtils.createPreviousFolder(ab.getRealUserHomePath(), previousFolderName);
        } else {
            User user = new User();
            //System.out.println("doSpecLogin === previousFolderName--> " + previousFolderName);
            //System.out.println("doSpecLogin === ab.getRaw_spec_folder()--> " + ab.getRaw_spec_folder());
            user.setName(previousFolderName);
            user.setRelativeDir(ab.getRaw_spec_folder() + previousFolderName);
            user.setHomeDir(ab.getRaw_spec_folder() + previousFolderName);
            user.setOrigHomeDir(ab.getRaw_spec_folder() + previousFolderName);
            currentUser = user; //DataUtils.createRawSpecUser(ab.getRaw_spec_folder());
        }

        RC = getRConnection(analType);

        if (RC == null) {
            addMessage("Error", "Cannot connect to Rserve. Please make sure that your application has been authenticated, and start your Rserver with the right permission!");
            return false;
        }

        performCleaning();

        //setDefaultTableView();
        this.setDataType(dataType);
        this.setAnalType(analType);
        this.setPaired(paired);
        this.setRegression(isRegression);

        if (naviTrack.keySet().size() > 2) {
            int count = 0;
            Iterator<String> iterator = naviTrack.keySet().iterator();
            while (iterator.hasNext()) {
                count++;
                iterator.next();
                if (count > 2) {
                    iterator.remove();
                }
            }
        }

        RDataUtils.initDataObjects(RC, dataType, analType, paired);

        if ("spec".equals(dataType)) {

            //job scheduler init path for output files
            String path = "";
            if (ab.shouldUseScheduler()) {
                path = ab.getRaw_spec_folder();
            } else if (ab.isOnLocalServer()) {
                path = RDataUtils.getPathForScheduler(RC);
            }
            path = path + currentUser.getName() + "/";
            RDataUtils.setUserPathForScheduler(RC, path);
            RDataUtils.initDataPlan(RC); //record to R
        }

        this.setLoggedIn(true);

        RDataUtils.setPrimaryUser(RC);
        //PrimeFaces.current().executeScript("localStorage.clear();");
        //PrimeFaces.current().executeScript("console.log('spec clear========');");

        return true;
    }

    //1 logout and to homepage
    //0 logout only
    public void doLogout(int returnHome) {
        if (registeredLogin) {

            Faces.addResponseCookie("user", fub.getEmail(), "/", 3600);
            fb.getUserMap().put(fub.getEmail(), fub);
        }
        if (loggedIn) {
            if (RC != null) {
                if (ab.isOnLocalServer()) {
                    RCenter.showMemoryUsage(RC);
                }
                RC.close();
            }
            loggedIn = false;

            FacesContext.getCurrentInstance().getExternalContext().invalidateSession();
            FacesContext.getCurrentInstance().getViewRoot().getViewMap().clear();
            reset2DefaultState();
            if (returnHome == 1) {
                DataUtils.doRedirect(ab.getDomainURL(), ab);
            } else {
                FacesContext.getCurrentInstance().getExternalContext().getSessionMap().put("MA6_PRO_user", true);
            }
            //System.out.println("=====called logout: ");
        } else {

            String rootId = FacesContext.getCurrentInstance().getViewRoot().getViewId();
            if (!rootId.endsWith("home.xhtml") & returnHome == 1) {
                // Still need to destory the session beans once user click 'Exit' -- zhiqiang
                // for exit 1 only. Issues with ModuleView page 
                FacesContext.getCurrentInstance().getExternalContext().invalidateSession();
                FacesContext.getCurrentInstance().getViewRoot().getViewMap().clear();

                DataUtils.doRedirect(ab.getDomainURL(), ab);
            }
        }

    }

    public RConnection getRConnection(String myAnalType) {
        if (!ab.isInDocker() | ab.isDockerAuthed()) {
            return RCenter.getRConnectionRawSharing(currentUser.getHomeDir(), ab.getRscriptLoaderPath(), myAnalType);
        } else {
            return null;
        }
    }

    public boolean isLoggedIn() {
        return loggedIn;
    }

    public void setLoggedIn(boolean loggedIn) {
        this.loggedIn = loggedIn;
        if (loggedIn) {
            RCenter.recordMessage(RC, "Log in successful.");
        }
    }

    public boolean isPriviliedged() {
        return privileged;
    }

    public String getSessionToken() {
        return sessionToken;
    }

    //private final Set<String> overViewPageIDs = Set.of("Modules", "Projects", "Workflows");
    private final Set<String> overViewPageIDs = Set.of("Modules", "Projects");

    public boolean isOverviewPage() {
        return overViewPageIDs.contains(currentPageID);
    }

    public void setSessionToken(String sessionToken) {
        this.sessionToken = sessionToken;
    }

    public String getCurrentModelURL() {
        return moduleURL;
    }

    public void setCurrentModelURL(String currentModelURL) {
        this.moduleURL = currentModelURL;
    }

    /**
     * Record key steps
     */
    public void reset2DefaultState() {
        this.moduleURL = null;
        this.dataUploaded = false;
        this.integChecked = false;
        this.dataProcessed = false;
        this.dataNormed = false;
        imgMap.clear();
        naviTrack.clear();
        naviTrackAnalType.clear();

    }

    public void setDataUploaded() {
        this.dataUploaded = true;
        this.integChecked = false;
        this.dataProcessed = false;
        this.dataNormed = false;
        addMessage("info", "Data upload successful.");
    }

    public void setIntegChecked() {
        this.dataUploaded = true;
        this.integChecked = true;
        this.dataProcessed = false;
        this.dataNormed = false;
        addMessage("info", "Integrity check successful.");
    }

    public void setDataProcessed() {
        this.dataUploaded = true;
        this.integChecked = true;
        this.dataProcessed = true;
        this.dataNormed = false;
        addMessage("info", "Data processed successful.");
    }

    public void setDataNormed() {
        this.dataUploaded = true;
        this.integChecked = true;
        this.dataProcessed = true;
        this.dataNormed = true;
        addMessage("info", "Data normalization successful.");
    }

    public boolean isDataUploaded() {
        return dataUploaded;
    }

    public void setDataUploaded(boolean dataUploaded) {
        this.dataUploaded = dataUploaded;
    }

    public boolean isDataProcessed() {
        return dataProcessed;
    }

    public boolean isIntegChecked() {
        return integChecked;
    }

    public boolean isDataNormed() {
        return dataNormed;
    }


    /*
     * record the pages that have been visited or is visiting
     */
    private void registerPage(String pageName) {
        currentPageID = pageName;
        if (naviTree == null) {
            return;
        }
        if (pageName.equals("Name check")) {
            String info[] = RDataUtils.getNameCheckMsgs(RC);
            int state = Integer.parseInt(info[0]);
            String msg = info[1];
            switch (state) {
                case 1 -> {
                }
                case 2 ->
                    addMessage("Warn", msg);
                default ->
                    addMessage("Error", msg);
            }
        }

//        PrimeFaces.current().executeScript("sendMsg("+ pageName +")");
        NaviUtils.getSelectedNode(naviTree, pageName);
    }

    public String getCurrentImage(String key) {
        if (!imgMap.containsKey(key)) {
            //  System.out.println("=========== called here!! " + key );
            imgMap.put(key, 0);
        }
        return key + "_" + imgMap.get(key) + "_";
    }

    public String getNewImage(String key) {
        if (!imgMap.containsKey(key)) {
            imgMap.put(key, 0);
        } else {
            imgMap.put(key, imgMap.get(key) + 1);
        }
        return key + "_" + imgMap.get(key) + "_";
    }

    public int getFileCount() {
        fileCount++;
        return fileCount;
    }

    public boolean isReloadReportImage() {
        return reloadReportImage;
    }

    public void setReloadReportImage(boolean reloadReportImage) {
        this.reloadReportImage = reloadReportImage;
    }

    /**
     * Get images for display
     *
     * @param name: the short image name
     * @return the image at the specified URL
     */
    //@return path to image
    public String getCurrentImageURL(String name) {
        return ab.getRootContext() + getCurrentUser().getRelativeDir() + File.separator + getCurrentImage(name) + "dpi150.png";
    }

    /**
     * get JSON files for interactive
     *
     * @param name: file name
     * @return the URL
     */
    public String getJsonDir(String name) {
        if (reloadReportImage && reportJsonMap.containsKey(name)) {
            return currentUser.getRelativeDir() + "/" + reportJsonMap.get(name);
        } else {
            return currentUser.getRelativeDir() + "/" + getCurrentImage(name) + ".json";
        }
    }

    public String getUserDir() {
        return "/MetaboAnalyst/" + currentUser.getRelativeDir();
    }

    public String getRawUserDir() {
        String guestName = getCurrentUser().getName();
        return "/MetaboAnalyst/resources/users/" + guestName;
    }

    public void addGraphicsCMD(String key, String rcmd) {
        graphicsMap.put(key, rcmd);
    }

    public void addGraphicsMapLink(String key, String rcmd) {
        graphicsMapLink.put(key, rcmd);
    }

    public HashMap<String, String> getGraphicsMap() {
        return graphicsMap;
    }

    //when data changed, reset to allow recomputing
    public void resetAnalysis() {
        if (naviTree != null) {
            Iterator<TreeNode> i = naviTree.getChildren().iterator();
            while (i.hasNext()) {
                TreeNode nd = i.next();
                if (nd.getData().toString().equals("Statistics")
                        || nd.getData().toString().equals("Multi-factors")
                        || nd.getData().toString().equals("Pathway")
                        || nd.getData().toString().equals("Enrichment")
                        || nd.getData().toString().equals("ROC Analysis")
                        || nd.getData().toString().equals("Power Analysis")) {
                    Iterator<TreeNode> i2 = nd.getChildren().iterator();
                    while (i2.hasNext()) {
                        TreeNode nd2 = i2.next();
                        naviTrack.remove(nd2.getData().toString());
                    }
                }
            }
        }
        colorBeanLists = null;
    }

    public boolean isAnalInit(String pageID) {
        return naviTrack.keySet().contains(pageID);
    }

    public String getCurrentPageID() {
        return currentPageID;
    }

    public void buildCustomNaviTree() {
        // Iterate over the keys from naviTrack
        naviTree = new DefaultTreeNode("Root", null); // Initialize the root
        for (String key : naviTrack.keySet()) {
            if (!key.equals("Workflows") && !key.equals("Modules")) {
                TreeNode node = new DefaultTreeNode(key, naviTree); // Add to root
                node.setSelectable(true); // Make it selectable (optional)
            }
        }
    }

    public void initNaviTree(String type) {
        if (!naviType.equals(type)) {
            naviType = type;
            naviTree = NaviUtils.createNaviTree(type);
        }
        //WorkflowBean wb = (WorkflowBean) DataUtils.findBean("workflowBean");
        wb.setEditMode(false);
    }

    public TreeNode getNaviTree() {
        if (naviTree == null) { //upload page
            TreeNode tmpTree = new DefaultTreeNode("Root", null);
            TreeNode upNode = new DefaultTreeNode("Upload", tmpTree);
            return tmpTree;
        }
        return naviTree;
    }

    public void setNaviTree(TreeNode naviTree) {
        this.naviTree = naviTree;
    }

    public String getCmpdIDType() {
        return cmpdIDType;
    }

    public void setCmpdIDType(String cmpdIDType) {
        this.cmpdIDType = cmpdIDType;
    }

    public String getFeatType() {
        return featType;
    }

    public void setFeatType(String featType) {
        this.featType = featType;
    }

    public String getUploadType() {
        return uploadType;
    }

    public void setUploadType(String uploadType) {
        this.uploadType = uploadType;
    }

    public String getNaviType() {
        return naviType;
    }

    public String getAnalType() {
        return analType;
    }

    public void setAnalType(String type) {
        analType = type;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        //System.out.println("pro.metaboanalyst.controllers.general.SessionBean1.setDataType()" + dataType);
        this.dataType = dataType;
    }

    public void setRegression(boolean regression) {
        this.regression = regression;
    }

    public boolean isPaired() {
        return paired;
    }

    public void setPaired(boolean paired) {
        this.paired = paired;
    }

    public boolean isRegresion() {
        return regression;
    }

    public User getCurrentUser() {
        return currentUser;
    }

    public void setCurrentUser(User currentUser) {
        this.currentUser = currentUser;
    }

    public RConnection getRConnection() {
        return RC;
    }

    public void setRConnetion(RConnection RC) {
        this.RC = RC;
    }

    public boolean isMultiGroup() {
        return multiGroup;
    }

    public void setMultiGroup(boolean multiGroup) {
        this.multiGroup = multiGroup;
    }

    public boolean isKeepClsOrder() {
        return keepClsOrder;
    }

    public void setKeepClsOrder(boolean keepClsOrder) {
        this.keepClsOrder = keepClsOrder;
    }

    public boolean isSmallSmplSize() {
        return smallSmplSize;
    }

    public void setSmallSmplSize(boolean smallSmplSize) {
        this.smallSmplSize = smallSmplSize;
    }

    public int getDefaultFilterCutoff() {
        return defaultFilterCutoff;
    }

    public void setDefaultFilterCutoff(int defaultFilterCutoff) {
        this.defaultFilterCutoff = defaultFilterCutoff;
    }

    public int getFilterMin() {
        return filterMin;
    }

    public void setFilterMin(int filterMin) {
        this.filterMin = filterMin;
    }

    public boolean isLmSig() {
        return lmSig;
    }

    public void setLmSig(boolean lmSig) {
        this.lmSig = lmSig;
    }

    public boolean isCovSig() {
        return covSig;
    }

    public void setCovSig(boolean covSig) {
        this.covSig = covSig;
    }

    public boolean isAnovaSig() {
        return anovaSig;
    }

    public void setAnovaSig(boolean anovaSig) {
        this.anovaSig = anovaSig;
    }

    public boolean isTtSig() {
        return ttSig;
    }

    public void setTtSig(boolean ttSig) {
        this.ttSig = ttSig;
    }

    public String detailsLnk_action(String code) {
        sigSource = code;
        return "featuredetails";
    }

    public String getSigSource() {
        return sigSource;
    }

    public void graphicsLnk_action(String code) {
        setImgDownloadTxt("");
        imgSource = code;
        //System.out.println("===========current img key: " + code);
    }

    public String getImageSource() {
        return imgSource;
    }

    public String getCurrentPathName() {
        return currentPathName;
    }

    public void setCurrentPathName(String currentPathName) {
        this.currentPathName = currentPathName;
    }

    public String getCurrentCmpdName() {
        return currentCmpdName;
    }

    public void setCurrentCmpdName(String currentCmpdName) {
        this.currentCmpdName = currentCmpdName;
    }

    public boolean isMsPeakAligned() {
        return msPeakAligned;
    }

    public void setMsPeakAligned(boolean msPeakAligned) {
        this.msPeakAligned = msPeakAligned;
    }

    public boolean isMsSpecAligned() {
        return msSpecAligned;
    }

    public void setMsSpecAligned(boolean msSpecAligned) {
        this.msSpecAligned = msSpecAligned;
    }

    public String getFormatOpt() {
        return formatOpt;
    }

    public void setFormatOpt(String formatOpt) {
        this.formatOpt = formatOpt;
    }

    public int getDpiOpt() {
        return dpiOpt;
    }

    public void setDpiOpt(int dpiOpt) {
        this.dpiOpt = dpiOpt;
    }

    public String getSizeOpt() {
        return sizeOpt;
    }

    public void setSizeOpt(String sizeOpt) {
        this.sizeOpt = sizeOpt;
    }

    public String getImgDownloadTxt() {
        return imgDownloadTxt;
    }

    public void setImgDownloadTxt(String imgDownloadTxt) {
        this.imgDownloadTxt = imgDownloadTxt;
    }

    public boolean isBigFeature() {
        return bigFeature;
    }

    public int getFeatureNumber() {
        return featureNum;
    }

    public int getSampleNumber() {
        return sampleNum;
    }

    public void setupDataSize(int featNum, int smplNum) {
        featureNum = featNum;
        sampleNum = smplNum;
    }

    public void updateFeatureNum(int featureNum) {
        this.featureNum = featureNum;
        bigFeature = featureNum >= 300;
    }

    public boolean isRoc1Col() {
        return roc1Col;
    }

    public void settingRoc1Col(int colNum) {
        roc1Col = colNum == 1;
    }

    public int getMaxtopNum() {
        return maxtopNum;
    }

    public void setMaxtopNum(int maxtopNum) {
        this.maxtopNum = maxtopNum;
    }

    private Map<String, Boolean> naviTrackStatus = new LinkedHashMap<>();

    public Map<String, Boolean> getNaviTrackStatus() {
        return naviTrackStatus;
    }

    public void setNaviTrackStatus(Map<String, Boolean> naviTrackStatus) {
        this.naviTrackStatus = naviTrackStatus;
    }

    public void addNaviTrack(String pageName, String naviCode, boolean success) {

        if (!naviTrack.containsKey(pageName)) {
            addNaviTrack(pageName, naviCode);
        }
        naviTrackStatus.put(pageName, success);

    }

    public void addNaviTrack(String pageName, String naviCode) {
        registerPage(pageName);
        currentNaviUrl = naviCode;

        boolean add = true;
        if (isOverviewPage()) {
            add = false;
        } else if (naviTrack.keySet().contains(pageName)) {
            if (naviTrack.get(pageName).equals(naviCode)) {
                add = false;
            }
        } else if (naviTrack.containsValue("/MetaboAnalyst" + naviCode)) {
            add = false;
        }

        if (add) {
            if (naviCode != null) {
                naviTrack.put(pageName, "/MetaboAnalyst" + naviCode);
            } else {
                naviTrack.put(pageName, "0");
            }
            naviTrackAnalType.put(pageName, getAnalType());
            naviTrackStatus.put(pageName, true);  // 🔹 Track success/failure

            wfv.addToWorkflow(naviCode);
        }

        if (pageName.equals("Upload")) {
            fbc.reloadUserInfo();
        } else {
            //force random mem cleaning for R session
            if (Math.random() < 0.5) {
                RCenter.cleanMemory(RC);
            }
        }
    }

    public void settingCurrentPage(String naviCode) {
        currentNaviUrl = naviCode;
    }

    public LinkedHashMap<String, String> getNaviTrack() {
        return naviTrack;
    }

    public String getOrg() {
        return org;
    }

    public void setOrg(String org) {
        this.org = org;
    }

    public HashMap<String, Integer> getImgMap() {
        return imgMap;
    }

    public void setImgMap(HashMap<String, Integer> imgMap) {
        this.imgMap = imgMap;
    }

    public String getPartialId() {
        return partialId;
    }

    public void setPartialId(String partialId) {
        this.partialId = partialId;
    }

    public String getNaviString() {
        return naviString;
    }

    public void setNaviString(String naviString) {
        this.naviString = naviString;
    }

    public UserLoginModel getCurrentLoginUser() {
        return currentLoginUser;
    }

    public void setCurrentLoginUser(UserLoginModel currentLoginUser) {
        this.currentLoginUser = currentLoginUser;
    }

    public boolean isRegisteredLogin() {
        return registeredLogin;
    }

    public void setRegisteredLogin(boolean registeredLogin) {
        this.registeredLogin = registeredLogin;
        if (registeredLogin) {
            FacesContext.getCurrentInstance().getExternalContext().getSessionMap().put("MA6_PRO_user", true);
        } else {
            FacesContext.getCurrentInstance().getExternalContext().getSessionMap().remove("MA6_PRO_user");
        }
    }

    public boolean isSaveEnabled() {
        return saveEnabled;
    }

    public void setSaveEnabled(boolean saveEnabled) {
        this.saveEnabled = saveEnabled;
    }

    public String getResetToken() {
        return resetToken;
    }

    public void setResetToken(String resetToken) {
        this.resetToken = resetToken;
    }

    public void visitHome() {
        if (registeredLogin) {
            PrimeFaces.current().executeScript("PF('logoutProjDialog').show()");
        } else {
            doLogout(1);
            DataUtils.doRedirect("/MetaboAnalyst/home.xhtml", ab);
        }
    }

    public boolean isDspcNet() {
        return dspcNet;
    }

    public void setDspcNet(boolean dspcNet) {
        this.dspcNet = dspcNet;
    }

    public List<ColorBean> getColorBeanLists() {
        if (colorBeanLists == null) {
            setupColorPicker();
        }
        return colorBeanLists;
    }

    public void setColorBeanLists(List<ColorBean> colorBeanLists) {
        this.colorBeanLists = colorBeanLists;
    }

    public SelectItem[] getMetaInfo() {
        if (metaInfo == null) {
            setupMetaInfo();
        }
        return metaInfo;
    }

    public void setupMetaInfo() {
        if (analType.equals("mf")) {
            String[] metas = RDataUtils.getMetaInfo(RC);
            //setSelectedMetas(new String[]{metaInfo[0]});
            metaInfo = new SelectItem[metas.length];
            String meta = "";
            for (int i = 0; i < metas.length; i++) {
                meta = meta + " " + metas[i];
                metaInfo[i] = new SelectItem(metas[i], metas[i]);
            }
            setExpFac(metas[0]);
        } else {
            metaInfo = new SelectItem[]{new SelectItem("", "Group")};
        }
    }

    public String getExpFac() {
        return expFac;
    }

    public void setExpFac(String expFac) {
        this.expFac = expFac;
        setupColorPicker();
    }

    public void setupColorPicker() {
        colorBeanLists = new ArrayList<>();
        String[] grpNms = RDataUtils.getGroupNames(RC, expFac);
        if (grpNms != null && grpNms.length > 0) {
            for (String grpNm : grpNms) {
                ColorBean cb = new ColorBean(grpNm);
                cb.setShapeType(0);
                colorBeanLists.add(cb);
            }
        }
    }

    public String getHeatmapType() {
        return heatmapType;
    }

    public void setHeatmapType(String heatmapType) {
        this.heatmapType = heatmapType;
    }

    public List<SampleBean> getSampleBeans() {
        if (sampleBeans == null) {
            sampleBeans = RDataUtils.createOrigSampleBeans(RC, "Class", false);
        }
        return sampleBeans;
    }

    public void setSampleBeans(List<SampleBean> sampleBeans) {
        this.sampleBeans = sampleBeans;
    }

    public String getBoxplotUrl() {
        return boxplotUrl;
    }

    public void setBoxplotUrl(String boxplotUrl) {
        this.boxplotUrl = boxplotUrl;
    }

    public boolean isJobDone() {
        return jobDone;
    }

    public void setJobDone(boolean jobDone) {
        this.jobDone = jobDone;
    }

    public boolean isPartialLinkValide() {
        return partialLinkValide;
    }

    public void setPartialLinkValide(boolean partialLinkValide) {
        this.partialLinkValide = partialLinkValide;
    }

    public String getPartialLinkCheckingRes() {
        if (partialLinkValide) {
            return "OK. Your job link is valid! Retrieving your job. <br/>Please wait .....";
        } else {
            return "<font style='color:darkorange'>Oops! Your job link is invalid or expired. <br/>Please double check.</font>";
        }
    }

    public String getTsDesign() {
        return tsDesign;
    }

    public void setTsDesign(String tsDesign) {
        this.tsDesign = tsDesign;
    }

    public boolean isTimeOnly() {
        return tsDesign.equals("time0");
    }

    public boolean isContainsTime() {
        return tsDesign.equals("time0") || tsDesign.equals("time");
    }

    public boolean showMultiBoxView() {
        if (analType.equals("mf")) {
            return tsDesign.equals("multi");
        } else {
            return false;
        }
    }

    public String getCmpdSummaryNm() {
        return cmpdSummaryNm;
    }

    public void setCmpdSummaryNm(String cmpdSummaryNm) {
        this.cmpdSummaryNm = cmpdSummaryNm;
    }

    public void viewCmpdSummary(String name) {
        UniVarTests.setCmpdSummaryType(getRConnection(), getCmpdSummaryType());
        if (getAnalType().equals("mf") && getTsDesign().equals("multi")) {
            mfb.setBoxId(name);
            mfb.updateBoxplotMeta();
            mfb.setBoxMetaVersionNum(mfb.getBoxMetaVersionNum() + 1);
        } else {
            cmpdSummaryNm = UniVarTests.plotCmpdSummary(this, name, "NA", "NA", imgCount, "png", 150 + "");
            //PrimeFaces.current().executeScript("PF('FeatureView').show();");
        }
        //System.out.println(name + "=======viewcmpd");
        setCurrentCmpdName(name);

        imgCount++;
    }

    public String getCmpdSummaryImg() {
        String imgNm = getCurrentCmpdName();
        if (imgNm == null) {
            return ab.getRootContext() + "/resources/images/background.png";
        }
        //imgNm = sb.getCurrentCmpdName().replaceAll("\\/", "_");
        return ab.getRootContext() + getCurrentUser().getRelativeDir() + File.separator + cmpdSummaryNm;
    }

    private String cmpdImgSize = "width:7.5in; height:4.875in;";

    public String getCmpdImgSize() {
        return cmpdImgSize;
    }

    public void setCmpdImgSize(String cmpdSummaryImgSize) {
        this.cmpdImgSize = cmpdSummaryImgSize;
    }

    public boolean isSwitchMode() {
        return switchMode;
    }

    public void setSwitchMode(boolean switchMode) {
        this.switchMode = switchMode;
    }

    public String computeDspcNet() {
        if (wb.isEditMode()) {
            addMessage("Info", "Parameters have been updated!");

            jrd.record_computeDspcNet();
            return null;
        }
        setDspcNet(true);
        int[] res = UniVarTests.computeDSPC(this);
        if (res.length == 1 & res[0] == 0) {
            addMessage("error", "DSPC failed - possible reason: some variables are highly collinear or sample size is too small.");
            return null;
        }

        setVisMode("dspc");
        jrd.record_computeDspcNet();
        return (mnb.setupDspcNetwork(res));
    }

    public boolean isShowResumable() {
        return showResumable;
    }

    public void setShowResumable(boolean showResumable) {
        this.showResumable = showResumable;
    }

    //performPeaks2Fun;plsPermBtn_action;
    public Semaphore getPermissionToStart() {

        Semaphore semaphore = resourceSemaphore.getSemaphore();
        try {
            if (semaphore.tryAcquire(ACQUIRE_TIMEOUT, TimeUnit.SECONDS)) {
                // code for success
                // The user should call semaphore.release() 
                //after complete the task,otherwise, only 10 permit every 10 minutes
                return semaphore;
            } else {
                addMessage("error", "Too many users are calling this function. Please try again later!");
            }
        } catch (InterruptedException e) {
            // code for timed-out
            addMessage("error", "Too many users are calling this function. Please try again later!");
        }
        return null;
    }

    public String getCurrentNaviUrl() {
        return currentNaviUrl;
    }

    public void setCurrentNaviUrl(String currentNaviUrl) {
        this.currentNaviUrl = currentNaviUrl;
    }

    public void setGrpNmOpts(SelectItem[] grpNmOpts) {
        this.grpNmOpts = grpNmOpts;
    }

    public SelectItem[] getGrpNmOpts() {
        if (grpNmOpts == null) {
            setupGrpNmOpts();
        }
        return grpNmOpts;
    }

    private SelectItem[] grpNmOpts = null;

    private void setupGrpNmOpts() {
        if (!isRegresion()) {
            String[] grpNms = RDataUtils.getPrenormGroupNames(RC);
            int grpLen = grpNms.length;
            grpNmOpts = new SelectItem[grpLen];
            for (int i = 0; i < grpLen; i++) {
                grpNmOpts[i] = new SelectItem(grpNms[i], grpNms[i]);
            }
        } else {
            grpNmOpts = new SelectItem[]{new SelectItem("NULL", "<Not set>")};
        }
    }

    public HashMap<String, String> getReportImgMap() {
        return reportImgMap;
    }

    public void setReportImgMap(HashMap<String, String> reportImgMap) {
        this.reportImgMap = reportImgMap;
    }

    public HashMap<String, String> getReportJsonMap() {
        return reportJsonMap;
    }

    public void setReportJsonMap(HashMap<String, String> reportJsonMap) {
        this.reportJsonMap = reportJsonMap;
    }

    public boolean doSoftLogin(String dataType, String analType, String userFolder, boolean isRegression, boolean paired) {
        if (!ab.isCompiled()) {
            if (!ab.compileRScripts(analType)) {
                addMessage("error", "Cannot connect to Rserver! Please start your Rserver with the right permission!");
                return false;
            }
        }

        if (currentUser != null) {
            if (RC != null) {
                RC.close();
            }
            currentUser = null;
            if (analType.equals("roc")) {
                FacesContext.getCurrentInstance().getExternalContext().getSessionMap().remove("rocAnalBean");
            }
        }

        if (analType.equals("raw") && ab.shouldUseScheduler()) {
            currentUser = DataUtils.createRawSpecUser(ab.getRaw_spec_folder());
        } else {
            currentUser = DataUtils.createTempUser(ab.getRealUserHomePath());
        }

        String myAnalType = analType;
        if (analType.equals("mummichog")) {
            myAnalType = dataType; //"mass_all" or "mass_table" for refined R function loading
        }

        RC = RCenter.getRConnection(currentUser.getHomeDir(), ab.getRscriptLoaderPath(), myAnalType);

        if (RC == null) {
            addMessage("Error", "Cannot connect to Rserve, please start your Rserver with the right permission!");
            return false;
        }

        performCleaning();

        //setDefaultTableView();
        this.setDataType(dataType);
        this.setAnalType(analType);
        this.setPaired(paired);
        this.setRegression(isRegression);

        if (naviTrack.keySet().size() > 2) {
            int count = 0;
            Iterator<String> iterator = naviTrack.keySet().iterator();
            while (iterator.hasNext()) {
                count++;
                iterator.next();
                if (count > 2) {
                    iterator.remove();
                }
            }
        }

        RDataUtils.initDataObjects(RC, dataType, analType, paired);

        if ("spec".equals(dataType)) {
            //check optilcms version
            String checkres = RDataUtils.checkOptiLCMS(RC, ab.getOptiLCMSversion());
            if (!checkres.equals("TRUE")) {
                System.out.println("MP Updating your local OPtiLCMS now....");
                int upres = RDataUtils.updateOptiLCMS(RC);
                System.out.println("MP Updating finished!" + upres);
            }

            //job scheduler init path for output files
            String path = "";
            if (ab.shouldUseScheduler()) {
                path = ab.getRaw_spec_folder();
            } else if (ab.isOnLocalServer()) {
                path = RDataUtils.getPathForScheduler(RC);
            }
            path = path + currentUser.getName() + "/";
            RDataUtils.setUserPathForScheduler(RC, path);
            RDataUtils.initDataPlan(RC); //record to R
        }

        this.setLoggedIn(true);

        RDataUtils.setPrimaryUser(RC);
        return true;
    }

    private boolean isBatchProject = false;

    public boolean isIsBatchProject() {
        if (analType.equals("mf")) {
            return false; // this is a special case for stats module [multi factor]. We will ask user to create batch project at the next step
        }
        return isBatchProject;
    }

    public boolean isIsMetaBatchProject() {
        return isBatchProject & analType.equals("mf");
    }

    public void setIsBatchProject(boolean isBatchProject) {
        this.isBatchProject = isBatchProject;
    }

    public String getShareToken() {
        return shareToken;
    }

    public void setShareToken(String shareToken) {
        this.shareToken = shareToken;
    }

    public String getCmpdSummaryType() {
        return cmpdSummaryType;
    }

    public void setCmpdSummaryType(String cmpdSummaryType) {
        this.cmpdSummaryType = cmpdSummaryType;
    }

    private int volcanoLabelNum = 5;

    public int getVolcanoLabelNum() {
        return volcanoLabelNum;
    }

    public void setVolcanoLabelNum(int volcanoLabelNum) {
        this.volcanoLabelNum = volcanoLabelNum;
    }

    public void performCleaning() {

        long elapse = (System.currentTimeMillis() - ab.getLastCleaningTime()) / 1000; //in seconds
        //call after 10 min break
        if (elapse > 600) {
            System.out.println("performCleaning??????=====");

            if (!ab.isCleaningOn()) {
                System.out.println("performCleaningOK=====");
                ab.performResourceCleaning(RC);
            }
        }
    }

    public void doSpectLoginDirect(ActionEvent event) {
        boolean res = true;
        if (!loggedIn) {
            res = doLogin("spec", "raw", false, false);
        }
        PrimeFaces.current().ajax().addCallbackParam("loggedIn", res + "");
    }

    public void goToResultPage() {
        if (!loggedIn) {
            addMessage("Error", "You need to start your analysis first!");
            return;
        }
        if (isWorkflowMode()) {
            if (dv.isWorkflowFinished()) {
                DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/xialabpro/DashboardView.xhtml", ab);
                return;
            } else {
                addMessage("Error", "Your result is not ready. If you have submmited job, please visit 'Workflow' page to check its status.");
                return;
            }
        }
        /*
        if (naviTrack.get("Upload").contains("/workflow")) {
            //System.out.println("===========" +naviTrack.get("Upload"));
            sb.addMessage("Error", "Your result is not ready. If you have submmited job, please visit 'Workflow' page to check its status.");
            return;
        }*/
        //for normal web-based analysis
        DataUtils.doRedirect("/" + ab.getAppName() + "/Secure/ResultView.xhtml", ab);

    }

    public boolean isWorkflowMode() {
        if (naviTrack != null) {
            if (naviTrack.keySet().contains("Upload")) {
                if (naviTrack.get("Upload").contains("/workflow")) {
                    return true;
                }
            }
        }
        return false;
    }

    public String getTemplatePath() {
        if (isWorkflowMode()) {
            return "/template/_template_workflow.xhtml";
        } else {
            return "/template/_template.xhtml";
        }
    }

    private ArrayList<String> notice = new ArrayList<>(); // Create an ArrayList object

    public ArrayList<String> getNotice() {
        return notice;
    }

    public int getNoticeSize() {
        return noticeSize;
    }

    public void setNotice(ArrayList<String> notice) {
        this.notice = notice;
    }

    private int noticeSize = 0; // only for warn and danger

    public void addMessage(String type, String msg) {
        String pre;
        if (type.equalsIgnoreCase("error")) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", msg));
            pre = "<font color='red'>[ERROR]: ";
            noticeSize = noticeSize + 1;
            PrimeFaces.current().ajax().update(":formBell");
            PrimeFaces.current().ajax().update(":errMessage");
        } else if (type.equalsIgnoreCase("warn")) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_WARN, "Warning", msg));
            pre = "<font color='orange'>[WARNING]: ";
            noticeSize = noticeSize + 1;
            PrimeFaces.current().ajax().update(":formBell");
        } else {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", msg));
            pre = "<font color='#4BB543'>[Success]: ";
        }

        notice.add(pre + msg + "</font>");
    }

    //relay center
    public void recordRCommandFunctionInfo(String rCmd, String functionName) {
        jrd.recordRCommandFunctionInfo(RC, rCmd, functionName);
    }

}
