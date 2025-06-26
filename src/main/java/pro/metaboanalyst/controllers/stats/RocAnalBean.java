/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.FeatureBean;
import pro.metaboanalyst.models.NameBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RocUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author jianguox
 */
@SessionScoped
@Named("rocAnalBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class RocAnalBean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;
    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(RocAnalBean.class);
    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;
    List<String> selectedSmpl1, selectedSmpl2;
    String rocDetailImg = null;
    String rocDetailBp = null;
    private int kmClustNm = 5;
    private boolean partialRoc = false;
    private String featText = "";
    private String univPerfOpt = "sp";
    private double univThresh = 0.2;
    private double rocCutOff = 0.2;
    private SelectItem[] mdlOpts = null;
    private SelectItem[] rocMdlOpts = null;
    private int rocMdlDD;
    private boolean showMisCls = false;
    //analysis mode : univ/explore/test
    private String analMode = "class";
    /**
     * a temp variable to store the current selected table row number this var
     * is shared by all the tables in the program since at any time only one
     * table can be selected upon; note the first row is 0; need to add 1 to
     * call R table row
     */
    private int tableRowInx;
    private ArrayList<FeatureBean> featureBeans;
    private NameBean[] smpl1Beans;
    private NameBean[] smpl2Beans;
    private String currentCmpd = null;
    private int lvNum = 2;
    private String clsMethodOpt = "svm";
    private String featRankOpt = "svm";
    private boolean testInit = false;
    private boolean univInit = false;
    private List<FeatureBean> selectedFeatureBeans;
    private List<FeatureBean> lassoFeatureBeans = new ArrayList();
    private DualListModel<String> sampleItems1, sampleItems2;
    private boolean smplHoldOut = false;
    private boolean showOptPoint = true;
    private boolean showCI = false;
    private String optimalDD = "closest.topleft";
    private String multPerfOpt = "fpr";
    //Multi ROC
    private boolean showConf = false;
    private boolean showConfLR = false;
    private int featNum = 15;
    private String rankMeasure = "freq";
    private String univROCBoxPlot;
    private String univROCImg;
    private String avgMtd = "threshold";
    private boolean showHoldOut = false;
    private boolean showPredCls = false;
    private String perfMeasure = "auroc";
    private int permNum = 0;
    private ArrayList<NameBean> smplPredBeans;
    private String downloadTxt = "";
    private ArrayList<FeatureBean> rocDetailsBeans;
    private String cutoff, sens, spec;
    private boolean canEdit = true;
    private int count = 0;
    private boolean featureRatioOptOut = false;

    public int getKmClustNm() {
        return kmClustNm;
    }

    public void setKmClustNm(int kmClustNm) {
        this.kmClustNm = kmClustNm;
    }

    public boolean isPartialRoc() {
        return partialRoc;
    }

    public void setPartialRoc(boolean partialRoc) {
        this.partialRoc = partialRoc;
    }

    public String getFeatText() {
        return featText;
    }

    public String getUnivPerfOpt() {
        return univPerfOpt;
    }

    public void setUnivPerfOpt(String univPerfOpt) {
        this.univPerfOpt = univPerfOpt;
    }

    public double getUnivThresh() {
        return univThresh;
    }

    public void setUnivThresh(double univThresh) {
        this.univThresh = univThresh;
    }

    public double getRocCutOff() {
        return rocCutOff;
    }

    public void setRocCutOff(double rocCutOff) {
        this.rocCutOff = rocCutOff;
    }

    public void setupMdlOptions() {
        String[] nms = RocUtils.getModelNames(sb.getRConnection());
        int nmLen = nms.length;
        mdlOpts = new SelectItem[nmLen + 1];
        mdlOpts[0] = new SelectItem(-1, "Best Model");
        rocMdlOpts = new SelectItem[nmLen + 1];
        rocMdlOpts[0] = new SelectItem(0, "Compare All Models");

        int curInx;
        for (int i = 0; i < nms.length; i++) {
            curInx = i + 1;
            mdlOpts[curInx] = new SelectItem(curInx, nms[i]);
            rocMdlOpts[curInx] = new SelectItem(curInx, nms[i]);
        }
        rocMdlDD = 0;
    }

    @JsonIgnore
    public int getBestModelIndex() {
        return RocUtils.getBestModelInx(sb.getRConnection());
    }

    public String toNextPage(String mode) {
        if (sb.isRoc1Col()) {
            sb.addMessage("Error", "Your data contains only one feature, and is not suitable for this analysis!");
            return null;
        }
        return mode;
    }

    public SelectItem[] getMdlOpts() {
        return mdlOpts;
    }

    public SelectItem[] getRocMdlOpts() {
        return rocMdlOpts;
    }

    public int getRocMdlDD() {
        return rocMdlDD;
    }

    public void setRocMdlDD(int rocMdlDD) {
        this.rocMdlDD = rocMdlDD;
    }

    public boolean isShowMisCls() {
        return showMisCls;
    }

    public void setShowMisCls(boolean showMisCls) {
        this.showMisCls = showMisCls;
    }

    public String getAnalMode() {
        return analMode;
    }

    public void setAnalMode(String analMode) {
        this.analMode = analMode;
    }

    //reset data beans after editing
    public void resetData() {
        featureBeans = null;
        sampleItems1 = null;
        univInit = false;
    }

    public int getTableRowInx() {
        return tableRowInx;
    }

    public void setTableRowInx(int tableRowInx) {
        this.tableRowInx = tableRowInx;
    }

    public ArrayList<FeatureBean> getFeatureBeans() {
        return featureBeans;
    }

    public void setFeatureBeans(ArrayList<FeatureBean> featureBeans) {
        this.featureBeans = featureBeans;
    }

    public NameBean[] getSmpl1Beans() {
        return smpl1Beans;
    }

    public void setSmpl1Beans(NameBean[] smplBeans) {
        this.smpl1Beans = smplBeans;
    }

    public NameBean[] getSmpl2Beans() {
        return smpl2Beans;
    }

    public void setSmpl2Beans(NameBean[] smplBeans) {
        this.smpl2Beans = smplBeans;
    }

    public void createSmplVec() {
        RConnection RC = sb.getRConnection();
        String[] names = RocUtils.getGrp1SampleNames(RC);
        smpl1Beans = new NameBean[names.length];
        for (int i = 0; i < smpl1Beans.length; i++) {
            smpl1Beans[i] = new NameBean(names[i]);
        }

        names = RocUtils.getGrp2SampleNames(RC);
        smpl2Beans = new NameBean[names.length];
        for (int i = 0; i < smpl2Beans.length; i++) {
            smpl2Beans[i] = new NameBean(names[i]);
        }
    }

    public String getCurrentCmpd() {
        return currentCmpd;
    }

    public void setCurrentCmpd(String currentCmpd) {
        this.currentCmpd = currentCmpd;
    }

    public int getLvNum() {
        return lvNum;
    }

    public void setLvNum(int lvlNum) {
        this.lvNum = lvlNum;
    }
    
    @JsonIgnore
    public String getNewSampleNames() {
        return RDataUtils.getNewSampleNames(sb.getRConnection());
    }

    public String getClsMethodOpt() {
        return clsMethodOpt;
    }

    public void setClsMethodOpt(String clsMethodOpt) {
        this.clsMethodOpt = clsMethodOpt;
    }

    public boolean getShowLRTap() {
        return (!this.clsMethodOpt.equals("lr"));
    }

    public String getFeatRankOpt() {
        return featRankOpt;
    }

    public void setFeatRankOpt(String featRankOpt) {
        this.featRankOpt = featRankOpt;
    }
    
    @JsonIgnore
    public String getClsMethodLabel() {
        if (featRankOpt.equals("svm")) {
            return "SVM";
        }
        if (featRankOpt.equals("rf")) {
            return "RandomForest";
        }
        if (featRankOpt.equals("pls")) {
            return "PLS-DA";
        }
        if (featRankOpt.equals("lr")) {
            return "Logistic Regression";
        }
        return null;
    }

    @JsonIgnore
    public String getFeatRankLabel() {
        if (featRankOpt.equals("svm")) {
            return "SVM built-in";
        }
        if (featRankOpt.equals("rf")) {
            return "RandomForest built-in";
        }
        if (featRankOpt.equals("pls")) {
            return "PLSDA built-in";
        }
        if (featRankOpt.equals("auroc")) {
            return "Univariate AUROC";
        }
        if (featRankOpt.equals("fisher")) {
            return "Fisher's Exact";
        }
        if (featRankOpt.equals("tt")) {
            return "T-statistics";
        }
        return null;
    }

    public String performExploreAnalysis() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_performExploreAnalysis(this);
            return null;
        }

        if (getFactor1().equals(getFactor2())) {
            sb.addMessage("Warn", "Both factors can not be the same.");
            return null;
        }

        setAnalMode("explore");
        RocUtils.setAnalysisMode(sb, analMode);
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCData(RC, selMeta, factor1, factor2);
        RocUtils.performRocCVExplorer(sb, clsMethodOpt, featRankOpt, lvNum);
        RocUtils.plotProbView(sb, sb.getNewImage("cls_prob"), "png", 150, -1, 0, 0);
        RocUtils.plotImpBiomarkers(sb, sb.getNewImage("cls_imp"), "png", 150, -1, "freq", 15);
        RocUtils.plotAccuracies(sb, sb.getNewImage("cls_accu"), "png", 150);
        RocUtils.plotROC(sb, sb.getNewImage("cls_roc"), "png", 150, 0, "threshold", 0, 0, "fpr", 0.5);
        setupMdlOptions();
        showConf = false;
        showMisCls = false;
        rankMeasure = "freq";
        jrd.record_performExploreAnalysis(this);
        wb.getCalledWorkflows().add("Multivariate ROC");
        return "Explorer";
    }

    public void setupFeatureTable(boolean init) {
        if (!init && getFactor1().equals(getFactor2())) {
            sb.addMessage("Warn", "Both factors can not be the same.");
            return;
        }
        setAnalMode("test");
        RocUtils.setAnalysisMode(sb, analMode);
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCData(RC, selMeta, factor1, factor2);
        testInit = false;
        if (!init) {
            featureBeans = null;
            sampleItems1 = null;
        }

        if (featureBeans == null) {
            RocUtils.computeUnivFeatureRanking(RC);
            prepareFeatureBean();
        }
        if (sampleItems1 == null) {
            prepareSampleBeans();
        }

        //sb.addNaviTrack("Builder", null);
    }

    public String performDefaultUnivAnalysis() {

        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return ""; // Skip ajax requests.
        }
        performDefaultUnivAnalysis_internal();
        return "Univariate";
    }

    public void performDefaultUnivAnalysis_update() {
        univInit = false;
        performDefaultUnivAnalysis_internal();
    }

    public void performDefaultUnivAnalysis_internal() {

        jrd.record_performDefaultUnivAnalysis_internal(this);
        if (wb.isEditMode()) {
            return;
        }

        if (getFactor1().equals(getFactor2())) {
            sb.addMessage("Warn", "Both factors can not be the same.");
            return;
        }
        setAnalMode("univ");
        RocUtils.setAnalysisMode(sb, analMode);
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCData(RC, selMeta, factor1, factor2);
        if (!univInit) {
            RocUtils.computeUnivFeatureRanking(RC);
            prepareFeatureBean();
            //sb.addNaviTrack("Univariate", null);
            univInit = true;
        }
    }

    private void prepareFeatureBean() {
        //set up ranking and feature beans
        RConnection RC = sb.getRConnection();
        String[] rownames = RocUtils.getUnivRankedFeatureNames(RC);
        double[][] sigmat = RocUtils.getUnivFeatureRankingMat(RC);

        //set up content
        if (rownames == null || rownames.length == 0) {
            return;
        }

        int totLen = rownames.length;

        //now set up the feature beans
        featureBeans = new ArrayList();
        FeatureBean fb;

        for (int i = 0; i < totLen; i++) {
            fb = new FeatureBean();
            fb.addName(rownames[i]);
            fb.setUniqID(i);
            for (int m = 0; m < 4; m++) {
                fb.addValue(sigmat[i][m]);
            }
            featureBeans.add(fb);
        }
    }

    public List<FeatureBean> getSelectedFeatureBeans() {
        return selectedFeatureBeans;
    }

    public void setSelectedFeatureBeans(List<FeatureBean> selectedFeatureBeans) {
        this.selectedFeatureBeans = selectedFeatureBeans;
    }

    public void computeFeatureBeans() {
        lassoFeatureBeans = new ArrayList();
        RConnection RC = sb.getRConnection();
        double[] freqs = RocUtils.getLassoFreqs(RC);
        String[] rownames = RocUtils.getLassoFreqNames(RC);
        int totLen = rownames.length;
        FeatureBean fb;
        for (int i = 0; i < totLen; i++) {
            fb = new FeatureBean();
            fb.addName(rownames[i]);
            fb.setUniqID(i);
            fb.addValue(freqs[i]);
            lassoFeatureBeans.add(fb);
        }
    }

    public List<FeatureBean> getLassoFeatureBeans() {
        return lassoFeatureBeans;
    }

    public boolean isSmplHoldOut() {
        return smplHoldOut;
    }

    public void setSmplHoldOut(boolean smplHoldOut) {
        this.smplHoldOut = smplHoldOut;
    }

    public DualListModel<String> getSampleItems1() {
        return sampleItems1;
    }

    public void setSampleItems1(DualListModel<String> sampleItems1) {
        this.sampleItems1 = sampleItems1;
    }

    public DualListModel<String> getSampleItems2() {
        return sampleItems2;
    }

    public void setSampleItems2(DualListModel<String> sampleItems2) {
        this.sampleItems2 = sampleItems2;
    }

    public void prepareSampleBeans() {
        RConnection RC = sb.getRConnection();
        String[] names1 = RocUtils.getGrp1SampleNames(RC);
        String[] names2 = RocUtils.getGrp2SampleNames(RC);
        sampleItems1 = new DualListModel(Arrays.asList(names1), new ArrayList());
        sampleItems2 = new DualListModel(Arrays.asList(names2), new ArrayList());
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public String prepareTesterData() {
        if (setupVarTester()) {
            if (setupSmplTester()) {
                testInit = false;
                return "Evaluator";
            }
            return null;
        }
        return null;
    }

    public List<String> getSelectedSmpl1() {
        return selectedSmpl1;
    }

    public List<String> getSelectedSmpl2() {
        return selectedSmpl2;
    }

    private boolean setupVarTester() {
        if (selectedFeatureBeans.isEmpty()) {
            sb.addMessage("Error", "No features were selected!");
            return false;
        }

        RConnection RC = sb.getRConnection();
        String nm = selectedFeatureBeans.get(0).getName();
        featText = nm;
        for (int i = 1; i < selectedFeatureBeans.size(); i++) {
            nm = nm + "\", \"" + selectedFeatureBeans.get(i).getName();
            featText = featText + ", " + selectedFeatureBeans.get(i).getName();
        }
        String cmd = "selected.cmpds <- c(\"" + nm + "\");";
        try {
            RCenter.recordRCommand(RC, cmd, true);
            RC.voidEval(cmd);
            sb.addMessage("info", "A total of " + count + " compounds are selected.");
            return true;
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("setupVarTester", e);
            sb.addMessage("Error", "Failed to set up selected features!");
            return false;
        }

    }

    private boolean setupSmplTester() {
        RConnection RC = sb.getRConnection();
        smplHoldOut = false;
        selectedSmpl1 = sampleItems1.getTarget();
        selectedSmpl2 = sampleItems2.getTarget();

        if (selectedSmpl1.isEmpty() && selectedSmpl2.isEmpty()) {
            String cmd = "selected.smpls <- c()";
            try {
                RCenter.recordRCommand(RC, cmd, true);
                RC.voidEval(cmd);
                sb.addMessage("info", "A total of 0 samples are selected.");
                return true;
            } catch (Exception e) {
                //e.printStackTrace();
                LOGGER.error("setupSmplTester", e);
                return false;
            }
        } else {
            if (selectedSmpl1.isEmpty()) {
                sb.addMessage("Error", "No samples are selected for group 1!");
                return false;
            }
            if (selectedSmpl2.isEmpty()) {
                sb.addMessage("Error", "No samples are selected for group 2!");
                return false;
            }
        }
        int smplCount = 0;
        String nm;
        String cmd = "selected.smpls <- c(\"";
        for (int i = 0; i < selectedSmpl1.size(); i++) {
            nm = selectedSmpl1.get(i);
            if (count == 0) {
                cmd = cmd + nm;
            } else {
                cmd = cmd + "\", \"" + nm;
            }
            smplCount++;
        }
        for (int m = 0; m < selectedSmpl2.size(); m++) {
            nm = selectedSmpl2.get(m);
            cmd = cmd + "\", \"" + nm;
            smplCount++;
        }
        cmd = cmd + "\"); ";
        try {
            RCenter.recordRCommand(RC, cmd, true);
            RC.voidEval(cmd);
            smplHoldOut = true;
            sb.addMessage("info", "A total of " + smplCount + " samples are selected.");
            return true;
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("setupSmplTester", e);
            return false;
        }
    }

    public boolean isShowOptPoint() {
        return showOptPoint;
    }

    public void setShowOptPoint(boolean showOptPoint) {
        this.showOptPoint = showOptPoint;
    }

    public boolean isShowCI() {
        return showCI;
    }

    public void setShowCI(boolean showCI) {
        this.showCI = showCI;
    }

    public String getOptimalDD() {
        return optimalDD;
    }

    public void setOptimalDD(String optimalDD) {
        this.optimalDD = optimalDD;
    }

    public void updateROC() {
        if (currentCmpd == null) {
            sb.addMessage("Error", "Please choose a compound first (clicking the corresponding View link)");
        } else {
            plotUnivROCSummary(currentCmpd);
        }
    }

    public String getMultPerfOpt() {
        return multPerfOpt;
    }

    public void setMultPerfOpt(String multPerfOpt) {
        this.multPerfOpt = multPerfOpt;
    }

    public boolean isShowConf() {
        return showConf;
    }

    public void setShowConf(boolean showConf) {
        this.showConf = showConf;
    }

    public boolean isShowConfLR() {
        return showConfLR;
    }

    public void setShowConfLR(boolean showConfLR) {
        this.showConfLR = showConfLR;
    }

    public void updateMultiROC() {
        RocUtils.plotROC(sb, sb.getNewImage("cls_roc"), "png", 150, rocMdlDD, "threshold", showConf ? 1 : 0, 0, multPerfOpt, rocCutOff);
    }

    public void updateProbView() {
        RocUtils.plotProbView(sb, sb.getNewImage("cls_prob"), "png", 150, rocMdlDD, showMisCls ? 1 : 0, 0);
    }
    
    @JsonIgnore
    public String getConfMat() {
        return (RocUtils.getConfusionMatrix(sb.getRConnection()));
    }
    
    @JsonIgnore
    public String getTestConfMat() {
        if (showPredCls) {
            return (RocUtils.getConfusionMatrixTest(sb.getRConnection()));
        } else {
            return (RocUtils.getConfusionMatrix(sb.getRConnection()));
        }
    }
    
    @JsonIgnore
    public String getAccuText() {
        return RocUtils.getAccuSummary(sb.getRConnection());
    }

    public int getFeatNum() {
        return featNum;
    }

    public void setFeatNum(int featNum) {
        this.featNum = featNum;
    }

    public String getRankMeasure() {
        return rankMeasure;
    }

    public void setRankMeasure(String rankMeasure) {
        this.rankMeasure = rankMeasure;
    }

    public void updateImpView() {
        RocUtils.plotImpBiomarkers(sb, sb.getNewImage("cls_imp"), "png", 150, rocMdlDD, rankMeasure, featNum);
        //PrimeFaces.current().scrollTo("ac:form4:impPane");
    }

    public void plotUnivROCSummary(String cmpdName) {

        currentCmpd = cmpdName;
        String isPartial = partialRoc ? "T" : "F";
        String measure = univPerfOpt;
        double mycutoff = univThresh;
        if (mycutoff < 0 || mycutoff > 1) {
            sb.addMessage("Error", "The threshold must be within [0, 1]!");
            return;
        }

        String isAUC = showCI ? "T" : "F";
        String isOpt = showOptPoint ? "T" : "F";
        String optMtd = optimalDD;

        univROCImg = RocUtils.performUnivROC(sb, cmpdName, count, "png", "150", isAUC, isOpt, optMtd, isPartial, measure, mycutoff);
        univROCBoxPlot = RocUtils.plotUnivROCBP(sb, cmpdName, count, "png", "150", isOpt, "FALSE");

        sb.getImgMap().put("roc_boxplot_" + cmpdName, count);
        sb.getImgMap().put("roc_univ_" + cmpdName, count);
        showCI = false;
        count++;
    }
    
    @JsonIgnore
    public String getRocUnivImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + univROCImg;
    }
    
    @JsonIgnore
    public String getRocUnivBPImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + univROCBoxPlot;
    }

    public void performTestAnalysis() {

        //   multivInit = false; // b/c overwrite the explorer, force re-generate
        RConnection RC = sb.getRConnection();

        int res = RocUtils.performRocCVTest(RC, clsMethodOpt, lvNum);

        if (res == 0) {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Failed to perform the analysis! " + err);
        }

        RocUtils.plotROCTest(sb, sb.getNewImage("cls_test_roc"), "png", 150, 0, avgMtd, 0, showHoldOut ? 1 : 0, "fpr", 0.5);
        RocUtils.plotProbViewTest(sb, sb.getNewImage("cls_test_prob"), "png", 150, -1, 0, 0);
        RocUtils.plotTestAccuracies(sb, sb.getNewImage("cls_test_accu"), "png", 150);
        setupMdlOptions();
        setupSamplePredTable();
        showConf = false;
        if (clsMethodOpt.equals("lr")) {
            showConfLR = false;
            RocUtils.plotROCLR(sb, sb.getNewImage("cls_roc_lr"), "png", 150, showConfLR ? 1 : 0);
        }
    }

    public String performDefaultTestAnalysis(boolean forceUpdate) {
        if (analMode.equals("explore")) {
            sb.addMessage("Error", "You need to access this page from Builder!");
            return null;
        }
        if (!testInit | forceUpdate) {
            RConnection RC = sb.getRConnection();
            RocUtils.setCustomData(RC);
            RocUtils.performRocCVTest(RC, clsMethodOpt, lvNum);
            RocUtils.plotROCTest(sb, sb.getCurrentImage("cls_test_roc"), "png", 150, 0, avgMtd, 0, 0, "fpr", 0.5);
            RocUtils.plotProbViewTest(sb, sb.getCurrentImage("cls_test_prob"), "png", 150, -1, 0, 0);
            RocUtils.plotTestAccuracies(sb, sb.getCurrentImage("cls_test_accu"), "png", 150);
            setupMdlOptions();
            setupSamplePredTable();
            //sb.addNaviTrack("Evaluator", null);
            testInit = true;
            showConf = false;
            showConfLR = false;
            //  multivInit = false; // b/c overwrite the explorer, force re-generate
            if (clsMethodOpt.equals("lr")) {
                RocUtils.plotROCLR(sb, sb.getNewImage("cls_roc_lr"), "png", 150, showConfLR ? 1 : 0);
            }
        }
        if (forceUpdate) {
            return "Builder";
        } else {
            return null;
        }
    }

    public String getAvgMtd() {
        return avgMtd;
    }

    public void setAvgMtd(String avgMtd) {
        this.avgMtd = avgMtd;
    }

    public boolean isShowHoldOut() {
        return showHoldOut;
    }

    public void setShowHoldOut(boolean showHoldOut) {
        this.showHoldOut = showHoldOut;
    }

    public String updateKmeans() {
        if (kmClustNm < 2) {
            sb.addMessage("Error", "A meaningful cluster number should be 2 or higher");
            return null;
        }
        RocUtils.updateKmeans(sb.getRConnection(), kmClustNm);
        prepareFeatureBean();
        return null;
    }

    public void updateTestRoc() {
        RocUtils.plotROCTest(sb, sb.getNewImage("cls_test_roc"), "png", 150, 0, avgMtd, showConf ? 1 : 0, showHoldOut ? 1 : 0, multPerfOpt, rocCutOff);
    }

    public void updateROCLRplot() {
        RocUtils.plotROCLR(sb, sb.getNewImage("cls_roc_lr"), "png", 150, showConfLR ? 1 : 0);
    }

    public boolean isShowPredCls() {
        return showPredCls;
    }

    public void setShowPredCls(boolean showPredCls) {
        this.showPredCls = showPredCls;
    }

    public void updateTestProbView() {
        RocUtils.plotProbViewTest(sb, sb.getNewImage("cls_test_prob"), "png", 150, rocMdlDD, showMisCls ? 1 : 0, showPredCls ? 1 : 0);
    }

    public String getPerfMeasure() {
        return perfMeasure;
    }

    public void setPerfMeasure(String perfMeasure) {
        this.perfMeasure = perfMeasure;
    }

    public int getPermNum() {
        return permNum;
    }

    public void setPermNum(int permNum) {
        this.permNum = permNum;
    }

    public String performROCpermutation() {
        if (permNum == 0) {
            sb.addMessage("error", "Please select a permutation number first!");
            return null;
        }
        int res = RocUtils.performPermut(sb.getRConnection(), perfMeasure, permNum);
        if (res == 1) {
            RocUtils.plotPermut(sb, sb.getNewImage("roc_perm"), "png", 150);
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", err);
        }
        return null;
    }

    public void setupSamplePredTable() {

        RConnection RC = sb.getRConnection();
        if (RocUtils.containNewSamples(RC) == 1) {
            String[] nms = RDataUtils.getNewSampleNameVec(RC);
            double[] probs = RDataUtils.getNewSampleProbs(RC);
            String[] grps = RDataUtils.getNewSampleGrps(RC);
            smplPredBeans = new ArrayList();
            for (int i = 0; i < nms.length; i++) {
                NameBean nb = new NameBean(nms[i]);
                nb.setProb(probs[i]);
                nb.setCls(grps[i]);
                smplPredBeans.add(nb);
            }
        }
    }

    public ArrayList<NameBean> getSmplPredBeans() {
        return smplPredBeans;
    }

    public void setSmplPredBeans(ArrayList<NameBean> smplPredBeans) {
        this.smplPredBeans = smplPredBeans;
    }
    
    @JsonIgnore
    public String getLRConvergence() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRConvergence(sb.getRConnection()));
        } else {
            return (null);
        }
    }
    
    @JsonIgnore
    public String getLREquation() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLREquation(sb.getRConnection()));
        } else {
            return (null);
        }
    }
    
    @JsonIgnore
    public String getLRmodelTable() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRmodelTable(sb.getRConnection()));
        } else {
            return (null);
        }
    }
    
    @JsonIgnore
    public String getLRperformTable() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRperformanceTable(sb.getRConnection()));
        } else {
            return (null);
        }
    }
    
    @JsonIgnore
    public String getLRclsLabel() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRclsLabel(sb.getRConnection()));
        } else {
            return (null);
        }
    }
    
    @JsonIgnore
    public String getLRclsLabelNew() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRclsLabelNew(sb.getRConnection()));
        } else {
            return (null);
        }
    }
    
    @JsonIgnore
    public String getLRthreshold() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRthreshold(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    // private boolean univInit = false;
    public String performDefaultDetailRocAnalysis(String cmpd) {
        if (currentCmpd == null) {
            plotUnivROCSummary(cmpd);
        }
        if (!currentCmpd.equals(cmpd)) {
            //need to reset count for new cmpds
            count = 0;
            plotUnivROCSummary(cmpd);
        }
        prepareDetailROCBeans();
        return ("ROC detail");
    }

    private void prepareDetailROCBeans() {
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCDetails(RC, currentCmpd);

        double[][] sigmat = RocUtils.getRocValues(RC);

        //set up content
        if (sigmat == null || sigmat.length == 0) {
            return;
        }
        //now set up the feature beans
        rocDetailsBeans = new ArrayList();
        FeatureBean fb;

        for (int i = 0; i < sigmat.length; i++) {
            fb = new FeatureBean();
            fb.addName(sigmat[i][0] + "");
            fb.setUniqID(i);
            for (int m = 1; m < 6; m++) { //total six columns
                fb.addValue(sigmat[i][m]);
            }
            rocDetailsBeans.add(fb);
        }
        String cmpdName = currentCmpd.replaceAll("\\/", "_");
        downloadTxt = "<b>You can download the result table:</b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/" + cmpdName + "_roc.csv\"><b>" + "here" + "</b></a>";
    }

    public String getDownloadTxt() {
        return downloadTxt;
    }

    public void setDownloadTxt(String downloadTxt) {
        this.downloadTxt = downloadTxt;
    }

    public ArrayList<FeatureBean> getRocDetailsBeans() {
        return rocDetailsBeans;
    }

    public void setRocDetailsBeans(ArrayList<FeatureBean> rocDetailsBeans) {
        this.rocDetailsBeans = rocDetailsBeans;
    }

    public String getCutoff() {
        return cutoff;
    }

    public void setCutoff(String cutoff) {
        this.cutoff = cutoff;
    }

    public String getSens() {
        return sens;
    }

    public void setSens(String sens) {
        this.sens = sens;
    }

    public String getSpec() {
        return spec;
    }

    public void setSpec(String spec) {
        this.spec = spec;
    }

    public boolean isCanEdit() {
        return canEdit;
    }

    public void setCanEdit(boolean canEdit) {
        this.canEdit = canEdit;
    }

    public void threshBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.

        String fld;
        double val;
        if (cutoff != null && cutoff.replaceAll("\\s+$", "").length() > 0) {
            fld = "threshold";
            val = Double.parseDouble(cutoff);
        } else if (sens != null && sens.replaceAll("\\s+$", "").length() > 0) {
            fld = "sensitivity";
            val = Double.parseDouble(sens);
            if (val < 0 || val > 1) {
                sb.addMessage("Error", "Value must be between [0 1]");
                return;
            }
        } else if (spec != null && spec.replaceAll("\\s+$", "").length() > 0) {
            fld = "specificity";
            val = Double.parseDouble(spec);
            if (val < 0 || val > 1) {
                sb.addMessage("Error", "Value must be between [0 1]");
                return;
            }
        } else {
            sb.addMessage("Error", "No meaningful values were detected");
            return;
        }
        count++;
        String myImg = currentCmpd.replaceAll("\\/", "_") + "_" + count;
        String[] res = RocUtils.getROCcoords(sb.getRConnection(), fld, val, "TRUE", myImg);
        rocDetailImg = myImg + "_dpi150.png";
        rocDetailBp = RocUtils.plotUnivROCBP(sb, currentCmpd, count, "png", "150", "FALSE", "TRUE");

        cutoff = res[0];
        spec = res[1];
        sens = res[2];
        canEdit = false;
    }

    public void resetBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        cutoff = "";
        spec = "";
        sens = "";
        canEdit = true;
    }

    @JsonIgnore
    public String getProbDownloadLink() {
        return "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName() + "/roc_pred_prob.csv\"><b>" + "Details" + "</b></a>";
    }
    
    @JsonIgnore
    public String getProbDownloadLink2() {
        return "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName() + "/roc_pred_prob1.csv\"><b>" + "Details" + "</b></a>";
    }

    @JsonIgnore
    public String getRocDetailImg() {
        //System.out.println("univROCImg======" + univROCImg);
        if (rocDetailImg == null) {
            System.out.println("univROCImg======" + rocDetailImg);

            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + univROCImg;
        }
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + rocDetailImg;
    }

    @JsonIgnore
    public String getRocBPDetailImg() {
        if (rocDetailBp == null) {
            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + univROCBoxPlot;
        }
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + rocDetailBp;
    }

    private int featureNum = -1;
    
    @JsonIgnore
    public boolean isFeatureRatioOptOut() {
        if (featureNum == -1) {
            featureNum = RDataUtils.getFeatureNum(sb.getRConnection());
        }
        if (featureNum > 1000) {
            featureRatioOptOut = true;
            return featureRatioOptOut;
        } else {
            return false;
        }

    }

    public void setFeatureRatioOptOut(boolean featureRatioOptOut) {
        this.featureRatioOptOut = featureRatioOptOut;
    }

    //metadata selection
    private String selMeta = "NA";
    private String factor1 = "NA";
    private String factor2 = "NA";
    private String[] grps;

    public void analysisMetaChangeListener() {
        grps = RDataUtils.getMetaDataCol(sb.getRConnection(), selMeta);
        List<SelectItem> list = new ArrayList<>();

        for (String grp : grps) {
            list.add(new SelectItem(grp, grp));
        }

        mfb.setUniqueMetaList(list);
    }

    public String[] getGrps() {
        return grps;
    }

    public void setGrps(String[] grps) {
        this.grps = grps;
    }

    public String getSelMeta() {

        return selMeta;
    }

    public void setSelMeta(String selMeta) {
        this.selMeta = selMeta;
    }
    
    @JsonIgnore
    public String getFactor1() {
        if (factor1.equals("NA")) {
            factor1 = mfb.getUniqueMetaList().get(0).getValue().toString();
        }
        return factor1;
    }

    public void setFactor1(String factor1) {
        this.factor1 = factor1;
    }
    
    @JsonIgnore
    public String getFactor2() {
        if (factor2.equals("NA")) {
            factor2 = "all";
        }
        return factor2;
    }

    public void setFactor2(String factor2) {
        this.factor2 = factor2;
    }

}
