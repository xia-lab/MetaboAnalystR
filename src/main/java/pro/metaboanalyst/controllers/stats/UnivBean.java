/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import jakarta.enterprise.context.RequestScoped;
import java.io.File;
import java.io.Serializable;
import jakarta.inject.Named;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.DetailsBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.model.DefaultStreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("univBean")
public class UnivBean implements Serializable {

    @Inject
    private ApplicationBean1 ab;

    @Inject
    private SessionBean1 sb;

    @Inject
    private WorkflowBean wb;

    private String pairedFcAnal = "FALSE";

    public String getPairedFcAnal() {
        return pairedFcAnal;
    }

    public void setPairedFcAnal(String pairedAnal) {
        this.pairedFcAnal = pairedAnal;
    }

    private SelectItem[] cmpOpts = null;

    public SelectItem[] getCmpOpts() {
        if (cmpOpts == null) {
            String[] grpNms = RDataUtils.getNormGroupNames(sb.getRConnection());
            int grpLen = grpNms.length;
            if (grpLen == 2) {
                cmpOpts = new SelectItem[grpLen];
                cmpOpts[0] = new SelectItem(0, grpNms[0] + "/" + grpNms[1]);
                cmpOpts[1] = new SelectItem(1, grpNms[1] + "/" + grpNms[0]);
            } else {
                cmpOpts = new SelectItem[]{new SelectItem("NULL", "<Not set>")};
            }
        }
        return cmpOpts;
    }

    private int cmpType = 0;

    public int getCmpType() {
        return cmpType;
    }

    public void setCmpType(int cmpType) {
        this.cmpType = cmpType;
    }

    private String viewOpt = "overview";

    public String getViewOpt() {
        return viewOpt;
    }

    public void setViewOpt(String viewOpt) {
        this.viewOpt = viewOpt;
    }

    private double fcThresh = 2;
    private int plotLbl = 1;

    public int getPlotLbl() {
        return plotLbl;
    }

    public void setPlotLbl(int plotLbl) {
        this.plotLbl = plotLbl;
    }

    private int plotTheme = 0;

    public int getPlotTheme() {
        return plotTheme;
    }

    public void setPlotTheme(int plotTheme) {
        this.plotTheme = plotTheme;
    }

    public double getFcThresh() {
        return fcThresh;
    }

    public void setFcThresh(double fcThresh) {
        this.fcThresh = fcThresh;
    }

    public String fcButton_action() {

        String paired = pairedFcAnal.equals("TRUE") ? "TRUE" : "FALSE";
        UniVarTests.initFC(sb, fcThresh, cmpType, paired);

        UniVarTests.plotFC(sb, sb.getNewImage("fc"), "png", 72);
        DetailsBean db = (DetailsBean) DataUtils.findBean("detailsBean");
        db.update3CompModel("fc");

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_fcButton_action(ub);
        return null;
    }

    private double corrThresh = 0.0;

    public double getCorrThresh() {
        return corrThresh;
    }

    public void setCorrThresh(double corrThresh) {
        this.corrThresh = corrThresh;
    }

    private int fontSize = 6;

    public int getFontSize() {
        return fontSize;
    }

    public void setFontSize(int fontSize) {
        this.fontSize = fontSize;
    }

    private int unit = 10;

    public int getUnit() {
        return unit;
    }

    public void setUnit(int unit) {
        this.unit = unit;
    }

    private double ttPThresh = 0.05;

    public double getTtPThresh() {
        return ttPThresh;
    }

    public void setTtPThresh(double ttPThresh) {
        this.ttPThresh = ttPThresh;
    }

    private String pairedTtAnal = "FALSE";

    public String getPairedTtAnal() {
        return pairedTtAnal;
    }

    public void setPairedTtAnal(String pairedAnal) {
        this.pairedTtAnal = pairedAnal;
    }

    private String equalVar = "TRUE";

    public String getEqualVar() {
        return equalVar;
    }

    public void setEqualVar(String equalVar) {
        this.equalVar = equalVar;
    }

    private boolean nonParTt = false;

    public boolean isNonParTt() {
        return nonParTt;
    }

    public void setNonParTt(boolean nonParTt) {
        this.nonParTt = nonParTt;
    }

    private String ttPvalType = "fdr";

    public String getTtPvalType() {
        return ttPvalType;
    }

    public void setTtPvalType(String ttPvalType) {
        this.ttPvalType = ttPvalType;
    }

    public void ttButton_action() {
        //double thresh = Double.parseDouble(ttPThresh);
        RConnection RC = sb.getRConnection();
        String nonpar = "F";
        if (nonParTt) {
            nonpar = "T";
        }

        int res = UniVarTests.performTtests(sb, nonpar, ttPThresh, pairedTtAnal, equalVar, ttPvalType);
        if (res == 0) {
            sb.setTtSig(false);
        } else {
            sb.setTtSig(true);
        }
        DetailsBean db = (DetailsBean) DataUtils.findBean("detailsBean");
        db.update2CompModel("tt");
        UniVarTests.plotTT(sb, sb.getNewImage("tt"), "png", 72);
        sb.addMessage("info", RDataUtils.getCurrentMsg(RC));

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_ttButton_action(ub);
    }

    private String corDirection = "col";

    public String getCorDirection() {
        return corDirection;
    }

    public void setCorDirection(String corDirection) {
        this.corDirection = corDirection;
    }

    private String pairedVC = "FALSE";

    public String getPairedVC() {
        return pairedVC;
    }

    public void setPairedVC(String pairedVC) {
        this.pairedVC = pairedVC;
    }

    private double vcPThresh = 0.1;
    private double vcFcThresh = 2;

    public double getVcPThresh() {
        return vcPThresh;
    }

    public void setVcPThresh(double vcPThresh) {
        this.vcPThresh = vcPThresh;
    }

    public double getVcFcThresh() {
        return vcFcThresh;
    }

    public void setVcFcThresh(double vcFcThresh) {
        this.vcFcThresh = vcFcThresh;
    }

    private boolean nonParVcTt = false;

    public boolean isNonParVcTt() {
        return nonParVcTt;
    }

    public void setNonParVcTt(boolean nonParVcTt) {
        this.nonParVcTt = nonParVcTt;
    }

    private SelectItem[] normVarNmOpts = null;

    public SelectItem[] getNormVarNmOpts() {
        if (normVarNmOpts == null) {
            String[] varNms = RDataUtils.getNormFeatureNames(sb.getRConnection());
            int colLen = varNms.length;
            SelectItem[] varOpts = new SelectItem[colLen];
            for (int i = 0; i < colLen; i++) {
                varOpts[i] = new SelectItem(varNms[i], varNms[i]);
            }
            normVarNmOpts = varOpts;
        }
        return normVarNmOpts;
    }

    public boolean isVcPair() {
        return !pairedVC.equals("FALSE");
    }

    private String labelOpt = "all";

    public String getLabelOpt() {
        return labelOpt;
    }

    public void setLabelOpt(String labelOpt) {
        this.labelOpt = labelOpt;
    }

    public String vcButton_action() {
        String nonpar = "F";
        if (nonParVcTt) {
            nonpar = "T";
        }
        if (vcFcThresh < 1) {
            sb.addMessage("Error", "Cannot be smaller than 1. The method will consider both directions for the given FC cutoff. You can update "
                    + "Direction of Comparison to indicate what is considered up and down");
            return null;
        }
        UniVarTests.performVolcano(sb, pairedVC, vcFcThresh, cmpType, nonpar, vcPThresh, equalVar, vcPvalType);
        //if (pairedVC.equals("FALSE")) {
        DetailsBean db = (DetailsBean) DataUtils.findBean("detailsBean");
        db.update3CompModel("volcano");
        //}
        //System.out.println(labelOpt + "======labelOpt");
        if (labelOpt.equals("all")) {
            UniVarTests.plotVolcano(sb, sb.getNewImage("volcano"), plotLbl, plotTheme, "png", 72, -1);
        } else {
            UniVarTests.plotVolcano(sb, sb.getNewImage("volcano"), plotLbl, plotTheme, "png", 72, sb.getVolcanoLabelNum());
        }
        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_vcButton_action(ub);
        return null;
    }

    private String aovPThresh = "0.05";

    public String getAovPThresh() {
        return aovPThresh;
    }

    public void setAovPThresh(String aovPThresh) {
        this.aovPThresh = aovPThresh;
    }

    private String posthocThresh = "0.05";

    public String getPosthocThresh() {
        return posthocThresh;
    }

    public void setPosthocThresh(String posthocThrehs) {
        this.posthocThresh = posthocThrehs;
    }

    private boolean nonParam = false;

    public boolean isNonParam() {
        return nonParam;
    }

    public void setNonParam(boolean nonParam) {
        this.nonParam = nonParam;
    }

    private String posthocType = "fisher";

    public String getPosthocType() {
        return posthocType;
    }

    public void setPosthocType(String posthocType) {
        this.posthocType = posthocType;
    }

    public void aovButton_action() {
        // TODO: Replace with your code
        double sigThresh = Double.parseDouble(aovPThresh);
        String nonPar = "F";
        if (nonParam) {
            nonPar = "T";
        }
        int res = UniVarTests.performANOVA(sb, nonPar, sigThresh);
        if (res == 0) {
            sb.setAnovaSig(false);
        } else {
            sb.setAnovaSig(true);
        }
        DetailsBean db = (DetailsBean) DataUtils.findBean("detailsBean");
        db.update2CompModel("aov");
        UniVarTests.plotAOV(sb, sb.getNewImage("aov"), "png", 72);
        sb.addMessage("info", RDataUtils.getCurrentMsg(sb.getRConnection()));

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_aovButton_action(ub);
    }

    private boolean fixRange = false;

    public boolean isFixRange() {
        return fixRange;
    }

    public void setFixRange(boolean fixRange) {
        this.fixRange = fixRange;
    }

    private String vcPvalType = "raw";

    public String getVcPvalType() {
        return vcPvalType;
    }

    public void setVcPvalType(String volcanoPvalType) {
        this.vcPvalType = volcanoPvalType;
    }

    private boolean noClust = false;

    public boolean isNoClust() {
        return noClust;
    }

    public void setNoClust(boolean noClust) {
        this.noClust = noClust;
    }

    private String hmDistMeasure = "pearson";

    public String getHmDistMeasure() {
        return hmDistMeasure;
    }

    public void setHmDistMeasure(String hmDistMeasure) {
        this.hmDistMeasure = hmDistMeasure;
    }

    private String colContrast = "bwm";

    public String getColContrast() {
        return colContrast;
    }

    public void setColContrast(String colContrast) {
        this.colContrast = colContrast;
    }

    public DefaultStreamedContent getCorrPvalFile() {

        int res = UniVarTests.computeCorrP(sb);
        if (res == 1) {
            return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/pval_corr_table.csv");
        } else {
            sb.addMessage("Error", "Failed to compute the request.");
            return null;
        }
    }

    public DefaultStreamedContent getCorrValFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/correlation_table.csv");
    }

    public void corrBtn_action() {

        String fix = fixRange ? "T" : "F";
        String clst = noClust ? "T" : "F";
        //String top = topCB.isChecked() ? "T" : "F";
        //int topNum= Integer.parseInt(topFld.getText().toString());
        UniVarTests.plotCorrHeatMap(sb, sb.getNewImage("corr"), "png", 72, corDirection, hmDistMeasure, colContrast, fix, clst, fontSize, unit, corrThresh);
        //PrimeFaces.current().scrollTo("form1:corrPane");

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_corrBtn_action(ub);
        wb.getCalledWorkflows().add("Correlation Heatmap");

    }

    private String ptnDistMeasure = "pearson";

    public String getPtnDistMeasure() {
        return ptnDistMeasure;
    }

    public void setPtnDistMeasure(String ptnDistMeasure) {
        this.ptnDistMeasure = ptnDistMeasure;
    }

    private String ptnFeature = "";

    public String getPtnFeature() {
        if (ptnFeature.equals("")) {
            ptnFeature = getNormVarNmOpts()[0].getValue().toString();
        }
        return ptnFeature;
    }

    public void setPtnFeature(String ptnFeature) {
        this.ptnFeature = ptnFeature;
    }

    private String ptnTemplate;

    public String getPtnTemplate() {
        return ptnTemplate;
    }

    public void setPtnTemplate(String ptnTemplate) {
        this.ptnTemplate = ptnTemplate;
    }

    public SelectItem[] getTemplatePtnOpts() {
        String[] templates = UniVarTests.getTempateNames(sb);
        SelectItem[] opts;
        if (templates == null) {
            opts = new SelectItem[1];
            sb.addMessage("Error", "Failed to generate the templates based on your data.");
            opts[0] = new SelectItem("na", "-- Not Available --");
        } else {
            opts = new SelectItem[templates.length];
            opts[0] = new SelectItem("na", templates[0]);
            for (int i = 1; i < templates.length; i++) {
                opts[i] = new SelectItem(templates[i], templates[i]);
            }
        }
        return opts;
    }

    private String ptnType = "featptn";

    public String getPtnType() {
        return ptnType;
    }

    public void setPtnType(String ptnType) {
        this.ptnType = ptnType;
    }

    private String usrPtn;

    public String getUsrPtn() {
        return usrPtn;
    }

    public void setUsrPtn(String usrPtn) {
        this.usrPtn = usrPtn;
    }

    public String ptnBtn_action() {
        String imgName = sb.getNewImage("ptn");
        RConnection RC = sb.getRConnection();

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_ptnBtn_action(ub);
        switch (ptnType) {
            case "featptn" -> {
                if (UniVarTests.matchFeaturePattern(sb, ptnDistMeasure, ptnFeature)) {
                    UniVarTests.plotMatchedFeatures(sb, imgName, "feature", "png", 72);
                    wb.getCalledWorkflows().add("Pattern Search");

                } else {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    wb.getCalledWorkflowsError().add("Pattern Search");

                    return null;
                }
            }
            case "preptn" -> {
                if (ptnTemplate.equals("na")) {
                    sb.addMessage("Error", "The first item is group label!");
                    wb.getCalledWorkflowsError().add("Pattern Search");

                    return null;
                } else if (UniVarTests.matchPattern(sb, ptnDistMeasure, ptnTemplate)) {
                    UniVarTests.plotMatchedFeatures(sb, imgName, "feature", "png", 72);
                    wb.getCalledWorkflows().add("Pattern Search");

                } else {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    wb.getCalledWorkflowsError().add("Pattern Search");

                    return null;
                }
            }
            default -> {
                //self defined
                if (usrPtn == null || usrPtn.trim().length() == 0) {
                    sb.addMessage("Error", "Please define a pattern first!");
                    wb.getCalledWorkflowsError().add("Pattern Search");
                    return null;
                }
                if (UniVarTests.matchPattern(sb, ptnDistMeasure, usrPtn)) {
                    UniVarTests.plotMatchedFeatures(sb, imgName, "feature", "png", 72);
                    wb.getCalledWorkflows().add("Pattern Search");

                } else {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    wb.getCalledWorkflowsError().add("Pattern Search");

                    return null;
                }
            }
        }
        PrimeFaces.current().scrollTo("form2:ptnPane");
        return null;
    }

    public void setupVolcano() throws Exception {
        if (!sb.isAnalInit("Volcano plot")) {
            sb.addNaviTrack("Volcano", "/Secure/analysis/VolcanoView.xhtml");
            UniVarTests.performVolcano(sb, "FALSE", 2, 0, "F", 0.1, "TRUE", "raw");
            UniVarTests.plotVolcano(sb, sb.getCurrentImage("volcano"), 1, 0, "png", 72, -1);
        }

        wb.getCalledWorkflows().add("Volcano");
        DetailsBean db = (DetailsBean) DataUtils.findBean("detailsBean");
        db.update3CompModel("volcano");
    }

    public void generateUnivReport() {
        UniVarTests.computeUnivReport(sb.getRConnection());
    }

    public String getCurrentUserRelativeDir(String downloadFileName) {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + downloadFileName;
    }

    private final int maxPostHoc = 2000;

    public String doPostHocTest() {
        double thresh = Double.parseDouble(posthocThresh);
        UniVarTests.performAnovaPostHoc(sb, posthocType, thresh, maxPostHoc);
        return sb.detailsLnk_action("anova");
    }

    public void doDefaultStaticCorrelation() {

        UniVarTests.plotStaticCorrHeatMap(sb, sb.getCurrentImage("corr_heatmap"), "png", 72, "col", "pearson", "bwm", "overview", "F", "F", 0.0);
    }
}
