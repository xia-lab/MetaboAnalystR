/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RMetaPathUtils;
import pro.metaboanalyst.utils.DataUtils;
import java.beans.ConstructorProperties;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import pro.metaboanalyst.controllers.general.SessionBean1;
import software.xdev.chartjs.model.charts.PieChart;
import software.xdev.chartjs.model.color.RGBAColor;
import software.xdev.chartjs.model.data.PieData;
import software.xdev.chartjs.model.dataset.PieDataset;

/**
 * @author qiang
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class MetaPathModel {

    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(MetaPathModel.class);

    @JsonIgnore
    ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");

    @JsonIgnore
    SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    //Section I ----> variables, getter and setter of this model <-------
    @JsonIgnore
    private RConnection RC;
    private String name;
    private String name2;
    private String dataName;
    private String groupInfo;
    private int smplNum;
    private int geneNum;
    private int smplNum2;
    private int geneNum2;
    private int modeNum = 1;
    private boolean uploaded = false;
    private boolean processed = false; // normalization
    private boolean normed = false;
    private boolean analDone = false;
    private boolean allDone = false;
    private boolean include = false;
    private boolean autoscaleOpt = false;
    private boolean disabledModify = false;

    private double sigLevel = 0.05;
    private double ppm = 10;
    private String species = "hsa";
    private String MumRT = "seconds";
    private String ionMode = "NA";
    private String dataformat = "colu";
    private String[] adducts = null;
    private String diapos = "center center";
    private String diapos2 = "center center";
    private int deNum;
    private boolean vennInclude = false;
    private String missingInfo;
    private String normOpt = "logMed";
    private String msgText;
    @JsonIgnore
    private String pieModel;

    // Section IV --------> adducts customization <--------
    private DualListModel<String> adductItems = new DualListModel(Arrays.asList("Unknown"), Arrays.asList("Unknowns"));

    @JsonCreator
    @ConstructorProperties({"name", "name2", "dataName", "groupInfo", "smplNum", "geneNum", "smplNum2", "geneNum2", "modeNum",
        "uploaded", "processed", "normed", "analDone", "allDone", "include", "autoscaleOpt", "disabledModify",
        "sigLevel", "ppm", "species", "MumRT", "ionMode", "dataformat", "adducts", "diapos", "diapos2",
        "deNum", "vennInclude", "missingInfo", "normOpt", "msgText"})
    public MetaPathModel(String name, String name2, String dataName, String groupInfo, int smplNum, int geneNum, int smplNum2, int geneNum2, int modeNum,
            boolean uploaded, boolean processed, boolean normed, boolean analDone, boolean allDone, boolean include, boolean autoscaleOpt, boolean disabledModify,
            double sigLevel, double ppm, String species, String MumRT, String ionMode, String dataformat, String[] adducts, String diapos, String diapos2,
            int deNum, boolean vennInclude, String missingInfo, String normOpt, String msgText) {
        this.name = name;
        this.name2 = name2;
        this.dataName = dataName;
        this.groupInfo = groupInfo;
        this.smplNum = smplNum;
        this.geneNum = geneNum;
        this.smplNum2 = smplNum2;
        this.geneNum2 = geneNum2;
        this.modeNum = modeNum;
        this.uploaded = uploaded;
        this.processed = processed;
        this.normed = normed;
        this.analDone = analDone;
        this.allDone = allDone;
        this.include = include;
        this.autoscaleOpt = autoscaleOpt;
        this.disabledModify = disabledModify;
        this.sigLevel = sigLevel;
        this.ppm = ppm;
        this.species = species;
        this.MumRT = MumRT;
        this.ionMode = ionMode;
        this.dataformat = dataformat;
        this.adducts = adducts;
        this.diapos = diapos;
        this.diapos2 = diapos2;
        this.deNum = deNum;
        this.vennInclude = vennInclude;
        this.missingInfo = missingInfo;
        this.normOpt = normOpt;
        this.msgText = msgText;
    }

    public String getDiapos() {
        return diapos;
    }

    public String getDiapos2() {
        return diapos2;
    }

    public String getDataformat() {
        return dataformat;
    }

    public void setDataformat(String dataformat) {
        this.dataformat = dataformat;
    }

    public int getModeNum() {
        if (name2 != null) {
            modeNum = 2;
        }
        return modeNum;
    }

    public String getName2() {
        return name2;
    }

    public void setName2(String name2) {
        this.name2 = name2;
    }

    public double getPpm() {
        return ppm;
    }

    public void setPpm(double ppm) {
        this.ppm = ppm;
    }

    public String getSpecies() {
        return species;
    }

    public void setSpecies(String species) {
        this.species = species;
    }

    public String getMumRT() {
        return MumRT;
    }

    public void setMumRT(String MumRT) {
        this.MumRT = MumRT;
    }

    public RConnection getRC() {
        return RC;
    }

    public void setRC(RConnection RC) {
        this.RC = RC;
    }

    public int getSmplNum() {
        if (name2 != null) {
            int smplNumTotal = smplNum + smplNum2;
            return smplNumTotal;
        } else {
            return smplNum;
        }
    }

    public int getGeneNum() {
        if (name2 != null) {
            int geneNumTotal = geneNum + geneNum2;
            return geneNumTotal;
        } else {
            return geneNum;
        }
    }

    public boolean isAllDone() {
        return allDone;
    }

    public void setAllDone() {
        this.allDone = true;
    }

    public boolean isAutoscaleOpt() {
        return autoscaleOpt;
    }

    public void setAutoscaleOpt(boolean autoscaleOpt) {
        this.autoscaleOpt = autoscaleOpt;
    }

    public boolean isAnalDone() {
        return analDone;
    }

    public void setAnalDone(boolean analDone) {
        this.analDone = analDone;
    }

    public boolean isDisabledModify() {
        return disabledModify;
    }

    public void setDisabledModify(boolean disabledModify) {
        this.disabledModify = disabledModify;
        this.include = true;
    }

    public int getSmplNum2() {
        return smplNum2;
    }

    public void setSmplNum2(int smplNum2) {
        this.smplNum2 = smplNum2;
    }

    public int getGeneNum2() {
        return geneNum2;
    }

    public void setGeneNum2(int geneNum2) {
        this.geneNum2 = geneNum2;
    }

    public MetaPathModel(RConnection RC, String name) {
        this.RC = RC;
        this.name = name;
    }

    public String getDataName() {
        return dataName;
    }

    public void setDataName(String dataName) {
        this.dataName = dataName;
    }

    public String getGroupInfo() {
        return groupInfo;
    }

    public boolean isUploaded() {
        return uploaded;
    }

    public void setUploaded(boolean uploaded) {
        this.uploaded = uploaded;
    }

    public boolean isProcessed() {
        return processed;
    }

    public void setProcessed(boolean processed) {
        this.processed = processed;
    }

    public boolean isNormed() {
        return normed;
    }

    public void setNormed(boolean normed) {
        this.normed = normed;
    }

    public boolean isInclude() {
        return include;
    }

    public void setInclude(boolean include) {
        if (this.include != include) {
            String msg = "Dataset: " + name + " is included";
            if (!include) {
                msg = "Dataset: " + name + " is excluded";
            }

            sb.addMessage("info", msg);
            this.include = include;
        }
    }

    public String[] getAdducts() {
        return adducts;
    }

    public void setAdducts(String[] adducts) {
        this.adducts = adducts;
    }

    public String getName() {
        if (name.endsWith(".txt") | name.endsWith(".csv")) {
            String namefinal = name.substring(0, name.length() - 4);
            if (name2 != null) {
                namefinal = namefinal + " + " + name2.substring(0, name2.length() - 4);
            }
            return namefinal;
        }
        return name;
    }

    public void setName(String model) {
        this.name = model;
    }

    public String getName0() {
        return name;
    }

    public String getFullName() {
        return name;
    }

    public int getDeNum() {
        return deNum;
    }

    public void setDeNum(int num) {
        this.deNum = num;
        if (deNum > 0) {
            vennInclude = true;
        }
    }

    public boolean isVennInclude() {
        return vennInclude;
    }

    public void setVennInclude(boolean vennInclude) {
        if (deNum == 0) {
            if (vennInclude) {
                sb.addMessage("warn", "Data without sig. hits was excluded!");
            }
            this.vennInclude = false;
        } else {
            this.vennInclude = vennInclude;
        }
    }

    public String getMissingInfo() {
        return missingInfo;
    }

    public String getNormInfo() {

        String msgNorm = "";

        if ("logMed".equals(normOpt)) {
            msgNorm = msgNorm + "Log + Medium Normalization";
        }

        if ("log".equals(normOpt)) {
            msgNorm = msgNorm + "Log Tranformation";
        }

        if (autoscaleOpt) {
            msgNorm = msgNorm + " AutoScale";
        }

        return msgNorm;
    }

    public String getNormOpt() {
        return normOpt;
    }

    public void setNormOpt(String normOpt) {
        this.normOpt = normOpt;
    }

    public String getMsgText() {
        return msgText;
    }

    public double getSigLevel() {
        return sigLevel;
    }

    public void setSigLevel(double sigLevel) {
        this.sigLevel = sigLevel;
    }

    public String getPieModel() {
        return pieModel;
    }

    // Section II -------> Functional Untilities <-----------
    private void updateDataInfo() {
        int[] dims = RMetaPathUtils.getPathDataDims(RC, name);
        geneNum = dims[1];
        smplNum = dims[0];
        String[] nms = RMetaPathUtils.getMetaPathGroupNames(RC, name);
        groupInfo = nms[0] + " vs. " + nms[1];
        if (name2 != null) {
            int[] dims2 = RMetaPathUtils.getPathDataDims(RC, name2);
            geneNum2 = dims2[1];
            smplNum2 = dims2[0];
        }
    }

    public void processMetaPathData() {

        processed = false;

        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;

        try {
            if (RMetaPathUtils.sanityCheckMetaPathData(RC, name, name2)) {
                if (name2 != null) {
                    msgVec.add("Checking data content of " + name + " & " + name2 + " ...passed.");
                } else {
                    msgVec.add("Checking data content of " + name + " ...passed.");
                }

                msgArray = RDataUtils.getMetaPathSanityCheckMessage(RC, name);
                processed = true;
                updateDataInfo();
            } else {
                msgVec.add("Checking data content " + name + " ...failed.");
                msgArray = RDataUtils.getErrorMsgs(RC);
            }
        } catch (Exception e) {
            msgVec.add("Checking data content ...failed.");
            //e.printStackTrace();
            LOGGER.error("processMetaPathData", e);
        }

        msgVec.addAll(Arrays.asList(msgArray));

        String msg = "<table face=\"times\" size = \"3\">";
        msg = msg + "<tr><th> Data processing information: " + "</th></tr>";
        for (int i = 0; i < msgVec.size(); i++) {
            msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
        }
        msg = msg + "</table>";
        msgText = msg;
    }

    public void PlotPathDataProfile() {
        RMetaPathUtils.plotPathDataProfile(RC, name, name2, "_qc_box", "_qc_box2", dataformat);
    }

    public void performNormalization() {
        normed = false;
        analDone = false;
        include = false;
        disabledModify = false;
        allDone = false;
        diapos = "center top";

        int res;
        //System.out.println("we are currently performNormalization on data " + name + "|" + name2);
        RMetaPathUtils.cacheQSClean(RC);

        if (null == normOpt) {
            res = RMetaPathUtils.performMetaPathNormalization(RC, "NULL", "NULL", (autoscaleOpt) ? "\"AutoNorm\"" : "\"NULL\"", name, name2);
        } else {
            res = switch (normOpt) {
                case "logMed" ->
                    RMetaPathUtils.performMetaPathNormalization(RC, "MedianNorm", "LogNorm", (autoscaleOpt) ? "\"AutoNorm\"" : "\"NULL\"", name, name2);
                case "log" ->
                    RMetaPathUtils.performMetaPathNormalization(RC, "NULL", "LogNorm", (autoscaleOpt) ? "\"AutoNorm\"" : "\"NULL\"", name, name2);
                default ->
                    RMetaPathUtils.performMetaPathNormalization(RC, "NULL", "NULL", (autoscaleOpt) ? "\"AutoNorm\"" : "\"NULL\"", name, name2);
            };
        }

        if (res == 1) {
            sb.addMessage("info", RDataUtils.getCurrentMsg(RC));
            normed = true;
        } else {
            sb.addMessage("info", "Failed to perform normalization! " + RDataUtils.getErrMsg(RC));
        }

        getAdductItems();
    }

    private void updatePieModel(int count1, int count2) {
        pieModel = new PieChart()
                .setData(new PieData()
                        .addDataset(new PieDataset()
                                .setData(BigDecimal.valueOf(count1), BigDecimal.valueOf(count2))
                                //.setLabel("My First Dataset")
                                .addBackgroundColors(new RGBAColor(255, 99, 132), new RGBAColor(54, 162, 235))
                        )
                        .setLabels("Sig [" + count1 + "]", "Unsig [" + count2 + "]"))
                .toJson();
    }

    /*
    private void updatePieModel(int count1, int count2) {
        if (pieModel == null) {
            pieModel = new PieChartModel();
        }

        ChartData data = new ChartData();

        PieChartDataSet dataSet = new PieChartDataSet();
        List<Number> values = new ArrayList<>();
        values.add(count1);
        values.add(count2);
        dataSet.setData(values);

        List<String> bgColors = new ArrayList<>();
        bgColors.add("rgb(255, 99, 132)");
        bgColors.add("rgb(54, 162, 235)");
        dataSet.setBackgroundColor(bgColors);

        data.addChartDataSet(dataSet);
        List<String> labels = new ArrayList<>();
        labels.add("Sig [" + count1 + "]");
        labels.add("Unsig [" + count2 + "]");
        data.setLabels(labels);

        pieModel.setData(data);
    }
     */
    public void performPathAnalysis() {
        MetaPathLoadBean mplb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");

        include = false;
        disabledModify = false;
        allDone = false;
        diapos2 = "center top";

        ionMode = mplb.getDataIon();
        double res = RMetaPathUtils.prepareMetaPath(RC, ionMode, name, name2, ppm, "v1", sigLevel, MumRT);
        setAnalDone(true);

        int intRes = (int) res;
        setDeNum(intRes);
        setAnalDone(true);
        updatePieModel(intRes, 100 - intRes);
    }

    public void performmSetQSclean() {
        RMetaPathUtils.mSetQSDelete(RC);
    }

    // Section III --------> Update Icons <--------
    public String getUploadIcon() {
        if (uploaded) {
            return "pi pi-check";
        }
        return "pi pi-cloud-upload";
    }

    public String getProcessIcon() {
        if (processed) {
            return "pi pi-check";
        }
        return "pi pi-caret-right";
    }

    public String getNormIcon() {
        if (normed) {
            return "pi pi-check";
        }
        return "pi pi-cog";
    }

    public String getAnalIcon() {
        if (analDone) {
            return "pi pi-check";
        }
        return "pi pi-search";
    }

    public String getSummaryIcon() {
        if (allDone) {
            return "pi pi-check";
        }
        return "pi pi-info-circle";
    }

    public void initilizeAdductItem() {
        MetaPathLoadBean mplb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");

        String adductSPath;
        String adductTPath;
        String mode = mplb.getDataIon();

        switch (mode) {
            case "positive":
                adductSPath = ab.getInternalData("source_pos_add_list.txt");
                adductTPath = ab.getInternalData("target_pos_add_list.txt");
                break;
            case "negative":
                adductSPath = ab.getInternalData("source_neg_add_list.txt");
                adductTPath = ab.getInternalData("target_neg_add_list.txt");
                break;
            default:
                // need to deal w. mixed mode
                adductSPath = ab.getInternalData("source_mixed_add_list.txt");
                adductTPath = ab.getInternalData("target_mixed_add_list.txt");
                break;
        }

        String allSAdducts = DataUtils.readTextFile(adductSPath);
        String[] newsadducts = allSAdducts.split("\n");

        String allTAdducts = DataUtils.readTextFile(adductTPath);
        String[] newtadducts = allTAdducts.split("\n");
        adductItems = new DualListModel(Arrays.asList(newsadducts), Arrays.asList(newtadducts));

    }

    public DualListModel<String> getAdductItems() {
        initilizeAdductItem();
        return adductItems;
    }

    public void setAdductItems(DualListModel<String> adductItems) {
        this.adductItems = adductItems;
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public void defineAdducts() {

        var addVec = adductItems.getTarget().toArray(String[]::new);
        if (addVec.length == 0) {
            addVec = new String[]{""};
        }
        //RConnection RC = sb.getRConnection();                
        RMetaPathUtils.customizeMetaAdduct(RC, name, name2, addVec, ionMode);
        //System.out.println("====== performadductdefine res" + res + "dataName: " +dataName + "name: " + name + "name2:" + name2);
    }

}
