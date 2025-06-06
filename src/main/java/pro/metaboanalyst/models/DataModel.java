/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RMetaUtils;
import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import software.xdev.chartjs.model.charts.PieChart;
import software.xdev.chartjs.model.color.RGBAColor;
import software.xdev.chartjs.model.data.PieData;
import software.xdev.chartjs.model.dataset.PieDataset;
import java.math.BigDecimal;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author jianguox
 */
public class DataModel implements Serializable {

    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(DataModel.class);

    @JsonIgnore
    private SessionBean1 sb;
    private RConnection RC;
    private String name;
    private String dataName;
    private String groupInfo;
    private int smplNum;
    private int geneNum;
    private boolean uploaded = false;
    private boolean processed = false; // normalization
    private boolean normed = false;
    private boolean analDone = false;
    private boolean allDone = false;
    private boolean include = true;
    private String featureType = "NA"; //entrez/refseq as well as built-in affy platform ID, "probe" indicated custome platform
    private boolean autoscaleOpt = false;
    private double sigLevel = 0.1;
    private double fcLevel = 0.0;
    private String missingInfo;
    private String zeroInfo;
    private String normOpt = "none";
    private String msgText;
    @JsonIgnore
    private String pieModel;
    private String primaryMeta;
    //number of significant features in individual analysis
    private int deNum;
    //can be used for venn diagram?
    private boolean vennInclude = false;

    @JsonCreator
    @ConstructorProperties({"name", "dataName", "groupInfo", "smplNum", "geneNum", "uploaded", "processed", "normed",
        "analDone", "allDone", "include", "featureType", "autoscaleOpt", "sigLevel", "fcLevel", "missingInfo",
        "zeroInfo", "normOpt", "msgText", "deNum", "vennInclude", "primaryMeta"})
    public DataModel(String name, String dataName, String groupInfo, int smplNum, int geneNum, boolean uploaded, boolean processed, boolean normed,
            boolean analDone, boolean allDone, boolean include, String featureType, boolean autoscaleOpt, double sigLevel, double fcLevel,
            String missingInfo, String zeroInfo, String normOpt, String msgText, int deNum, boolean vennInclude, String primaryMeta) {
        this.name = name;
        this.dataName = dataName;
        this.groupInfo = groupInfo;
        this.smplNum = smplNum;
        this.geneNum = geneNum;
        this.uploaded = uploaded;
        this.processed = processed;
        this.normed = normed;
        this.analDone = analDone;
        this.allDone = allDone;
        this.include = include;
        this.featureType = featureType;
        this.autoscaleOpt = autoscaleOpt;
        this.sigLevel = sigLevel;
        this.fcLevel = fcLevel;
        this.missingInfo = missingInfo;
        this.zeroInfo = zeroInfo;
        this.normOpt = normOpt;
        this.msgText = msgText;
        this.deNum = deNum;
        this.vennInclude = vennInclude;
        this.primaryMeta = primaryMeta;
    }

    public DataModel(SessionBean1 sb, String name) {
        this.sb = sb;
        this.name = name;
        RC = sb.getRConnection();
    }

    public double getFcLevel() {
        return fcLevel;
    }

    public void setFcLevel(double fcLevel) {
        this.fcLevel = fcLevel;
    }

    public RConnection getRC() {
        return RC;
    }

    public void setRC(RConnection RC) {
        this.RC = RC;
    }

    public int getSmplNum() {
        return smplNum;
    }

    public void setGroupInfo(String groupInfo) {
        this.groupInfo = groupInfo;
    }

    public void setSmplNum(int smplNum) {
        this.smplNum = smplNum;
    }

    public void setGeneNum(int geneNum) {
        this.geneNum = geneNum;
    }

    public int getGeneNum() {
        return geneNum;
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

    public String getName() {
        if (name.endsWith(".txt") | name.endsWith(".csv")) {
            return name.substring(0, name.length() - 4);
        }

        return name;
    }

    public void setName(String model) {
        this.name = model;
    }

    public String getFullName() {
        return name;
    }

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

    /**
     * The methods below are data-specific information Should be handled by the
     * data itself
     */
    public void processMetaData() {

        processed = false;

        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;
        try {
            if (RDataUtils.sanityCheckMetaData(sb.getRConnection(), name)) {
                System.out.println("pro.metaboanalyst.models.DataModel.processMetaData()" + name);
                msgVec.add("Checking data content ...passed.");
                msgArray = RDataUtils.getMetaSanityCheckMessage(sb.getRConnection(), name);
                processed = true;
                updateDataInfo();
            } else {
                msgVec.add("Checking data content ...failed.");
                msgArray = RDataUtils.getErrorMsgs(sb.getRConnection());
            }
        } catch (Exception e) {
            msgVec.add("Checking data content ...failed.");
            LOGGER.error("processMetaData", e);
            //e.printStackTrace();
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

    private void updateDataInfo() {
        int[] dims = RMetaUtils.getDataDims(sb.getRConnection(), name);
        geneNum = dims[1];
        smplNum = dims[0];
        String[] nms = RDataUtils.getMetaGroupNames(sb.getRConnection(), name);
        groupInfo = nms[0] + " vs. " + nms[1];
    }

    public String getMissingInfo() {
        return missingInfo;
    }

    public String getNormInfo() {
        if (normOpt.equals("none")) {
            if (autoscaleOpt) {
                return ("Autoscale only");
            } else {
                return ("None performed");
            }
        } else {
            if (autoscaleOpt) {
                return ("Log2 transform followed by autoscale");
            } else {
                return ("Log2 transform only");
            }
        }
    }

    public String getZeroInfo() {
        return zeroInfo;
    }

    public void plotDataProfile() {
        RMetaUtils.plotDataProfiles(sb.getRConnection(), name, sb.getNewImage("qc_boxplot"), sb.getNewImage("qc_pca"));
    }

    public String getFeatureType() {
        return featureType;
    }

    public void setFeatureType(String featureType) {
        this.featureType = featureType;
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

    public void performNormalization() {
        normed = false;
        int res = RMetaUtils.performIndNormalization(sb.getRConnection(), name, normOpt, (autoscaleOpt) ? 1 : 0);
        if (res == 1) {
            sb.addMessage("info", RDataUtils.getCurrentMsg(sb.getRConnection()));
            normed = true;
        } else {
            sb.addMessage("error", "Failed to perform normalization!" + RDataUtils.getErrMsg(sb.getRConnection()));
        }
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

    public String getPrimaryMeta() {
        return primaryMeta;
    }

    public void setPrimaryMeta(String primaryMeta) {
        this.primaryMeta = primaryMeta;
    }

    public void performDEAnalysis() {
        int[] res = RMetaUtils.performLimmaDE(sb.getRConnection(), name, sigLevel, fcLevel);
        if (res[0] == 1) {
            setDeNum(res[1]);
            setAnalDone(true);
            updatePieModel(res[1], res[2]);
        }
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

}
