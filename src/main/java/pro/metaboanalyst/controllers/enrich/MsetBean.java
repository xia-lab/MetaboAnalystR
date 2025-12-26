/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.enrich;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetSetBean;
import pro.metaboanalyst.models.OraBean;
import pro.metaboanalyst.models.QeaBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import software.xdev.chartjs.model.charts.PieChart;
import software.xdev.chartjs.model.charts.BarChart;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;
import software.xdev.chartjs.model.data.BarData;
import software.xdev.chartjs.model.data.PieData;
import software.xdev.chartjs.model.dataset.BarDataset;
import software.xdev.chartjs.model.dataset.PieDataset;
import software.xdev.chartjs.model.enums.IndexAxis;
import software.xdev.chartjs.model.options.BarOptions;
import software.xdev.chartjs.model.options.scale.Scales;
import software.xdev.chartjs.model.options.scale.cartesian.CartesianScaleOptions;
import software.xdev.chartjs.model.options.scale.cartesian.CartesianTickOptions;

/**
 * @author jianguox
 */
@SessionScoped
@Named("msetBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class MsetBean implements Serializable {

    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(MsetBean.class);

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private final List<String> chemLibs = Arrays.asList("super_class", "main_class", "sub_class");
    private String msetOpt = "smpdb_pathway";
    private String libOpt = "all";
    private UploadedFile msetLibFile;
    private String checkMsg = "";
    private boolean doMetabolomeFilter = false;
    private boolean doMsetFilter = true;
    private int minMsetNum = 2;
    private UploadedFile metabolomeFile;
    private OraBean[] oraBeans;
    private QeaBean[] qeaBeans;
    private String msetNm;
    private String imgOpt = "net";
    @JsonIgnore
    private String barModel;
    @JsonIgnore
    private String pieModel;
    private boolean oratabswitch = true;
    private boolean ssptabswitch = true;
    private boolean qeatabswitch = true;

    public String getMsetOpt() {
        return msetOpt;
    }

    public void setMsetOpt(String msetOpt) {
        this.msetOpt = msetOpt;
    }

    public String getLibOpt() {
        return libOpt;
    }

    public void setLibOpt(String libOpt) {
        this.libOpt = libOpt;
    }

    @JsonIgnore
    public UploadedFile getMsetLibFile() {
        return msetLibFile;
    }

    public void setMsetLibFile(UploadedFile msetLibFile) {
        this.msetLibFile = msetLibFile;
    }

    public String getCheckMsg() {
        return checkMsg;
    }

    public void setCheckMsg(String checkMsg) {
        this.checkMsg = checkMsg;
    }

    public void msetUploadBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {
            RConnection RC = sb.getRConnection();

            //check if data is uploaded
            if (msetLibFile == null) {
                sb.addMessage("Error", "Please upload your file");
                return;
            }
            if (msetLibFile.getSize() == 0) {
                sb.addMessage("Error", "File is empty");
                return;
            }

            if (!msetLibFile.getFileName().endsWith(".csv")) {
                sb.addMessage("Error", "Only comma separated format (*.csv) will be accepted!");
                return;
            }

            // File libFile = new File(homeDir + File.separatorChar + fileName);
            // uploadedFile.write(libFile);
            String fileName = DataUtils.uploadFile(sb, msetLibFile, sb.getCurrentUser().getHomeDir(), null, false);
            boolean res = RDataUtils.readMsetLibData(RC, fileName);
            if (res) {
                checkMsg = RDataUtils.getMsetLibCheckMsg(RC);
                sb.addMessage("OK", checkMsg);
                msetOpt = "self";
            } else {
                sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
            }
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("msetUploadBn_action", e);
        }
    }

    public boolean isDoMetabolomeFilter() {
        return doMetabolomeFilter;
    }

    public void setDoMetabolomeFilter(boolean doMetabolomeFilter) {
        this.doMetabolomeFilter = doMetabolomeFilter;
    }

    public boolean isDoMsetFilter() {
        return doMsetFilter;
    }

    public void setDoMsetFilter(boolean doMsetFilter) {
        this.doMsetFilter = doMsetFilter;
    }

    public int getMinMsetNum() {
        return minMsetNum;
    }

    public void setMinMsetNum(int minMsetNum) {
        this.minMsetNum = minMsetNum;
    }

    public UploadedFile getMetabolomeFile() {
        return metabolomeFile;
    }

    public void setMetabolomeFile(UploadedFile metabolomeFile) {
        this.metabolomeFile = metabolomeFile;
    }

    public void uploadMetabolomeBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {

            RConnection RC = sb.getRConnection();
            //check if data is uploaded
            if (metabolomeFile == null) {
                sb.addMessage("Error", "Please upload your file");
                return;
            }
            if (metabolomeFile.getSize() == 0) {
                sb.addMessage("Error", "File is empty");
                return;
            }
            String fileName = DataUtils.uploadFile(sb, metabolomeFile, sb.getCurrentUser().getHomeDir(), null, false);
            boolean res = RDataUtils.readHMDBRefLibData(RC, fileName);
            if (res) {
                sb.addMessage("OK", RDataUtils.getRefLibCheckMsg(RC));
                libOpt = "self";
            } else {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            }
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("uploadMetabolomeBn_action", e);
        }
    }

    public String submitBtn_action() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            jrd.record_submitBtn_action(this);
            return null;
        }
        RConnection RC = sb.getRConnection();
        int excludeNm = 0;
        if (doMsetFilter) {
            excludeNm = minMsetNum;
        }

        if (libOpt.equals("self")) {
            RDataUtils.setMetabolomeFilter(sb, true);
        } else {
            RDataUtils.setMetabolomeFilter(sb, false);
        }

        //sb.setMsetLibType(msetOpt);
        RDataUtils.setCurrentMsetLib(sb, msetOpt, excludeNm);

        jrd.record_submitBtn_action(this);
        if (sb.getAnalType().equals("msetqea")) {
            return doGlobalTest();
        } else {
            return doHyperGeom();

        }
    }

    @JsonIgnore
    public OraBean[] getOraBeans() {
        if (oraBeans == null) {
            populateOraBean();
        }
        return oraBeans;
    }

    public String doHyperGeom() {
        RConnection RC = sb.getRConnection();
        if (REnrichUtils.hypergeomTest(sb)) {
            populateOraBean();
            wb.getCalledWorkflows().add("ORA");
            return "oraview";
        } else {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            wb.getCalledWorkflowsError().add("ORA");
            return null;
        }
    }

    public void populateOraBean() {
        RConnection RC = sb.getRConnection();
        if (!RDataUtils.checkDetailsTablePerformed(sb.getRConnection(), "ora")) {
            return;
        }

        String imgName = sb.getNewImage("ora");
        String imgName2 = sb.getNewImage("ora_dot");
        REnrichUtils.plotORA(sb, imgOpt, imgName, "png", 150);
        REnrichUtils.plotEnrichmentDotPlot(sb, "ora", imgName2, "png", 150);

        if (isShowPie()) {
            REnrichUtils.plotEnrichPieChart(sb, "ora", sb.getNewImage("ora_pie"), "png", 150);
            preparePieChart(RC);
        }

        String[] rownames = REnrichUtils.getORARowNames(RC);
        String[] rowStyles = REnrichUtils.getORAColorBar(RC);
        double[][] mat = REnrichUtils.getORAMat(RC);

        oraBeans = new OraBean[rownames.length];

        for (int i = 0; i < rownames.length; i++) {
            oraBeans[i] = new OraBean(rownames[i], "background-color:" + rowStyles[i], (int) mat[i][0], mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5]);
        }

        createBarModel(rownames, rowStyles, mat);
    }

    private void preparePieChart(RConnection RC) {
        String[] pieNames = REnrichUtils.getEnrichPieNames(RC);
        String[] pieStyles = REnrichUtils.getEnrichPieColors(RC);
        double[][] piemat = REnrichUtils.getEnrichPieHits(RC);
        createPieModel(piemat, pieNames, pieStyles);
    }

    public boolean isShowPie() {
        return chemLibs.contains(msetOpt);
    }

    @JsonIgnore
    public QeaBean[] getQeaBeans() {
        if (qeaBeans == null) {
            populateQeaBean();
        }
        return qeaBeans;
    }

    public String doGlobalTest() {
        RConnection RC = sb.getRConnection();
        if (REnrichUtils.performGlobalTest(sb) == 1) {
            populateQeaBean();
            return "qeaview";
        } else {
            wb.getCalledWorkflowsError().add("QEA");

            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            return null;
        }
    }

    public void setMsetNm(String msetNm) {
        //System.out.println(msetNm + "=====setCurrentMsetLib");
        this.msetNm = msetNm;
    }

    @JsonIgnore
    public String getMsetImgPath() {
        String imgNm = REnrichUtils.plotQeaMset(sb, msetNm, "png", 150);
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + imgNm;
    }

    @JsonIgnore
    public MetSetBean getCurrentMset() {
        //System.out.println(msetNm + "=====getCurrentMsetLib");
        String[] details = REnrichUtils.getHTMLMetSet(sb.getRConnection(), msetNm);
        return (new MetSetBean(details[0], details[1], details[2]));
    }

    public String getImgOpt() {
        return imgOpt;
    }

    public void setImgOpt(String imgOpt) {
        this.imgOpt = imgOpt;
    }

    @JsonIgnore
    public DefaultStreamedContent getSifFile() {
        REnrichUtils.prepareSifDownload(sb);
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/metaboanalyst_enrich_sif.zip");
    }

    @JsonIgnore
    public String getBarModel() {
        if (barModel == null) {
            //System.out.println(sb.getAnalType() + "====analtype");
            if (sb.getAnalType().equals("msetqea")) {
                doGlobalTest();
            } else {
                doHyperGeom();
            }
        }
        return barModel;
    }

    public void createBarModel(String[] myLabels, String[] myColor, double[][] mat) {

        List<String> labels = new ArrayList<>();
        List<Number> values = new ArrayList<>();
        List<String> bgColor = new ArrayList<>();
        List<String> borderColor = new ArrayList<>();

        String myCol;
        int myLen = 25; //best for bar plot view
        if (myLabels.length < 25) {
            myLen = myLabels.length;
        }
        if (sb.getAnalType().equals("msetqea")) {
            for (int i = 0; i < myLen; i++) {
                values.add(mat[i][2] / mat[i][3]);
                labels.add(myLabels[i]);
                myCol = myColor[i];
                borderColor.add("rgb(211,211,211)");
                //myCol = myCol.substring(3);
                //myCol = "rgba" + myCol.substring(0, myCol.length()- 1) + ", 0.3)";
                bgColor.add(myCol);
            }
        } else {
            for (int i = 0; i < myLen; i++) {
                values.add(mat[i][2] / mat[i][1]);
                labels.add(myLabels[i]);
                myCol = myColor[i];
                borderColor.add("rgb(211,211,211)");
                //myCol = myCol.substring(3);
                //myCol = "rgba" + myCol.substring(0, myCol.length()- 1) + ", 0.3)";
                //System.out.println("==========" + myCol + "==========");
                bgColor.add(myCol);
            }
        }

        barModel = new BarChart()
                .setData(new BarData()
                        .addDataset(new BarDataset()
                                .setData(values)
                                .setBackgroundColor(bgColor)
                                .setBorderColor(borderColor)
                                .setBorderWidth(1))
                        .setLabels(labels))
                .setOptions(new BarOptions()
                        .setResponsive(true)
                        .setMaintainAspectRatio(false)
                        .setIndexAxis(IndexAxis.Y)
                        .setScales(new Scales().addScale(Scales.ScaleAxis.X, new CartesianScaleOptions()
                                .setStacked(false)
                                .setTicks(new CartesianTickOptions()
                                        .setAutoSkip(true)
                                        .setMirror(false)))
                        )
                ).toJson();
    }

    @JsonIgnore
    public String getPieModel() {
        if (pieModel == null) {
            preparePieChart(sb.getRConnection());
        }
        return pieModel;
    }

    private void createPieModel(double[][] myHits, String[] myLabels, String[] myColor) {

        List<String> labels = new ArrayList<>();
        List<Number> values = new ArrayList<>();
        List<String> bgColor = new ArrayList<>();
        List<String> borderColor = new ArrayList<>();

        String myCol;
        int myLen = 15;

        if (myLabels.length < 15) {
            myLen = myLabels.length;
        }

        for (int i = 0; i < myLen; i++) {
            values.add(myHits[i][0]);
            labels.add(myLabels[i]);
            myCol = myColor[i];
            borderColor.add(myCol);
            bgColor.add(myCol);
        }

        pieModel = new PieChart()
                .setData(new PieData()
                        .addDataset(new PieDataset()
                                .setData(values)
                                //.setLabel("My First Dataset")
                                .addBackgroundColors(bgColor)
                                .addBorderColors(borderColor)
                        )
                        .setLabels(labels))
                .toJson();

    }

    public boolean isOratabswitch() {
        return oratabswitch;
    }

    public void setOratabswitch(boolean oratabswitch) {
        this.oratabswitch = oratabswitch;
    }

    public boolean isSsptabswitch() {
        return ssptabswitch;
    }

    public void setSsptabswitch(boolean ssptabswitch) {
        this.ssptabswitch = ssptabswitch;
    }

    public boolean isQeatabswitch() {
        return qeatabswitch;
    }

    public void setQeatabswitch(boolean qeatabswitch) {
        this.qeatabswitch = qeatabswitch;
    }

    public void populateQeaBean() {
        //System.out.println("==populateQeaBean==");
        RConnection RC = sb.getRConnection();
        if (!RDataUtils.checkDetailsTablePerformed(sb.getRConnection(), "qea")) {
            return;
        }

        String imgName = sb.getNewImage("qea");
        String imgName2 = sb.getNewImage("qea_dot");

        if (isShowPie()) {
            String imgName3 = sb.getNewImage("qea_pie");
            REnrichUtils.plotEnrichPieChart(sb, "qea", imgName3, "png", 150);
            preparePieChart(RC);
        }

        String[] rownames = REnrichUtils.getQEARowNames(RC);
        double[][] mat = REnrichUtils.getQEAMat(RC);
        qeaBeans = new QeaBean[rownames.length];
        String[] rowStyles = REnrichUtils.getQEAColorBar(RC);
        for (int i = 0; i < rownames.length; i++) {
            qeaBeans[i] = new QeaBean(rownames[i], "background-color:" + rowStyles[i], (int) mat[i][0], (int) mat[i][1],
                    mat[i][2], mat[i][3], mat[i][4], mat[i][5], mat[i][6]);
        }
        REnrichUtils.plotQEA(sb, imgOpt, imgName, "png", 150);
        REnrichUtils.plotEnrichmentDotPlot(sb, "qea", imgName2, "png", 150);
        createBarModel(rownames, rowStyles, mat);
        wb.getCalledWorkflows().add("QEA");
    }
}
