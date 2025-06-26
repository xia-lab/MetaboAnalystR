/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.enrich;

import java.io.Serializable;
import java.util.ArrayList;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetSetBean;
import pro.metaboanalyst.models.PABean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.rwrappers.RGraphUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author jianguox
 */
@SessionScoped
@Named("pathBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class PathBean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;
    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(PathBean.class);
    @JsonIgnore
    @Inject
    private SessionBean1 sb;
    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    @JsonProperty("libVersion")
    private String libVersion = "current";
    @JsonProperty("libOpt")
    private String libOpt = "hsa";
    @JsonProperty("refLibOpt")
    private String refLibOpt = "all";
    @JsonIgnore
    private UploadedFile refLibFile;
    @JsonProperty("topoCode")
    private String topoCode = "rbc";
    @JsonProperty("oraStatCode")
    private String oraStatCode = "hyperg";
    @JsonProperty("qeaStatCode")
    private String qeaStatCode = "gt";
    @JsonProperty("paBeans")
    private PABean[] paBeans;
    @JsonProperty("downloadMsg")
    private String downloadMsg = "";
    @JsonProperty("analOption")
    private String analOption = "scatter";
    @JsonProperty("heatmapName")
    private String heatmapName = "";
    @JsonProperty("enrType")
    private String enrType = "";
    @JsonProperty("showGrid")
    private boolean showGrid = false;
    @JsonProperty("xlim")
    private double xlim = 0;
    @JsonProperty("ylim")
    private double ylim = 0;

    public String getLibVersion() {
        return libVersion;
    }

    public void setLibVersion(String libVersion) {
        this.libVersion = libVersion;
    }

    public String getLibOpt() {
        return libOpt;
    }

    public void setLibOpt(String libOpt) {
        this.libOpt = libOpt;
    }

    public String getRefLibOpt() {
        return refLibOpt;
    }

    public void setRefLibOpt(String refLibOpt) {
        this.refLibOpt = refLibOpt;
    }

    public UploadedFile getRefLibFile() {
        return refLibFile;
    }

    public void setRefLibFile(UploadedFile refLibFile) {
        this.refLibFile = refLibFile;
    }

    public String getTopoCode() {
        return topoCode;
    }

    public void setTopoCode(String topoCode) {
        this.topoCode = topoCode;
    }

    public String getOraStatCode() {
        return oraStatCode;
    }

    public void setOraStatCode(String oraStatCode) {
        this.oraStatCode = oraStatCode;
    }

    public String getQeaStatCode() {
        return qeaStatCode;
    }

    public void setQeaStatCode(String qeaStatCode) {
        this.qeaStatCode = qeaStatCode;
    }

    @JsonIgnore
    private SelectItem[] pathLibOpt = null;

    @JsonIgnore
    public SelectItem[] getPathLibOpt() {
        if (pathLibOpt == null) {
            setupPathLibOpt();
        }
        return pathLibOpt;
    }

    private void setupPathLibOpt() {
        String[] lbls = RDataUtils.getOrgPathLbl(sb.getRConnection());
        int rowLen = lbls.length;
        pathLibOpt = new SelectItem[rowLen];
        String[] vals = RDataUtils.getOrgPathVal(sb.getRConnection());
        for (int i = 0; i < rowLen; i++) {
            pathLibOpt[i] = new SelectItem(vals[i], lbls[i]);
        }
    }

    @JsonIgnore
    public MetSetBean[] getCurrentPathSet() {
        String[] details = REnrichUtils.getHTMLPathSet(sb.getRConnection(), sb.getCurrentPathName());
        ArrayList<MetSetBean> libVec = new ArrayList();
        libVec.add(new MetSetBean(details[0], details[1], ""));
        return libVec.toArray(new MetSetBean[0]);
    }

    public void handleRefLibUpload() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {

            RConnection RC = sb.getRConnection();
            //check if data is uploaded
            if (refLibFile == null) {
                sb.addMessage("Error", "Please upload your file!");
                return;
            }
            if (refLibFile.getSize() == 0) {
                sb.addMessage("Error", "File is empty");
                return;
            }
            String fileName = DataUtils.uploadFile(sb, refLibFile, sb.getCurrentUser().getHomeDir(), null, false);
            boolean res = RDataUtils.readKEGGRefLibData(RC, fileName);
            if (res) {
                sb.addMessage("OK", RDataUtils.getRefLibCheckMsg(RC));
                refLibOpt = "self";
            } else {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            }
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("handleRefLibUpload", e);
        }
    }

    public String paBn_proceed() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            jrd.record_paBn_proceed(this);
            return null;
        }
        String type = sb.getAnalType();
        jrd.record_paBn_proceed(this);

        if (type.equals("pathqea")) {
            wb.getCalledWorkflows().add("paBn_proceed_qea");
        } else {
            wb.getCalledWorkflows().add("paBn_proceed_ora");
        }
        if (analOption.equals("scatter")) {
            return (paBn_action());
        } else {
            return (paBn_heatmap(type));
        }
    }

    public String paBn_action() {

        RConnection RC = sb.getRConnection();
        if (RDataUtils.setPathLib(sb, libOpt, libVersion)) {

            String nextpage;
            if (libOpt.startsWith("smpdb")) {
                libOpt = libOpt.split("-")[1];
                RDataUtils.setOrganism(sb, libOpt);
                nextpage = "smpdbpathview";
            } else {
                nextpage = "pathview";
            }

            if (refLibOpt.equals("all")) {
                RDataUtils.setMetabolomeFilter(sb, false);
            } else {
                RDataUtils.setMetabolomeFilter(sb, true);
            }

            if (sb.getAnalType().equalsIgnoreCase("pathqea")) {

                if (REnrichUtils.doPathQeaTest(sb, topoCode, qeaStatCode)) {
                    RGraphUtils.plotPathSummary(sb, showGrid ? "T" : "F", sb.getNewImage("path_view"), "png", 150, 0, 0, nextpage);
                    String[] pathnames = REnrichUtils.getQEApathNames(RC);
                    String[] keggLnks = REnrichUtils.getQEAKeggIDs(RC);
                    String[] smpdbLnks = null;
                    if (libOpt.equals("hsa") || libOpt.equals("mmu")) {
                        smpdbLnks = REnrichUtils.getQEASMPDBIDs(RC);
                    }

                    if (pathnames.length == 1 && pathnames[0].equals("NA")) {
                        return null;
                    }

                    double[][] mat = REnrichUtils.getQEAMat(RC);
                    paBeans = new PABean[pathnames.length];
                    for (int i = 0; i < pathnames.length; i++) {
                        paBeans[i] = new PABean("query", pathnames[i], keggLnks[i], smpdbLnks == null ? "" : smpdbLnks[i], (int) mat[i][0], 0, (int) mat[i][1],
                                mat[i][2], mat[i][3], mat[i][4], mat[i][5], mat[i][6], 0, 0);
                    }
                } else {
                    String msg = RDataUtils.getErrMsg(RC);
                    sb.addMessage("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
                    return null;
                }
            } else if (REnrichUtils.doPathOraTest(sb, topoCode, oraStatCode)) {
                RGraphUtils.plotPathSummary(sb, showGrid ? "T" : "F", sb.getNewImage("path_view"), "png", 150, 0, 0, nextpage);
                String[] rownames = REnrichUtils.getORApathNames(RC);
                String[] keggLnks = REnrichUtils.getORAKeggIDs(RC);
                String[] smpdbLnks = null;
                if (libOpt.equals("hsa") || libOpt.equals("mmu")) {
                    smpdbLnks = REnrichUtils.getORASMPDBIDs(RC);
                }
                double[][] mat = REnrichUtils.getORAMat(RC);
                paBeans = new PABean[rownames.length];
                for (int i = 0; i < rownames.length; i++) {
                    paBeans[i] = new PABean("query", rownames[i], keggLnks[i], smpdbLnks == null ? "" : smpdbLnks[i],
                            (int) mat[i][0], mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5], mat[i][6], mat[i][7], 0, 0);
                }
            } else {
                String msg = RDataUtils.getErrMsg(RC);
                sb.addMessage("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
                return null;
            }
            return nextpage;
        } else {
            String msg = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
            return null;
        }
    }

    public String paBn_heatmap(String type) {

        RConnection RC = sb.getRConnection();
        if (RDataUtils.setPathLib(sb, libOpt, libVersion)) {

            if (libOpt.startsWith("smpdb")) {
                setEnrType(libOpt);
                libOpt = libOpt.split("-")[1];
                RDataUtils.setOrganism(sb, libOpt);
            } else {
            }

            if (refLibOpt.equals("all")) {
                RDataUtils.setMetabolomeFilter(sb, false);
            } else {
                RDataUtils.setMetabolomeFilter(sb, true);
            }
            String fileNm = "metaboanalyst_heatmap_" + sb.getFileCount() + ".json";
            setHeatmapName(fileNm);
            if (REnrichUtils.computePathHeatmap(sb, libOpt, fileNm, type)) {
                sb.setHeatmapType("pathway");
                return "Heatmap view";
            } else {
                String msg = RDataUtils.getErrMsg(RC);
                sb.addMessage("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
                return null;
            }

        } else {
            String msg = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
            return null;
        }
    }

    @JsonIgnore
    public PABean[] getPaBeans() {
        if (paBeans == null) {
            paBn_action();
        }
        return paBeans;
    }

    public void setPaBeans(PABean[] pabs) {
        paBeans = pabs;
    }

    public String getDownloadMsg() {
        return downloadMsg;
    }

    public void setDownloadMsg(String downloadMsg) {
        this.downloadMsg = downloadMsg;
    }

    public String getAnalOption() {
        return analOption;
    }

    public void setAnalOption(String analOption) {
        this.analOption = analOption;
    }

    public String getHeatmapName() {
        return heatmapName;
    }

    public void setHeatmapName(String heatmapName) {
        this.heatmapName = heatmapName;
    }

    public String getEnrType() {
        return enrType;
    }

    public void setEnrType(String enrType) {
        this.enrType = enrType;
    }

    public boolean isShowGrid() {
        return showGrid;
    }

    public void setShowGrid(boolean showGrid) {
        this.showGrid = showGrid;
    }

    public double getXlim() {
        return xlim;
    }

    public void setXlim(double xlim) {
        this.xlim = xlim;
    }

    public double getYlim() {
        return ylim;
    }

    public void setYlim(double ylim) {
        this.ylim = ylim;
    }

    public void updatePathView() {
        RGraphUtils.plotPathSummary(sb, showGrid ? "T" : "F", sb.getNewImage("path_view"), "png", 150, xlim, ylim, "pathview");
    }

    private boolean switchORAtab = true, switchQEAtab = true;

    public boolean isSwitchORAtab() {
        return switchORAtab;
    }

    public void setSwitchORAtab(boolean switchORAtab) {
        this.switchORAtab = switchORAtab;
    }

    public boolean isSwitchQEAtab() {
        return switchQEAtab;
    }

    public void setSwitchQEAtab(boolean switchQEAtab) {
        this.switchQEAtab = switchQEAtab;
    }

    public boolean isSmpdbBool() {
        System.out.println(libOpt);

        System.out.println("libOpt.startsWith(\"smpdb\")========" + libOpt.startsWith("smpdb"));
        return libOpt.startsWith("smpdb");
    }

    public String proceed2EnrichNet() {
        int res = RDataUtils.prepareEnrichNet(sb.getRConnection(), "enrichNet_" + sb.getAnalType(), "mixed", sb.getAnalType());
        if (res == 0) {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Failed to compute enrichment network: " + err);
        }
        sb.setEnrNetSavedInit(false);
        return "EnrichNetwork";
    }
}
