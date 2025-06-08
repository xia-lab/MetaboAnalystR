/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.metapath;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.ListDataModel;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.models.ColumnModel;
import pro.metaboanalyst.models.GseaBean;
import pro.metaboanalyst.models.MetaResultBean;
import pro.metaboanalyst.models.MummiBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.rwrappers.RMetaPathUtils;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;
import jakarta.inject.Inject;

/**
 *
 * @author qiang
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@SessionScoped
@Named("pMetaStatBean")
public class MetaPathStatBean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private MetaPathLoadBean mplb;

    @JsonIgnore
    @Inject
    private MummiAnalBean mumb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

       @JsonIgnore
    @Inject
    private JavaRecord jrd;
       
    //@JsonIgnore
    //RConnection RC = sb.getRConnection();
    private boolean resOK = false;

    public boolean isResOK() {
        return resOK;
    }

    public void setResOK(boolean resOK) {
        this.resOK = resOK;
    }

    private boolean resOK2 = false;

    public boolean isResOK2() {
        return resOK2;
    }

    public void setResOK2(boolean resOK2) {
        this.resOK2 = resOK2;
    }

    private int permuNUm = 100;
    private String lib = "hsa_kegg";
    private String libVersion = "current";
    private String metaLevel = "pathway";
    private String combinelevel = "both";
    private String pvalmethod = "fisher";
    private String esmethod = "fixed";
    private String rankmetric = "mean";
    private String integMethod = "pval";
    private String universe = "matchedc";
    private String pathAlgOpt = "mummichog";
    private String cpdAlgOpt = "mummichog";
    private String poolAlgOpt = "mummichog";
    private String pathAlgVersion = "v1";
    private String poolAlgVersion = "v1";
    private double pvalCutoff = 0.001;
    private boolean showNetwork = false;

    public String getPathAlgVersion() {
        return pathAlgVersion;
    }

    public void setPathAlgVersion(String pathAlgVersion) {
        this.pathAlgVersion = pathAlgVersion;
    }

    public String getPoolAlgVersion() {
        return poolAlgVersion;
    }

    public void setPoolAlgVersion(String poolAlgVersion) {
        this.poolAlgVersion = poolAlgVersion;
    }

    public String getPathAlgOpt() {
        return pathAlgOpt;
    }

    public void setPathAlgOpt(String pathAlgOpt) {
        this.pathAlgOpt = pathAlgOpt;
    }

    public String getCpdAlgOpt() {
        return cpdAlgOpt;
    }

    public void setCpdAlgOpt(String cpdAlgOpt) {
        this.cpdAlgOpt = cpdAlgOpt;
    }

    public String getPoolAlgOpt() {
        return poolAlgOpt;
    }

    public void setPoolAlgOpt(String poolAlgOpt) {
        this.poolAlgOpt = poolAlgOpt;
    }

    public double getPvalCutoff() {
        return pvalCutoff;
    }

    public void setPvalCutoff(double pvalCutoff) {
        this.pvalCutoff = pvalCutoff;
    }

    public String getUniverse() {
        return universe;
    }

    public void setUniverse(String universe) {
        this.universe = universe;
    }

    public String getIntegMethod() {
        return integMethod;
    }

    public void setIntegMethod(String integMethod) {
        this.integMethod = integMethod;
    }

    public int getPermuNUm() {
        return permuNUm;
    }

    public void setPermuNUm(int permuNUm) {
        this.permuNUm = permuNUm;
    }

    public String getLib() {
        return lib;
    }

    public void setLib(String lib) {
        this.lib = lib;
    }

    private int minMsetNum = 3;

    public int getMinMsetNum() {
        return minMsetNum;
    }

    public void setMinMsetNum(int minMsetNum) {
        this.minMsetNum = minMsetNum;
    }

    public String getLibVersion() {
        return libVersion;
    }

    public void setLibVersion(String libVersion) {
        this.libVersion = libVersion;
    }

    public String getMetaLevel() {
        return metaLevel;
    }

    public void setMetaLevel(String metaLevel) {
        this.metaLevel = metaLevel;
    }

    public String getCombinelevel() {
        return combinelevel;
    }

    public void setCombinelevel(String combinelevel) {
        this.combinelevel = combinelevel;
    }

    public String getPvalmethod() {
        return pvalmethod;
    }

    public void setPvalmethod(String pvalmethod) {
        this.pvalmethod = pvalmethod;
    }

    public String getEsmethod() {
        return esmethod;
    }

    public void setEsmethod(String esmethod) {
        this.esmethod = esmethod;
    }

    public String getRankmetric() {
        return rankmetric;
    }

    public void setRankmetric(String rankmetric) {
        this.rankmetric = rankmetric;
    }

    public boolean isShowNetwork() {
        if ("gsea".equals(poolAlgOpt)) {
            showNetwork = false;
        } else {
            showNetwork = true;
        }
        return showNetwork;
    }

    public void setShowNetwork(boolean showNetwork) {
        this.showNetwork = showNetwork;
    }

    @JsonIgnore
    private ListDataModel<MummiBean> listModel = null;
    @JsonIgnore
    private ListDataModel<GseaBean> listGSEAModel = null;

    public String performMetaPathAnalysis() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_performMetaPathAnalysis(this);
            return null;
        }

        RConnection RC = sb.getRConnection();
        mplb.setmDataSets(null);
        //RMetaPathUtils.resumeGlobalEnvir(RC);

        if (pathAlgOpt.equals("mummichog")) {
            if (pathAlgVersion.equalsIgnoreCase("v2")) {
                if (RMetaPathUtils.checkAllRT(sb).equals("false")) {
                    sb.addMessage("Error", "Cannout perform V2 if not all studies contain retention time information!");
                    return null;
                }
            }
            RMetaPathUtils.setPeakEnrichMethod(sb, "mum", pathAlgVersion);
        } else {
            RMetaPathUtils.setPeakEnrichMethod(sb, "gsea", pathAlgVersion);
        }

        resOK = RMetaPathUtils.performMetaMummiAnalysis(sb, lib, libVersion, minMsetNum, permuNUm,
                "pathway", combinelevel, pvalmethod, esmethod, rankmetric, false, pvalCutoff);

        if (resOK) {
            jrd.record_performMetaPathAnalysis(this);
            double pvalCutoffPlot = mplb.getPvalCutoff();
            plotOK = RMetaPathUtils.plotPathwayMetaAnalysis(sb, sb.getNewImage("meta_bubble"),
                    plotType, pvalCutoffPlot, overlap, maxPaths, "png", 300);
            //System.out.println("Meta-peak plotting finshed or not ?" + plotOK);
            if (plotOK) {
                populateResBeans();
                return "Meta-Analysis Results";
            } else {
                wb.getCalledWorkflowsError().add("Pathway-level integration");
                return null;
            }
        }
        return null;
    }

    public String performMetaCpdAnalysis() {

        //RMetaPathUtils.resumeGlobalEnvir(RC);
        boolean matchedfeats;
        RConnection RC = sb.getRConnection();
        if (universe.equals("allc") || universe.equals("allec")) {
            matchedfeats = false;
        } else {
            matchedfeats = true;
        }

        if (universe.equals("allc") || universe.equals("matchedc")) {
            metaLevel = "cpd";
        } else {
            metaLevel = "ec";
        }

        if (pathAlgVersion.equalsIgnoreCase("v2")) {
            if (RMetaPathUtils.checkAllRT(sb).equals("false")) {
                sb.addMessage("Error", "Cannout perform V2 if not all studies contain retention time information!");
            }
        }

        if (cpdAlgOpt.equals("mummichog")) {
            RMetaPathUtils.setPeakEnrichMethod(sb, "mum", pathAlgVersion);
        } else {
            RMetaPathUtils.setPeakEnrichMethod(sb, "gsea", pathAlgVersion);
        }

        resOK = RMetaPathUtils.performMetaMummiAnalysis(sb, lib, libVersion, minMsetNum, permuNUm,
                metaLevel, combinelevel, pvalmethod, esmethod, rankmetric, matchedfeats, pvalCutoff);

        if (resOK) {
            if (cpdAlgOpt.equals("mummichog")) {

                String imgName = sb.getNewImage("peaks_to_paths");
                REnrichUtils.plotPeaks(sb, imgName, "mummichog", "png", 150);

                ArrayList<MummiBean> mummiBeans = new ArrayList();
                String[] rownames = REnrichUtils.getMummiPathNames(RC);
                String[] keggLnks = rownames;
                double[][] mat = REnrichUtils.getMummiMat(RC);
                MummiBean mb;
                for (int i = 0; i < rownames.length; i++) {
                    mb = new MummiBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][5], mat[i][4], mat[i][6], mat[i][3], mat[i][7]);
                    mummiBeans.add(mb);
                }

                listModel = new ListDataModel(mummiBeans);
                mumb.setListModel(listModel);

                return "mummires";

            } else if (cpdAlgOpt.equals("gsea")) {

                String imgName = sb.getNewImage("peaks_to_paths_gsea");
                REnrichUtils.plotPeaks(sb, imgName, "gsea", "png", 150);

                ArrayList<GseaBean> gseaBeans = new ArrayList();
                String[] rownames = REnrichUtils.getMummiPathNames(RC);
                String[] keggLnks = rownames;
                double[][] mat = REnrichUtils.getMummiMat(RC);

                GseaBean gb;
                for (int i = 0; i < rownames.length; i++) {
                    gb = new GseaBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], mat[i][2], mat[i][3], mat[i][4]);
                    gseaBeans.add(gb);
                }

                listGSEAModel = new ListDataModel(gseaBeans);
                mumb.setListGSEAModel(listGSEAModel);

                return "gseapkview";
            }
        }
        return null;
    }

    public String performMetaPoolAnalysis() {

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_performMetaPoolAnalysis(this);
            return null;
        }
        //RMetaPathUtils.resumeGlobalEnvir(RC);
        RConnection RC = sb.getRConnection();
        if (poolAlgVersion.equalsIgnoreCase("v2")) {
            if (RMetaPathUtils.checkAllRT(sb).equals("false")) {
                sb.addMessage("Error", "Cannout perform V2 if not all studies contain retention time information!");
            }
        }

        if (poolAlgOpt.equals("mummichog")) {
            RMetaPathUtils.setPeakEnrichMethod(sb, "mum", poolAlgVersion);
        } else {
            RMetaPathUtils.setPeakEnrichMethod(sb, "gsea", poolAlgVersion);
        }

        resOK = RMetaPathUtils.performMetaMummiAnalysis(sb, lib, libVersion, minMsetNum, permuNUm,
                "cpd", "pool", pvalmethod, esmethod, rankmetric, false, pvalCutoff);

        if (resOK) {
            jrd.record_performMetaPoolAnalysis(this);
            //sb.addNaviTrack("Pooling peaks", null);
            resOK2 = resOK;

            if (poolAlgOpt.equals("mummichog")) {

                String imgName = sb.getNewImage("peaks_to_paths");
                REnrichUtils.plotPeaks(sb, imgName, "mummichog", "png", 150);

                ArrayList<MummiBean> mummiBeans = new ArrayList();
                String[] rownames = REnrichUtils.getMummiPathNames(RC);
                String[] keggLnks = rownames;
                double[][] mat = REnrichUtils.getMummiMat(RC);
                MummiBean mb;
                for (int i = 0; i < rownames.length; i++) {
                    mb = new MummiBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][5], mat[i][4], mat[i][6], mat[i][3], mat[i][7]);
                    mummiBeans.add(mb);
                }

                listModel = new ListDataModel(mummiBeans);
                mumb.setListModel(listModel);

                return "mummires";

            } else if (poolAlgOpt.equals("gsea")) {

                String imgName = sb.getNewImage("peaks_to_paths_gsea");
                REnrichUtils.plotPeaks(sb, imgName, "gsea", "png", 150);

                ArrayList<GseaBean> gseaBeans = new ArrayList();
                String[] rownames = REnrichUtils.getMummiPathNames(RC);
                String[] keggLnks = rownames;
                double[][] mat = REnrichUtils.getMummiMat(RC);

                GseaBean gb;
                for (int i = 0; i < rownames.length; i++) {
                    gb = new GseaBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], mat[i][2], mat[i][3], mat[i][4]);
                    gseaBeans.add(gb);
                }

                listGSEAModel = new ListDataModel(gseaBeans);
                mumb.setListGSEAModel(listGSEAModel);

                return "gseapkview";
            }
        } else {
            String msg = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "There is something wrong with the MS Peaks Meta-Analysis: " + msg);
        }
        return null;
    }

    // Plotting section
    public String getPlotType() {
        return plotType;
    }

    public void setPlotType(String plotType) {
        this.plotType = plotType;
    }

    public double getOverlap() {
        return overlap;
    }

    public void setOverlap(double overlap) {
        this.overlap = overlap;
    }

    public int getMaxPaths() {
        return maxPaths;
    }

    public void setMaxPaths(int maxPaths) {
        this.maxPaths = maxPaths;
    }
    private String plotType = "bubble";

    private double overlap = 0.25;
    private int maxPaths = 15;
    private boolean plotOK;

    public boolean isPlotOK() {
        return plotOK;
    }

    public void setPlotOK(boolean plotOK) {
        this.plotOK = plotOK;
    }

    public void updateplotPathwayMeta() {
        double pvalCutoffPlot = mplb.getPvalCutoff();
        plotOK = RMetaPathUtils.plotPathwayMetaAnalysis(sb, sb.getNewImage("meta_bubble"),
                plotType, pvalCutoffPlot, overlap, maxPaths, "png", 300);
        //NOTE: PLEASE DO NOT DELETE FOLLOWING LINE, OTHER WISE THE TABLE WILL NOT BE UDPATED !!!
        //Note updating the plot should not update the table. This is different thing! Table show all, plot show part
        //FacesContext.getCurrentInstance().getViewRoot().getViewMap().remove("pMetaResBean");
    }

    public String updatePlotPathMetaHigRes() {
        RConnection RC = sb.getRConnection();
        double pvalCutoffPlot = mplb.getPvalCutoff();
        int dpi = sb.getDpiOpt();
        String format = sb.getFormatOpt();
        if (format.equals("pdf") || format.equals("tiff")) {
            dpi = 150;
        }
        String sizeop = sb.getSizeOpt();
        int width;
        width = switch (sizeop) {
            case "NA" ->
                6;
            case "7" ->
                7;
            case "12" ->
                12;
            default ->
                8;
        };

        double height = width * 0.6;
        String fileNM = RMetaPathUtils.plotPathMetaUpdate(RC,
                sb.getNewImage("meta_bubble"),
                plotType, pvalCutoffPlot,
                overlap, maxPaths, dpi,
                format, width, height);

        return fileNM;
    }

    public String performNetworkAnalysis() {
        RConnection RC = sb.getRConnection();
        int res;

        try {
            res = RMetaPathUtils.performNetworkAnal(RC);
            if (res == 1) {
                return "MnetView";
            }
        } catch (Exception e) {
            sb.addMessage("error", "Failed to perform the network analysis");
            return null;
        }

        if (res != 1) {
            sb.addMessage("error", "Failed to perform the network analysis");
            return null;
        }
        return null;
    }

    public String getMetaResImage() throws IOException {
        String fileNM = sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("meta_bubble") + "dpi300.png";
        return (fileNM);
    }

    private List<MetaResultBean> resBeans = new ArrayList();
    private List<ColumnModel> columns = new ArrayList();

    public void populateResBeans() {
        resBeans.clear();
        columns.clear();
        RConnection RC = sb.getRConnection();

        String[] columnKeys = RMetaPathUtils.getMetaPathResColNames(RC);
        String[] ids = RMetaPathUtils.getMetaPathNMs(RC);
        columns.add(new ColumnModel("Pathway", "Pathway", "string")); //add entrez ID column
        for (String columnKey : columnKeys) {
            columns.add(new ColumnModel(columnKey, columnKey, "double"));
        }

        if (ids != null && ids.length > 0) {
            for (int i = 0; i < ids.length; i++) {
                MetaResultBean rb = new MetaResultBean(ids[i]);
                rb.setName(ids[i]);
                String[] resItem = RMetaPathUtils.GetMetaPathResultItem(RC, i + 1);
                rb.setValue("Pathway", ids[i]);
                for (int m = 0; m < columnKeys.length; m++) {
                    rb.setValue(columnKeys[m], resItem[m] + "");
                }
                resBeans.add(rb);
            }
        }
    }

    public List<MetaResultBean> getResBeans() {
        return resBeans;
    }

    public List<ColumnModel> getColumns() {
        return columns;
    }

}
