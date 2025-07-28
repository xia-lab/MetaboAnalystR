/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.mummichog;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.GseaBean;
import pro.metaboanalyst.models.IntegBean;
import pro.metaboanalyst.models.MetSetBean;
import pro.metaboanalyst.models.MummiBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.spectra.SpectraParamBean;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.model.DefaultStreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.ListDataModel;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.Semaphore;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import pro.metaboanalyst.rwrappers.RMetaPathUtils;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author jianguox
 */
@SessionScoped
@Named("mummiAnalBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class MummiAnalBean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @Inject
    private WorkflowBean wb;
    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    @JsonIgnore
    @Inject
    private SpectraParamBean spb;

    private String algOptSingle = "mum";
    private String enrichOpt = "fisher";
    private String filterOpt = "filtered";
    private String[] algOpts = {"mum"}; //"mum" and/or "gsea"
    private String pvalCutoffOpt = "default";
    private boolean disabledMumPval = false;
    private boolean disabledMum = false;
    private boolean disabledGsea = false;
    private boolean multigroups = false;
    private boolean disabledV2 = false;
    private double defaultCutoff = 0.1; // need to be updated to p value cutoff with 10%
    private double pvalCutoff = 0.001;
    private boolean doMumFilter = true;
    private int minMsetNum = 3;
    private String mumVersion = "v2";
    private String libVersion = "current";
    private String pathDBOpt = "hsa_mfn";
    @JsonIgnore
    private ListDataModel<MummiBean> listModel = null;
    @JsonIgnore
    private ListDataModel<GseaBean> listGSEAModel = null;
    @JsonIgnore
    private ListDataModel<IntegBean> listIntegModel = null;
    private double instrumentOpt = 5;
    private String heatmapName = "";
    private String analOption = "scatter";
    private boolean moduleSwitch = false;
    private SelectItem[] pathLibOpt = null;

    public String getAlgOptSingle() {
        return algOptSingle;
    }

    public void setAlgOptSingle(String algOptSingle) {
        this.algOptSingle = algOptSingle;
    }

    public String getEnrichOpt() {
        return enrichOpt;
    }

    public void setEnrichOpt(String enrichOpt) {
        this.enrichOpt = enrichOpt;
    }

    public String getFilterOpt() {
        return filterOpt;
    }

    public void setFilterOpt(String filterOpt) {
        this.filterOpt = filterOpt;
    }

    public String[] getAlgOpts() {
        return algOpts;
    }

    public void setAlgOpts(String[] algOpts) {
        this.algOpts = algOpts;
    }

    public String getPvalCutoffOpt() {
        return pvalCutoffOpt;
    }

    public void setPvalCutoffOpt(String pvalCutoffOpt) {
        this.pvalCutoffOpt = pvalCutoffOpt;
    }

    public boolean isDisabledMumPval() {
        return disabledMumPval;
    }

    public void setDisabledMumPval(boolean disabledMumPval) {
        this.disabledMumPval = disabledMumPval;
    }

    public boolean isDisabledMum() {
        return disabledMum;
    }

    public void setDisabledMum(boolean disabledMum) {
        this.disabledMum = disabledMum;
    }

    public boolean isDisabledGsea() {
        return disabledGsea;
    }

    public void setDisabledGsea(boolean disabledGsea) {
        this.disabledGsea = disabledGsea;
    }

    public boolean isMultigroups() {
        return multigroups;
    }

    public void setMultigroups(boolean multigroups) {
        this.multigroups = multigroups;
    }

    public String proceed2Net() {
        if (pathDBOpt.endsWith("mset")) {
            sb.addMessage("Error", "The network has not been created for metabolite set libraries yet. Please wait for the next release");
            return null;
        }
        return "Metabolic network";
    }

    public String proceed2EnrichNet() {
        int res = RDataUtils.prepareEnrichNet(sb.getRConnection(), "enrichNet_" + algOptSingle, "mixed", algOptSingle);
        if (res == 0) {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Failed to compute enrichment network: " + err);
        }
        sb.setEnrNetSavedInit(false);
        return "EnrichNetwork";
    }

    public void showPathDialog() {
        Map<String, String> params = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap();
        String latt = params.get("pathname");
        //System.out.println(latt);
        sb.setCurrentPathName(latt);
        PrimeFaces.current().executeScript("PF('msetDialog').show()");
    }

    public boolean isDisabledV2() {
        return disabledV2;
    }

    public void setDisabledV2(boolean disabledV2) {
        this.disabledV2 = disabledV2;
    }

    public double getDefaultCutoff() {
        return defaultCutoff;
    }

    public void setDefaultCutoff(double defaultCutoff) {
        this.defaultCutoff = defaultCutoff;
    }

    public double getPvalCutoff() {
        return pvalCutoff;
    }

    public void setPvalCutoff(double pvalCutoff) {
        this.pvalCutoff = pvalCutoff;
    }

    @JsonIgnore
    public MetSetBean[] getCurrentPathSet() {
        String pathname = sb.getCurrentPathName();
        String[] details = REnrichUtils.getMummichogHTMLPathSet(sb.getRConnection(), pathname);
        ArrayList<MetSetBean> libVec = new ArrayList();
        libVec.add(new MetSetBean(details[0], details[1], ""));
        return libVec.toArray(MetSetBean[]::new);
    }

    public boolean isDoMumFilter() {
        return doMumFilter;
    }

    public void setDoMumFilter(boolean doMumFilter) {
        this.doMumFilter = doMumFilter;
    }

    public int getMinMsetNum() {
        return minMsetNum;
    }

    public void setMinMsetNum(int minMsetNum) {
        this.minMsetNum = minMsetNum;
    }

    public String getMumVersion() {
        return mumVersion;
    }

    public void setMumVersion(String mumVersion) {
        this.mumVersion = mumVersion;
    }

    public String getLibVersion() {
        return libVersion;
    }

    public void setLibVersion(String libVersion) {
        this.libVersion = libVersion;
    }

    public String getPathDBOpt() {
        return pathDBOpt;
    }

    public void setPathDBOpt(String pathDBOpt) {
        this.pathDBOpt = pathDBOpt;
    }

    @JsonIgnore
    public SelectItem[] getPathLibOpt() {
        if (pathLibOpt == null) {
            setupPathLibOpt();
        }
        return pathLibOpt;
    }

    private void setupPathLibOpt() {
        String[] lbls = RDataUtils.getOrgMummichogLbl(sb.getRConnection());
        int rowLen = lbls.length;
        pathLibOpt = new SelectItem[rowLen];
        String[] vals = RDataUtils.getOrgMummichogVal(sb.getRConnection());
        for (int i = 0; i < rowLen; i++) {
            pathLibOpt[i] = new SelectItem(vals[i], lbls[i]);
        }
    }

    public boolean isShowMumNetwork() {
        return !pathDBOpt.endsWith("mset");
    }

    @JsonIgnore
    public ListDataModel<MummiBean> getMummiBeans() {
        if (listModel == null) {
            algOpts = new String[1];
            algOpts[0] = "mum";
            RMetaPathUtils.setPeakEnrichMethod(sb, "mum", mumVersion);
            populateResTable("mum");
        }
        return listModel;
    }

    public void setListModel(ListDataModel<MummiBean> listModel) {
        this.listModel = listModel;
    }

    @JsonIgnore
    public ListDataModel<GseaBean> getGseaBeans() {
        if (listGSEAModel == null) {
            algOpts = new String[1];
            algOpts[0] = "gsea";
            RMetaPathUtils.setPeakEnrichMethod(sb, "gsea", mumVersion);
            populateResTable("gsea");
        }
        return listGSEAModel;
    }

    public void setListGSEAModel(ListDataModel<GseaBean> listGSEAModel) {
        this.listGSEAModel = listGSEAModel;
    }

    @JsonIgnore
    public ListDataModel<IntegBean> getIntegBeans() {
        if (listIntegModel == null) {
            algOpts = new String[2];
            algOpts[0] = "gsea";
            algOpts[1] = "mum";
            RMetaPathUtils.setPeakEnrichMethod(sb, "integ", mumVersion);
            populateResTable("integ");
        }
        return listIntegModel;
    }

    public double getInstrumentOpt() {
        return instrumentOpt;
    }

    public void setInstrumentOpt(double instrumentOpt) {
        this.instrumentOpt = instrumentOpt;
    }

    public String performPeaks2Fun() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            jrd.record_performPeaks2Fun(this);
            return null;
        }
        if (algOpts.length == 0) {
            sb.addMessage("Error", "Select algorithm(s) to perform enrichment analysis!");
            return null;
        }
        jrd.record_performPeaks2Fun(this);
        wb.getCalledWorkflows().add("Heatmap");
        //throttling
        Semaphore semphore = sb.getPermissionToStart();
        if (semphore == null) {
            return null;
        }

        RConnection RC = sb.getRConnection();
        String nextpage = null;

        String version = mumVersion;

        if (isModuleSwitch()) {
            String TmpMSModeOpt = spb.getPolarity();
            RDataUtils.setInstrumentParams(sb, instrumentOpt, TmpMSModeOpt, "yes", 0.02);
        }

        if (analOption.equals("heatmap")) {

            if ((algOpts.length > 1)) {
                sb.addMessage("Error", "Heatmap visualization is only available for mummichog analysis!");
                return null;
            }

            if (!algOpts[0].equals("mum")) {
                sb.addMessage("Error", "Heatmap visualization is only available for mummichog analysis!");
                return null;
            }

            RDataUtils.setPeakEnrichMethod(sb, "mum", version);
            String[] libNmArr = pathDBOpt.split("_", 0);
            sb.setOrg(libNmArr[0]);
            RDataUtils.setOrganism(sb, libNmArr[0]);
            heatmapName = "metaboanalyst_heatmap_" + sb.getFileCount() + ".json";
            sb.setHeatmapType("mummichog");
            if (sb.getDataType().equals("mass_table")) {
                REnrichUtils.createHeatmapJson(sb, pathDBOpt, libVersion, minMsetNum, heatmapName, filterOpt, version);
            } else {
                REnrichUtils.createListHeatmapJson(sb, pathDBOpt, libVersion, minMsetNum, heatmapName, filterOpt, version);
            }

            nextpage = "Heatmap view";

        } else if (algOpts.length > 1) {

            RDataUtils.setPeakEnrichMethod(sb, "integ", version);
            nextpage = "peakintegview";
            String imgName = sb.getNewImage("integ_peaks");

            if (sb.getDataType().equals("mass_table")) {
                boolean res = REnrichUtils.convertTableToPeakList(sb);
                if (!res) {
                    String msg = RDataUtils.getErrMsg(sb.getRConnection());
                    sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
                }
            }

            if (REnrichUtils.setupMummichogPval(sb, pvalCutoff)) {

                if (REnrichUtils.performPSEA(sb, pathDBOpt, libVersion, minMsetNum)) {
                    // first create plot
                    REnrichUtils.plotPSEAIntegPaths(sb, imgName, "png", 150);

                    ArrayList<IntegBean> integBeans = new ArrayList();
                    String[] rownames = REnrichUtils.getMummiPathNames(RC);
                    String[] keggLnks = rownames;

                    double[][] mat = REnrichUtils.getMummiMat(RC);
                    IntegBean ib;
                    for (int i = 0; i < rownames.length; i++) {
                        ib = new IntegBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5]);
                        integBeans.add(ib);
                    }

                    listIntegModel = new ListDataModel(integBeans);

                } else {
                    String msg = RDataUtils.getErrMsg(sb.getRConnection());
                    sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
                }
            }

        } else if (algOpts[0].equals("mum")) {

            RDataUtils.setPeakEnrichMethod(sb, "mum", version);
            nextpage = "mummires";
            String imgName = sb.getNewImage("peaks_to_paths");

            if (sb.getDataType().equals("mass_table")) {
                boolean res = REnrichUtils.convertTableToPeakList(sb);
                if (!res) {
                    String msg = RDataUtils.getErrMsg(sb.getRConnection());
                    sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
                }
            }
            if (REnrichUtils.setupMummichogPval(sb, pvalCutoff)) {
                if (REnrichUtils.performPSEA(sb, pathDBOpt, libVersion, minMsetNum)) {

                    // first create plot
                    REnrichUtils.plotPeaks(sb, imgName, "mummichog", "png", 150);

                    ArrayList<MummiBean> mummiBeans = new ArrayList();
                    String[] rownames = REnrichUtils.getMummiPathNames(RC);
                    String[] keggLnks = rownames;
                    double[][] mat = REnrichUtils.getMummiMat(RC);
                    MummiBean mb;
                    for (int i = 0; i < rownames.length; i++) {
                        mb = new MummiBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][5], mat[i][4], mat[i][8], mat[i][3], mat[i][9]);
                        mummiBeans.add(mb);
                    }
                    listModel = new ListDataModel(mummiBeans);
                } else {
                    String msg = RDataUtils.getErrMsg(sb.getRConnection());
                    sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
                }
            } else {
                String msg = RDataUtils.getErrMsg(sb.getRConnection());
                sb.addMessage("Error", msg + "You can click the Submit button again to accept recommended p value.");
            }

        } else {
            RDataUtils.setPeakEnrichMethod(sb, algOpts[0], version);
            nextpage = "gseapkview";
            String imgName = sb.getNewImage("peaks_to_paths_gsea");

            if (sb.getDataType().equals("mass_table")) {
                boolean res = REnrichUtils.convertTableToPeakList(sb);
                if (!res) {
                    String msg = RDataUtils.getErrMsg(sb.getRConnection());
                    sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
                }
            }

            if (REnrichUtils.performPSEA(sb, pathDBOpt, libVersion, minMsetNum)) {

                // first create plot
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
            } else {
                String msg = RDataUtils.getErrMsg(sb.getRConnection());
                sb.addMessage("Error", "There is something wrong with the MS Peaks to Paths analysis: " + msg);
            }
        }

        //don't forget
        semphore.release();

        return nextpage;
    }

    @JsonIgnore
    public DefaultStreamedContent getPathEnrichFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/mummichog_pathway_enrichment_mummichog.csv");
    }

    @JsonIgnore
    public DefaultStreamedContent getGseaPathEnrichFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/mummichog_pathway_enrichment_gsea.csv");
    }

    @JsonIgnore
    public DefaultStreamedContent getIntegPathEnrichFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/mummicho_pathway_enrichment_integ.csv");
    }

    @JsonIgnore
    public DefaultStreamedContent getCmpdHitFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/mummichog_matched_compound_all.csv");
    }

    public String getHeatmapName() {
        return heatmapName;
    }

    public void setHeatmapName(String heatmapName) {
        this.heatmapName = heatmapName;
    }

    public String getAnalOption() {
        return analOption;
    }

    public void setAnalOption(String analOption) {
        this.analOption = analOption;
    }

    public boolean isModuleSwitch() {
        return moduleSwitch;
    }

    public void setModuleSwitch(boolean moduleSwitch) {
        this.moduleSwitch = moduleSwitch;
    }

    private boolean populateResTable(String type) {
        RConnection RC = sb.getRConnection();

        if (type.equals("mum")) {
            System.out.println("REnrichUtils.checkMumExists" + REnrichUtils.checkMumExists(RC, "mum"));
            if (REnrichUtils.checkMumExists(RC, "mum") == 0) {
                listModel = new ListDataModel<>(new ArrayList<>());
                return false;
            }
            ArrayList<MummiBean> mummiBeans = new ArrayList();
            String[] rownames = REnrichUtils.getMummiPathNames(RC);
            String[] keggLnks = rownames;
            double[][] mat = REnrichUtils.getMummiMat(RC);
            MummiBean mb;
            for (int i = 0; i < rownames.length; i++) {
                mb = new MummiBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][5], mat[i][4], mat[i][8], mat[i][3], mat[i][9]);
                mummiBeans.add(mb);
            }
            listModel = new ListDataModel(mummiBeans);
        } else if (type.equals("gsea")) {
            if (REnrichUtils.checkMumExists(RC, "gsea") == 0) {
                listGSEAModel = new ListDataModel<>(new ArrayList<>());
                return false;
            }
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
        } else {
            if (REnrichUtils.checkMumExists(RC, "integ") == 0) {
                listIntegModel = new ListDataModel<>(new ArrayList<>());
                return false;
            }
            ArrayList<IntegBean> integBeans = new ArrayList();
            String[] rownames = REnrichUtils.getMummiPathNames(RC);
            String[] keggLnks = rownames;
            double[][] mat = REnrichUtils.getMummiMat(RC);
            IntegBean ib;
            for (int i = 0; i < rownames.length; i++) {
                ib = new IntegBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5]);
                integBeans.add(ib);
            }
            listIntegModel = new ListDataModel(integBeans);
        }
        return true;
    }

    private boolean switch4masslist = true, switch4masstable = true;

    public boolean isSwitch4masslist() {
        return switch4masslist;
    }

    public void setSwitch4masslist(boolean switch4masslist) {
        this.switch4masslist = switch4masslist;
    }

    public boolean isSwitch4masstable() {
        return switch4masstable;
    }

    public void setSwitch4masstable(boolean switch4masstable) {
        this.switch4masstable = switch4masstable;
    }

    public boolean isMummiBeansEmpty() {
        getMummiBeans();
        return listModel == null || listModel.getRowCount() == 0;
    }

    public boolean isGseaBeansEmpty() {
        getGseaBeans();
        return listGSEAModel == null || listGSEAModel.getRowCount() == 0;
    }

    public boolean isIntegBeansEmpty() {
        getIntegBeans();
        return listIntegModel == null || listIntegModel.getRowCount() == 0;
    }

}
