/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.IOException;
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.ActionEvent;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import org.primefaces.PrimeFaces;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.controllers.general.NormBean;
import pro.metaboanalyst.controllers.general.ProcessBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.enrich.MappingBean;
import pro.metaboanalyst.controllers.enrich.MsetBean;
import pro.metaboanalyst.controllers.enrich.PathBean;
import pro.metaboanalyst.controllers.meta.MetaLoadBean;
import pro.metaboanalyst.controllers.meta.MetaStatBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.mnet.MnetMapBean;
import pro.metaboanalyst.controllers.mnet.MnetResBean;
import pro.metaboanalyst.controllers.multifac.Aov2Bean;
import pro.metaboanalyst.controllers.multifac.AscaBean;
import pro.metaboanalyst.controllers.multifac.HeatMap2Bean;
import pro.metaboanalyst.controllers.multifac.LimmaBean;
import pro.metaboanalyst.controllers.multifac.LivePCABean;
import pro.metaboanalyst.controllers.multifac.MebaBean;
import pro.metaboanalyst.controllers.multifac.MetaHeatmapBean;
import pro.metaboanalyst.controllers.multifac.MetaProcBean;
import pro.metaboanalyst.controllers.multifac.MultiCorrBean;
import pro.metaboanalyst.controllers.multifac.MultiRfBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.controllers.mummichog.PeakCustomBean;
import pro.metaboanalyst.controllers.stats.AnalysisBean;
import pro.metaboanalyst.controllers.stats.ClusterBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.spectra.SpectraUploadBean;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.controllers.stats.UnivBean;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.lts.FireProjectBean;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.lts.FunctionInfo;
import pro.metaboanalyst.models.FeatureBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RMetaPathUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;

/**
 * @author zgy
 */
@Named("workflowView")
@ViewScoped
public class WorkflowView implements Serializable {

    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private FireBase fb;

    @JsonIgnore
    @Inject
    private FireBaseController fbc;

    @JsonIgnore
    @Inject
    private FireProjectBean fpb;

    @JsonIgnore
    @Inject
    private RocAnalBean rocb;

    @JsonIgnore
    @Inject
    private MnetMapBean mnmb;

    @JsonIgnore
    @Inject
    private MappingBean mpb;

    @JsonIgnore
    @Inject
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private DatabaseClient dbc;

    @JsonIgnore
    @Inject
    private DiagramView dv;

    private int activeIndex = 0;

    private String enrichOpt = "ORA";

    public int getActiveIndex() {
        return activeIndex;
    }

    public void setActiveIndex(int activeIndex) {
        this.activeIndex = activeIndex;
    }

    public String getEnrichOpt() {
        return enrichOpt;
    }

    public void setEnrichOpt(String enrichOpt) {
        this.enrichOpt = enrichOpt;
    }

    private String refactoredName = "";

    public int executeWorkflow(String str) throws Exception {
        //String str = "Using the split() method to convert a string to array in Java";

        int success = 1;
        String[] arr = str.split("; ");
        refactoredName = arr[0].replaceAll("_", "");
        success = executeStat(arr);

        stopStatusCheck = false;
        //sb.addMessage("Info", "Analysis complete!");
        if (success == 1) {
            wb.getCalledWorkflows().add(arr[0]);
            RCenter.recordMessage(sb.getRConnection(), refactoredName + " - <b>passed</b>");

        } else {
            //wb.getCalledWorkflowsError().add(arr[0]);
            RCenter.recordMessage(sb.getRConnection(), arr[0] + " - <b>failed</b>");
        }

        return success;
    }

    public int executeStat(String[] arr) throws Exception {
        boolean success = true;
        for (String arr1 : arr) {
            String func = arr1.replaceAll("_[0-9]+$", "");

            try {
                switch (func) {
                    case "List" -> {
                        // Align with beans: set upload type FIRST
                        sb.setUploadType("list"); // like EnrichUploadBean/PathUploadBean.  // :contentReference[oaicite:2]{index=2} :contentReference[oaicite:3]{index=3}

                        // Convert list file -> qVec and set map data in R
                        String[] qVec = DataUtils.readListFileToNames(sb.getCurrentUser().getHomeDir(), "datalist.csv");
                        if (qVec.length == 0) {
                            sb.addMessage("Error", "Your list file appears to be empty.");
                            return 0;
                        }
                        RDataUtils.setMapData(sb.getRConnection(), qVec); // exactly like the upload beans.         // :contentReference[oaicite:4]{index=4} :contentReference[oaicite:5]{index=5}

                        // Choose the cross-reference routine. If you track lipid vs met feature type in state,
                        // reuse it here; otherwise default to the generic exact matcher (same as PathUploadBean).
                        // Example using a flag you maintain elsewhere:
                        boolean isLipid = "lipid".equalsIgnoreCase(sb.getFeatType());
                        if (isLipid) {
                            SearchUtils.crossReferenceExactLipid(sb, sb.getCmpdIDType());          // :contentReference[oaicite:6]{index=6}
                        } else {
                            SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());               // :contentReference[oaicite:7]{index=7} :contentReference[oaicite:8]{index=8}
                        }
                    }
                    case "Missing Values" -> {
                        ProcessBean pb = (ProcessBean) getBeanInstance("pb");
                        boolean resBool = checkWorkflowContained("performMissingImpute");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        pb.performMissingImpute();
                    }
                    case "Save Project" -> {
                        fbc.saveProject("workflow");
                    }
                    case "Data Processing", "Sanity Check", "Sanity Check Intensity", "Sanity Check Peak" -> {
                        sb.addNaviTrack("Data check", "/Secure/process/SanityCheck.xhtml");
                        ProcessBean pb = (ProcessBean) getBeanInstance("pb");
                        pb.setSanityChecked(false);
                        pb.performSanityCheck();
                        pb.skipButton_action_default();
                        if (!sb.isMissingDisabled()) {
                            boolean resBool = checkWorkflowContained("performMissingImpute");
                            if (!resBool && wb.isReloadingWorkflow()) {
                                return 2;
                            }
                            pb.performMissingImpute();
                        }
                    }
                    case "Filtering", "Filtering_Table", "Filtering Intensity" -> {
                        ProcessBean pb = (ProcessBean) getBeanInstance("pb");
                        if (pb != null) {
                            boolean resBool = checkWorkflowContained("Filtering");
                            if (!resBool && wb.isReloadingWorkflow()) {
                                return 2;
                            }
                            pb.filterButton_action();

                            if (!pb.isFiltered()) {
                                success = false;
                                sb.addNaviTrack("Data filter", "/Secure/process/FilterView.xhtml", false);
                            } else {
                                sb.addNaviTrack("Data filter", "/Secure/process/FilterView.xhtml", true);
                            }
                        }
                    }
                    case "Normalization", "Normalization_Table", "Normalization Intensity" -> {
                        boolean resBool = checkWorkflowContained("Normalization");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        NormBean nb = (NormBean) getBeanInstance("nb");
                        nb.preparePrenormData();
                        if (nb.getRowNormOpt().equals("SpecNorm") && nb.isSpecNormSpecifed()) {
                            RDataUtils.setSampleNormFactor(sb.getRConnection(), wb.getSampleBeans());
                        }
                        nb.performDataNormalization();
                        if (!nb.isNormPerformed()) {
                            sb.addNaviTrack("Normalization", "/Secure/process/NormalizationView.xhtml", false);
                        } else {
                            sb.addNaviTrack("Normalization", "/Secure/process/NormalizationView.xhtml", true);
                        }

                    }
                    case "Analysis Selection", "Visual Analytics" -> {
                        sb.addNaviTrack("Statistics", "/Secure/analysis/AnalysisView.xhtml");

                    }
                    case "Visual Analytics mf" -> {
                        sb.addNaviTrack("Methods Overview", "/Secure/multifac/MultifacOverview.xhtml");

                    }
                    case "Volcano" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        UnivBean ub = (UnivBean) getBeanInstance("ub");
                        ub.vcButton_action();
                    }
                    case "PCA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("PCA", "/Secure/analysis/PCAView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultPCA();
                    }
                    case "iPCA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("PCA", "/Secure/multifac/LivePCAView.xhtml");

                        LivePCABean lp = (LivePCABean) getBeanInstance("lp");
                        lp.initPCA3D();
                    }
                    case "ANOVA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("ANOVA", "/Secure/analysis/AnovaView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultANOVA();
                    }
                    case "Fold change" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Fold change", "/Secure/analysis/FoldChangeView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultFC();
                    }
                    case "T-test" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("T-test", "/Secure/analysis/TtestView.xhtml");

                        UnivBean ub = (UnivBean) getBeanInstance("ub");
                        ub.ttButton_action();
                    }
                    case "Pattern Search" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("PatternHunter", "/Secure/analysis/PatternView.xhtml");

                        UnivBean ub = (UnivBean) getBeanInstance("ub");
                        ub.ptnBtn_action();
                    }
                    case "Correlation Heatmap" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Correlations", "/Secure/analysis/CorrelationView.xhtml");

                        UnivBean ub = (UnivBean) getBeanInstance("ub");
                        ub.doDefaultStaticCorrelation();
                        ub.corrBtn_action();
                    }
                    case "PLSDA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("PLSDA", "/Secure/analysis/PLSDAView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultPLSDA();
                    }
                    case "sPLSDA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("sPLSDA", "/Secure/analysis/SparsePLSDAView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultSPLSDA();
                    }
                    case "OrthoPLSDA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("OrthoPLSDA", "/Secure/analysis/OrthoPLSDAView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultOPLSDA();
                    }
                    case "SAM" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("SAM", "/Secure/analysis/SAMView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultSAM();
                    }
                    case "EBAM" -> {

                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("EBAM", "/Secure/analysis/EBAMView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultEBAM();
                    }
                    case "Dendrogram" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Dendrogram", "/Secure/analysis/TreeView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultDendrogram();
                    }
                    case "Heatmap" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Heatmap", "/Secure/analysis/HeatmapView.xhtml");

                        ClusterBean cb = (ClusterBean) getBeanInstance("cb");
                        cb.hmButton_action();
                    }
                    case "K-means" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("K-means", "/Secure/analysis/KMView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultKmeanClust();
                    }
                    case "SOM" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("SOM", "/Secure/analysis/SOMView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultSOMClust();
                    }
                    case "Random Forest" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("RandomForest", "/Secure/analysis/RFView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultRF();
                    }
                    case "SVM" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("SVM", "/Secure/analysis/RSVMView.xhtml");

                        AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                        an.doDefaultSVM();
                    }
                    case "SSP" -> {

                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                        sb.addNaviTrack("Enrichment result", "/Secure/enrichment/OraView.xhtml");
                        MsetBean mb = (MsetBean) getBeanInstance("mb");
                        mb.submitBtn_action();
                    }
                    case "QEA" -> {

                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                        sb.addNaviTrack("Enrichment result", "/Secure/enrichment/QeaView.xhtml");
                        MsetBean mb = (MsetBean) getBeanInstance("mb");
                        //System.out.println(sb.getAnalType() + "=======QEA");
                        mb.submitBtn_action();
                    }
                    case "ORA", "Enrichment" -> {

                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                        //    return 2;
                        }
                        sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                        sb.addNaviTrack("Enrichment result", "/Secure/enrichment/OraView.xhtml");
                        MsetBean mb = (MsetBean) getBeanInstance("mb");
                        mb.submitBtn_action();
                    }
                    case "Functional Annotation" -> {
                        boolean resBool = checkWorkflowContained("InitLibrary");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Set parameter", "/Secure/mummichog/LibraryView.xhtml");

                        PeakCustomBean pc = (PeakCustomBean) getBeanInstance("pc");
                        pc.customButton_action();
                    }
                    case "performPeaks2Fun", "Scatter" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MummiAnalBean ma = (MummiAnalBean) getBeanInstance("ma");
                        String nextPage = ma.performPeaks2Fun();
                        wb.getCalledWorkflows().add("Scatter");
                        if (nextPage.equals("Heatmap view")) {
                            sb.addNaviTrack("Heatmap", "/Secure/viewer/HeatmapView.xhtml");
                        } else {
                            if (ma.getAlgOpts().length > 1) {
                                sb.addNaviTrack("Integ. result", "/Secure/mummichog/IntegMumResultView.xhtml");
                            } else if (ma.getAlgOpts()[0].equals("mum")) {
                                sb.addNaviTrack("Mummi. result", "/Secure/mummichog/MummiResultView.xhtml");
                            } else if (ma.getAlgOpts()[0].equals("gsea")) {
                                sb.addNaviTrack("GSEA result", "/Secure/mummichog/GseaResultView.xhtml");
                            }
                        }
                        refactoredName = "Scatter Visualization";

                        sb.addNaviTrack("Metabolic network", "/Secure/mummichog/KeggNetView.xhtml");
                    }
                    case "Heatmap_mum" -> {
                        boolean resBool = checkWorkflowContained("performPeaks2Fun");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MummiAnalBean ma = (MummiAnalBean) getBeanInstance("ma");
                        ma.setAnalOption("heatmap");
                        String nextPage = ma.performPeaks2Fun();
                        sb.addNaviTrack("Heatmap", "/Secure/viewer/HeatmapView.xhtml");
                        refactoredName = "Heatmap Visualization";

                    }
                    case "Network" -> {
                        sb.addNaviTrack("Metabolic network", "/Secure/mummichog/KeggNetView.xhtml");
                    }
                    case "Name check", "Name check_List", "Name check_Table" -> {
                        sb.addNaviTrack("Name check", "/Secure/process/NameMapView.xhtml");

                    }
                    case "paBn_heatmap" -> {
                        PathBean pab = (PathBean) getBeanInstance("pab");
                        boolean resBool = checkWorkflowContained("paBn_heatmap");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        String res = pab.paBn_heatmap(sb.getAnalType());
                        sb.addNaviTrack("Heatmap (Pathway)", "/Secure/viewer/HeatmapView.xhtml");

                    }
                    case "paBn_proceed_ora", "paBn_proceed_qea" -> {
                        PathBean pab = (PathBean) getBeanInstance("pab");
                        boolean resBool = checkWorkflowContained("paBn_proceed");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        String res = pab.paBn_proceed();
                        if (res.equals("smpdbpathview")) {
                            sb.addNaviTrack("SMPDB result", "/Secure/pathway/SMPDBResultView.xhtml");
                            refactoredName = "Pathway Analysis (Small Compounds)";
                            RDataUtils.genPathwayJSON(sb.getRConnection(), pab.getPaBeans()[0].getSetName());
                            RDataUtils.genPathwayJSON(sb.getRConnection(), pab.getPaBeans()[1].getSetName());
                            RDataUtils.genPathwayJSON(sb.getRConnection(), pab.getPaBeans()[2].getSetName());
                        } else if (res.equals("pathview")) {
                            sb.addNaviTrack("Path. result", "/Secure/pathway/PathResultView.xhtml");
                            refactoredName = "Pathway Analysis";

                        } else if (res.equals("Heatmap view")) {
                            sb.addNaviTrack("Heatmap (Pathway)", "/Secure/viewer/HeatmapView.xhtml");
                            refactoredName = "Heatmap Visualization";

                        } else {
                            success = false;
                        }
                        sb.addNaviTrack("Set parameter", "/Secure/pathway/PathParamView.xhtml", success);

                    }
                    case "doMnetworkAnalysis_static", "doMnetworkAnalysis_metabo_phenotypes", "doMnetworkAnalysis_gene_metabolites", "doMnetworkAnalysis_metabo_metabolites", "doMnetworkAnalysis_global" -> {
                        performNetworkAnalysis(func.replace("doMnetworkAnalysis_", ""));
                    }
                    case "Network Selection" -> {
                        sb.addNaviTrack("Set parameter", "/Secure/network/MnetParamView.xhtml");
                    }
                    case "Network Builder_dspc", "DSPC Networks" -> {
                        boolean resBool = checkWorkflowContained("doMnetworkAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                        sb.setVisMode("dspc");
                        String res = mn.doMnetworkAnalysis("dspc");
                        refactoredName = "Network Building (DSPC)";

                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Network stats", "/Secure/network/MnetStats.xhtml", success);

                    }
                    case "DSPC Network", "Network Viewer_dspc" -> {
                        sb.addNaviTrack("Network viewer", "/Secure/network/MphenoNetView.xhtml");
                        refactoredName = "Network Visualization (DSPC)";
                        /*boolean resBool = checkWorkflowContained(func);
                        MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                        mn.setVisMode("dspc");
                        String res = mn.doMnetworkAnalysis(mn.getVisMode());
                         */
                    }
                    case "KEGG Network" -> {
                        boolean resBool = checkWorkflowContained("doMnetworkAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Network viewer", "/Secure/network/MetaboNetView.xhtml");

                        MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                        sb.setVisMode("static");
                        String res = mn.doMnetworkAnalysis(sb.getVisMode());
                        //System.out.println("KEGGNETWORK+++++===============");
                    }
                    case "doMnetworkAnalysis" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Set parameter", "/Secure/network/MnetParamView.xhtml");

                        MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                        String res = mn.doMnetworkAnalysis(sb.getVisMode());
                        if (res == null) {
                            success = false;
                            sb.addNaviTrack("Network stats", "/Secure/network/MnetStats.xhtml", success);

                        } else if (res.equals("MnetView")) {
                            sb.addNaviTrack("Network viewer", "/Secure/network/MetaboNetView.xhtml", success);
                            refactoredName = "Global Metabolic Network";

                        } else {
                            sb.addNaviTrack("Network stats", "/Secure/network/MnetStats.xhtml", success);
                            sb.addNaviTrack("Network viewer", "/Secure/network/MphenoNetView.xhtml", success);
                            refactoredName = "Network Visualization";
                        }
                    }
                    case "Multivariate ROC" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Explorer", "/Secure/roc/MultiRocView.xhtml");
                        RocAnalBean b = (RocAnalBean) getBeanInstance("b");
                        b.performExploreAnalysis();
                    }
                    case "Model-based ROC" -> {
                        boolean resBool = checkWorkflowContained(func);
                        sb.addNaviTrack("Evaluator", "/Secure/roc/RocTestView.xhtml");
                    }
                    case "ROC Analysis" -> {
                        sb.addNaviTrack("ROC Analysis", "/Secure/roc/RocAnalysisView.xhtml");

                        rocb.setSelMeta("Class");
                    }
                    case "Univariate ROC" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        rocb.performDefaultUnivAnalysis_internal();
                        ArrayList<FeatureBean> featureBeans = rocb.getFeatureBeans();
                        int limit = Math.min(3, featureBeans.size()); // Ensure we don't exceed the list size

                        for (int i = 0; i < limit; i++) {
                            FeatureBean feature = featureBeans.get(i);
                            rocb.plotUnivROCSummary(feature.getName());
                            System.out.println(feature);
                        }

                        sb.addNaviTrack("Univariate", "/Secure/roc/UnivRocView.xhtml");
                    }
                    case "Metadata check" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaProcBean mp = (MetaProcBean) getBeanInstance("mp");
                        mp.metacheck_proceed();
                    }
                    case "Metadata Heatmap" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaHeatmapBean mh = (MetaHeatmapBean) getBeanInstance("mh");
                        success = mh.metaOverviewBn_action();
                        sb.addNaviTrack("Metadata", "/Secure/multifac/MetaDataView.xhtml", success);

                    }
                    case "Multifactor anova" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        Aov2Bean aov = (Aov2Bean) getBeanInstance("aov");
                        aov.doDefaultAov2();
                        success = aov.aov2Bn_action();
                        sb.addNaviTrack("ANOVA2", "/Secure/multifac/Anova2View.xhtml", success);

                    }
                    case "ASCA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        AscaBean as = (AscaBean) getBeanInstance("as");
                        as.doDefaultAsca();
                        success = as.mdlBtn_action();
                        sb.addNaviTrack("ASCA", "/Secure/multifac/AscaView.xhtml", success);

                    }
                    case "Clustering heatmap" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        HeatMap2Bean hm = (HeatMap2Bean) getBeanInstance("hm");
                        hm.doDefaultHeatmap2();
                        success = hm.hm2Bn_action();
                        sb.addNaviTrack("Heatmap2", "/Secure/multifac/Heatmap2View.xhtml", success);

                    }
                    case "Linear Models" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MultifacBean mf = (MultifacBean) getBeanInstance("mf");
                        LimmaBean lm = (LimmaBean) getBeanInstance("lm");
                        mf.setCovPerformed(false);
                        lm.covScatterButton_action();
                        if (!mf.isCovPerformed()) {
                            success = false;
                        }
                        sb.addNaviTrack("Linear Model", "/Secure/multifac/LinearModelView.xhtml", success);
                    }
                    case "MEBA" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        if (!sb.isContainsTime()) {
                            sb.addMessage("Error", "MEBA only work on time-series data.");
                            success = false;
                            sb.addNaviTrack("MEBA", "/Secure/multifac/TimeCourseView.xhtml", success);

                        } else {
                            MebaBean meba = (MebaBean) getBeanInstance("meba");
                            FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("MEBA"));
                        }
                    }
                    case "Random Forest2" -> {
                        boolean resBool = checkWorkflowContained("Random Forest2");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MultiRfBean rf = (MultiRfBean) getBeanInstance("rf");
                        success = rf.rfBn_action_time();
                        sb.addNaviTrack("RandomForest", "/Secure/multifac/MultifacRFView.xhtml", success);

                    }
                    case "Correlation Analysis" -> {
                        MultifacBean mf = (MultifacBean) getBeanInstance("mf");
                        mf.setCorrPerformed(false);
                        boolean resBool = checkWorkflowContained("corBtn_action");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MultiCorrBean mc = (MultiCorrBean) getBeanInstance("mc");
                        mc.corBtn_action();
                        if (!mf.isCorrPerformed()) {
                            success = false;
                        }
                        sb.addNaviTrack("Correlations", "/Secure/multifac/PartialCorrView.xhtml", success);

                    }
                    case "Correlation Networks (DSPC)" -> {
                        boolean resBool = checkWorkflowContained("computeDspcNet");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        String res = sb.computeDspcNet();
                        File newFile = new File(sb.getCurrentUser().getHomeDir() + "/networkanalyst_dspc.json");

                        if (!newFile.exists()) {
                            success = false;
                        }
                        sb.addNaviTrack("DSPC Network", "/Secure/network/MphenoNetView.xhtml", false);

                    }
                    case "DE Analysis" -> {
                        boolean resBool = checkWorkflowContained("Dose Differential Expression");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        DoseResponseBean dr = (DoseResponseBean) getBeanInstance("dr");
                        dr.updateDoseDEAnalysis();
                        success = dr.isSigOK();
                        sb.addNaviTrack("Sig. analysis", "/Secure/dose/SigFeatureView.xhtml", success);

                    }
                    case "Curve Fitting" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Curve Fit", "/Secure/dose/ModelFitView.xhtml");
                        DoseResponseBean dr = (DoseResponseBean) getBeanInstance("dr");
                        success = dr.performCurveFitting();
                        sb.addNaviTrack("Curve Fitting", "/Secure/dose/FitResultView.xhtml", success);
                    }
                    case "Result" -> {
                        boolean resBool = checkWorkflowContained(func);
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        sb.addNaviTrack("Result (Dose Resp.)", "/Secure/dose/FitResultView.xhtml");
                    }
                    case "Pathway Analysis Results_Table", "Pathway Analysis Results_List" -> {
                        //sb.addNaviTrack("View result", "/Secure/dose/FitResultView.xhtml");
                    }
                    case "Combine P-values" -> {
                        boolean resBool = checkWorkflowContained("performPvalCombination");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaStatBean ms = (MetaStatBean) getBeanInstance("ms");
                        String res = ms.performPvalCombination();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Meta-Anal. Result", "/Secure/metastat/MetaResultView.xhtml", success);
                    }
                    case "Vote Counting" -> {
                        boolean resBool = checkWorkflowContained("performVoteCounting");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaStatBean ms = (MetaStatBean) getBeanInstance("ms");
                        String res = ms.performVoteCounting();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Meta-Anal. Result", "/Secure/metastat/MetaResultView.xhtml", success);
                    }
                    case "Direct Merging" -> {
                        boolean resBool = checkWorkflowContained("performDirectMerging");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaStatBean ms = (MetaStatBean) getBeanInstance("ms");
                        String res = ms.performDirectMerging();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Meta-Anal. Result", "/Secure/metastat/MetaResultView.xhtml", success);
                    }
                    case "Upset Diagram" -> {
                        boolean resBool = checkWorkflowContained("prepareUpsetView");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaLoadBean ml = (MetaLoadBean) getBeanInstance("ml");
                        String res = ml.prepareUpsetView();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Upset Diagram", "/Secure/metastat/UpsetDiagramView.xhtml", success);
                    }
                    case "metapaths_Method Selection" -> {
                        sb.addNaviTrack("Set parameter", "/Secure/metapath/MetaPathAnalView.xhtml");
                        refactoredName = "Method Selection";
                    }
                    case "Pathway-level integration" -> {
                        boolean resBool = checkWorkflowContained("performMetaPathAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaPathStatBean metaps = (MetaPathStatBean) getBeanInstance("metaps");
                        String res = metaps.performMetaPathAnalysis();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Meta paths", "/Secure/metapath/MetaPathResultView.xhtml", success);
                    }
                    case "metapaths Network Explorer path" -> {
                        boolean resBool = checkWorkflowContained("performMetaPathAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        int res = RMetaPathUtils.performNetworkAnal(sb.getRConnection());
                        if (res != 1) {
                            success = false;
                        }
                        sb.addNaviTrack("Network viewer", "/Secure/network/MetaboNetView.xhtml", success);
                        refactoredName = "Network Visualization";

                    }
                    case "metapaths Upset Diagram path" -> {
                        boolean resBool = checkWorkflowContained("performMetaPathAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaPathLoadBean metapl = (MetaPathLoadBean) getBeanInstance("metapl");
                        String res = metapl.prepareMetaPathUpsetView();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Upset diagram", "/Secure/metastat/UpsetDiagramView.xhtml", success);
                        refactoredName = "Upset Diagram";

                    }
                    case "Pooling peaks" -> {
                        boolean resBool = checkWorkflowContained("performMetaPoolAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaPathStatBean metaps = (MetaPathStatBean) getBeanInstance("metaps");
                        String res = metaps.performMetaPoolAnalysis();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("View result", "/Secure/metapath/MetaPathResultView.xhtml");
                    }
                    case "metapaths Network Explorer pool" -> {
                        boolean resBool = checkWorkflowContained("performMetaPoolAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        int res = RMetaPathUtils.performNetworkAnal(sb.getRConnection());
                        if (res != 1) {
                            success = false;
                        }
                        sb.addNaviTrack("Network viewer", "/Secure/network/MetaboNetView.xhtml", success);
                        refactoredName = "Network Visualization (Pooling Peaks)";

                    }
                    case "metapaths Upset Diagram pool" -> {
                        boolean resBool = checkWorkflowContained("performMetaPoolAnalysis");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        MetaLoadBean ml = (MetaLoadBean) getBeanInstance("ml");
                        String res = ml.prepareUpsetView();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Upset Diagram", "/Secure/metastat/UpsetDiagramView.xhtml", success);
                        refactoredName = "Upset Diagram (Pooling Peaks)";

                    }
                    case "Spectra Check" -> {
                        // Add specific handling if required
                        sb.addNaviTrack("Spectra check", "/Secure/spectra/SpectraCheck.xhtml", success);
                    }
                    case "Spectra Parameters Settings" -> {
                        SpectraProcessBean sp = (SpectraProcessBean) getBeanInstance("sp");
                        SpectraControlBean sc = (SpectraControlBean) getBeanInstance("sc");
                        String res = "ok";
                        if (sp.isIsms2DIA()) {
                            boolean resBool = checkWorkflowContained("prepareDIASpec");
                            if (!resBool && wb.isReloadingWorkflow()) {
                                return 2;
                            }
                            res = sp.prepareDIASpec();
                        } else {
                            boolean resBool = checkWorkflowContained("prepareSpecProc");
                            if (!resBool && wb.isReloadingWorkflow()) {
                                return 2;
                            }
                            res = sp.prepareSpecProc();
                        }
                        if (res.isEmpty()) {
                            success = false;
                        } else {
                            sb.addNaviTrack("Spectra processing", "/Secure/spectra/SpectraProcess.xhtml", success);
                        }
                    }
                    case "Spectra Processing" -> {
                        SpectraControlBean sc = (SpectraControlBean) getBeanInstance("sc");
                        boolean resBool = checkWorkflowContained("spectraParams");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        String res = sc.goToJobStatus(false);
                        sc.setCount(5);
                        sb.addNaviTrack("Job Status", "/Secure/spectra/JobStatusView.xhtml");
                        boolean res2 = fbc.saveProject("workflow");
                        sc.performPlan("TRUE");
                        if (sc.isJobSubmitted()) {
                            sb.addNaviTrack("Job status", "/Secure/spectra/JobStatusView.xhtml", success);
                        } else {
                            success = false;
                        }
                    }
                    case "Spectra View Result" -> {
                        //sb.addNaviTrack("Spectra result", "/Secure/spectra/SpectraResult.xhtml");
                    }
                    case "Annotation_List_network" -> { // for network

                        mnmb.setupCmpdNameMaps();
                        //mb.setupGeneNameMaps();
                        String res = mnmb.prepareNetworkData();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Name check", "/Secure/process/MnetMapView.xhtml", success);

                    }
                    case "Annotation_List" -> { // for network
                        mpb.setupNameMaps();

                    }
                    case "Annotation_Conc" -> {
                        sb.addNaviTrack("Name check", "/Secure/process/NameMapView.xhtml");
                        refactoredName = "Annotation";
                    }
                    case "Comp. with Reference" -> {
                        boolean resBool = checkWorkflowContained("SSP");
                        if (!resBool && wb.isReloadingWorkflow()) {
                            return 2;
                        }
                        String res = mpb.sspNextBn_action();
                        if (res == null) {
                            success = false;
                        }
                        sb.addNaviTrack("Conc. check", "/Secure/enrichment/SspProfileView.xhtml");
                    }
                    default -> {

                    }
                }

            } catch (Exception e) {
                System.out.println("An error occurred during the execution of: " + func);
                e.printStackTrace();
                // Optionally, you can log the error or update a UI message
                // sb.addMessage("Error", "An error occurred during the execution of: " + func);
            }
        }

        if (success) {
            return 1;
        } else {
            return 0;
        }

    }

    public boolean checkWorkflowContained(String functionType) {
        FunctionInfo functionInfo = wb.getFunctionInfos().get(functionType);

        if (functionInfo != null) {
            try {
                System.out.println("FunctionInfo found: " + functionType);
                if (wb.getReloadingParameters().equals("saved") || !wb.isReloadingWorkflow()) {
                    FunctionInvoker.callSetters(functionInfo);
                }
            } catch (Exception ex) {
                Logger.getLogger(WorkflowView.class.getName()).log(Level.SEVERE, null, ex);
                return false;
            }
            return true;
        } else {
            System.out.println("FunctionInfo not found for function type: " + functionType + "=======wb.isReloadingWorkflow()===" + wb.isReloadingWorkflow());
            return !wb.isReloadingWorkflow();
        }
    }
    private boolean stopStatusCheck = true;

    public boolean isStopStatusCheck() {
        return stopStatusCheck;
    }

    public void setStopStatusCheck(boolean stopStatusCheck) {
        this.stopStatusCheck = stopStatusCheck;
    }

    private String statusText = "";

    public String getStatusText() {
        return statusText;
    }

    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }

    public void executeWorkflow(ActionEvent event) {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        String functionsStr = facesContext.getExternalContext().getRequestParameterMap().get("functionsStr");
        System.out.println("===== functionsStr==xxxx==> " + functionsStr);
        int resInt = 1;
        String res = "";
        try {
            resInt = executeWorkflow(functionsStr);
        } catch (Exception ex) {
            System.err.println("An error occurred while executing the workflow: " + ex.getMessage());
            resInt = 0;
        }
        System.out.println("===== sb.getAnalType() xxx ==> " + sb.getAnalType());
        if (sb.getAnalType().equals("raw") || sb.getAnalType().equals("spec")) {
            if (resInt == 1) {
                res = "pending";
            } else if (resInt == 0) {
                res = "failed";
            }
        } else {
            if (resInt == 1) {
                res = "ok";
            } else if (resInt == 0) {
                res = "failed";
            }
        }

        System.out.println("res=====" + functionsStr + "===" + res);
        // Add response to request map to pass to the client
        // Add response to the context
        if (res.equals("ok") && functionsStr.equals("Save Project")) {

            res = "ok=" + fpb.getSelectedProject().getId();
        }
        PrimeFaces.current().ajax().addCallbackParam("res", res);
    }

    private Object getBeanInstance(String beanName) {
        // Programmatic lookup using CDI BeanManager
        switch (beanName) {
            case "sp" -> {
                return CDI.current().select(SpectraProcessBean.class).get();
            }
            case "sup" -> {
                return CDI.current().select(SpectraUploadBean.class).get();
            }
            case "sc" -> {
                return CDI.current().select(SpectraControlBean.class).get();
            }
            case "metapl" -> {
                return CDI.current().select(MetaPathLoadBean.class).get();
            }
            case "metaps" -> {
                return CDI.current().select(MetaPathStatBean.class).get();
            }
            case "mf" -> {
                return CDI.current().select(MultifacBean.class).get();
            }
            case "ms" -> {
                return CDI.current().select(MetaStatBean.class).get();
            }
            case "ml" -> {
                return CDI.current().select(MetaLoadBean.class).get();
            }
            case "dr" -> {
                return CDI.current().select(DoseResponseBean.class).get();
            }
            case "mc" -> {
                return CDI.current().select(MultiCorrBean.class).get();
            }
            case "rf" -> {
                return CDI.current().select(MultiRfBean.class).get();
            }
            case "meba" -> {
                return CDI.current().select(MebaBean.class).get();
            }
            case "lm" -> {
                return CDI.current().select(LimmaBean.class).get();
            }
            case "hm" -> {
                return CDI.current().select(HeatMap2Bean.class).get();
            }
            case "as" -> {
                return CDI.current().select(AscaBean.class).get();
            }
            case "aov" -> {
                return CDI.current().select(Aov2Bean.class).get();
            }
            case "mh" -> {
                return CDI.current().select(MetaHeatmapBean.class).get();
            }
            case "mp" -> {
                return CDI.current().select(MetaProcBean.class).get();
            }
            case "b" -> {
                return CDI.current().select(RocAnalBean.class).get();
            }
            case "mn" -> {
                return CDI.current().select(MnetResBean.class).get();
            }
            case "ma" -> {
                return CDI.current().select(MummiAnalBean.class).get();
            }
            case "pab" -> {
                return CDI.current().select(PathBean.class).get();
            }
            case "pc" -> {
                return CDI.current().select(PeakCustomBean.class).get();
            }
            case "nb" -> {
                return CDI.current().select(NormBean.class).get();
            }
            case "fp" -> {
                return CDI.current().select(WorkflowView.class).get();
            }
            case "ub" -> {
                return CDI.current().select(UnivBean.class).get();
            }
            case "cb" -> {
                return CDI.current().select(ClusterBean.class).get();
            }
            case "an" -> {
                return CDI.current().select(AnalysisBean.class).get();
            }
            case "mb" -> {
                return CDI.current().select(MsetBean.class).get();
            }
            case "mapb" -> {
                return CDI.current().select(MappingBean.class).get();
            }
            case "pb" -> {
                return CDI.current().select(ProcessBean.class).get();
            }
            case "ab" -> {
                return CDI.current().select(ApplicationBean1.class).get();
            }
            case "lp" -> {
                return CDI.current().select(LivePCABean.class).get();
            }
            case "sb" -> {
                return CDI.current().select(SessionBean1.class).get();
            }
            default ->
                throw new IllegalArgumentException("Unknown bean name: " + beanName);
        }
    }

    public void addToWorkflow(String url) {

        switch (url) {
            case "/Secure/mummichog/IntegMumResultView.xhtml" -> {
                wb.getCalledWorkflows().add("Scatter");
            }
            case "/Secure/mummichog/MummiResultView.xhtml" -> {
                wb.getCalledWorkflows().add("Scatter");
            }
            case "/Secure/mummichog/GseaResultView.xhtml" -> {
                wb.getCalledWorkflows().add("Scatter");
            }
            case "/Secure/mummichog/KeggNetView.xhtml" -> {
                wb.getCalledWorkflows().add("Network");

            }
            case "/Secure/network/MnetParamView.xhtml" -> {
                wb.getCalledWorkflows().add("Network Selection");

            }

            case "/Secure/roc/RocTestView.xhtml" -> {
                wb.getCalledWorkflows().add("Model-based ROC");
            }

            case "/Secure/roc/UnivRocView.xhtml" -> {
                wb.getCalledWorkflows().add("Univariate ROC");
            }
            case "/Secure/dose/FitResultView.xhtml" -> {
                if (sb.getAnalType().equals("pathora")) {
                    wb.getCalledWorkflows().add("Pathway Analysis Results_List");
                } else {
                    wb.getCalledWorkflows().add("Pathway Analysis Results_Table");
                }
            }
            case "/Secure/metapath/MetaPathAnalView.xhtml" -> {
                wb.getCalledWorkflows().add("metapaths_Method Selection");
            }
            case "/Secure/spectra/SpectraResult.xhtml" -> {
                wb.getCalledWorkflows().add("Spectra View Result");
            }
            case "/Secure/multifac/MultifacOverview.xhtml", "/Secure/stat/AnalysisView.xhtml" -> {
                wb.getCalledWorkflows().add("Visual Analytics");
            }

            default -> {

            }
        }
    }

    public String backToWorkflow() throws IOException {
        wb.setEditMode(false);
        // if (wb.getEditModeReturn().equals("overview")) {
        //    DataUtils.doRedirect("/MetaboAnalyst/xialabpro/WorkflowOverview.xhtml");
        // } else {
        DataUtils.doRedirect("/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml", ab);
        //}
        return null;
    }

    public String saveParams() throws IOException {
        boolean success = true;
        String func = wb.getCurrentStep();
        wb.setEditMode(true);
        try {
            switch (func) {
                case "Data Processing", "Sanity Check", "Sanity Check Intensity", "Sanity Check Peak" -> {
                    ProcessBean pb = (ProcessBean) getBeanInstance("pb");
                    pb.skipButton_action_default();
                }
                case "Filtering", "Filtering_Table", "Filtering Intensity" -> {
                    ProcessBean pb = (ProcessBean) getBeanInstance("pb");
                    pb.filterButton_action();
                }
                case "Normalization", "Normalization_Table", "Normalization Intensity" -> {
                    NormBean nb = (NormBean) getBeanInstance("nb");
                    nb.preparePrenormData();
                    nb.performDataNormalization();
                }
                case "Volcano" -> {

                    //UnivBean ub = (UnivBean) getBeanInstance("ub");
                    //ub.setupVolcano();
                }
                case "PCA" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultPCA();
                }
                case "iPCA" -> {
                    //LivePCABean lp = (LivePCABean) getBeanInstance("lp");
                    //lp.initPCA3D();
                }
                case "ANOVA" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultANOVA();
                }
                case "Fold change" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultFC();
                }
                case "T-test" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultTT();
                }
                case "Pattern Search" -> {
                    //UnivBean ub = (UnivBean) getBeanInstance("ub");
                    //ub.ptnBtn_action();
                }
                case "Correlation Heatmap" -> {
                    //UnivBean ub = (UnivBean) getBeanInstance("ub");
                    //ub.corrBtn_action();
                }
                case "PLSDA" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultPLSDA();
                }
                case "sPLSDA" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultSPLSDA();
                }
                case "OrthoPLSDA" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultOPLSDA();
                }
                case "SAM" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultSAM();
                }
                case "EBAM" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultEBAM();
                }
                case "Dendrogram" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultDendrogram();
                }
                case "Heatmap" -> {
                    //ClusterBean cb = (ClusterBean) getBeanInstance("cb");
                    //cb.hmButton_action();
                }
                case "K-means" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultKmeanClust();
                }
                case "SOM" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultSOMClust();
                }
                case "Random Forest" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultRF();
                }
                case "SVM" -> {
                    //AnalysisBean an = (AnalysisBean) getBeanInstance("an");
                    //an.doDefaultSVM();
                }
                case "SSP" -> {
                    MappingBean mapb = (MappingBean) getBeanInstance("mapb");
                    mapb.sspNextBn_action();
                }
                case "QEA" -> {
                    MsetBean mb = (MsetBean) getBeanInstance("mb");
                    mb.submitBtn_action();
                }
                case "ORA" -> {
                    MsetBean mb = (MsetBean) getBeanInstance("mb");
                    mb.submitBtn_action();
                }
                case "Functional Annotation" -> {
                    PeakCustomBean pc = (PeakCustomBean) getBeanInstance("pc");
                    pc.customButton_action();
                }
                case "performPeaks2Fun", "Scatter" -> {
                    MummiAnalBean ma = (MummiAnalBean) getBeanInstance("ma");
                    String nextPage = ma.performPeaks2Fun();
                }
                case "Heatmap_mum" -> {
                    MummiAnalBean ma = (MummiAnalBean) getBeanInstance("ma");
                    ma.setAnalOption("heatmap");
                    String nextPage = ma.performPeaks2Fun();
                }
                case "Network" -> {

                }
                case "Name check", "Name check_List", "Name check_Table" -> {

                }
                case "paBn_proceed_ora", "paBn_proceed_qea" -> {
                    PathBean pab = (PathBean) getBeanInstance("pab");
                    String res = pab.paBn_proceed();

                }

                case "doMnetworkAnalysis_static", "doMnetworkAnalysis_gene_metabolites", "doMnetworkAnalysis_metabo_metabolites", "doMnetworkAnalysis_global" -> {
                    // Add specific handling if required
                }
                case "Network Selection" -> {
                    sb.addNaviTrack("Set parameter", "/Secure/network/MnetParamView.xhtml");
                }
                case "Network Builder_dspc" -> {
                    MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                    sb.setVisMode("dspc");
                    String res = mn.doMnetworkAnalysis(sb.getVisMode());

                }
                case "DSPC Network", "Network Viewer_dspc" -> {
                    //sb.addNaviTrack("Network viewer", "/Secure/network/MphenoNetView.xhtml");
                    /*boolean resBool = checkWorkflowContained(func);
                        MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                        mn.setVisMode("dspc");
                        String res = mn.doMnetworkAnalysis(mn.getVisMode());
                     */
                }
                case "KEGG Network" -> {
                    MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                    sb.setVisMode("static");
                    String res = mn.doMnetworkAnalysis(sb.getVisMode());
                }
                case "doMnetworkAnalysis" -> {
                    MnetResBean mn = (MnetResBean) getBeanInstance("mn");
                    String res = mn.doMnetworkAnalysis(sb.getVisMode());
                }
                case "Multivariate ROC" -> {
                    RocAnalBean b = (RocAnalBean) getBeanInstance("b");
                    b.performExploreAnalysis();
                }
                case "Model-based ROC" -> {
                    //sb.addNaviTrack("Evaluator", "/Secure/roc/RocTestView.xhtml");
                }
                case "Univariate ROC" -> {
                    rocb.performDefaultUnivAnalysis_internal();
                }
                case "Metadata check" -> {
                    //MetaProcBean mp = (MetaProcBean) getBeanInstance("mp");
                    //mp.metacheck_proceed();
                }
                case "Metadata Heatmap" -> {
                    MetaHeatmapBean mh = (MetaHeatmapBean) getBeanInstance("mh");
                    mh.metaOverviewBn_action();
                }
                case "Multifactor anova" -> {
                    Aov2Bean aov = (Aov2Bean) getBeanInstance("aov");
                    aov.doDefaultAov2();
                    aov.aov2Bn_action();
                }
                case "ASCA" -> {
                    AscaBean as = (AscaBean) getBeanInstance("as");
                    as.doDefaultAsca();
                    as.mdlBtn_action();
                }
                case "Clustering heatmap" -> {
                    HeatMap2Bean hm = (HeatMap2Bean) getBeanInstance("hm");
                    hm.doDefaultHeatmap2();
                    hm.hm2Bn_action();
                }
                case "Linear Models" -> {
                    MultifacBean mf = (MultifacBean) getBeanInstance("mf");
                    LimmaBean lm = (LimmaBean) getBeanInstance("lm");
                    mf.setCovPerformed(false);
                    lm.covScatterButton_action();
                }
                case "MEBA" -> {
                    if (!sb.isContainsTime()) {
                        sb.addMessage("Error", "MEBA only work on time-series data.");
                        success = false;
                    } else {
                        MebaBean meba = (MebaBean) getBeanInstance("meba");
                        FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("MEBA"));
                    }
                }
                case "Random Forest2" -> {
                    MultiRfBean rf = (MultiRfBean) getBeanInstance("rf");
                    rf.rfBn_action_time();
                }
                case "Correlation Analysis" -> {
                    MultiCorrBean mc = (MultiCorrBean) getBeanInstance("mc");
                    mc.corBtn_action();
                }
                case "Correlation Networks (DSPC)" -> {
                    String res = sb.computeDspcNet();

                }
                case "DE Analysis" -> {
                    DoseResponseBean dr = (DoseResponseBean) getBeanInstance("dr");
                    dr.updateDoseDEAnalysis();
                }
                case "Curve Fitting" -> {

                    DoseResponseBean dr = (DoseResponseBean) getBeanInstance("dr");
                    dr.performCurveFitting();
                }
                case "Result" -> {
                    //boolean resBool = checkWorkflowContained(func);
                    //sb.addNaviTrack("View result", "/Secure/dose/FitResultView.xhtml");
                }
                case "Pathway Analysis Results_Table", "Pathway Analysis Results_List" -> {
                    //sb.addNaviTrack("View result", "/Secure/dose/FitResultView.xhtml");
                }
                case "Combine P-values" -> {
                    MetaStatBean ms = (MetaStatBean) getBeanInstance("ms");
                    ms.performPvalCombination();
                }
                case "Vote Counting" -> {
                    MetaStatBean ms = (MetaStatBean) getBeanInstance("ms");
                    ms.performVoteCounting();
                }
                case "Direct Merging" -> {
                    MetaStatBean ms = (MetaStatBean) getBeanInstance("ms");
                    ms.performDirectMerging();
                }
                case "Upset Diagram" -> {
                    MetaLoadBean ml = (MetaLoadBean) getBeanInstance("ml");
                    ml.prepareUpsetView();
                }
                case "metapaths_Method Selection" -> {

                }
                case "Pathway-level integration" -> {
                    MetaPathStatBean metaps = (MetaPathStatBean) getBeanInstance("metaps");
                    metaps.performMetaPathAnalysis();
                }
                case "metapaths Network Explorer path" -> {
                    //int res = RMetaPathUtils.performNetworkAnal(sb.getRConnection());
                }
                case "metapaths Upset Diagram path" -> {
                    MetaPathLoadBean metapl = (MetaPathLoadBean) getBeanInstance("metapl");
                    metapl.prepareMetaPathUpsetView();
                }
                case "Pooling peaks" -> {
                    MetaPathStatBean metaps = (MetaPathStatBean) getBeanInstance("metaps");
                    metaps.performMetaPoolAnalysis();
                }
                case "metapaths Network Explorer pool" -> {
                    //int res = RMetaPathUtils.performNetworkAnal(sb.getRConnection());
                }
                case "metapaths Upset Diagram pool" -> {
                    MetaLoadBean ml = (MetaLoadBean) getBeanInstance("ml");
                    ml.prepareUpsetView();
                }
                case "Spectra Check" -> {
                    // Add specific handling if required
                }
                case "Spectra Parameters Settings" -> {
                    SpectraProcessBean sp = (SpectraProcessBean) getBeanInstance("sp");
                    SpectraControlBean sc = (SpectraControlBean) getBeanInstance("sc");
                    if (sp.isIsms2DIA()) {
                        sp.prepareDIASpec();
                    } else {
                        sp.prepareSpecProc();
                    }
                    sc.goToJobStatus(false);
                }
                case "Spectra Processing" -> {
                    SpectraControlBean sc = (SpectraControlBean) getBeanInstance("sc");
                    sc.setCount(5);
                    //sc.performPlan();
                    if (sc.isJobSubmitted()) {
                        sb.addNaviTrack("Job status", "/Secure/spectra/JobStatusView.xhtml");
                    } else {
                        success = false;
                    }

                }
                case "Spectra View Result" -> {

                }
                default -> {
                    // Handle any other cases as needed
                }
            }
        } catch (Exception e) {
            System.out.println("An error occurred during the execution of: " + func);
            e.printStackTrace();
            // Optionally, you can log the error or update a UI message
            // sb.addMessage("Error", "An error occurred during the execution of: " + func);
        }
        wb.setEditMode(false);
        //if (wb.getEditModeReturn().equals("overview")) {
        //    wb.setEditModeReturn("default");
        //    DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/xialabpro/WorkflowOverview.xhtml", "info", "Parameters have been saved!");
        //} else {
        DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml", "info", "Parameters have been saved!");
        //}
        return "";
    }

    public void generateWorkflowJson(String projectType, boolean insert) throws IOException {
        generateWorkflowJson(wb.getName(), wb.getDescription(), projectType, insert, true);
    }
    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    public void generateWorkflowJson(String wName, String wDescription, String projectType, boolean insert, boolean msgBool) throws IOException {
        if (wb.getFunctionInfos() == null || wb.getFunctionInfos().isEmpty()) {
            if (msgBool) {
                sb.addMessage("Error", "Workflow is empty!");
            } else {
                System.out.println("workflow is empty!");
            }
            return;
        }

        jrd.record_workflowState(wb);
        String path = sb.getCurrentUser().getHomeDir() + "/workflow.json";
        FunctionInvoker.saveFunctionInfosToFile(wb.getFunctionInfos(), path);
        if (insert && !projectType.equals("workflow")) {
            File projSubFolder = new File(fb.getProjectPath() + "/user_folders/" + fub.getEmail());
            if (!projSubFolder.exists()) {
                boolean result = projSubFolder.mkdirs();
                if (result) {
                    System.out.println("Directory created successfully.");
                } else {
                    sb.addMessage("Error", "Saving workflow failed, project folder doesn't exist!");
                    return;
                }
            } else {
                System.out.println("Directory already exists.");
            }

            String fileName = File.createTempFile("workflow_" + sb.getAnalType(), "").getName();
            FunctionInvoker.saveFunctionInfosToFile(wb.getFunctionInfos(), fb.getProjectPath() + "user_folders/" + fub.getEmail() + "/" + fileName + ".json");
            String fileNameOverview = fileName + "_overview";

            dv.saveDiagramState(fb.getProjectPath() + "user_folders/" + fub.getEmail() + "/" + fileNameOverview + ".json");
            HashMap<String, Object> selectedWorkflow = wb.getSelectedWorkflow();
            dbc.insertWorkflow(fub.getEmail(), wName, wDescription, sb.getAnalType(), ab.getAppName(), fileName + ".json", ab.getToolLocation(), (String) selectedWorkflow.get("input"), (String) selectedWorkflow.get("analysisGoal"), (String) selectedWorkflow.get("analysisMethods"), (String) selectedWorkflow.get("output"), (String) selectedWorkflow.get("other"));
            if (msgBool) {
                sb.addMessage("info", "Workflow has been successfully saved!");
            }
        }

    }

    /*
    public void onTabChange(TabChangeEvent event) {
        // Check if the selected tab is at index 2 (Workflow View)
        if (activeIndex == 2) {
            System.out.println("ontabchange===2");
            PrimeFaces.current().executeScript("initNetwork()");
        }
    }*/
    public void updateWorkflowDescription() {
        int id = fpb.getSelectedProject().getId();
        wb.setName("Saved Workflow");
        wb.setDescription("Workflow Associated with Project " + id);
    }

    public String getReadableType(String type) {
        return switch (type) {
            case "network" ->
                "Network Analysis";
            case "dose" ->
                "Dose Response Analysis";
            case "metapaths" ->
                "Functional Meta-Analysis";
            case "metadata" ->
                "Statistical Meta-Analysis";
            case "raw" ->
                "LC-MS Spectra Processing";
            case "pathinteg" ->
                "Joint Pathway Analysis";
            case "stat" ->
                "Statistical Analysis [one factor]";
            case "mf" ->
                "Statistical Analysis [metadata table]";
            case "path" ->
                "Pathway Analysis";
            case "mset" ->
                "Enrichment Analysis";
            case "roc" ->
                "Biomarker Analysis";
            case "power" ->
                "Power Analysis";
            case "utils" ->
                "Other Utilities";
            case "mass_all", "mummichog", "mass_table" ->
                "Functional Analysis of MS Peaks";
            case "msetora" ->
                "Enrichment Analysis (ORA)";
            case "msetqea" ->
                "Enrichment Analysis (QEA)";
            case "pathqea" ->
                "Pathway Analysis (QEA)";
            case "pathora" ->
                "Pathway Analysis (ORA)";
            case "msetssp" ->
                "Enrichment Analysis (Single Sample Profiling data)";
            default ->
                type;
        };
    }

    public String getUrl(String func) {
        String url = "";
        switch (func) {
            case "Missing Values" -> {

            }
            case "Save Project" -> {

            }
            case "Data Processing", "Sanity Check", "Sanity Check Intensity", "Sanity Check Peak" -> {
                url = "/Secure/process/SanityCheck.xhtml";
            }
            case "Filtering", "Filtering_Table", "Filtering Intensity" -> {
                url = "/Secure/process/FilterView.xhtml";

            }
            case "Normalization", "Normalization_Table", "Normalization Intensity" -> {
                url = "/Secure/process/NormalizationView.xhtml";

            }
            case "Volcano" -> {
                url = "/Secure/analysis/VolcanoView.xhtml";

            }
            case "PCA" -> {
                url = "/Secure/analysis/PCAView.xhtml";
            }
            case "iPCA" -> {
                url = "/Secure/multifac/LivePCAView.xhtml";

            }
            case "ANOVA" -> {
                url = "/Secure/analysis/AnovaView.xhtml";
            }
            case "Fold change" -> {
                url = "/Secure/analysis/FoldChangeView.xhtml";
            }
            case "T-test" -> {
                url = "/Secure/analysis/TtestView.xhtml";
            }
            case "Pattern Search" -> {
                url = "/Secure/analysis/PatternView.xhtml";
            }
            case "Correlation Heatmap" -> {
                url = "/Secure/analysis/CorrelationView.xhtml";
            }
            case "PLSDA" -> {
                url = "/Secure/analysis/PLSDAView.xhtml";
            }
            case "sPLSDA" -> {
                url = "/Secure/analysis/SparsePLSDAView.xhtml";
            }
            case "OrthoPLSDA" -> {
                url = "/Secure/analysis/OrthoPLSDAView.xhtml";

            }
            case "SAM" -> {
                url = "/Secure/analysis/SAMView.xhtml";
            }
            case "EBAM" -> {
                url = "/Secure/analysis/EBAMView.xhtml";
            }
            case "Dendrogram" -> {
                url = "/Secure/analysis/TreeView.xhtml";
            }
            case "Heatmap" -> {
                url = "/Secure/analysis/HeatmapView.xhtml";
            }
            case "K-means" -> {
                url = "/Secure/analysis/KMView.xhtml";

            }
            case "SOM" -> {
                url = "/Secure/analysis/SOMView.xhtml";
            }
            case "Random Forest" -> {
                url = "/Secure/analysis/RFView.xhtml";

            }
            case "SVM" -> {
                url = "/Secure/analysis/RFView.xhtml";

            }
            case "SSP" -> {
                //sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                url = "/Secure/enrichment/OraView.xhtml";

            }
            case "QEA" -> {
                //sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                url = "/Secure/enrichment/QeaView.xhtml";

            }
            case "ORA" -> {
                //sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                url = "/Secure/enrichment/OraView.xhtml";

            }
            case "Functional Annotation" -> {
                url = "/Secure/mummichog/LibraryView.xhtml";

            }
            case "performPeaks2Fun", "Scatter" -> {
                boolean resBool = checkWorkflowContained(func);
                MummiAnalBean ma = (MummiAnalBean) getBeanInstance("ma");

                if (ma.getAlgOpts().length > 1) {
                    url = "/Secure/mummichog/IntegMumResultView.xhtml";
                } else if (ma.getAlgOpts()[0].equals("mum")) {
                    url = "/Secure/mummichog/MummiResultView.xhtml";
                } else if (ma.getAlgOpts()[0].equals("gsea")) {
                    url = "/Secure/mummichog/GseaResultView.xhtml";
                }

            }
            case "Heatmap_mum" -> {

                url = "/Secure/viewer/HeatmapView.xhtml";

            }
            case "Network" -> {
                url = "/Secure/mummichog/KeggNetView.xhtml";
            }
            case "Name check", "Name check_List", "Name check_Table" -> {
                url = "/Secure/process/NameMapView.xhtml";

            }
            case "paBn_proceed_ora", "paBn_proceed_qea" -> {
                PathBean pab = (PathBean) getBeanInstance("pab");
                boolean resBool = checkWorkflowContained("paBn_proceed");
                String res = pab.paBn_proceed();
                if (pab.getAnalOption().equals("Scatter")) {
                    url = "/Secure/pathway/SMPDBResultView.xhtml";
                    url = "/Secure/pathway/PathResultView.xhtml";

                } else {
                    url = "/Secure/viewer/HeatmapView.xhtml";
                }
            }
            case "doMnetworkAnalysis_static", "doMnetworkAnalysis_metabo_phenotypes", "doMnetworkAnalysis_gene_metabolites", "doMnetworkAnalysis_metabo_metabolites", "doMnetworkAnalysis_global" -> {
                // Add specific handling if required
            }
            case "Network Selection" -> {
                url = "/Secure/network/MnetParamView.xhtml";
            }
            case "Network Builder_dspc" -> {
                url = "/Secure/network/MphenoStatsxhtml";

            }
            case "DSPC Network", "Network Viewer_dspc", "DSPC Networks" -> {
                url = "/Secure/network/MphenoNetView.xhtml";

            }
            case "KEGG Network" -> {
                url = "/Secure/network/MetaboNetView.xhtml";

            }
            case "doMnetworkAnalysis" -> {
                url = "/Secure/network/MphenoNetView.xhtml";

            }
            case "Multivariate ROC" -> {
                url = "/Secure/roc/MultiRocView.xhtml";

            }
            case "Model-based ROC" -> {

                url = "/Secure/roc/RocTestView.xhtml";
            }
            case "Univariate ROC" -> {

                url = "/Secure/roc/UnivRocView.xhtml";
            }
            case "Metadata check" -> {
                url = "/Secure/process/MetaDataCheck.xhtml";

            }
            case "Metadata Heatmap" -> {
                url = "/Secure/multifac/MetaDataView.xhtml";
            }
            case "Multifactor anova" -> {
                url = "/Secure/multifac/Anova2View.xhtml";
            }
            case "ASCA" -> {
                url = "/Secure/multifac/AscaView.xhtml";

            }
            case "Clustering heatmap" -> {
                url = "/Secure/multifac/Heatmap2View.xhtml";

            }
            case "Linear Models" -> {
                url = "/Secure/multifac/LinearModelView.xhtml";
            }
            case "MEBA" -> {
                url = "/Secure/multifac/TimeCourseView.xhtml";

            }
            case "Random Forest2" -> {
                url = "/Secure/multifac/MultifacRFView.xhtml";

            }
            case "Correlation Analysis" -> {
                url = "/Secure/multifac/PartialCorrView.xhtml";
            }
            case "Correlation Networks (DSPC)" -> {
                url = "/Secure/network/MphenoNetView.xhtml";
            }
            case "DE Analysis" -> {
                url = "/Secure/dose/SigFeatureView.xhtml";
            }
            case "Curve Fitting" -> {
                url = "/Secure/dose/ModelFitView.xhtml";
            }
            case "Result" -> {
                url = "/Secure/dose/FitResultView.xhtml";
            }
            case "Pathway Analysis Results_Table", "Pathway Analysis Results_List" -> {
                url = "/Secure/dose/FitResultView.xhtml";
            }
            case "Combine P-values" -> {
                url = "/Secure/metastat/MetaResultView.xhtml";
            }
            case "Vote Counting" -> {

                url = "/Secure/metastat/MetaResultView.xhtml";
            }
            case "Direct Merging" -> {

                url = "/Secure/metastat/MetaResultView.xhtml";
            }
            case "Upset Diagram" -> {

                url = "/Secure/metastat/UpsetDiagramView.xhtml";
            }
            case "metapaths_Method Selection" -> {
                url = "/Secure/metapath/MetaPathAnalView.xhtml";
            }
            case "Pathway-level integration" -> {
                url = "/Secure/metapath/MetaPathResultView.xhtml";
            }
            case "metapaths Network Explorer path" -> {
                url = "/Secure/network/MetaboNetView.xhtml";

            }
            case "metapaths Upset Diagram path" -> {

                url = "/Secure/metastat/UpsetDiagramView.xhtml";

            }
            case "Pooling peaks" -> {

                url = "/Secure/metapath/MetaPathResultView.xhtml";
            }
            case "metapaths Network Explorer pool" -> {
                url = "/Secure/network/MetaboNetView.xhtml";
            }
            case "metapaths Upset Diagram pool" -> {
                url = "/Secure/metastat/UpsetDiagramView.xhtml";
            }
            case "Spectra Check" -> {
                // Add specific handling if required
                url = "/Secure/spectra/SpectraCheck.xhtml";
            }
            case "Spectra Parameters Settings" -> {

                url = "/Secure/spectra/SpectraProcess.xhtml";

            }
            case "Spectra Processing" -> {

                url = "/Secure/spectra/JobStatusView.xhtml";

            }
            case "Spectra View Result" -> {
                url = "/Secure/spectra/SpectraResult.xhtml";

            }
            case "Annotation_Conc" -> {
                url = "/Secure/process/NameMapView.xhtml";
            }
            case "Comp. with Reference" -> {
                url = "/Secure/enrichment/SspProfileView.xhtml";
            }
            default -> {

            }
        }
        return url;
    }

    private void performNetworkAnalysis(String func) {
        boolean resBool = checkWorkflowContained(func);
        if (!resBool && wb.isReloadingWorkflow()) {
            return;
        }
        sb.addNaviTrack("Set parameter", "/Secure/network/MnetParamView.xhtml");
        MnetResBean mn = (MnetResBean) getBeanInstance("mn");
        String res = mn.doMnetworkAnalysis(sb.getVisMode());
        if (res.equals("MnetView")) {
            sb.addNaviTrack("Network viewer", "/Secure/network/MetaboNetView.xhtml");
            refactoredName = "Global Metabolic Network";

        } else {
            sb.addNaviTrack("Network stats", "/Secure/network/MnetStats.xhtml");
            mn.prepareNetworks();
            sb.addNaviTrack("Network viewer", "/Secure/network/MphenoNetView.xhtml");
            refactoredName = "Network Visualization";
        }

    }

}
