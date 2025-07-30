package pro.metaboanalyst.agents;

import java.io.Serializable;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import pro.metaboanalyst.chat.Message;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;

@Named("rPlotCustomizationBean")
@ViewScoped
public class RPlotCustomizationBean implements Serializable {

    @Inject
    private SessionBean1 sb;

    @Inject
    private ApplicationBean1 ab;

    @Inject
    private RPlotCustomizationAgent aiCustomizer;

    private String plotType = "";
    private String prompt;
    private String aiResponse;
    private String welcomeMsg = "<p>Describe how you want to customize the plot. For example:</p>\n"
            + "\n"
            + "<ul>\n"
            + "  <li>Change colors (e.g., <code>Use a blue color scheme</code>)</li>\n"
            + "  <li>Adjust text size (e.g., <code>Make labels larger</code>)</li>\n"
            + "  <li>Modify layout (e.g., <code>Increase spacing between plots</code>)</li>\n"
            + "  <li>Change dimensions (e.g., <code>Make the plot wider</code>)</li>\n"
            + "  <li>Add features (e.g., <code>Add a grid to the background</code>)</li>\n"
            + "</ul>";

    private List<Message> messages;

    public List<Message> getMessages() {
        if (messages == null) {
            messages = new ArrayList<>();
            messages.add(new Message("Assistant", welcomeMsg));
        }
        return messages;
    }

    public void setMessages(List<Message> messages) {
        this.messages = messages;
    }

    public String getWelcomeMsg() {
        return welcomeMsg;
    }

    public void setWelcomeMsg(String welcomeMsg) {
        this.welcomeMsg = welcomeMsg;
    }

    public String getPrompt() {
        return prompt;
    }

    public void setPrompt(String prompt) {
        this.prompt = prompt;
    }

    public String getAiResponse() {
        return aiResponse;
    }

    public void setAiResponse(String aiResponse) {
        this.aiResponse = aiResponse;
    }

/**
    public void applyCustomization() {
        try {
            if (prompt == null || prompt.trim().isEmpty()) {
                sb.addMessage("error", "Please provide customization instructions");
                return;
            }

            String source = sb.getImageSource();
            plotType = GRAPHICS_CMD_TO_R_FUNC.get(source);

            if (plotType == null) {
                sb.addMessage("error", "Unsupported plot type for source: " + source);
                return;
            }

            // Get the R command from the session bean's graphics map
            String rCommand = sb.getGraphicsMap().get(source);
            if (rCommand == null) {
                sb.addMessage("error", "No R command found for source: " + source);
                return;
            }

            // Get the AI response
            String response = aiCustomizer.customizePlot(source, plotType, prompt);
            aiResponse = response;
        } catch (Exception e) {
            sb.addMessage("error", "Error applying customization: " + e.getMessage());
        }
    }

*/
    public void applyCustomization() {
        try {
            if (prompt == null || prompt.trim().isEmpty()) {
                sb.addMessage("error", "Please provide customization instructions");
                return;
            }

            String source = sb.getImageSource();
            plotType = GRAPHICS_CMD_TO_R_FUNC.get(source);

            if (plotType == null) {
                sb.addMessage("error", "Unsupported plot type for source: " + source);
                return;
            }

            // Get the R command from the session bean's graphics map
            String rCommand = sb.getGraphicsMap().get(source);
            if (rCommand == null) {
                sb.addMessage("error", "No R command found for source: " + source);
                return;
            }

            // Get helpers for this plot type
            List<String> helpers = HELPERS.getOrDefault(plotType, List.of());
            
            // Get the AI response
            String response = aiCustomizer.customizePlot(source, plotType, helpers, prompt);
            aiResponse = response;
        } catch (Exception e) {
            sb.addMessage("error", "Error applying customization: " + e.getMessage());
        }
    }
    
    public String getPreviewImage() {
        String plotSource = sb.getImageSource();
        System.out.println(plotSource + "========plotSource");
        if (plotSource.equals("volcano")) {
            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(plotSource) + "dpi150.png" + "?t=" + System.currentTimeMillis();
        } else {
            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(plotSource) + "dpi150.png" + "?t=" + System.currentTimeMillis();
        }
    }


    public static final Map<String, String> GRAPHICS_CMD_TO_R_FUNC = Map.ofEntries(
            Map.entry("aov", "PlotANOVA"),
            Map.entry("volcano", "PlotVolcano"),
            Map.entry("venn", "plotVennDiagram"),
            Map.entry("scatter3d", "Plot3D"),
            Map.entry("heatmap", "PlotHeatmap"),
            Map.entry("pca", "PlotPCA"),
            Map.entry("plsda", "PlotPLSDA"),
            Map.entry("sparseplsda", "PlotSparsePLSDA"),
            Map.entry("rf", "PlotRandomForest"),
            Map.entry("cls_roc", "PlotROC"),
            Map.entry("tsne", "PlotTSNE"),
            Map.entry("umap", "PlotUMAP"),
            Map.entry("clustpca", "PlotClustPCA"),
            Map.entry("pca_biplot", "PlotPCABiplot"),
            Map.entry("tt", "PlotTT"),
            Map.entry("cmpdview", "PlotCmpdView"),
            Map.entry("drcurve", "PlotMetaboliteDRCurve"),
            Map.entry("drmodelbars", "PlotDRModelBars"),
            Map.entry("drhistogram", "PlotDRHistogram"),
            Map.entry("pcapair", "PlotPCAPairSummaryMeta"),
            Map.entry("selectedfeature", "PlotSelectedFeature"),
            Map.entry("roc_boxplot_", "PlotRocUnivBoxPlot"),
            Map.entry("roc_univ_", "Perform.UnivROC"),
            Map.entry("cls_imp", "PlotImpBiomarkers"),
            Map.entry("pca_scree", "PlotPCAScree"),
            Map.entry("pca_pair", "PlotPCAPairSummary"),
            Map.entry("spls_perm", "PlotSPLS.Permutation"),
            Map.entry("dose_volcano", "PlotDoseVolcano"), // Error message: No R command found for source: dose_volcano
            Map.entry("dr_histogram", "PlotDRHistogram"),
            Map.entry("raw_spec_stic", "plotSingleTIC"),
            Map.entry("plot_kegg_graph", "PlotKEGGPath"),
            Map.entry("tree", "PlotHCTree"),
            Map.entry("pca_loading", "PlotPCALoading"),
            Map.entry("pca_score2d", "PlotPCA2DScore"),
            Map.entry("load_boxplot", "PlotLoadBoxplot"),
            Map.entry("power_stat", "PlotPowerStat"),
            Map.entry("power_profile", "PlotPowerProfile"),
            Map.entry("plot_bpis", "plotBPIs"),
            Map.entry("plot_rtcor", "PlotSpectraRTadj"),
            Map.entry("plot_bpicor", "PlotSpectraBPIadj"),
            Map.entry("cmpd_summary", "PlotCmpdSummary"),
            Map.entry("multifac_cmpd_summary", "PlotMultiFacCmpdSummary"),
            Map.entry("corr_heatmap", "PlotStaticCorrHeatMap"),
            Map.entry("metaCorrHeatmap", "PlotMetaCorrHeatmap"),
            Map.entry("meta_density", "PlotMetaDensity"),
            Map.entry("pathway_meta", "PlotPathwayMetaAnalysis"),
            Map.entry("asca_model", "PlotASCAModel"),
            Map.entry("asca_interaction", "PlotASCAInteraction"),
            Map.entry("_mr_scatter_plot", "PlotScatter"),
            Map.entry("_mr_forest_plot", "PlotForest"),
            Map.entry("_mr_leaveoneout_plot", "PlotLeaveOneOut"),
            Map.entry("_mr_funnel_plot", "PlotFunnel"),
            Map.entry("fc", "PlotFC"),
            Map.entry("opls_score2d", "PlotOPLS2DScore"),
            Map.entry("opls_splot", "PlotOPLS.Splot"),
            Map.entry("opls_imp", "PlotOPLS.Imp"),
            Map.entry("opls_mdl", "PlotOPLS.MDL"),
            Map.entry("sam_imp", "PlotSAM.Cmpd"), 
            Map.entry("ebam_imp", "PlotEBAM.Cmpd"), 
            Map.entry("spls_pair", "PlotSPLSPairSummary"),
            Map.entry("spls_loading", "PlotSPLSLoading"),
            Map.entry("spls_score2d", "PlotSPLS2DScore"),
            Map.entry("spls_cv", "PlotSPLSDA.Classification"),
            Map.entry("ptn", "PlotCorr"),
            Map.entry("km_pca", "PlotClustPCA"),
            Map.entry("km", "PlotKmeans"),
            Map.entry("som_pca", "PlotClustPCA"),
            Map.entry("som", "PlotSOM"),
            Map.entry("pca_score3d", "PlotPCA3DScore"),
            Map.entry("pca_loading3d", "PlotPCA3DLoading"),
            Map.entry("pls_score2d", "PlotPLS2DScore"),
            Map.entry("pls_score3d", "PlotPLS3DScoreImg"),
            Map.entry("pls_loading", "PlotPLSLoading"),
            Map.entry("pls_loading3d", "PlotPLS3DLoading"),
            Map.entry("pls_biplot", "PlotPLSBiplot"),
            Map.entry("pls_pair", "PlotPLSPairSummary"),
            Map.entry("cls_prob", "PlotProbView"),
            Map.entry("cls_accu", "PlotAccuracy"),
            Map.entry("cls_test_roc", "PlotROCTest"),
            Map.entry("cls_test_prob", "PlotProbViewTest"),
            Map.entry("cls_test_accu", "PlotTestAccuracy"),
            Map.entry("roc_perm", "Plot.Permutation"),
            Map.entry("metaHeatmap", "PlotMetaHeatmap"),
            Map.entry("pca_pair_meta", "PlotPCAPairSummaryMeta"),
            Map.entry("covariate_plot", "PlotCovariateMap"),
            Map.entry("aov2", "PlotANOVA2"),
            Map.entry("asca_impa", "PlotAscaImpVar"),
            Map.entry("asca_impb", "PlotAscaImpVar"),
            Map.entry("asca_impab", "PlotAscaImpVar"),
            Map.entry("rf_imp", "PlotRF.VIPMeta"),
            Map.entry("rf_cls", "PlotRF.ClassifyMeta"),
            Map.entry("rf_outlier", "PlotRF.Outlier"),
            Map.entry("mb", "PlotMBTimeProfile"),
            Map.entry("integ_peaks", "PlotPSEAIntegPaths"),
            Map.entry("peaks_to_paths", "PlotPeaks2Paths"),
            Map.entry("norm", "PlotNormSummary"),
            Map.entry("snorm", "PlotSampleNormSummary"),
            Map.entry("qea", "PlotQEA.Overview"),
            Map.entry("qea_dot", "PlotEnrichDotPlot"),
            Map.entry("ora_dot", "PlotEnrichDotPlot"),
            Map.entry("path_view", "PlotPathSummary"),
            Map.entry("svm_imp", "PlotRSVM.Cmpd"),
            Map.entry("svm_cls", "PlotRSVM.Classification"),
            Map.entry("opls_perm", "PlotOPLS.Permutation"),
            Map.entry("asca_scree", "PlotASCAModelScree"),
            Map.entry("asca_fa", "PlotASCAModel"),
            Map.entry("asca_fb", "PlotASCAModel"),
            Map.entry("asca_fab", "PlotASCAInteraction"),
            Map.entry("asca_perm", "PlotASCA.Permutation"),
            Map.entry("qc_meta_pca", "PlotMetaPCA"),
            Map.entry("pls_imp", "PlotPLS.Imp"),
            Map.entry("pls_cv", "PlotSPLSDA.Classification"),
            Map.entry("pls_perm", "PlotSPLS.Permutation"),
            Map.entry("qc_miss_filt", "PlotMissingDistr"),
            Map.entry("qc_missheatmap_filt", "PlotMissingHeatmap"),
            Map.entry("cls_roc_lr", "PlotROC.LRmodel")
            
    );
    
    
    
    private static final Map<String, List<String>> HELPERS = Map.ofEntries(
        Map.entry("PlotSPLSLoading", List.of("PlotImpVar")),
        Map.entry("PlotPLS.Imp", List.of("PlotImpVar")),
        Map.entry("PlotOPLS.Imp", List.of("PlotImpVar")),
        Map.entry("PlotRSVM.Cmpd", List.of("PlotImpVar")),
        Map.entry("PlotSPLSPairSummary", List.of("Plot.PairScatter")),
        Map.entry("PlotSAM.Cmpd", List.of(".prepare.sam.cmpd")),
        Map.entry("PlotEBAM.Cmpd", List.of(".prepare.ebam.cmpd")),
        Map.entry("PlotPCAPairSummary", List.of("Plot.PairScatter")),
        Map.entry("PlotPLSPairSummary", List.of("Plot.PairScatter")),
        Map.entry("PlotScatter", List.of(".mr_scatterPlot")),
        Map.entry("PlotForest", List.of(".mr_forestPlot")),
        Map.entry("PlotFunnel", List.of(".mr_funnelPLot")),
        Map.entry("PlotLeaveOneOut", List.of(".mr_looPlot")),
        Map.entry("rf_imp", List.of("PlotImpVarMeta")),
        Map.entry("PlotQEA.Overview", List.of("PlotMSEA.Overview"))
    );
    
   
}