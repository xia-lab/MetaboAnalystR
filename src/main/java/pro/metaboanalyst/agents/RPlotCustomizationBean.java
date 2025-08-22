package pro.metaboanalyst.agents;

import com.fasterxml.jackson.annotation.JsonIgnore;
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
import pro.metaboanalyst.controllers.stats.RocAnalBean;

@Named("rPlotCustomizationBean")
@ViewScoped
public class RPlotCustomizationBean implements Serializable {

    @JsonIgnore
    @Inject
    private RocAnalBean rcb;
    
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
     * public void applyCustomization() { try { if (prompt == null ||
     * prompt.trim().isEmpty()) { sb.addMessage("error", "Please provide
     * customization instructions"); return; }
     *
     * String source = sb.getImageSource(); plotType =
     * GRAPHICS_CMD_TO_R_FUNC.get(source);
     *
     * if (plotType == null) { sb.addMessage("error", "Unsupported plot type for
     * source: " + source); return; }
     *
     * // Get the R command from the session bean's graphics map String
     * rCommand = sb.getGraphicsMap().get(source); if (rCommand == null) {
     * sb.addMessage("error", "No R command found for source: " + source);
     * return; }
     *
     * // Get the AI response String response =
     * aiCustomizer.customizePlot(source, plotType, prompt); aiResponse =
     * response; } catch (Exception e) { sb.addMessage("error", "Error applying
     * customization: " + e.getMessage()); } }
     *
     */
    public void applyCustomization() {
        try {
            if (prompt == null || prompt.trim().isEmpty()) {
                sb.addMessage("error", "Please provide customization instructions");
                return;
            }

            String source = sb.getImageSource();
            if (source.contains("roc_univ_")) {
                plotType = "Perform.UnivROC";
            } else if (source.contains("roc_boxplot_")) {
                plotType = "PlotRocUnivBoxPlot";
            } else {
                plotType = GRAPHICS_CMD_TO_R_FUNC.get(source);
            }

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
        if (plotSource.equals("cmpd")) {
            if (!sb.showMultiBoxView()) {
                System.out.println(sb.getCmpdSummaryImg());
                return sb.getCmpdSummaryImg() + "?t=" + System.currentTimeMillis();
            } else {
                System.out.println(sb.getBoxplotUrl());
                return sb.getBoxplotUrl() + "?t=" + System.currentTimeMillis();
            }
        } else if (plotSource.equals("roc_univ_")) {
            return rcb.getRocUnivImg() + "?t=" + System.currentTimeMillis();
        }else if (plotSource.equals("roc_boxplot_")) {
            return rcb.getRocUnivBPImg() + "?t=" + System.currentTimeMillis();
        } else {
            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(plotSource) + "dpi150.png" + "?t=" + System.currentTimeMillis();
        }
    }

    public static final Map<String, String> GRAPHICS_CMD_TO_R_FUNC = Map.ofEntries(
            Map.entry("norm", "PlotNormSummary"), // ok
            Map.entry("snorm", "PlotSampleNormSummary"), //ok
            Map.entry("qc_rsd", "PlotRSDViolin"),
            Map.entry("aov", "PlotANOVA"),
            Map.entry("volcano", "PlotVolcano"), // ok
            Map.entry("venn", "plotVennDiagram"),
            Map.entry("scatter3d", "Plot3D"),
            Map.entry("heatmap", "PlotHeatmap"),
            Map.entry("pca", "PlotPCA"),
            Map.entry("plsda", "PlotPLSDA"),
            Map.entry("sparseplsda", "PlotSparsePLSDA"),
            Map.entry("rf", "PlotRandomForest"),
            Map.entry("cls_roc", "PlotROC"), // ok
            Map.entry("tsne", "PlotTSNE"),
            Map.entry("umap", "PlotUMAP"),
            Map.entry("clustpca", "PlotClustPCA"),
            Map.entry("pca_biplot", "PlotPCABiplot"), //ok
            Map.entry("tt", "PlotTT"), // ok
            Map.entry("cmpdview", "PlotCmpdView"),
            Map.entry("metabolite_dr_curve", "PlotMetaboliteDRCurve"),
            Map.entry("drmodelbars", "PlotDRModelBars"),
            Map.entry("drhistogram", "PlotDRHistogram"),
            Map.entry("pcapair", "PlotPCAPairSummaryMeta"),
            Map.entry("selectedfeature", "PlotSelectedFeature"),
            Map.entry("roc_boxplot_", "PlotRocUnivBoxPlot"), // to fix
            Map.entry("roc_univ_", "Perform.UnivROC"),//to fix
            Map.entry("cls_imp", "PlotImpBiomarkers"), // ok
            Map.entry("pca_scree", "PlotPCAScree"), // ok
            Map.entry("pca_pair", "PlotPCAPairSummary"), // ok
            Map.entry("spls_perm", "PlotSPLS.Permutation"),
            Map.entry("dose_volcano", "PlotDoseVolcano"), // Error message: No R command found for source: dose_volcano
            Map.entry("dr_histogram", "PlotDRHistogram"), // ok
            Map.entry("raw_spec_stic", "plotSingleTIC"),
            Map.entry("plot_kegg_graph", "PlotKEGGPath"),
            Map.entry("tree", "PlotHCTree"), // ok
            Map.entry("pca_loading", "PlotPCALoading"), //ok
            Map.entry("pca_score2d", "PlotPCA2DScore"), //ok
            Map.entry("pca_score2d_meta", "PlotPCA2DScoreMeta"), // ok
            Map.entry("load_boxplot", "PlotLoadBoxplot"),
            Map.entry("power_stat", "PlotPowerStat"),
            Map.entry("power_profile", "PlotPowerProfile"),
            Map.entry("plot_bpis", "plotBPIs"),
            Map.entry("plot_rtcor", "PlotSpectraRTadj"),
            Map.entry("plot_bpicor", "PlotSpectraBPIadj"),
            Map.entry("cmpd_summary", "PlotCmpdSummary"),
            Map.entry("multifac_cmpd_summary", "PlotMultiFacCmpdSummary"),
            Map.entry("corr_heatmap", "PlotStaticCorrHeatMap"),
            Map.entry("metaCorrHeatmap", "PlotMetaCorrHeatmap"), // AI can update the code, no update in UI, but new plot exist in user folder
            Map.entry("meta_density", "PlotMetaDensity"),
            Map.entry("pathway_meta", "PlotPathwayMetaAnalysis"),
            Map.entry("asca_model", "PlotASCAModel"),
            Map.entry("asca_interaction", "PlotASCAInteraction"),
            Map.entry("_mr_scatter_plot", "PlotScatter"), // unsupport plot type, perhaps should be `mr_scatter_plot`? (source: /mgwas/ResultView.xhtml)
            Map.entry("_mr_forest_plot", "PlotForest"), // unsupport plot type, perhaps should be `mr_forest_plot`? (source: /mgwas/ResultView.xhtml)
            Map.entry("_mr_leaveoneout_plot", "PlotLeaveOneOut"), // unsupport plot type, perhaps should be `mr_leaveoneout_plot`? (source: /mgwas/ResultView.xhtml)
            Map.entry("_mr_funnel_plot", "PlotFunnel"), // unsupport plot type, perhaps should be `mr_funnel_plot`? (source: /mgwas/ResultView.xhtml)
            Map.entry("fc", "PlotFC"), // ok
            Map.entry("opls_score2d", "PlotOPLS2DScore"), //ok
            Map.entry("opls_splot", "PlotOPLS.Splot"), // ok
            Map.entry("opls_imp", "PlotOPLS.Imp"), // ok
            Map.entry("opls_mdl", "PlotOPLS.MDL"), //ok
            Map.entry("sam_imp", "PlotSAM.Cmpd"), // AI receives and changed the code, but no change in user interface, no new plot in user folder
            Map.entry("ebam_imp", "PlotEBAM.Cmpd"), // AI receives and changed the code, but no change in user interface, no new plot in user folder
            Map.entry("spls_pair", "PlotSPLSPairSummary"), //ok
            Map.entry("spls_loading", "PlotSPLSLoading"), // ok
            Map.entry("spls_score2d", "PlotSPLS2DScore"), // ok
            Map.entry("spls_cv", "PlotSPLSDA.Classification"), // ok
            Map.entry("ptn", "PlotCorr"), // AI can update the code, new image in users folder, web interface not updating
            Map.entry("km_pca", "PlotClustPCA"), // ok
            Map.entry("km", "PlotKmeans"), // ok
            Map.entry("som_pca", "PlotClustPCA"), // ok
            Map.entry("som", "PlotSOM"), // ok
            Map.entry("pca_score3d", "PlotPCA3DScore"),
            Map.entry("pca_loading3d", "PlotPCA3DLoading"),
            Map.entry("pls_score2d", "PlotPLS2DScore"),
            Map.entry("pls_score3d", "PlotPLS3DScoreImg"),
            Map.entry("pls_loading", "PlotPLSLoading"), //ok
            Map.entry("pls_loading3d", "PlotPLS3DLoading"),
            Map.entry("pls_biplot", "PlotPLSBiplot"),
            Map.entry("pls_pair", "PlotPLSPairSummary"),
            Map.entry("cls_prob", "PlotProbView"), // ok
            Map.entry("cls_accu", "PlotAccuracy"), // ok
            Map.entry("cls_test_roc", "PlotROCTest"), //ok
            Map.entry("cls_test_prob", "PlotProbViewTest"), //ok
            Map.entry("cls_test_accu", "PlotTestAccuracy"), // ok
            Map.entry("roc_perm", "Plot.Permutation"), //ok
            Map.entry("metaHeatmap", "PlotMetaHeatmap"), // AI can update the code, but the final function is called: PlotStaticMetaHeatmapAI, differs from original
            Map.entry("pca_pair_meta", "PlotPCAPairSummaryMeta"),
            Map.entry("covariate_plot", "PlotCovariateMap"), // ok
            Map.entry("aov2", "PlotANOVA2"), // ok
            Map.entry("asca_impa", "PlotAscaImpVar"),
            Map.entry("asca_impb", "PlotAscaImpVar"),
            Map.entry("asca_impab", "PlotAscaImpVar"),
            Map.entry("rf_imp", "PlotRF.VIP"), // updated, ok now
            Map.entry("rf_cls", "PlotRF.ClassifyMeta"),
            Map.entry("rf_outlier", "PlotRF.Outlier"),
            Map.entry("mb", "PlotMBTimeProfile"), // Ai updated and generated the code, no update in users folder
            Map.entry("integ_peaks", "PlotPSEAIntegPaths"), // ok
            Map.entry("peaks_to_paths", "PlotPeaks2Paths"),
            Map.entry("peaks_to_paths_gsea", "PlotPeaks2Paths"),
            Map.entry("qea", "PlotQEA.Overview"), // ok
            Map.entry("qea_dot", "PlotEnrichDotPlot"), // ok
            Map.entry("ora_dot", "PlotEnrichDotPlot"),
            Map.entry("path_view", "PlotPathSummary"), // ok
            Map.entry("svm_imp", "PlotRSVM.Cmpd"), // ok
            Map.entry("svm_cls", "PlotRSVM.Classification"), // ok
            Map.entry("opls_perm", "PlotOPLS.Permutation"), // ok
            Map.entry("asca_scree", "PlotASCAModelScree"),
            Map.entry("asca_fa", "PlotASCAModel"),
            Map.entry("asca_fb", "PlotASCAModel"),
            Map.entry("asca_fab", "PlotASCAInteraction"),
            Map.entry("asca_perm", "PlotASCA.Permutation"),
            Map.entry("qc_meta_pca", "PlotMetaPCA"), // ok
            Map.entry("pls_imp", "PlotPLS.Imp"), //ok
            Map.entry("pls_cv", "PlotPLS.Classification"), // ok
            Map.entry("pls_perm", "PlotPLS.Permutation"),
            Map.entry("qc_miss_filt", "PlotMissingDistr"), //ok
            Map.entry("qc_missheatmap_filt", "PlotMissingHeatmap"), // ok
            Map.entry("cls_roc_lr", "PlotROC.LRmodel"),
            Map.entry("cmpd", "PlotCmpdSummary"), // ok
            Map.entry("dr_barplot", "PlotDRModelBars"), // Unsupported plot type for source: dr_barplot
            Map.entry("rf_imp_meta", "PlotRF.VIPMeta") // no backend logic to link up this script to the front-end

    );

    private static final Map<String, List<String>> HELPERS = Map.ofEntries(
            Map.entry("PlotSPLSLoading", List.of("PlotImpVar")),
            Map.entry("PlotPLS.Imp", List.of("PlotImpVar")),
            Map.entry("PlotOPLS.Imp", List.of("PlotImpVar")),
            Map.entry("PlotSPLSPairSummary", List.of("Plot.PairScatter")),
            Map.entry("PlotSAM.Cmpd", List.of(".prepare.sam.cmpd")),
            Map.entry("PlotEBAM.Cmpd", List.of(".prepare.ebam.cmpd")),
            Map.entry("PlotPCAPairSummary", List.of("Plot.PairScatter")),
            Map.entry("PlotPLSPairSummary", List.of("Plot.PairScatter")),
            Map.entry("PlotScatter", List.of(".mr_scatterPlot")),
            Map.entry("PlotForest", List.of(".mr_forestPlot")),
            Map.entry("PlotFunnel", List.of(".mr_funnelPLot")),
            Map.entry("PlotLeaveOneOut", List.of(".mr_looPlot")),
            Map.entry("PlotRF.VIPMeta", List.of("PlotImpVarDisc")), // updated
            Map.entry("PlotRF.VIP", List.of("PlotImpVar")),
            Map.entry("PlotQEA.Overview", List.of("PlotMSEA.Overview"))
    );

}
