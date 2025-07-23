package pro.metaboanalyst.visualization;

import java.io.Serializable;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import pro.metaboanalyst.agents.RPlotCustomizationAgent;
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

    public String getPreviewImage() {
        String plotSource = sb.getImageSource();
        if (sb.getImageSource().equals("volcano")) {
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
            Map.entry("sparseplsda", "PlotSparsePLSDA"),
            Map.entry("rf", "PlotRandomForest"),
            Map.entry("roc", "PlotROC"),
            Map.entry("tsne", "PlotTSNE"),
            Map.entry("umap", "PlotUMAP"),
            Map.entry("som", "PlotSOM"),
            Map.entry("clustpca", "PlotClustPCA"),
            Map.entry("pcabiplot", "PlotPCABiplot"),
            Map.entry("tt", "PlotTT"),
            Map.entry("cmpdview", "PlotCmpdView"),
            Map.entry("drcurve", "PlotMetaboliteDRCurve"),
            Map.entry("drmodelbars", "PlotDRModelBars"),
            Map.entry("drhistogram", "PlotDRHistogram"),
            Map.entry("pca_pair_meta", "PlotPCAPairSummaryMeta"),
            Map.entry("selectedfeature", "PlotSelectedFeature"),
            Map.entry("rocboxplot", "PlotRocUnivBoxPlot"),
            Map.entry("testaccuracy", "PlotTestAccuracy"),
            Map.entry("impbiomarkers", "PlotImpBiomarkers"),
            Map.entry("pca_scree", "PlotPCAScree"),
            Map.entry("pca_pair", "PlotPCAPairSummary"),
            Map.entry("pls_biplot", "PlotPLSBiplot"),
            Map.entry("spls_perm", "PlotSPLS.Permutation"),
            Map.entry("spls_cv", "PlotSPLSDA.Classification"),
            Map.entry("dose_volcano", "PlotDoseVolcano"),
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
            Map.entry("meta_corr_heatmap", "PlotMetaCorrHeatmap"),
            Map.entry("meta_density", "PlotMetaDensity"),
            Map.entry("pathway_meta", "PlotPathwayMetaAnalysis"),
            Map.entry("asca_model", "PlotASCAModel"),
            Map.entry("asca_interaction", "PlotASCAInteraction"),
            Map.entry("mr_scatter", "PlotScatter"),
            Map.entry("mr_forest", "PlotForest"),
            Map.entry("mr_leaveoneout", "PlotLeaveOneOut"),
            Map.entry("mr_funnel", "PlotFunnel")
    );
    

}
