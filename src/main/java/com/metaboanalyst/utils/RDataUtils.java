package com.metaboanalyst.utils;

import java.util.Map;

public class RDataUtils {
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
            Map.entry("pcapair", "PlotPCAPairSummaryMeta"),
            Map.entry("selectedfeature", "PlotSelectedFeature"),
            Map.entry("rocboxplot", "PlotRocUnivBoxPlot"),
            Map.entry("testaccuracy", "PlotTestAccuracy"),
            Map.entry("impbiomarkers", "PlotImpBiomarkers")
    );
} 