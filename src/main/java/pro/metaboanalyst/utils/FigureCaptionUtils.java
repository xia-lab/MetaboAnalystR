/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.Arrays;
import java.util.List;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;

/**
 * @author jianguox
 */
@RequestScoped
@Named("figureCaptionUtils")
public class FigureCaptionUtils {

    @Inject
    private SessionBean1 sb;

    public String obtainLegend(String analNavi) {
        String text = RDataUtils.getImageDesc(sb.getRConnection(), analNavi);
        if (text == null || text.equals("NA")) {
            switch (analNavi) {
                case "upset" -> {
                    text = "<b>[Upset Diagram]</b>: Screenshot of an interactive Upset diagram. Each bar indicates the number of significant features identified in individual datasets.";
                }
                case "pca_3d" -> {
                    text = "<b>[3D PCA]</b>: Screenshot of an interactive 3D PCA plot. Rotate to view separation patterns from different perspectives.";
                }
                case "plsda_3d" ->
                    text = "<b>[3D PLS-DA]</b>: Screenshot of interactive 3D PLSDA plot. Rotate to view separation patterns from different perspectives.";
                case "splsda_3d" ->
                    text = "<b>[3D sPLS-DA]</b>: Screenshot of interactive 3-D sPLSDA plot. Rotate to view separation patterns from different perspectives.";
                case "ipca_3d" ->
                    text = "<b>[3D PCA]</b>: Screenshot of an interactive 3D PCA plot. Rotate to view separation patterns from different perspectives.";
                case "heatmap2" ->
                    text = "<b>[Clustering Heatmaps]</b>: Screenshot of an interactive heatmap displaying relationship between feature abundance and metadata factors.";
                case "opls_score2d" ->
                    text = "<b>[OPLS-DA]</b>: The main patterns of sample separations based on OPLS-DA.";
                case "opls_splot" ->
                    text = "<b>[S-plot]</b>: OPLS-DA S-plot showing the variable importance in a model, combining the covariance and the correlation (p(corr)) loading profile.";
                case "opls_mdl" ->
                    text = "<b>[Model overview]</b>: Overview of the OPLS-DA model. R2X and R2Y denote the explained rate of the proposed model for X and Y matrices. Q2 indicates the model predictive ability";
                case "opls_perm" ->
                    text = "<b>[Permutation Tests]</b>: OPLS-DA Permutation analysis, showing the observed and cross-validated R2Y and Q2 coefficients.";
                case "pls_pair" ->
                    text = "<b>[Pairwise Score Plots]</b>: Pairwise PLS-DA scores plots between the selected components. The explained variance of each component is shown in the corresponding diagonal cell.";
                case "spls_pair" ->
                    text = "<b>[Pairwise Score Plots]</b>: Pairwise sPLS-DA scores plots between the selected components. The explained variance of each component is shown in the corresponding diagonal cell.";
                case "spls_score2d" ->
                    text = "<b>[Score Plot]</b>: sPLS-DA Scores plot between the selected PCs. The explained variances are shown in brackets.";
                case "spls_loading" ->
                    text = "<b>[Loading Plot]</b>: The plot shows the variables selected by the sPLS-DA model for a given component. The variables are ranked by the absolute values of their loadings.";
                case "spls_cv" ->
                    text = "<b>[Cross-Validation]</b>: Plot of the performance of the sPLS-DA model evaluated using cross validations (CV) with increasing numbers of components created using the specified number of the variables. ";
                case "pls_score2d" ->
                    text = "<b>[Score Plot]</b>: PLS-DA scores plot between the selected componentss. The explained variances are shown in brackets.";
                case "pls_loading" ->
                    text = "<b>[Loading plot]</b>: PLS-DA loadings plot for the selected PCs. Click any data point to view a boxplot of the underlying feature.";
                case "pls_imp" ->
                    text = "<b>[VIP plot]</b>: The variable importance projection (VIP) score is calculated as a weighted sum of the squared correlations between the PLS-DA components and the original variable.";
                case "pls_cv" ->
                    text = "<b>[Cross-Validation]</b>: PLS-DA classification performance based on different number of components. The red star indicates the best classifier.";
                case "pls_perm" ->
                    text = "<b>[Permutation Tests]</b>: Validating PLS-DA model by permutation tests. An empircal p-value is also computed.";// could add more detail from java variables
                case "pca_pair" ->
                    text = "<b>[Pairwise Score Plots]</b>: The pairwise score plots between the selected PCs. The explained variance of each PC is shown in the corresponding diagonal cell.";
                case "pca_scree" ->
                    text = "<b>[Scress Plot]</b>: The green line on top shows the accumulated variance explained by PCs; the blue line underneath shows the variance explained by individual PC.";
                case "pca_score2d" ->
                    text = "<b>[Score Plots]</b>: Scores plot between the selected PCs. The explained variances are shown in brackets.";
                case "pca_biplot" ->
                    text = "<b>[Biplots]</b>: The points represent the scores (samples), and it uses arrows / vectors represent loadings, whose direction and length indicate their contribution to sample seperation patterns.";
                case "pca_pair_meta" ->
                    text = "<b>[Pairwise Score Plots]</b>: The plots indicate seperation patterns based on top PCs. The density plots at the diagnal indicate group dstributions.";
                case "heatmap_mummichog" -> {
                    text = "<b>[Heatmaps]</b>: Screenshot of an interactive peak intensity heatmap across samples. Users can explore enriched functions for any patterns of interest.";
                }
                case "heatmap_pathway" -> {
                    text = "<b>[Pathway Heatmap]</b>: Screenshot of an interactive peak heatmap of interest.";
                }
                case "network_gsea", "network_integ", "network_mummichog" -> {
                    switch (analNavi) {
                        case "network_gsea" -> {
                            text = "<b>[Metabolic Network]</b>: Screenshot of interactive network visualization displaying enriched pathways based on GSEA in the context of KEGG global metabolic network.";
                        }
                        case "network_integ" -> {
                            text = "<b>[Metabolic Network]</b>: Screenshot of interactive network visualization displaying enriched pathways based on integrative approach in the context of KEGG global metabolic network.";
                        }
                        default -> {
                            text = "<b>[Metabolic Network]</b>: Screenshot of interactive network visualization displaying enriched pathways based on mummichog in the context of KEGG global metabolic network.";
                        }
                    }
                }
                case "norm" ->
                    text = "<b>Nomalization</b> (feature-wise): Box plots and density plots before and after normalization. The boxplots show at most 50 features due to space limit.";
                case "correlation_heatmap" ->
                    text = "<b>[Correlation Heatmaps]</b>: The plot shows the overall correlations between different features / samples based on a selected distance measure.";
                case "volcano" -> {
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "volcano");
                }
                case "fc" -> {
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "fc");
                }
                case "tt" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "tt");
                case "ptn" ->
                    text = "<b>[Pattern Match]</b>: Important features selected by correlation analysis - light purple indicates positive correlation and blue indicate negative correlations.";
                case "ptn_multifac" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "ptn_multifac");
                case "sam" ->
                    text = "<b>[SAM Sig. Features]</b>: Significant features identified by SAM. The green circles represent features that exceed the specified threshold.";
                case "ebam" ->
                    text = "<b>[EBAM Sig. Features]</b>: Significant features identified by EBAM. The green circles represent features that exceed the specified threshold.";
                case "tree" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "tree");
                case "heatmap" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "heatmap");
                case "som" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "som");
                case "rf_cls" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "rf_cls");
                case "rf_imp" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "rf_imp");
                case "rf_outlier" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "rf_outlier");
                case "rf_cls_multifac" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), "rf_cls_multifac");
                case "rf_imp_multifac", "rf_outlier_multifac", "km", "km_pca", "svm_imp", "svm_cls", "roc_univ", "roc_boxplot", "cls_roc", "cls_prob", "cls_accu", "cls_imp", "cls_test_roc", "cls_test_prob", "cls_test_accu", "roc_perm", "roc_new_samples", "power_stat", "power_profile", "ora", "ora_dot", "qea", "qea_dot", "meta_peaks_to_paths", "meta_peaks_to_paths_gsea", "peaks_to_paths", "integ_peaks", "peaks_to_paths_gsea", "network_MetaboNet", "path_view_integ", "aov2", "asca_scree", "asca_fa", "asca_fab", "asca_fb", "asca_impa", "asca_impab", "asca_impb", "dose_volcano", "enrichment_network", "network", "gene_metabolites", "metabo_phenotypes", "metabo_metabolites", "global", "dspc", "PlotDRHistogram", "PlotDRModelBars", "mr_results_merge" ->
                    text = RDataUtils.getImageDesc(sb.getRConnection(), analNavi);
                case "mr_scatter_plot", "mr_forest_plot", "mr_leaveoneout_plot", "mr_funnel_plot" -> {
                    sb.setCurrentNaviUrl("/Secure/mgwas/MrResultView.xhtml");
                }
                default -> {
                    text = analNavi;
                }
            }
        }
        return text;
    }

    public String obtainCaption(String figure) {
        String text = "";
        return text;

    }

    private final List<String> ANALYSIS_ORDER = Arrays.asList(
            // 1. Data Preprocessing & Normalization
            "norm","snorm",
            // 3. Feature Selection & Differential Analysis
            "volcano", "fc", "tt", "sam", "ebam", "ptn", "ptn_multifac",
            "rf_cls", "rf_imp", "rf_outlier", "rf_cls_multifac", "rf_imp_multifac",
            "svm_imp", "svm_cls",
            // 2. Dimensionality Reduction & Clustering
            "pca_3d", "pca_pair", "pca_scree", "pca_score2d", "pca_biplot", "pca_pair_meta",
            "plsda_3d", "splsda_3d", "pls_pair", "spls_pair", "pls_score2d", "spls_score2d",
            "spls_loading", "pls_loading", "pls_imp", "spls_cv", "pls_cv", "pls_perm",
            "opls_score2d", "opls_splot", "opls_mdl", "opls_perm",
            "heatmap", "heatmap2", "correlation_heatmap", "heatmap_mummichog", "heatmap_pathway",
            // 4. Classification & Model Evaluation
            "roc_univ", "roc_boxplot", "cls_roc", "cls_prob", "cls_accu", "cls_imp",
            "cls_test_roc", "cls_test_prob", "cls_test_accu", "roc_perm", "roc_new_samples", "tree", "som", "dspc",
            // 5. Network & Pathway Analysis
            "network_gsea", "network_integ", "network_mummichog", "enrichment_network",
            "ora", "ora_dot", "qea", "qea_dot", "meta_peaks_to_paths", "meta_peaks_to_paths_gsea",
            "peaks_to_paths", "integ_peaks", "peaks_to_paths_gsea", "network_MetaboNet",
            "path_view_integ", "gene_metabolites", "metabo_phenotypes", "metabo_metabolites",
            // 6. Meta-Analysis & Multi-Omics Integration
            "meta_peaks_to_paths", "meta_peaks_to_paths_gsea", "integ_peaks",
            // 7. Power Analysis & Experimental Design
            "power_stat", "power_profile",
            // 8. GWAS & Mendelian Randomization
            "mr_results_merge", "mr_scatter_plot", "mr_forest_plot", "mr_leaveoneout_plot", "mr_funnel_plot",
            // 9. Miscellaneous Analyses & Custom Reports
            "upset",  "PlotDRHistogram", "PlotDRModelBars",
            // 10. ASCA Analysis
            "aov2", "asca_scree", "asca_fa", "asca_fab", "asca_fb", "asca_impa", "asca_impab", "asca_impb"
    );

    public List<String> getANALYSIS_ORDER() {
        return ANALYSIS_ORDER;
    }
}
