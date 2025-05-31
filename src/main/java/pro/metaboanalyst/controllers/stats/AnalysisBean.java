/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.ChemoMetrics;
import pro.metaboanalyst.rwrappers.Classifying;
import pro.metaboanalyst.rwrappers.Clustering;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.SigVarSelect;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("analBean")
public class AnalysisBean implements Serializable {

    @Inject
    private SessionBean1 sb;

    @Inject
    private WorkflowBean wb;

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {

            String pageURL = "";
            switch (pageID) {
                case "PCA" -> {
                    doDefaultPCA();
                    //    pageURL = "/Secure/analysis/PCAView.xhtml";
                }
                case "ANOVA" -> {
                    doDefaultANOVA();
                    //    pageURL = "/Secure/analysis/AnovaView.xhtml";
                }
                case "Fold change" -> {
                    doDefaultFC();
                    //   pageURL = "/Secure/analysis/FoldChangeView.xhtml";
                }
                case "T-test" -> {
                    doDefaultTT();
                    //   pageURL = "/Secure/analysis/TtestView.xhtml";
                }
                case "Volcano plot" -> {
                    doDefaultVC();
                    //   pageURL = "/Secure/analysis/VolcanoView.xhtml";
                }
                case "Correlations" -> {
                    //doDefaultCorrelation();
                    //   pageURL = "/Secure/analysis/CorrelationView.xhtml";
                }
                case "PLSDA" -> {
                    doDefaultPLSDA();
                    //    pageURL = "/Secure/analysis/PLSDAView.xhtml";
                }
                case "sPLSDA" -> {
                    doDefaultSPLSDA();
                    //   pageURL = "/Secure/analysis/SparsePLSDAView.xhtml";
                }
                case "OrthoPLSDA" -> {
                    doDefaultOPLSDA();
                    //   pageURL = "/Secure/analysis/OrthoPLSDAView.xhtml";
                }
                case "SAM" -> {
                    doDefaultSAM();
                    //   pageURL = "/Secure/analysis/SAMView.xhtml";
                }
                case "EBAM" -> {
                    doDefaultEBAM();
                    //   pageURL = "/Secure/analysis/EBAMView.xhtml";
                }
                case "Dendrogram" -> {
                    doDefaultDendrogram();
                    // pageURL = "/Secure/analysis/TreeView.xhtml";
                }
                case "Heatmap" -> {
                    //  pageURL = "/Secure/analysis/HeatmapView.xhtml";
                    //doDefaultHmClust();
                }
                case "K-means" -> {
                    //  pageURL = "/Secure/analysis/KMView.xhtml";
                    doDefaultKmeanClust();
                }
                case "SOM" -> {
                    // pageURL = "/Secure/analysis/SOMView.xhtml";
                    doDefaultSOMClust();
                }
                case "RandomForest" -> {
                    // pageURL = "/Secure/analysis/RFView.xhtml";
                    doDefaultRF();
                }
                case "SVM" -> {
                    //pageURL = "/Secure/analysis/RSVMView.xhtml";
                    doDefaultSVM();
                }
            }
            sb.addNaviTrack(pageID, pageURL);
            sb.setCurrentNaviUrl(pageURL);
        }
    }

    public void doDefaultANOVA() {

        int res = UniVarTests.performANOVA(sb, "F", 0.05);
        if (res == 0) {
            sb.setAnovaSig(false);
        } else {
            sb.setAnovaSig(true);
        }
        UniVarTests.plotAOV(sb, sb.getCurrentImage("aov"), "png", 72);

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_aovButton_action(ub);
        wb.getCalledWorkflows().add("ANOVA");
    }

    public void doDefaultFC() {
        UniVarTests.initFC(sb, 2, 0, "FALSE");
        UniVarTests.plotFC(sb, sb.getCurrentImage("fc"), "png", 72);

        UnivBean ub = (UnivBean) DataUtils.findBean("univBean");
        JavaRecord.record_fcButton_action(ub);
        wb.getCalledWorkflows().add("Fold change");
    }

    public void doDefaultTT() {
        //this is perform before intial data analysis
        int res = UniVarTests.performTtests(sb, "F", 0.05, "FALSE", "TRUE", "fdr");//default not paired and equal variance
        if (res == 0) {
            sb.setTtSig(false);
        } else {
            sb.setTtSig(true);
        }
        UniVarTests.plotTT(sb, sb.getCurrentImage("tt"), "png", 72);

        wb.getCalledWorkflows().add("T-test");
    }

    public void doDefaultVC() {
                            System.out.println("doDefaultVC=====");

        UniVarTests.performVolcano(sb, "FALSE", 2, 0, "F", 0.1, "TRUE", "raw");
        UniVarTests.plotVolcano(sb, sb.getCurrentImage("volcano"), 1, 0, "png", 72, -1);

        //UnivBean ub = DataUtils.findBean("univBean");
        //JavaRecord.record_aovButton_action(ub);
    }

    public void doDefaultCorrelation() {
        UniVarTests.plotCorrHeatMap(sb, sb.getCurrentImage("corr"), "png", 72, "col", "pearson", "bwm", "F", "F", 6, 10, 0.0);
    }


    public void doDefaultPCA() {
        if (ChemoMetrics.initPCA(sb)) {
            ChemoMetrics.plotPCAPairSummary(sb, sb.getCurrentImage("pca_pair"), "png", 72, 5);
            ChemoMetrics.plotPCAScree(sb, sb.getCurrentImage("pca_scree"), "png", 72, 5);
            ChemoMetrics.plotPCA2DScore(sb, sb.getCurrentImage("pca_score2d"), "png", 72, 1, 2, 0.95, 0, 0, "na");
            ChemoMetrics.plotPCALoading(sb, sb.getCurrentImage("pca_loading"), "png", 72, 1, 2);  // setLoadingTable(pcImpInx);
            ChemoMetrics.plotPCABiplot(sb, sb.getCurrentImage("pca_biplot"), "png", 72, 1, 2, 10);
            // ChemoMetrics.PlotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "png", 72, 1, 2, 3, 40);
            ChemoMetrics.plotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "json", 72, 1, 2, 3);
            ChemoMetrics.plotPCA3DLoading(sb, sb.getCurrentImage("pca_loading3d"), "json", 72, 1, 2, 3);
            wb.getCalledWorkflows().add("PCA");
        } else {
            wb.getCalledWorkflowsError().add("PCA");
            // Seems not working due the pre-render view will cause issue
            // TODO: to redesign to enable the ERROR SHOWING
            sb.addMessage("Error", RDataUtils.getErrMsg(sb.getRConnection()));
        }
    }

    public void doDefaultPLSDA() {
        if (ChemoMetrics.initPLS(sb)) {
            ChemoMetrics.plotPLSPairSummary(sb, sb.getCurrentImage("pls_pair"), "png", 72, ChemoMetrics.getDefaultPLSPairNumber(sb));
            ChemoMetrics.plotPLS2DScore(sb, sb.getCurrentImage("pls_score2d"), "png", 72, 1, 2, 0.95, 0, 0, "na");
            // ChemoMetrics.PlotPLS3DScore(sb, sb.getCurrentImage("pls_score3d"), "png", 72, 1, 2, 3, 40);
            ChemoMetrics.plotPLS3DScore(sb, sb.getCurrentImage("pls_score3d"), "json", 72, 1, 2, 3);
            ChemoMetrics.plotPLSLoading(sb, sb.getCurrentImage("pls_loading"), "png", 72, 1, 2);
            ChemoMetrics.plotPLS3DLoading(sb, sb.getCurrentImage("pls_loading3d"), "json", 72, 1, 2, 3);
            ChemoMetrics.plotPLSImp(sb, sb.getCurrentImage("pls_imp"), "png", 72, "vip", "Comp. 1", 15, "FALSE");
            ChemoMetrics.plotPLSBiplot(sb, sb.getNewImage("pls_biplot"), "png", 72, 1, 2, 10);
            wb.getCalledWorkflows().add("PLSDA");

            // Disable the default analysis, which could take very long for large data
            // It should be done by user explicit selection
            //String cvMethod = "T";
            //int minSize = RDataUtils.getMinGroupSize(sb.getRConnection());
            //if (minSize < 11) {
            //    cvMethod = "L";
            //}
            //ChemoMetrics.trainPLSClassifier(sb, cvMethod, ChemoMetrics.getDefaultPLSCVNumber(sb), "Q2");
            //ChemoMetrics.plotPLSClassification(sb, sb.getCurrentImage("pls_cv"), "png", 72);
        } else {
            wb.getCalledWorkflowsError().add("PLSDA");
            // Seems not working due the pre-render view will cause issue
            // TODO: to redesign to enable the ERROR SHOWING
            sb.addMessage("Error", RDataUtils.getErrMsg(sb.getRConnection()));
        }
    }

    public void doDefaultSPLSDA() {

        ChemoMetrics.initSPLS(sb, 5, 10, "same", "Mfold", 5, "F");
        ChemoMetrics.plotSPLSPairSummary(sb, sb.getCurrentImage("spls_pair"), "png", 72, ChemoMetrics.getDefaultSPLSPairNumber(sb));
        ChemoMetrics.plotSPLS2DScore(sb, sb.getCurrentImage("spls_score2d"), "png", 72, 1, 2, 0.95, 0, 0, "na");
        ChemoMetrics.plotSPLS3DScore(sb, sb.getCurrentImage("spls_score3d"), "json", 72, 1, 2, 3);
        ChemoMetrics.plotSPLSLoading(sb, sb.getCurrentImage("spls_loading"), "png", 72, 1, "overview");
        //ChemoMetrics.plotSPLSDAClassification(sb, sb.getCurrentImage("spls_cv"), "png", 72);
        ChemoMetrics.plotSPLS3DLoading(sb, sb.getCurrentImage("spls_loading3d"), "json", 72, 1, 2, 3);
        wb.getCalledWorkflows().add("sPLSDA");

    }

    public void doDefaultOPLSDA() {
        //OPLSDABean b = DataUtils.findBean("oplsBean");
        ChemoMetrics.initOPLS(sb);
        ChemoMetrics.plotOPLS2DScore(sb, sb.getCurrentImage("opls_score2d"), "png", 72, 1, 2, 0.95, 0, 0, "na");
        ChemoMetrics.plotOplsSplot(sb, sb.getCurrentImage("opls_splot"), "all", "png", 72);
        ChemoMetrics.plotOPLSImp(sb, sb.getCurrentImage("opls_imp"), "png", 72, "vip", "tscore", 15, "FALSE");
        ChemoMetrics.plotOplsMdlView(sb, sb.getCurrentImage("opls_mdl"), "png", 72);

        wb.getCalledWorkflows().add("OrthoPLSDA");

    }

    public void doDefaultSAM() {
        SigVarSelect.initSAM(sb, "d.stat", "FALSE", "TRUE", 0, sb.getCurrentImage("sam_imp"));
        //double delta = SigVarSelect.GetSAMSuggestedDelta(sb);
        //SigVarSelect.PlotSAM_Cmpd(sb, sb.getCurrentImage("sam_imp"), "png", 72);
        SigVarSelect.plotSAM_FDR(sb, sb.getCurrentImage("sam_view"), "png", 72);
        wb.getCalledWorkflows().add("SAM");

    }

    public void doDefaultEBAM() {
        SigVarSelect.initEBAM(sb, "FALSE", "TRUE", "FALSE", -99, 0.9, sb.getCurrentImage("ebam_view"), sb.getCurrentImage("ebam_imp"));
        //SigVarSelect.PlotEBAM_A0(sb, sb.getCurrentImage("ebam_view"), "png", 72);
        //double a0 = SigVarSelect.GetEBAMSuggestedA0(sb);
        //SigVarSelect.InitEBAM_Cmpd(sb, "z.ebam", a0, "FALSE", "TRUE");
        //SigVarSelect.PlotEBAM_Cmpd(sb, sb.getCurrentImage("ebam_imp"), "png", 72, 0.9);
        wb.getCalledWorkflows().add("EBAM");
    }

    public void doDefaultDendrogram() {
        Clustering.plotClustTree(sb, sb.getCurrentImage("tree"), "png", 72, "euclidean", "ward.D");
        wb.getCalledWorkflows().add("Dendrogram");

    }

    public void doDefaultHmClust() {
        Clustering.plotHeatMap(sb, sb.getCurrentImage("heatmap"), "png", 72, "norm", "row", "euclidean", "ward.D", "bwj", 8, 8, 10, 0.05, 10, 10, "T", "T", "T", "F", "T", "T", "T", "T", 5000);
        wb.getCalledWorkflows().add("Heatmap");

    }

    public void doDefaultKmeanClust() {
        Clustering.plotKmeans(sb, sb.getCurrentImage("km"), "png", 72, 3, "default", "T");
        Clustering.plotKmeansPCA(sb, sb.getCurrentImage("km_pca"), "png", 72, "default", "T");
        wb.getCalledWorkflows().add("K-means");

    }

    public void doDefaultSOMClust() {
        Clustering.plotSOM(sb, sb.getCurrentImage("som"), "png", 72, 1, 3, "linear", "gaussian", "default", "T");
        Clustering.plotSOMPCA(sb, sb.getCurrentImage("som_pca"), "png", 72, "default", "T");
        wb.getCalledWorkflows().add("SOM");

    }

    public void doDefaultRF() {
        ClassificationBean b = (ClassificationBean) DataUtils.findBean("classBean");

        Classifying.initRF(sb, b.getTreeNum(), b.getTryNum(), b.getRfRandom());
        Classifying.plotRFClassication(sb, sb.getCurrentImage("rf_cls"), "png", 72);
        Classifying.plotRFCmpd(sb, sb.getCurrentImage("rf_imp"), "png", 72);
        Classifying.plotRFOutlier(sb, sb.getCurrentImage("rf_outlier"), "png", 72);
        wb.getCalledWorkflows().add("Random Forest");

        //JavaRecord.record_rfBn_action(b);
    }

    public void doDefaultSVM() {
        ClassificationBean b = (ClassificationBean) DataUtils.findBean("classBean");

        Classifying.initSVMAnal(sb, b.getValidationOpt());
        Classifying.plotSVMClassification(sb, sb.getCurrentImage("svm_cls"), "png", 72);
        Classifying.plotSVMSigCmpds(sb, sb.getCurrentImage("svm_imp"), "png", 72);
        wb.getCalledWorkflows().add("SVM");

        //ClassificationBean b = DataUtils.findBean("classBean");
        //JavaRecord.record_svmBn_action(b);
    }

}
