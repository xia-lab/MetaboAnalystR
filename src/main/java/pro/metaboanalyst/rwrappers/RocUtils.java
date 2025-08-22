/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author jianguox
 */
public class RocUtils {

    private static final Logger LOGGER = LogManager.getLogger(RocUtils.class);

    public static String[] getModelNames(RConnection RC) {
        try {
            String rCommand = "GetModelNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getModelNames", rse);
        }
        return null;
    }

    public static String[] getGrp1SampleNames(RConnection RC) {
        try {
            String[] names = RC.eval("rownames(mSet$dataSet$norm)[mSet$dataSet$cls==levels(mSet$dataSet$cls)[1]]").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getGrp1SampleNames", e);
        }
        return null;
    }

    public static String[] getGrp2SampleNames(RConnection RC) {
        try {
            String[] names = RC.eval("rownames(mSet$dataSet$norm)[mSet$dataSet$cls==levels(mSet$dataSet$cls)[2]]").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getGrp2SampleNames", e);
        }
        return null;
    }

    public static void performRocCVExplorer(SessionBean1 sb, String cls, String rkOpt, int lvNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PerformCV.explore(NA" + ", \"" + cls + "\", \"" + rkOpt + "\", " + lvNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Multivariate ROC");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("PerformRocCVExplorer", rse);
        }
    }

    public static int performRocCVTest(RConnection RC, String cls, int lvNum) {
        try {
            String rCommand = "PerformCV.test(NA" + ", \"" + cls + "\", " + lvNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("PerformRocCVTest", rse);
        }
        return 0;
    }

    public static String performUnivROC(SessionBean1 sb, String featNm, int version, String format, String dpi, String isAUC, String isOpt, String optMethod, String isPartial, String measure, double cutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Perform.UnivROC(NA" + ", \"" + featNm + "\", " + version + ", \"" + format + "\", " + dpi + ", " + isAUC + ", " + isOpt + ", \"" + optMethod + "\", " + isPartial + ", \"" + measure + "\", " + cutoff + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("roc_univ_" + featNm, rCommand);
            sb.addGraphicsCMD("roc_univ_", rCommand);

            sb.addGraphicsMapLink("roc_univ_" + featNm, "/Secure/roc/UnivRocView.xhtml");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("PerformUnivROC", rse);
        }
        return null;
    }

    public static String plotUnivROCBP(SessionBean1 sb, String featNm, int version, String format, String dpi, String isOpt, String isQuery) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotRocUnivBoxPlot(NA" + ", \"" + featNm + "\", " + version + ", \"" + format + "\", " + dpi + ", " + isOpt + ", " + isQuery + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("roc_boxplot_" + featNm, rCommand);
            sb.addGraphicsCMD("roc_boxplot_", rCommand);

            sb.addGraphicsMapLink("roc_boxplot_" + featNm, "/Secure/roc/UnivRocView.xhtml");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("PlotUnivROCBP", rse);
        }
        return null;
    }

    public static void plotPredView(RConnection RC, String imgName, String format, int dpi) {
        try {
            String pdfCommand = "PlotPredView()";
            RCenter.recordRCommand(RC, pdfCommand);
            String webCommand = "PNG.PlotPredView(\"" + imgName + "\")";
            RC.voidEval(webCommand);
        } catch (RserveException rse) {
            LOGGER.error("PlotPredView", rse);
        }
    }

    public static void plotProbView(SessionBean1 sb, String imgName, String format, int dpi, int mdlInx, int showNm, int showPred) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotProbView(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + mdlInx + ", " + showNm + ", " + showPred + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("cls_prob", rCommand);
            sb.addGraphicsMapLink("cls_prob", "/Secure/roc/MultiRocView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "Multivariate ROC");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("PlotProbView", rse);
        }
    }

    public static void plotProbViewTest(SessionBean1 sb, String imgName, String format, int dpi, int mdlInx, int showNm, int showPred) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotProbViewTest(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + mdlInx + ", " + showNm + ", " + showPred + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("cls_test_prob", rCommand);
            sb.addGraphicsMapLink("cls_test_prob", "/Secure/roc/RocTestView.xhtml");
            sb.recordRCommandFunctionInfo(rCommand, "Evaluator ROC");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("PlotProbView", rse);
        }
    }

    public static void plotROC(SessionBean1 sb, String imgName, String format, int dpi, int mdlInx, String method, int showConf, int showHoldOut, String focus, double cutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotROC(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + mdlInx + ", \"" + method + "\", " + showConf + ", " + showHoldOut + ", \"" + focus + "\", " + cutoff + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            if (imgName.contains("cls_roc")) {
                sb.addGraphicsCMD("cls_roc", rCommand);
                sb.addGraphicsMapLink("cls_roc", "/Secure/roc/MultiRocView.xhtml");
                sb.recordRCommandFunctionInfo(rCommand, "Multivariate ROC");

            } else if (imgName.contains("cls_test_roc")) {
                sb.addGraphicsCMD("cls_test_roc", rCommand);
                sb.addGraphicsMapLink("cls_test_roc", "/Secure/roc/RocTestView.xhtml");
                sb.recordRCommandFunctionInfo(rCommand, "Evaluator ROC");

            }

        } catch (RserveException rse) {
            LOGGER.error("PlotROC", rse);
        }
    }

    public static void plotROCTest(SessionBean1 sb, String imgName, String format, int dpi, int mdlInx, String method, int showConf, int showHoldOut, String focus, double cutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotROCTest(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + mdlInx + ", \"" + method + "\", " + showConf + ", " + showHoldOut + ", \"" + focus + "\", " + cutoff + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_test_roc", rCommand);
            sb.addGraphicsMapLink("cls_test_roc", "/Secure/roc/RocTestView.xhtml");
            sb.recordRCommandFunctionInfo(rCommand, "Evaluator ROC");

        } catch (RserveException rse) {
            LOGGER.error("PlotROC", rse);
        }
    }

    public static void plotROCLR(SessionBean1 sb, String imgName, String format, int dpi, int showConf) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotROC.LRmodel(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + showConf + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_roc_lr", rCommand);
            sb.addGraphicsMapLink("cls_roc_lr", "/Secure/roc/RocTestView.xhtml");
            sb.recordRCommandFunctionInfo(rCommand, "Evaluator ROC");

        } catch (RserveException rse) {
            LOGGER.error("PlotROCLR", rse);
        }
    }

    public static int getBestModelInx(RConnection RC) {
        try {
            String rCommand = "GetBestModelIndex(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("getBestModelInx", rse);
            return 0;
        }
    }

    public static String getModelInfo(RConnection RC, int mdlInx, String usrName) {
        try {
            String rCommand = "GetModelInfo(" + mdlInx + ", \"" + usrName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getModelInfo", rse);
            return null;
        }
    }

    public static String getAccuSummary(RConnection RC) {
        try {
            String rCommand = "GetAccuracyInfo(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getAccuSummary", rse);
            return null;
        }
    }

    public static void plotImpBiomarkers(SessionBean1 sb, String imgName, String format, int dpi, int modelInx, String measure, int featNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotImpBiomarkers(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + modelInx + ", \"" + measure + "\", " + featNum + ");";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Multivariate ROC");

            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_imp", rCommand);
            sb.addGraphicsMapLink("cls_imp", "/Secure/roc/MultiRocView.xhtml");

        } catch (RserveException rse) {
            LOGGER.error("PlotImpBiomarkers", rse);
        }
    }

    public static String getConfusionMatrix(RConnection RC) {
        try {
            return RC.eval("GetCurrentConfMat(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("getConfusionMatrix", e);
        }
        return null;
    }

    public static String getConfusionMatrixTest(RConnection RC) {
        try {
            return RC.eval("GetCurrentConfMatTest(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("getConfusionMatrixTest", e);
        }
        return null;
    }

    public static void plotAccuracies(SessionBean1 sb, String imageName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotAccuracy(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Multivariate ROC");

            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_accu", rCommand);
            sb.addGraphicsMapLink("cls_accu", "/Secure/roc/MultiRocView.xhtml");

        } catch (RserveException rse) {
            LOGGER.error("PlotAccuracies", rse);
        }
    }

    public static void plotTestAccuracies(SessionBean1 sb, String imageName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotTestAccuracy(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_test_accu", rCommand);
            sb.addGraphicsMapLink("cls_test_accu", "/Secure/roc/RocTestView.xhtml");

        } catch (RserveException rse) {
            LOGGER.error("PlotTestAccuracies", rse);
        }
    }

    public static int performPermut(RConnection RC, String measure, int num) {
        try {
            String rCommand = "Perform.Permut(NA" + ", \"" + measure + "\", " + num + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("PerformPermut", rse);
        }
        return 0;
    }

    public static void plotPermut(SessionBean1 sb, String imageName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.Permutation(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("roc_perm", rCommand);
            sb.addGraphicsMapLink("roc_perm", "/Secure/roc/RocTestView.xhtml");

        } catch (RserveException rse) {
            LOGGER.error("PlotPermut", rse);
        }
    }

    public static double[][] getImpSigMat(RConnection RC) {
        try {

            String rCommand = "GetImpValues(NA)";
            double[][] loadings = RC.eval(rCommand).asDoubleMatrix();
            return loadings;
        } catch (Exception e) {
            LOGGER.error("GetImpValues", e);
        }
        return null;
    }

    public static String[] getImpRowNames(RConnection RC) {
        try {
            String rCommand = "GetImpRowNames(NA)";
            String[] rowNames = RC.eval(rCommand).asStrings();
            return rowNames;
        } catch (Exception e) {
            LOGGER.error("GetImpRowName", e);
        }
        return null;
    }

    public static String getRocSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetRocSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getImpHighLow(RConnection RC, int inx) {
        try {
            String rCommand = "GetImpHighLow(NA" + ", " + inx + ")";
            String[] rowNames = RC.eval(rCommand).asStrings();
            return rowNames;
        } catch (Exception e) {
            LOGGER.error("GetImpHighLow", e);
        }
        return null;
    }

    public static String[] getImpColNames(RConnection RC) {
        try {
            String rCommand = "GetImpColNames(NA)";
            String[] colNames = RC.eval(rCommand).asStrings();
            return colNames;
        } catch (Exception e) {
            LOGGER.error("GetImpColName", e);
        }
        return null;
    }

    public static void setCustomData(RConnection RC) {
        try {
            String rCommand = "SetCustomData(NA, selected.cmpds, selected.smpls)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setCustomData", e);
        }
    }

    public static double[][] getRocValues(RConnection RC) {
        try {

            String rCommand = "as.matrix(mSet$analSet$roc.mat)";
            double[][] loadings = RC.eval(rCommand).asDoubleMatrix();
            return loadings;
        } catch (Exception e) {
            LOGGER.error("GetRocValues", e);
        }
        return null;
    }

    public static String[] getRocColName(RConnection RC) {
        try {
            String rCommand = "colnames(mSet$analSet$roc.mat)";
            String[] colNames = RC.eval(rCommand).asStrings();
            return colNames;
        } catch (Exception e) {
            LOGGER.error("GetRocColName", e);
        }
        return null;
    }

    public static String[] getROCcoords(RConnection RC, String queryFld, double value, String plot, String imgNm) {
        try {
            String rCommand = "GetROC.coords(NA" + ", \"" + queryFld + "\", " + value + ", plot=" + plot + ", \"" + imgNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            LOGGER.error("GetROCcoords", rse);
        }
        return null;
    }

    public static double[][] getUnivFeatureRankingMat(RConnection RC) {
        try {
            String rCommand = "GetFeatureRankingMat()";
            double[][] mat = RC.eval(rCommand).asDoubleMatrix();
            return mat;
        } catch (Exception rse) {
            LOGGER.error("getUnivFeatureRankingMat", rse);
        }
        return null;
    }

    public static String[] getUnivRankedFeatureNames(RConnection RC) {
        try {
            String rCommand = "GetUnivRankedFeatureNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getUnivRankedFeatureNames", rse);
        }
        return null;
    }

    public static double[] getLassoFreqs(RConnection RC) {
        try {
            String rCommand = "GetLassoFreqs()";
            return RC.eval(rCommand).asDoubles();
        } catch (Exception rse) {
            LOGGER.error("getLassoFreqs", rse);
        }
        return null;
    }

    public static String[] getLassoFreqNames(RConnection RC) {
        try {
            String rCommand = "GetLassoFreqNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getLassoFreqNames", rse);
        }
        return null;
    }

    public static void updateKmeans(RConnection RC, int clustNm) {
        try {
            String rCommand = "UpdateKmeans(NA" + ", " + clustNm + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("updateKmeans", rse);
        }
    }

    public static void computeUnivFeatureRanking(RConnection RC) {
        try {
            String rCommand = "CalculateFeatureRanking(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("ComputeUnivFeatureRanking", rse);
        }
    }

    public static void prepareROCDetails(RConnection RC, String featNm) {
        try {
            String rCommand = "PrepareROCDetails(NA" + ", \"" + featNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("PrepareROCDetails", rse);
        }
    }

    public static void setAnalysisMode(SessionBean1 sb, String mode) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetAnalysisMode(NA" + ", \"" + mode + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Multivariate ROC");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setAnalysisMode", e);
        }
    }

    public static void prepareROCData(RConnection RC, String selMeta, String factor1, String factor2) {
        try {
            String rCommand = "PrepareROCData(NA, \"" + selMeta + "\",\"" + factor1 + "\",\"" + factor2 + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("prepareROCData", e);
        }
    }

    public static int containNewSamples(RConnection RC) {
        try {
            String rCommand = "ContainNewSamples(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("containNewSamples", e);
            return 0;
        }
    }

    public static String getLRConvergence(RConnection RC) {
        try {
            return RC.eval("GetLRConvergence()").asString();
        } catch (Exception e) {
            LOGGER.error("getLRConvergence", e);
        }
        return "FALSE";
    }

    public static String getLREquation(RConnection RC) {
        try {
            return RC.eval("GetLREquation()").asString();
        } catch (Exception e) {
            LOGGER.error("getLREquation", e);
        }
        return null;
    }

    public static String getLRmodelTable(RConnection RC) {
        try {
            return RC.eval("GetLRmodelTable()").asString();
        } catch (Exception e) {
            LOGGER.error("getLRmodelTable", e);
        }
        return null;
    }

    public static String getLRperformanceTable(RConnection RC) {
        try {
            return RC.eval("GetLRperformTable()").asString();
        } catch (Exception e) {
            LOGGER.error("getLRperformanceTable", e);
        }
        return null;
    }

    public static String getLRclsLabel(RConnection RC) {
        try {
            return RC.eval("GetLR_clsLbl(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("getLRclsLabel", e);
        }
        return null;
    }

    // integer class label
    public static String getLRclsLabelNew(RConnection RC) {
        try {
            return RC.eval("GetLR_clsLblNew(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("getLRclsLabelNew", e);
        }
        return null;
    }

    public static String getLRthreshold(RConnection RC) {
        try {
            return RC.eval("GetLRthreshold()").asString();
        } catch (Exception e) {
            LOGGER.error("getLRthreshold", e);
        }
        return null;
    }

}
