/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import pro.metaboanalyst.utils.DataUtils;

/**
 *
 * @author Jeff
 */
public class UniVarTests {

    public static void initPairedFC(SessionBean1 sb, double fcThresh, double pairThresh, int cmpType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FC.Anal.paired(NA" + ", " + fcThresh + ", " + pairThresh + ", " + cmpType + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void initFC(SessionBean1 sb, double fcThresh, int cmpType, String paired) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FC.Anal(NA" + ", " + fcThresh + ", " + cmpType + ", " + paired + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Fold Change");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotFC(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotFC(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Fold Change");

            sb.addGraphicsCMD("fc", rCommand);
            sb.addGraphicsMapLink("fc", "/Secure/analysis/FoldChangeView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void computeUnivReport(RConnection RC) {
        try {
            String rCommand = "GetUnivReport(NA)";
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] getFCSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetFCSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getFCSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetFCSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getFCSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetFCSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int performTtests(SessionBean1 sb, String nonpar, double pthresh, String paired, String equalVar, String pvalType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Ttests.Anal(NA" + ", " + nonpar + ", " + pthresh + ", " + paired + ", " + equalVar + ", \"" + pvalType + "\", FALSE)"; //"p" or "u"
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "T-test");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static double getRawCovThresh(RConnection RC) {
        try {
            String rCommand = "GetRawCovThresh(NA)"; //"p" or "u"
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asDouble();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0.0;
        }
    }

    public static int setTtestsRes(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Setup.TtestsRes(NA)"; //"p" or "u"
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static void plotTT(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotTT(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "T-test");

            sb.addGraphicsCMD("tt", rCommand);
            sb.addGraphicsMapLink("tt", "/Secure/analysis/TtestView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] getTtDnMat(RConnection RC) {
        try {
            String rCommand = "GetTtDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getTtUpMat(RConnection RC) {
        try {
            String rCommand = "GetTtUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTtCmpds(RConnection RC) {
        try {
            String rCommand = "GetTtCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnovaDnMat(RConnection RC) {
        try {
            String rCommand = "GetAnovaDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnovaUpMat(RConnection RC) {
        try {
            String rCommand = "GetAnovaUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAnovaCmpds(RConnection RC) {
        try {
            String rCommand = "GetAnovaCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoUnsigMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoSigLftMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoUpLftMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoSigRgtMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoUpRgtMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoSigLftIDs(RConnection RC) {
        try {
            String rCommand = "GetVolcanoUpLftIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoSigRgtIDs(RConnection RC) {
        try {
            String rCommand = "GetVolcanoUpRgtIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoUnsigIDs(RConnection RC) {
        try {
            String rCommand = "GetVolcanoDnIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoCmpds(RConnection RC) {
        try {
            String rCommand = "GetVolcanoCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getTTSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetTTSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getCorSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetCorSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTTSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetTTSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTTSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetTTSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void performVolcano(SessionBean1 sb, String paired, double fcThresh, int cmpType, String nonpar, double pThresh, String varEqual, String vcPvalType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Volcano.Anal(NA" + ", " + paired + ", " + fcThresh + ", " + cmpType + ", " + nonpar + ", " + pThresh + ", " + varEqual + ", \"" + vcPvalType + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Volcano");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotVolcano(SessionBean1 sb, String imgName, int plotLbl, int plotTheme, String format, int dpi, int labelNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotVolcano(NA" + ", \"" + imgName + "\"," + plotLbl + ", " + plotTheme + ", \"" + format + "\", " + dpi + ", width=NA, " + labelNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Volcano");

            sb.addGraphicsCMD("volcano", rCommand);
            sb.addGraphicsMapLink("volcano", "/Secure/analysis/VolcanoView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getVolcanoSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoAllMat(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoAllMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoSigAllNames(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoAllRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoAllColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoAllColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static boolean containsInfValuesVolcano(SessionBean1 sb) {
        try {
            String rCommand = "ContainInfiniteVolcano(NA)";
            return sb.getRConnection().eval(rCommand).asString().equalsIgnoreCase("TRUE");
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean containsInfValuesTT(SessionBean1 sb) {
        try {
            String rCommand = "ContainInfiniteTT(NA)";
            return sb.getRConnection().eval(rCommand).asString().equalsIgnoreCase("TRUE");
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static int performANOVA(SessionBean1 sb, String nonPar, double thresh) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ANOVA.Anal(NA, " + nonPar + ", " + thresh + ", FALSE)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "ANOVA");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static int performAnovaPostHoc(SessionBean1 sb, String postNm, double thresh, int maxNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Calculate.ANOVA.posthoc(NA, \"" + postNm + "\", " + thresh + ", " + maxNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static int performCovariateAnal(SessionBean1 sb, String imgName, String format, int dpi, String styleOpt, String analysisVar, String[] adjustedVar, String reference, String block, double thresh, String pvalType, String contrast) {

        try {
            RConnection RC = sb.getRConnection();
            RC.assign("adj.vec", adjustedVar);
            String rcmd = "adj.vec <- " + DataUtils.convertArrayToVecInR(adjustedVar);
            sb.recordRCommandFunctionInfo(rcmd, "Linear Models");

            RCenter.recordRCommand(RC, rcmd);
            String rCommand = "CovariateScatter.Anal(NA" + ", \"" + imgName + "\", \"" + format + "\", \"" + analysisVar + "\", \"" + reference + "\", \"" + block + "\" , " + thresh + ", \"" + pvalType + "\", \"" + contrast + "\")";
            String plotCommand = "PlotCovariateMap(NA, \"default\"" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ")";
            sb.addGraphicsCMD("covariate_plot", plotCommand);
            sb.addGraphicsMapLink("covariate_plot", "/Secure/multifac/LinearModelView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "Linear Models");
            sb.recordRCommandFunctionInfo(plotCommand, "Linear Models");
            RCenter.recordRCommand(RC, rCommand);
            RCenter.recordRCommand(RC, plotCommand);
            RC.eval(rCommand).asInteger();
            return RC.eval(plotCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return -1;
        }
    }

    public static int performCombineFacAnal(SessionBean1 sb, String imgName, String format, int dpi, String selectedMeta0, String selectedMeta1, String[] adjustedVar, String design, String par1, String par2, String nestedOpt, String block, double thresh, String pvalType) {

        try {
            RConnection RC = sb.getRConnection();
            RC.assign("adj.vec", adjustedVar);
            String rcmd = "adj.vec <- " + DataUtils.convertArrayToVecInR(adjustedVar);
            RCenter.recordRCommand(RC, rcmd);
            String rCommand = "CombineFacScatter.Anal(NA" + ", \"" + imgName + "\", \"" + format + "\", \"" + selectedMeta0 + "\", \"" + selectedMeta1 + "\", \"" + design + "\", \"" + par1 + "\" , \"" + par2 + "\"  , " + nestedOpt + ", \"" + block + "\", " + thresh + ",  \"" + pvalType + "\")";
            String plotCommand = "PlotCovariateMap(NA, \"default\"" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ")";
            sb.addGraphicsCMD("covariate_plot", plotCommand);
            sb.addGraphicsMapLink("covariate_plot", "/Secure/multifac/LinearModelView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            RCenter.recordRCommand(RC, plotCommand);
            RC.eval(rCommand).asInteger();
            return RC.eval(plotCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return -1;
        }
    }

    public static int plotCovariateMap(SessionBean1 sb, String theme, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCovariateMap(NA" + ", \"" + theme + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ")";
            sb.addGraphicsCMD("covariate_plot", rCommand);
            sb.addGraphicsMapLink("covariate_plot", "/Secure/multifac/LinearModelView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static int setSelectedDataNames(RConnection RC, String[] nmVec) {
        try {
            RC.assign("nm.vec", nmVec);
            RCenter.recordRCommand(RC, "SelectMultiData(mSetObj)");
            String rcmd = "nm.vec <- " + DataUtils.convertArrayToVecInR(nmVec);
            RCenter.recordRCommand(RC, rcmd);
            return RC.eval("SelectMultiData(NA)").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void plotAOV(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotANOVA(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "ANOVA");

            sb.addGraphicsCMD("aov", rCommand);
            sb.addGraphicsMapLink("aov", "/Secure/analysis/AnovaView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getAovSigMat(RConnection RC) {
        try {
            String rCommand = "GetAovSigMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAovSigRowNames(RConnection RC) {
        try {
            String rCommand = "GetAovSigRowNames(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAovUpIDs(RConnection RC) {
        try {
            String rCommand = "GetAovUpIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAovDnIDs(RConnection RC) {
        try {
            String rCommand = "GetAovDnIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTtUpIDs(RConnection RC) {
        try {
            String rCommand = "GetTtUpIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTtDnIDs(RConnection RC) {
        try {
            String rCommand = "GetTtDnIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAovSigColNames(RConnection RC) {
        try {
            String rCommand = "GetAovSigColNames(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAovPostHocSigNames(RConnection RC) {
        try {
            String rCommand = "GetAovPostHocSig(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String plotCmpdSummary(SessionBean1 sb, String cmpdName, String meta, String meta2, int ver, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCmpdSummary(NA" + ", \"" + cmpdName + "\",\"" + meta + "\",\"" + meta2 + "\", " + ver + ", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.setCurrentCmpdName(cmpdName);
            sb.addGraphicsCMD("cmpd", rCommand);
            String imgNm = RC.eval(rCommand).asString();
            String size = RGraphUtils.getCurrentCmpdImgSize(RC);
            sb.setCmpdImgSize(size);
            return imgNm;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String plotMultiFacCmpdSummary(SessionBean1 sb, String cmpdName, String meta, String meta2, int ver, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotMultiFacCmpdSummary(NA" + ", \"" + cmpdName + "\",\"" + meta + "\", \"" + meta2 + "\", " + ver + ", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("cmpd_mf", rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String plotCmpdView(RConnection RC, String cmpdName, String format, String dpi) {
        try {
            String rCommand = "PlotCmpdView(NA" + ", \"" + cmpdName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void updateLoadingCmpd(RConnection RC, String cmpdName) {

        try {
            String rCommand = "UpdateLoadingCmpd(NA" + ", \"" + cmpdName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static boolean matchPattern(SessionBean1 sb, String dist, String pattern) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Match.Pattern(NA" + ", \"" + dist + "\", \"" + pattern + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Pattern Search");

            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            } else {
                return false;
            }
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean matchFeaturePattern(SessionBean1 sb, String dist, String featName) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FeatureCorrelation(NA" + ", \"" + dist + "\", \"" + featName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Pattern Search");

            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            } else {
                return false;
            }
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static void plotMatchedFeatures(SessionBean1 sb, String imgName, String searchType, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCorr(NA" + ", \"" + imgName + "\", \"" + searchType + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ptn", rCommand);
            sb.addGraphicsMapLink("ptn", "/Secure/analysis/PatternView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "corBtn_action");
            sb.recordRCommandFunctionInfo(rCommand, "Pattern Search");

            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static String[] getCorSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetCorSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getCorSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetCorSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTempateNames(SessionBean1 sb) {
        try {
            String rCommand = "GenerateTemplates(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    /*
    public static void plotCorrHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String target, String smplDist,
            String colors, String viewOpt, String fix, String clst, double corrThresh) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCorrHeatMap(NA" + ", \"" + imgName + "\", \"" + format + "\", "
                    + dpi + ", width=NA, \"" + target + "\", \"" + smplDist + "\", \"" + colors + "\", \"" + viewOpt + "\", " + fix + ", "
                    + clst + ", " + corrThresh + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
     */
    public static void plotCorrHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String target, String smplDist,
            String colors, String fix, String clst, int fz, int unit, double corrThresh) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCorrHeatMap(NA" + ", \"" + imgName + "\", \"" + format + "\", "
                    + dpi + ", width=NA, \"" + target + "\", \"" + smplDist + "\", \"" + colors + "\", " + fix + ", "
                    + clst + ", " + fz + ", " + unit + ", " + corrThresh + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr", rCommand);
            sb.addGraphicsMapLink("corr", "/Secure/analysis/CorrelationView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "Correlation Heatmap");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotStaticCorrHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String target, String corMethod,
            String colors, String viewOpt, String fixCol, String noClst, double corrCutoff) {
        try {
            RConnection RC = sb.getRConnection();

            // Construct the R function call with properly formatted arguments
            String rCommand = "PlotStaticCorrHeatMap(NA, \"" + imgName + "\", \"" + format + "\", "
                    + dpi + ", NA, \"" + target + "\", \"" + corMethod + "\", \"" + colors + "\", \""
                    + viewOpt + "\", " + fixCol + ", " + noClst + ", " + corrCutoff + ")";
            System.out.println(rCommand);
            // Record the R command for debugging/logging purposes
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_heatmap", rCommand);

            // Execute the R command
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println("Error in plotStaticCorrHeatMap: " + rse.getMessage());
        }
    }

    public static int computeCorrP(SessionBean1 sb) {
        try {
            String rCommand = "ComputeCorrP(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String getCorrSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetCorrSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String getTtestSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetTtestSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String getAnovaSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetAnovaSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getFcSigUpMat(RConnection RC) {
        try {
            String rCommand = "GetFcSigUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getFcSigUpIDs(RConnection RC) {
        try {
            String rCommand = "GetFcSigUpIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getFcSigDnMat(RConnection RC) {
        try {
            String rCommand = "GetFcSigDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getFcSigDnIDs(RConnection RC) {
        try {
            String rCommand = "GetFcSigDnIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getFcUnsigMat(RConnection RC) {
        try {
            String rCommand = "GetFcUnsigMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getFcUnsigIDs(RConnection RC) {
        try {
            String rCommand = "GetFcUnsigIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int[] computeDSPC(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ComputeDSPC(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "computeDspcNet");

            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return new int[0];
    }

    public static void setCmpdSummaryType(RConnection RC, String type) {

        try {
            String rCommand = "SetCmpdSummaryType(NA" + ", \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            System.out.println("rCommand");
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }
    
    
    public static void plotVolcanoCustom(SessionBean1 sb, String imgName, int plotLbl, int plotTheme, String format, int dpi, int labelNum, int plotStyle) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotVolcanoCustom(NA" + ", \"" + imgName + "\"," + plotLbl + ", " + plotTheme + ", \"" + format + "\", " + dpi + ", width=NA, " + labelNum + "," + plotStyle + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Volcano");

            sb.addGraphicsCMD("volcano", rCommand);
            sb.addGraphicsMapLink("volcano", "/Secure/analysis/VolcanoView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static void plotVolcanoAI(SessionBean1 sb, String imgName, int plotLbl, int plotTheme, String format, int dpi, int labelNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotVolcanoAI(NA" + ", \"" + imgName + "\"," + plotLbl + ", " + plotTheme + ", \"" + format + "\", " + dpi + ", width=NA, " + labelNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Volcano");

            sb.addGraphicsCMD("volcano", rCommand);
            sb.addGraphicsMapLink("volcano", "/Secure/analysis/VolcanoView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

}
