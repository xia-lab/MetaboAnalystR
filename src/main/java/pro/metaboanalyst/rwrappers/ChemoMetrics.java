/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import java.util.logging.Level;
import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author Jeff
 */
public class ChemoMetrics {

    private static final Logger LOGGER = LogManager.getLogger(ChemoMetrics.class);

    public static boolean initPCA(SessionBean1 sb) {
        try {
            String rCommand = "PCA.Anal(NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");

            return RC.eval(rCommand).asInteger() == 1;
        } catch (RserveException rse) {
            LOGGER.error("InitPCA", rse);
        } catch (REXPMismatchException ex) {
            java.util.logging.Logger.getLogger(ChemoMetrics.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    public static void flipPCA(SessionBean1 sb, String axisOpt) {
        try {
            String rCommand = "PCA.Flip(NA" + ", \"" + axisOpt + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] getPCALoadings(SessionBean1 sb) {
        try {
            String rCommand = "signif(mSet$analSet$pca$imp.loads, 5)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPCALoadingRowName(SessionBean1 sb) {
        try {
            String rCommand = "rownames(mSet$analSet$pca$imp.loads)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPCALoadingColName(SessionBean1 sb) {
        try {
            String rCommand = "colnames(mSet$analSet$pca$imp.loads)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void plotPCALoading(SessionBean1 sb, String imgName, String format, int dpi, int pcImpInx1, int pcImpInx2) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCALoading(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcImpInx1 + "," + pcImpInx2 + ");";
            sb.addGraphicsCMD("pca_loading", rCommand);
            sb.addGraphicsMapLink("pca_loading", "/Secure/analysis/PCAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String getPCAStats(RConnection RC) {
        try {
            String rCommand = "GetPCAStats(NA);";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getPCAStats", e);
        }
        return null;
    }

    public static String getPCAPermANOVAText(RConnection RC) {
        try {
            String rCommand = "GetPCAPermANOVAText(NA);";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getPCAPermANOVAText", e);
        }
        return null;
    }

    public static void plotPCA2DScore(SessionBean1 sb, String imgName, String format, int dpi, int pc1Inx, int pc2Inx, double conf, int show, int greyScale, String cexOpt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCA2DScore(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pc1Inx + "," + pc2Inx + "," + conf + "," + show + "," + greyScale + ", \"" + cexOpt + "\")";
            sb.addGraphicsCMD("pca_score2d", rCommand);
            sb.addGraphicsMapLink("pca_score2d", "/Secure/analysis/PCAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPCA3DScore(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCA3DScore(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            //tmp-test-js; String rCommand_png = "PlotPCA3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";
            //tmp-test-js; sb.addGraphicsCMD("pca_score3d", rCommand_png);
            //System.out.println("============pca_score3d: " + rCommand_png);
            //tmp-test-js; RCenter.recordRCommand(RC, rCommand_png);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPCA3DLoading(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCA3DLoading(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            sb.addGraphicsCMD("pca_score3dloading", rCommand);
            sb.addGraphicsMapLink("pca_score3dloading", "/Secure/analysis/PCAView.xhtml");

            //System.out.println("============pca_score3d: " + rCommand_png);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotLoadingBoxplot(RConnection RC, String symb) {
        try {
            String rCommand = "PlotLoadBoxplot(NA" + ", \"" + symb + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static void plotPCAPairSummary(SessionBean1 sb, String imageName, String format, int dpi, int pcNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCAPairSummary(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ")";
            sb.addGraphicsCMD("pca_pair", rCommand);
            sb.addGraphicsMapLink("pca_pair", "/Secure/analysis/PCAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPCABiplot(SessionBean1 sb, String imageName, String format, int dpi, int pc1Inx, int pc2Inx, int topnum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCABiplot(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pc1Inx + "," + pc2Inx + "," + topnum + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");

            sb.addGraphicsCMD("pca_biplot", rCommand);
            sb.addGraphicsMapLink("pca_biplot", "/Secure/analysis/PCAView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPLSBiplot(SessionBean1 sb, String imageName, String format, int dpi, int pc1Inx, int pc2Inx, int topnum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLSBiplot(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pc1Inx + "," + pc2Inx + "," + topnum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pls_biplot", rCommand);
            sb.addGraphicsMapLink("pls_biplot", "/Secure/analysis/PLSDAView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPCAScree(SessionBean1 sb, String imageName, String format, int dpi, int pcNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCAScree(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PCA");
            sb.addGraphicsMapLink("pca_scree", "/Secure/analysis/PCAView.xhtml");

            sb.addGraphicsCMD("pca_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static boolean initPLS(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PLSR.Anal(NA, reg=FALSE)";
            if (sb.isKeepClsOrder()) {
                rCommand = "PLSR.Anal(NA, reg=TRUE)";
            }
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static int initOPLS(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "OPLSR.Anal(NA, reg=FALSE)";
            String myCmd = ".prepare.oplsr.anal(NA,reg=FALSE)";
            if (sb.isKeepClsOrder()) {
                rCommand = "OPLSR.Anal(NA, reg=TRUE)";
                myCmd = ".prepare.oplsr.anal(NA,reg=TRUE)";
            }
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");
            RCenter.recordRCommand(RC, rCommand);

            if (RC.eval(myCmd).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
                myCmd = ".save.oplsr.anal(NA)";
                return RC.eval(myCmd).asInteger();
            }

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void plotOPLS2DScore(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, double conf, int show, int greyScale, String cexOpt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS2DScore(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + conf + ","
                    + show + "," + greyScale + ", \"" + cexOpt + "\")";
            sb.addGraphicsCMD("opls_score2d", rCommand);
            sb.addGraphicsMapLink("opls_score2d", "/Secure/analysis/OrthoPLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotOplsSplot(SessionBean1 sb, String imageName, String plotType, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS.Splot(NA" + ", \"" + imageName + "\", \"" + plotType + "\", \"" + format + "\", " + dpi + ", width=NA);";
            sb.addGraphicsCMD("opls_splot", rCommand);
            sb.addGraphicsMapLink("opls_splot", "/Secure/analysis/OrthoPLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void updatePCALoadType(SessionBean1 sb, String plotType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "UpdatePCA.Loading(NA, \"" + plotType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void updatePLSLoadType(SessionBean1 sb, String plotType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "UpdatePLS.Loading(NA, \"" + plotType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void updateOplsSplotType(SessionBean1 sb, String plotType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "UpdateOPLS.Splot(NA, \"" + plotType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPLSLoading(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLSLoading(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + ", " + pcInx2 + ");";
            sb.addGraphicsCMD("pls_loading", rCommand);
            sb.addGraphicsMapLink("pls_loading", "/Secure/analysis/PLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPLS2DScore(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, double conf, int show, int greyScale, String cexOpt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS2DScore(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + conf + "," + show + "," + greyScale + ", \"" + cexOpt + "\")";
            sb.addGraphicsCMD("pls_score2d", rCommand);
            sb.addGraphicsMapLink("pls_score2d", "/Secure/analysis/PLSDAView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //json file generation
    public static void plotPLS3DScore(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS3DScore(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            //String rCommand_png = "PlotPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            String rCommand_png = "PlotPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";

            sb.addGraphicsCMD("pls_score3d", rCommand_png);
            sb.addGraphicsMapLink("pls_score3d", "/Secure/analysis/PLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand_png);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand_png, "PLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotPLS3DLoading(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS3DLoading(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";

            sb.addGraphicsCMD("pls_loading3d", rCommand);
            sb.addGraphicsMapLink("pls_loading3d", "/Secure/analysis/PLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int initSPLS(SessionBean1 sb, int compNum, int varNum, String varSpec, String validOpt, int foldNum, String doCV) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SPLSR.Anal(NA, " + compNum + ", " + varNum + ", \"" + varSpec + "\", \"" + validOpt + "\", " + foldNum + ", " + doCV + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

            //use microservice
            rCommand = ".prepare.splsr.anal(NA, " + compNum + ", " + varNum + ", \"" + varSpec + "\", \"" + validOpt + "\", " + foldNum + ", " + doCV + ")";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            rCommand = ".save.splsr.anal(NA)";
            return RC.eval(rCommand).asInteger();

        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static void plotSPLSPairSummary(SessionBean1 sb, String imgName, String format, int dpi, int cmpdNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLSPairSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + cmpdNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

            sb.addGraphicsCMD("spls_pair", rCommand);
            sb.addGraphicsMapLink("spls_pair", "/Secure/analysis/SparsePLSDAView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSPLSLoading(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, String viewOpt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLSLoading(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + ",\"" + viewOpt + "\");";

            sb.addGraphicsCMD("spls_loading", rCommand);
            sb.addGraphicsMapLink("spls_loading", "/Secure/analysis/SparsePLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSPLS2DScore(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, double conf, int show, int greyScale, String cexOpt) {
        try {
            String rCommand = "PlotSPLS2DScore(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + conf + ","
                    + show + "," + greyScale + ",\"" + cexOpt + "\")";

            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("spls_score2d", rCommand);
            sb.addGraphicsMapLink("spls_score2d", "/Secure/analysis/SparsePLSDAView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSPLS3DLoading(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            String rCommand = "PlotSPLS3DLoading(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            //String rCommand_png = "PlotPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            //String rCommand_png = "PlotPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";
            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("spls_loading3d", rCommand);
            sb.addGraphicsMapLink("spls_loading3d", "/Secure/analysis/SparsePLSDAView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSPLS3DScore(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {

            String rCommand = "PlotSPLS3DScore(NA" + ", \"" + imgName + "\", \"" + format + "\")";
            String rCommand_png = "PlotSPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";

            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("spls_score3d", rCommand_png);
            sb.addGraphicsMapLink("spls_score3d", "/Secure/analysis/SparsePLSDAView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");
            RCenter.recordRCommand(RC, rCommand_png);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int performSPLSCV(SessionBean1 sb, int compNum, int varNum, String varSpec, String validOpt, int foldNum, String doCV) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SPLSR.Anal(NA, " + compNum + ", " + varNum + ", \"" + varSpec + "\", \"" + validOpt + "\", " + foldNum + ", " + doCV + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

            //use microservice
            rCommand = ".prepare.splsr.anal(NA, " + compNum + ", " + varNum + ", \"" + varSpec + "\", \"" + validOpt + "\", " + foldNum + ", " + doCV + ")";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            rCommand = ".save.splsr.anal(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static int trainSPLSClassifier(SessionBean1 sb, String LorT, int cmpdNum, String choice) {
        try {
            //first perform PLSDA cross validation
            String rCommand = "SPLSDA.CV(" + "\"" + LorT + "\"," + cmpdNum + ", \"" + choice + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return (0);
        }
    }

    public static void plotSPLSPermutation(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotSPLS.Permutation(\"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("spls_perm", rCommand);
            sb.addGraphicsMapLink("spls_perm", "/Secure/analysis/SparsePLSDAView.xhtml");

            RC.voidEval(rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSPLSDAClassification(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLSDA.Classification(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("spls_cv", rCommand);
            sb.addGraphicsMapLink("spls_cv", "/Secure/analysis/SparsePLSDAView.xhtml");

            RC.voidEval(rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "sPLSDA");

        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static void plotPLSPairSummary(SessionBean1 sb, String imgName, String format, int dpi, int cmpdNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLSPairSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + cmpdNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            sb.addGraphicsCMD("pls_pair", rCommand);
            sb.addGraphicsMapLink("pls_pair", "/Secure/analysis/PLSDAView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int trainPLSClassifier(SessionBean1 sb, String cvOpt, int foldNum, int cmpdNum, String choice) {
        try {
            //first perform PLSDA cross validation
            RConnection RC = sb.getRConnection();
            String rCommand = "PLSDA.CV(NA" + ", \"" + cvOpt + "\", " + foldNum + "," + cmpdNum + ", \"" + choice + "\")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return (0);
        }
    }

    // check if CV has been performed
    public static int checkCVperformed(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "checkCVperformed(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }

    }

    public static String performPLSPermute(SessionBean1 sb, int permutNum, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PLSDA.Permut(NA" + ", " + permutNum + ", \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int performOPLSPermute(SessionBean1 sb, int permutNum, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "OPLSDA.Permut(NA, " + permutNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");

            //use microservice
            rCommand = ".prepare.oplsda.permut(NA, " + permutNum + ")";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            rCommand = ".save.oplsda.permut(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    //should perform TrainPLSClassifier first
    public static int getBestTuneNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetPLSBestTune(NA)").asInteger();
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("GetBestTuneNumber", e);
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int getDefaultPLSPairNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultPLSPairComp(NA)").asInteger();
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("GetDefaultPLSPairNumber", e);
        }
        return 0;
    }

    public static int getDefaultSPLSPairNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultSPLSPairComp(NA)").asInteger();
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("GetDefaultSPLSPairNumber", e);
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int getDefaultPLSCVNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultPLSCVComp(NA)").asInteger();
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("GetDefaultPLSCVNumber", e);
        }
        return 0;
    }

    public static int getDefaultSPLSCVNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultSPLSCVComp(NA)").asInteger();
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("GetDefaultSPLSCVNumber", e);
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int getMaxPLSPairCompNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPLSPairComp(NA)").asInteger();
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("GetMaxPLSPairCompNumber", e);
        }
        return 0;
    }

    public static int getMaxPCACompNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPCAComp(NA)").asInteger();
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("GetMaxPCACompNumber", e);
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int getMaxPLSCVCompNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPLSCVComp(NA)").asInteger();
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("GetMaxPLSCVCompNumber", e);
        }
        return 0;
    }

    public static int getMaxPLSBiplotNum(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPLSBiplotNum(NA)").asInteger();
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("GetMaxPLSBiplotNum", e);
        }
        return 0;
    }

    public static void plotPLSClassification(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS.Classification(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pls_cv", rCommand);
            sb.addGraphicsMapLink("pls_cv", "/Secure/analysis/PLSDAView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            //System.out.println(rse);
            LOGGER.error("PlotPLSClassification", rse);
        }
    }

    public static void plotOplsMdlView(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS.MDL(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");

            sb.addGraphicsCMD("opls_mdl", rCommand);
            sb.addGraphicsMapLink("opls_mdl", "/Secure/analysis/OrthoPLSDAView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            // System.out.println(rse);
            LOGGER.error("PlotOplsMdlView", rse);
        }
    }

    public static void plotPLSPermutation(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS.Permutation(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pls_perm", rCommand);
            sb.addGraphicsMapLink("pls_perm", "/Secure/analysis/PLSDAView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            // System.out.println(rse);
            LOGGER.error("PlotPLSPermutation", rse);
        }
    }

    public static String plotOPLSPermutation(SessionBean1 sb, String imgName, String format, int dpi, int permNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS.Permutation(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("opls_perm", rCommand);
            sb.addGraphicsMapLink("opls_perm", "/Secure/analysis/OrthoPLSDAView.xhtml");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            //System.out.println(rse);
            LOGGER.error("PlotOPLSPermutation", rse);
            return null;
        }
    }

    public static void plotPLSImp(SessionBean1 sb, String imgName, String format, int dpi, String type, String featNm, int num, String colorBW) {
        try {
            String rCommand = "PlotPLS.Imp(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\", \"" + featNm + "\", " + num + "," + colorBW + ")";
            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("pls_imp", rCommand);
            sb.addGraphicsMapLink("pls_imp", "/Secure/analysis/PLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "PLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            //System.out.println(rse);
            LOGGER.error("PlotPLSImp", rse);
        }
    }

    public static void plotOPLSImp(SessionBean1 sb, String imgName, String format, int dpi, String type, String featNm, int num, String colorBW) {
        try {
            String rCommand = "PlotOPLS.Imp(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\", \"" + featNm + "\", " + num + "," + colorBW + ")";
            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("opls_imp", rCommand);
            sb.addGraphicsMapLink("opls_imp", "/Secure/analysis/OrthoPLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "OrthoPLSDA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            //System.out.println(rse);
            LOGGER.error("PlotOPLSImp", rse);
        }
    }

    public static void plotSPLSImp(SessionBean1 sb, String imgName, String format, int dpi, String type, String featNm, int num, String colorBW) {
        try {
            String rCommand = "PlotSPLS.Imp(\"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\", \"" + featNm + "\", " + num + "," + colorBW + ")";
            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("spls_imp", rCommand);
            sb.addGraphicsMapLink("spls_imp", "/Secure/analysis/SparsePLSDAView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            //System.out.println(rse);
            LOGGER.error("PlotSPLSImp", rse);
        }
    }

    public static String[] getPLSSigColNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetPLSSigColNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetPLSSigColNames", e);
        }
        return null;
    }

    public static String[] getSPLSSigColNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetSPLSSigColNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetSPLSSigColNames", e);
        }
        return null;
    }

    public static String[] getPLSSigRowNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetPLSSigRowNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetPLSSigRowNames", e);
        }
        return null;
    }

    public static double[][] getPLSSigMat(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetPLSSigMat(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("GetPLSSigMat", e);
        }
        return null;
    }

    public static String[] getPLSCVColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetPLS_CVColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetPLSCVColNames", e);
        }
        return null;
    }

    public static String[] getSPLSCVColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSPLS_CVColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetSPLSCVColNames", e);
        }
        return null;
    }

    public static String[] getPLSCVRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetPLS_CVRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetPLSCVRowNames", e);
        }
        return null;
    }

    public static String[] getSPLSCVRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSPLS_CVRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetSPLSCVRowNames", e);
        }
        return null;
    }

    public static double[][] getPLS_CVMat(SessionBean1 sb) {
        try {
            String rCommand = "GetPLS_CVMat(NA)";
            RCenter.recordRCommand(sb.getRConnection(), rCommand);
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("GetPLS_CVMat", e);
        }
        return null;
    }

    public static double[][] getSPLS_CVMat(SessionBean1 sb) {
        try {
            String rCommand = "GetSPLS_CVMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("GetSPLS_CVMat", e);
        }
        return null;
    }

    public static String[] getPCALoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetPCALoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getPCALoadCmpds", e);
        }
        return null;
    }

    public static double[][] getPCALoadMat(RConnection RC) {
        try {
            String rCommand = "GetPCALoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getPCALoadMat", e);
        }
        return null;
    }

    public static double[] getPCALoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetPCALoadAxesSpec(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (Exception e) {
            LOGGER.error("getPCALoadAxesSpec", e);
        }
        return null;
    }

    public static String[] getPLSLoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getPLSLoadCmpds", e);
        }
        return null;
    }

    public static String[] getSPLSLoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getSPLSLoadCmpds", e);
        }
        return null;
    }

    public static double[][] getOPLSSigMat(RConnection RC, String type) {
        try {
            String rCommand = "GetOPLSSigMat(NA" + ", \"" + type + "\")";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getOPLSSigMat", e);
        }
        return null;
    }

    public static String[] getOPLSSigCmpds(RConnection RC, String type) {
        try {
            String rCommand = "GetOPLSSigCmpds(NA" + ", \"" + type + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getOPLSSigCmpds", e);
        }
        return null;
    }

    public static String[] getOPLSSigColNames(RConnection RC, String type) {
        try {
            String rCommand = "GetOPLSSigColNames(NA" + ", \"" + type + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getOPLSSigColNames", e);
        }
        return null;
    }

    public static double[][] getPLSLoadMat(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getPLSLoadMat", e);
        }
        return null;
    }

    public static double[][] getSPLSLoadMat(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getSPLSLoadMat", e);
        }
        return null;
    }

    public static double[] getPLSLoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadAxesSpec(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (Exception e) {
            LOGGER.error("getPLSLoadAxesSpec", e);
        }
        return null;
    }

    public static double[] getSPLSLoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadAxesSpec(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (Exception e) {
            LOGGER.error("getSPLSLoadAxesSpec", e);
        }
        return null;
    }

    public static double[] getOPLSLoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetOPLSLoadAxesSpec()";
            return RC.eval(rCommand).asDoubles();
        } catch (Exception e) {
            LOGGER.error("getOPLSLoadAxesSpec", e);
        }
        return null;
    }

    public static String computeEncasing(RConnection RC, String dataName, String type, String names, String level, String omicstype) {
        try {
            String rCommand = "ComputeEncasing(\"" + dataName + "\", \"" + type + "\", \"" + names + "\", \"" + level + "\", \"" + omicstype + "\");";
            RCenter.recordRCommand(RC, rCommand);
            //RDataUtils.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("computeEncasing", e);
        }
        return "NA";
    }

    public static String[] getVipRows(RConnection RC) {
        try {
            String rCommand = "GetVipRows();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static String[] getVipColumns(RConnection RC) {
        try {
            String rCommand = "GetVipColumns();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static double[][] getVipMat(RConnection RC) {
        try {
            String rCommand = "GetVipMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getSPLSLoadMat", e);
        }
        return null;
    }

}
