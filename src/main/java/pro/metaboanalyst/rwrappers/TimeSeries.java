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
public class TimeSeries {

    public static void initIPCA(RConnection RC, String fileNm, String selMeta1, String selMeta2, String colGradient) {
        try {
            String rCommand = "iPCA.Anal(NA" + ", \"" + fileNm + "\", \"" + selMeta1 + "\", \"" + selMeta2 + "\", \"" + colGradient + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void prepareLiveGraphics(RConnection RC, String urlPath, int inx) {
        try {
            String rCommand = "SetLiveGraphics(\"" + urlPath + "\", " + inx + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int plotHeatMap2(SessionBean1 sb, String imgName, String dataOpt, String scaleOpt, String format,
            int dpi, String smplDist, String clstDist, String colors, int fzCol, int fzRow, double annoFz,
            double annoHeight, int unitCol, int unitRow, String rankMethod, int topFeature, String[] sortNames,
            String useSigFeature, String drawBorder, String showLegend, String showAnnotLegend, String showColNames,
            String showRowNames, String[] selectedMetas, int maxFeatNum) {
        try {
            RConnection RC = sb.getRConnection();
            RC.assign("meta.vec.hm2", selectedMetas);
            RCenter.recordRCommand(RC, "meta.vec.hm2 <- " + DataUtils.convertArrayToVecInR(selectedMetas));

            sb.recordRCommandFunctionInfo("meta.vec.hm2 <- " + DataUtils.convertArrayToVecInR(selectedMetas), "Heatmap2");

            RC.assign("sort.vec.hm2", sortNames);

            sb.recordRCommandFunctionInfo("sort.vec.hm2 <- " + DataUtils.convertArrayToVecInR(sortNames), "Heatmap2");

            RCenter.recordRCommand(RC, "sort.vec.hm2 <- " + DataUtils.convertArrayToVecInR(sortNames));
            String rCommand = "PlotHeatMap2(NA" + ", \"" + imgName + "\", \"" + dataOpt + "\", \"" + scaleOpt + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + smplDist + "\",\"" + clstDist + "\",\"" + colors + "\", " + fzCol + "," + fzRow + ", " + annoFz + "," + annoHeight + "," + unitCol + ", " + unitRow + ", \"" + rankMethod + "\"," + topFeature + ", " + useSigFeature + ",  "
                    + drawBorder + ", " + showLegend + ", " + showAnnotLegend + ", " + showColNames + ", " + showRowNames + ", " + maxFeatNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Heatmap2");

            String metaAssign = "meta.vec.hm2 <- " + DataUtils.convertArrayToVecInR(selectedMetas);
            String sortAssign = "sort.vec.hm2 <- " + DataUtils.convertArrayToVecInR(sortNames);
            String combinedCmd = metaAssign + ";\n" + sortAssign + ";\n" + rCommand;
            sb.addGraphicsCMD("heatmap2", combinedCmd);
            sb.addGraphicsMapLink("heatmap2", "/Secure/multifac/Heatmap2View.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void plotPCAPairSummaryMeta(SessionBean1 sb, String imageName, String code, String format, int dpi, int pcNum, String meta, String metaShape) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCAPairSummaryMeta(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ", \"" + meta + "\", \"" + metaShape + "\")";
            //sb.addGraphicsCMD("pca_pair_meta", rCommand);
            sb.addGraphicsCMD(code, rCommand);
            if (code.equals("pca_pair_meta")) {
                sb.addGraphicsMapLink(code, "/Secure/multifac/LivePCAView.xhtml");
            } else {
                sb.addGraphicsMapLink(code, "/Secure/analysis/PCAView.xhtml");
            }
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int initANOVA2(SessionBean1 sb, double thresh, String corType, String type, String phenOpt, String[] selectedMetas) {
        try {
            RConnection RC = sb.getRConnection();
            RC.assign("meta.vec.aov", selectedMetas);
            //RCenter.recordRCommand(RC, "meta.vec.aov <- " + Arrays.toString(selectedMetas));
            String rcmd = "meta.vec.aov <- " + DataUtils.convertArrayToVecInR(selectedMetas);
            RCenter.recordRCommand(RC, rcmd);
            //System.out.println(rcmd);

            String rCommand = "ANOVA2.Anal(NA, " + thresh + ", \"" + corType + "\", \"" + type + "\", \"" + phenOpt + "\", 500)";
            RCenter.recordRCommand(RC, rCommand);
            //System.out.println(rCommand);

            sb.recordRCommandFunctionInfo(rCommand, "Multifactor anova");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void plotAOV2(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotANOVA2(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("aov2", rCommand);
            sb.addGraphicsMapLink("aov2", "/Secure/multifac/Anova2View.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "Multifactor anova");
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String getAov2SigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetAov2SigFileName(NA)";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String getAscaSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetAscaSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getAov2SigMat(RConnection RC) {
        try {
            String rCommand = "GetAov2SigMat(NA)";
            double[][] cmpds = RC.eval(rCommand).asDoubleMatrix();
            return cmpds;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAov2SigRowNames(RConnection RC) {
        try {
            String rCommand = "GetAov2SigRowNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAov2SigColNames(RConnection RC) {
        try {
            String rCommand = "GetAov2SigColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int performASCA(SessionBean1 sb, int a, int b, int x, int res, String[] selectedMetas) {
        try {
            RConnection RC = sb.getRConnection();
            RC.assign("meta.vec.asca", selectedMetas);
            String rcmd = "meta.vec.asca <- " + DataUtils.convertArrayToVecInR(selectedMetas);
            RCenter.recordRCommand(RC, rcmd);

            String rCommand = "Perform.ASCA(NA" + ", " + a + ", " + b + ", " + x + ", " + res + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "ASCA");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void performASCAVarSelection(SessionBean1 sb, double speThresh, double lvThresh) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CalculateImpVarCutoff(NA" + ", " + speThresh + ", " + lvThresh + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "ASCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void performASCAPermutation(RConnection RC, int permNum) {
        try {
            String rCommand = "Perform.ASCA.permute(NA" + ", " + permNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAPermSummary(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotASCA.Permutation(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_perm", rCommand);
            sb.addGraphicsMapLink("asca_perm", "/Secure/multifac/AscaView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAscree(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotASCAModelScree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_scree", rCommand);
            sb.addGraphicsMapLink("asca_scree", "/Secure/multifac/AscaView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "ASCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAModels(SessionBean1 sb, String imgName, String format, int dpi, String type, String colorBW) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotASCAModel(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\"," + colorBW + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_f" + type, rCommand);
            sb.addGraphicsMapLink("asca_f" + type, "/Secure/multifac/AscaView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "ASCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAInteraction(SessionBean1 sb, String imgName, String format, int dpi, String colorBW) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotASCAInteraction(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + "," + colorBW + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_fab", rCommand);
            sb.addGraphicsMapLink("asca_fab", "/Secure/multifac/AscaView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "ASCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAImpVar(SessionBean1 sb, String imgName, String format, int dpi, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotAscaImpVar(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_imp" + type, rCommand);
            sb.addGraphicsMapLink("asca_imp" + type, "/Secure/multifac/AscaView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "ASCA");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getAscaSigMat(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetAscaSigMat(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAscaSigRowNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetAscaSigRowNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAscaSigColNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetAscaSigColNames(\"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int performMB(SessionBean1 sb, int topPerc, String[] metas) {
        try {
            RConnection RC = sb.getRConnection();
            RC.assign("meta.vec.mb", metas);
            //RCenter.recordRCommand(RC, "meta.vec.mb <- " + Arrays.toString(metas));
            String rcmd = "meta.vec.mb <- " + DataUtils.convertArrayToVecInR(metas);
            RCenter.recordRCommand(RC, rcmd);
            sb.recordRCommandFunctionInfo(rcmd, "MEBA");

            String rCommand = "performMB(NA" + ", " + topPerc + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "MEBA");

            return (RC.eval(rCommand).asInteger());
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String plotMBTimeProfile(SessionBean1 sb, String cmpdName, int version, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotMBTimeProfile(NA" + ", \"" + cmpdName + "\", " + version + ", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("mb", rCommand);
            sb.addGraphicsMapLink("mb", "/Secure/multifac/AscaView.xhtml");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getMBSigMat(RConnection RC) {
        try {
            String rCommand = "GetMBSigMat(NA)";
            double[][] cmpds = RC.eval(rCommand).asDoubleMatrix();
            return cmpds;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMBSigRowNames(RConnection RC) {
        try {
            String rCommand = "GetMBSigRowNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMBSigColNames(RConnection RC) {
        try {
            String rCommand = "GetMBSigColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnova2DnMat(RConnection RC) {
        try {
            String rCommand = "GetAnova2DnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnova2UpMat(RConnection RC) {
        try {
            String rCommand = "GetAnova2UpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAnova2UpCmpds(RConnection RC) {
        try {
            String rCommand = "GetAov2UpIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAnova2DnCmpds(RConnection RC) {
        try {
            String rCommand = "GetAov2DnIDs(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getCovDnMat(RConnection RC) {
        try {
            String rCommand = "GetCovDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getCovUpMat(RConnection RC) {
        try {
            String rCommand = "GetCovUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getCovUpIDs(RConnection RC) {
        try {
            String rCommand = "GetCovUpIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getCovDnIDs(RConnection RC) {
        try {
            String rCommand = "GetCovDnIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getCovSigMat(RConnection RC) {
        try {
            String rCommand = "GetCovSigMat(NA)";
            double[][] cmpds = RC.eval(rCommand).asDoubleMatrix();
            return cmpds;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getCovSigRowNames(RConnection RC) {
        try {
            String rCommand = "GetCovSigRowNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getCovSigColNames(RConnection RC) {
        try {
            String rCommand = "GetCovSigColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String getCovSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetCovSigFileName(NA)";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static boolean performCorrAnal(SessionBean1 sb, String algo, String tgtType, String tgtName, String[] adjustedVar) {
        try {
            RConnection RC = sb.getRConnection();
            RC.assign("cov.vec", adjustedVar);
            String rcmd = "meta.vec.rf <- " + DataUtils.convertArrayToVecInR(adjustedVar);
            RCenter.recordRCommand(RC, rcmd);

            String rCommand = "FeatureCorrelationMeta(NA" + ", \"" + algo + "\", \"" + tgtType + "\", \"" + tgtName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "corBtn_action");

            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static void plotPCA2DScoreMeta(SessionBean1 sb,
            String imgName,
            String format,
            int dpi,
            int pc1Inx,
            int pc2Inx,
            double conf,
            int show,
            int greyScale,
            String cexOpt,
            String meta,
            String metaShape) {
        try {
            RConnection RC = sb.getRConnection();

            /* build R call ------------------------------------------------- */
            String metaArg = (meta == null || meta.trim().isEmpty())
                    ? "NULL" : "\"" + meta + "\"";
            String metaShapeArg = (metaShape == null || metaShape.trim().isEmpty())
                    ? "NULL" : "\"" + metaShape + "\"";

            String rCommand
                    = "PlotPCA2DScoreMeta(NA"
                    + ", \"" + imgName + "\""
                    + ", \"" + format + "\""
                    + ", " + dpi
                    + ", width=NA"
                    + ", " + pc1Inx
                    + ", " + pc2Inx
                    + ", " + conf
                    + ", " + show
                    + ", " + greyScale
                    + ", \"" + cexOpt + "\""
                    + ", " + metaArg
                    + ", " + metaShapeArg
                    + ")";

            sb.addGraphicsCMD("pca_score2d_meta", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
}
