/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author qiang
 */
public class RMetaPathUtils {

    private static final Logger LOGGER = LogManager.getLogger(RMetaPathUtils.class);

    public static void resumeGlobalEnvir(RConnection RC) {

        try {
            String rCommand = "if(!file.exists(\"global.RData\")) {save.image(\"global.RData\") } else {rm(list=ls()); load(\"global.RData\")}";
            RC.voidEval(rCommand);
            //RCenter.recordRCommand(RC, rCommand);
        } catch (Exception rse) {
            LOGGER.error("resumeGlobalEnvir", rse);
        }

    }

    public static double prepareMetaPath(RConnection RC, String ionMode, String dataName, String dataName2,
            double ppm, String version, double pcutoff, String rt) {

        try {
            String rCommand = "PrepareMetaPath(NA, \""
                    + ionMode + "\"," + ppm + ", \"" + version + "\"," + pcutoff + ", \""
                    + rt + "\",\"" + dataName + "\",\"" + dataName2 + "\")";
            double sig_per = RC.eval(rCommand).asDouble();
            RCenter.recordRCommand(RC, rCommand);
            return sig_per;
        } catch (Exception rse) {
            LOGGER.error("PrepareMetaPath", rse);
        }

        return 0.0;
    }

    public static boolean mSetQSDelete(RConnection RC) {

        try {
            String rCommand = "mSetQSDelete()";
            RC.voidEval(rCommand);
            //RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (Exception rse) {
            LOGGER.error("mSetQSDelete", rse);
        }

        return false;
    }

    public static void cacheQSClean(RConnection RC) {

        try {
            String rCommand = "CacheQSClean()";
            RC.voidEval(rCommand);
            //RCenter.recordRCommand(RC, rCommand);
        } catch (Exception rse) {
            LOGGER.error("CacheQSClean", rse);
        }

    }

    public static int readMetaPathTable(RConnection RC, String dataName, String dataFormat, String dataType) {
        try {
            String rCommand = "ReadMetaPathTable(NA" + ", \"" + dataName + "\", \"" + dataFormat + "\", \"" + dataType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("readMetaPathTable", rse);
        }
        return 0;
    }

    public static int readMetaPathTableMix(RConnection RC, String dataName, String dataName2, String dataFormat, String dataType) {
        try {
            String rCommand = "ReadMetaPathTableMix(NA" + ", \""
                    + dataName + "\", \""
                    + dataName2 + "\", \""
                    + dataFormat + "\", \""
                    + dataType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("readMetaPathTableMix", rse);
        }
        return 0;
    }

    public static boolean sanityCheckMetaPathData(RConnection RC, String dataNm, String dataNm2) {
        try {
            String rCommand = "SanityCheckMetaPathTable(NA" + ", \"" + dataNm + "\",\"" + dataNm2 + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("sanityCheckMetaPathData", rse);
        }
        return false;
    }

    public static String checkAllRT(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CheckAllRT()";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performMetaPathAnalysis");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("checkAllRT", rse);
        }
        return null;
    }

    public static int performMetaPathNormalization(RConnection RC, String sampleNor, String tranform, String scale, String name, String name2) {

        try {
            String rCommand = "MetaPathNormalization(NA, \""
                    + sampleNor
                    + "\", \""
                    + tranform
                    + "\", "
                    + scale
                    + ", \""
                    + name
                    + "\",\""
                    + name2
                    + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("performMetaPathNormalization", e);
        }
        return 0;
    }

    public static int[] getPathDataDims(RConnection RC, String dataName) {
        try {
            String rCommand = "GetMetaPathDataDims(NA" + ", \"" + dataName + "\")";
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("getPathDataDims", rse);
        }
        return null;
    }

    public static String[] getMetaPathGroupNames(RConnection RC, String dataName) {
        try {
            String rCommand = "GetMetaPathGroupNames(NA" + ", \"" + dataName + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getMetaPathGroupNames", rse);
        }
        return null;
    }

    public static int customizeMetaAdduct(RConnection RC, String name, String name2, String[] addVec, String mode) {
        int res = 0;
        try {
            String updateCmd = "Customize.MetaAdduct(NA, \""
                    + name + "\",\"" + name2 + "\","
                    + DataUtils.createStringVector(addVec) + ",\""
                    + mode + "\");";
            RCenter.recordRCommand(RC, updateCmd);
            res = RC.eval(updateCmd).asInteger();
        } catch (Exception e) {
            LOGGER.error("CustomizeMetaAdduct", e);
        }

        return res;
    }

    public static int plotPathDataProfile(RConnection RC, String dataName, String dataName2, String boxName, String boxName2, String format) {

        try {
            String rCommand = "PlotPathDataProfile(\""
                    + dataName
                    + "\", \""
                    + dataName2
                    + "\", \""
                    + boxName
                    + "\", \""
                    + boxName2
                    + "\", \""
                    + format
                    + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("PlotPathDataProfile", e);
        }
        return 0;
    }

    public static boolean setPeakEnrichMethod(SessionBean1 sb, String algOpt, String version) {

        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetPeakEnrichMethod(NA" + ", \"" + algOpt + "\", \"" + version + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performMetaPathAnalysis");

            //System.out.println(rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("setPeakEnrichMethod", rse);
        }
        return false;
    }

    public static boolean performMetaMummiAnalysis(SessionBean1 sb, String lib, String libVersion, int minMsetNum, int permNum, String metaLevel,
            String combinelevel, String pvalmethod, String esmethod, String rankmetric, Boolean matchedfeats, double pvalCutoff) {
        try {
            RConnection RC = sb.getRConnection();
            if (matchedfeats) {
                String rCommand = "PerformMetaPSEA(NA, \"" + lib + "\", \"" + libVersion + "\", " + minMsetNum + ", "
                        + permNum + ", \"" + metaLevel + "\", \""
                        + combinelevel + "\", \"" + pvalmethod + "\", \""
                        + esmethod + "\", \"" + rankmetric + "\", TRUE, " + pvalCutoff + ");";
                RCenter.recordRCommand(RC, rCommand);
                sb.recordRCommandFunctionInfo(rCommand, "performMetaPathAnalysis");

                return RC.eval(rCommand).asInteger() == 1;
            } else {
                String rCommand = "PerformMetaPSEA(NA, \"" + lib + "\", \"" + libVersion + "\", " + minMsetNum + ", "
                        + permNum + ", \"" + metaLevel + "\", \""
                        + combinelevel + "\", \"" + pvalmethod + "\", \""
                        + esmethod + "\", \"" + rankmetric + "\", FALSE, " + pvalCutoff + ");";
                RCenter.recordRCommand(RC, rCommand);
                sb.recordRCommandFunctionInfo(rCommand, "performMetaPathAnalysis");

                return RC.eval(rCommand).asInteger() == 1;
            }
        } catch (Exception e) {
            LOGGER.error("performMetaMummiAnalysis", e);
        }

        return false;
    }

    public static boolean plotPathwayMetaAnalysis(SessionBean1 sb, String imgNm, String PlotType,
            double pvalCutoff, double overlap, int maxPath, String format, int dpi) {

        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPathwayMetaAnalysis(NA, \""
                    + imgNm + "\", \""
                    + PlotType + "\", "
                    + "pvalCutoff = " + pvalCutoff + ","
                    + "overlap = " + overlap + ","
                    + "bubbleMaxPaths = " + maxPath + ","
                    + "format = \"" + format + "\"" + ","
                    + "dpi = " + dpi + ");";

            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("meta_bubble", rCommand);
            sb.addGraphicsMapLink("meta_bubble", "/Secure/metapath/MetaPathResultView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "performMetaPathAnalysis");

            RC.eval(rCommand);
            return true;
        } catch (Exception e) {
            LOGGER.error("plotPathwayMetaAnalysis", e);
        }

        return false;
    }

    public static String plotPathMetaUpdate(
            RConnection RC,
            String imgNm,
            String PlotType,
            double pvalCutoff,
            double overlap,
            int maxPath,
            int dpi,
            String format,
            int width,
            double height) {

        try {
            String rCommand = "PlotPathwayMetaAnalysis(NA, \""
                    + imgNm + "\", \""
                    + PlotType + "\", "
                    + "pvalCutoff = " + pvalCutoff + ","
                    + "overlap = " + overlap + ","
                    + "bubbleMaxPaths = " + maxPath + ","
                    + "format = \"" + format + "\", "
                    + "dpi = " + dpi + ","
                    + "width = " + width + ","
                    + "height = " + height + ","
                    + ");";

            RCenter.recordRCommand(RC, rCommand);
            String res = RC.eval(rCommand).asString();;
            return res;
        } catch (Exception e) {
            LOGGER.error("plotPathMetaUpdate", e);
        }

        return "null_image";
    }

    // the result matrix
    public static String[] GetMetaPathResultItem(RConnection RC, int rowN) {
        try {
            String rCommand = "GetMetaPathResultItem(" + rowN + ")";
            String[] dinn = RC.eval(rCommand).asStrings();
            return dinn;
        } catch (Exception rse) {
            LOGGER.error("GetMetaPathResultItem", rse);
        }
        return null;
    }

    public static String[] getMetaPathResColNames(RConnection RC) {
        try {
            String rCommand = "GetMetaPathResultColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getMetaPathResColNames", rse);
        }
        return null;
    }

    public static String[] getMetaPathNMs(RConnection RC) {
        try {
            String rCommand = "GetMetaPathNMs()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getMetaPathNMs", rse);
        }
        return null;
    }

    public static int performNetworkAnal(RConnection RC) {
        try {
            String rCommand = "Prepare4Network()";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("performNetworkAnal", rse);
        }
        return 0;
    }

    public static int[] performPathSum(SessionBean1 sb, double pvalCutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "GetSigPathNums(" + pvalCutoff + ")";
            sb.recordRCommandFunctionInfo(rCommand, "prepareMetaPathUpsetView");

            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("performPathSum", rse);
        }
        return null;
    }

    public static int prepareMetaPathData(SessionBean1 sb, String imgNm) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PrepareMetaPathData(NA" + ", \"" + imgNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "prepareMetaPathUpsetView");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("prepareMetaPathData", rse);
            return 0;
        }

    }

    public static void setSelectedMetaPathNames(SessionBean1 sb, String[] nmVec) {
        try {
            RConnection RC = sb.getRConnection();
            String nmVecS = String.join("\",\"", nmVec);
            String rCommand = "SelectMultiPathData(NA, c(\"" + nmVecS + "\"))";
            sb.recordRCommandFunctionInfo(rCommand, "prepareMetaPathUpsetView");

            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("setSelectedMetaPathNames", rse);
        }

    }

    public static String getVennPathsNames(RConnection RC, String areas) {
        try {
            String rCommand = "GetVennPathsNames(NA" + ", \"" + areas + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getVennPathsNames", e);
        }
        return null;
    }

    public static int setInclusionDataSets(RConnection RC, String datasVec) {
        try {
            String rCommand = "setInclusionDataSets(NA," + datasVec + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("setInclusionDataSets", e);
        }
        return 0;
    }

    public static int finishDataSet(RConnection RC, String dataName, String dataName2) {
        try {
            String rCommand = "Finish.DataSet(\"" + dataName + "\", \"" + dataName2 + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("finishDataSet", e);
        }
        return 0;
    }

    public static void restoreCmdHistory(RConnection RC) {
        try {
            String rCommand = "Restore.CmdHistory();";
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("restoreCmdHistory", e);
        }
    }
}
