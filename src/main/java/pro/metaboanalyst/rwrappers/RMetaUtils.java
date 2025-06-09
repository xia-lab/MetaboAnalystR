/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author jianguox
 */
public class RMetaUtils {

    //return dataName (strip .zip or txt) to be consistent java/R
    public static int readIndExpressTable(RConnection RC, String dataName, String dataFormat) {
        try {
            String rCommand = "ReadIndData(NA" + ", \"" + dataName + "\", \"" + dataFormat + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    //only for single gene expression
    public static int setCurrentData(RConnection RC, String nm) {
        try {
            return RC.eval("SetCurrentData(\"" + nm + "\")").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void removeData(RConnection RC, String dataName) {
        try {
            String rCommand = "RemoveData(\"" + dataName + "\")";
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static int[] getDataDims(RConnection RC, String dataName) {
        try {
            String rCommand = "GetDataDims(NA" + ", \"" + dataName + "\")";
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int checkMetaDataConsistency(RConnection RC, String adjustBatch) {
        try {
            String rCommand = "CheckMetaDataConsistency(NA" + ", " + adjustBatch + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int plotDataProfiles(RConnection RC, String dataName, String boxName, String pcaName, String format, int dpi) {
        try {
            String rCommand = "PlotDataProfile(\"" + dataName + "\", \"" + boxName + "\", \"" + pcaName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static void plotSelectedFeature(SessionBean1 sb, String symb, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSelectedFeature(NA" + ", \"" + symb + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("meta_ft_" + symb, rCommand);
            RC.voidEval(rCommand);
            String size = RGraphUtils.getCurrentCmpdImgSize(RC);
            sb.setCmpdImgSize(size);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static int performVoteCounting(SessionBean1 sb, double sigLvl, double minVote) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PerformVoteCounting(NA" + ", " + sigLvl + ", " + minVote + ")";

            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performVoteCounting");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performPvalCombination(SessionBean1 sb, String method, double sigLvl) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PerformPvalCombination(NA" + ", \"" + method + "\", " + sigLvl + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performPvalCombination");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performMetaEffectSize(RConnection RC, String method, double sigLvl) {
        try {
            String rCommand = "PerformMetaEffectSize(\"" + method + "\", " + sigLvl + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performMetaMerge(SessionBean1 sb, double sigLvl) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PerformMetaMerge(NA" + ", " + sigLvl + ")";
            sb.recordRCommandFunctionInfo(rCommand, "performDirectMerging");
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String getMetaGeneIDType(RConnection RC) {
        try {
            String rCommand = "GetMetaGeneIDType()";
            String nms = RC.eval(rCommand).asString();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMetaResColNames(RConnection RC) {
        try {
            String rCommand = "GetMetaResultColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //entrez ids
    public static String[] getMetaResGeneIDs(RConnection RC) {
        try {
            String rCommand = "GetMetaResultGeneIDs()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMetaResGeneSymbols(RConnection RC) {
        try {
            String rCommand = "GetMetaResultGeneSymbols()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMetaResGeneIDLinks(RConnection RC) {
        try {
            String rCommand = "GetMetaResultGeneIDLinks()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    // the result matrix
    public static double[][] getMetaResMatrix(RConnection RC, String indFld) {
        try {
            String rCommand = "GetMetaResultMatrix(NA" + ", \"" + indFld + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int[] performLimmaDE(RConnection RC, String dataName, double sigLevel, double fcLvl) {
        try {
            String rCommand = "PerformLimmaDE(NA" + ", \"" + dataName + "\", " + sigLevel + ", " + fcLvl + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return new int[0];
    }

    public static int performIndNormalization(RConnection RC, String dataName, String opt, int autoOpt) {
        try {
            String rCommand = "PerformIndNormalization(NA, \"" + dataName + "\", \"" + opt + "\", " + autoOpt + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int doHeatmapEnrichmentTest(RConnection RC, String fileNm, String libNm, String IDs) {
        try {
            String rCommand = "PerformHeatmapEnrichment(\"" + fileNm + "\", \"" + libNm + "\", \"" + IDs + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int checkMetaPerformed(RConnection RC, String type) {
        try {
            String rCommand = "CheckMetaPerformed(\"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

}
