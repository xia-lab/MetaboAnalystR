/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
public class MgwasUtils {

    public static int performExposureSearch(RConnection RC, String qvec) {
        try {
            String rCommand = "QueryExposure(NA,\"" + qvec + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static String[] getResRowNames(RConnection RC, String netType) {
        try {
            String rCommand = "GetResRowNames(\"" + netType + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception e) {
            System.out.println(e);
        }
        return null;
    }

    public static String[] getResCol(RConnection RC, String netType, int colInx) {
        try {
            String rcmd = "GetResCol(\"" + netType + "\"," + colInx + ")";
            //RCenter.recordRCommand(RC, rcmd);
            return RC.eval(rcmd).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] getResColByName(RConnection RC, String netType, String name) {
        try {
            String rcmd = "GetResColByName(\"" + netType + "\",\"" + name + "\")";
            //RCenter.recordRCommand(RC, rcmd);
            return RC.eval(rcmd).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int performOutcomeSearch(RConnection RC, String myVec) {
        try {
            String rCommand = "QueryOutcome(\"" + myVec + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static int setMappingType(RConnection RC, String[] nms) {
        try {
            RC.assign("nms.vec", nms);
            String rCommand = "SetMappingType();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void saveMgwasResult(RConnection RC, String tableNm) {
        try {
            String rCommand = "PrepareMgwasCSV(\"" + tableNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    public static String[] getSummaryRow(RConnection RC) {
        try {
            String rCommand = "GetSummaryRow();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static String[] getSummaryTargetNum(RConnection RC) {
        try {
            String rCommand = "GetSummaryTargetNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            System.out.println(e);
        }
        return null;
    }

    public static String[] getSummaryTargets(RConnection RC) {
        try {
            String rCommand = "GetSummaryTargets();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            System.out.println(e);
        }
        return null;
    }

    public static int[] filterNetByPval(RConnection RC, double pvalThresh) {
        try {
            String rCommand = "FilterNetByPval(NA" + ", " + pvalThresh + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int performMRAnalysis(RConnection RC) {
        try {
            String rCommand = "PerformMRAnalysis(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static int performSnpFiltering(RConnection RC, String ldclumpOpt,
            String ldProxyOpt, boolean ldProxies, double ldThresh, boolean pldSNPs,
            double mafThresh, String harmonizeOpt, String current_key) {
        try {
            String rCommand = "PerformSnpFiltering(NA, \"" + ldclumpOpt + "\", \"" + ldProxyOpt + "\", \""
                    + ldProxies + "\", " + ldThresh + ", \"" + pldSNPs + "\", " + mafThresh + ", \"" + harmonizeOpt
                    + "\", \"" + current_key + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return -1;
    }

    public static String readOpenGWASKey(RConnection RC) {
        try {
            String rCommand = "readOpenGWASKey()";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            System.out.println(e);
        }
        return "";
    }

    /*
    public static int performLDClumping(RConnection RC, String clumpOpt) {
        try {
            String rCommand = "PerformLDClumping(NA,\"" + clumpOpt + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return -1;
    }

    public static int performLDProxies(RConnection RC, boolean ldProxies, double ldThresh, boolean pldSNPs, double mafThresh) {
        try {
            String rCommand = "PerformLDProxies(NA, \"" + ldProxies + "\", \"" + ldThresh + "\", \"" + pldSNPs + "\", \"" + mafThresh + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return -1;
    }
    
    public static int performHarmonization(RConnection RC, String harmonizeOpt) {
        try {
            String rCommand = "PerformHarmonization(NA,\"" + harmonizeOpt + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return -1;
    }*/
    public static int plotScatter(SessionBean1 sb, String exposure, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotScatter(NA" + ", \"" + exposure + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(exposure + "_mr_scatter_plot", rCommand);
            sb.addGraphicsMapLink(exposure + "_mr_scatter_plot", "/Secure/analysis/RSVMView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int plotForest(SessionBean1 sb, String exposure, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotForest(NA" + ", \"" + exposure + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(exposure + "_mr_forest_plot", rCommand);
            sb.addGraphicsMapLink(exposure + "_mr_scatter_plot", "/Secure/mgwas/ResultView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int plotLeaveOneOut(SessionBean1 sb, String exposure, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotLeaveOneOut(NA" + ", \"" + exposure + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(exposure + "_mr_leaveoneout_plot", rCommand);
            sb.addGraphicsMapLink(exposure + "_mr_leaveoneout_plot", "/Secure/mgwas/ResultView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int plotFunnel(SessionBean1 sb, String exposure, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotFunnel(NA" + ", \"" + exposure + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(exposure + "_mr_funnel_plot", rCommand);
            sb.addGraphicsMapLink(exposure + "_mr_funnel_plot", "/Secure/mgwas/ResultView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int setMRMethod(RConnection RC, String[] nms) {
        try {
            RC.assign("nms.vec", nms);
            String rCommand = "SetMRMethod();";

            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int removeEntry(RConnection RC, String rowID) {
        try {
            String rCommand = "RemoveEntryExposure(NA, \"" + rowID + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int removeEntries(RConnection RC, String[] rowIDs) {
        try {
            RC.assign("entries.vec", rowIDs);
            String rCommand = "RemoveEntriesExposure(NA);";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int addEntry(RConnection RC, String rowID) {
        try {
            String rCommand = "AddEntryExposure(NA, \"" + rowID + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getPathRowNames(RConnection RC) {
        try {
            String rCommand = "GetPathRowNames();";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception e) {
            System.out.println(e);
        }
        return null;
    }

    public static String[] getPathCol(RConnection RC, int colInx) {
        try {
            String rcmd = "GetPathCol(" + colInx + ")";
            //RCenter.recordRCommand(RC, rcmd);
            return RC.eval(rcmd).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] getSumCol(RConnection RC, String type, String exp) {
        try {
            String rcmd = "GetSumCol(\"" + type + "\",\"" + exp + "\")";
            //RCenter.recordRCommand(RC, rcmd);
            return RC.eval(rcmd).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] updateSNPEntries(RConnection RC, String filterCol, String filterOpt, String filterValue, String act, String tableType) {
        try {
            String rCommand = "UpdateSNPEntries(\"" + filterCol + "\", \"" + filterOpt + "\", \"" + filterValue + "\", \"" + act + "\", \"" + tableType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception e) {
            System.out.println(e);
        }
        return null;
    }

    public static int resetSNPEntries(RConnection RC, String tableType) {
        try {
            String rCommand = "ResetSNPEntries( \"" + tableType + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;

    }

    public static int performLiteratureSearch(RConnection RC, String exposure, String outcome) {
        try {
            String rCommand = "QueryLiteratureMelodiPresto(\"" + exposure + "\", \"" + outcome + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }
}
