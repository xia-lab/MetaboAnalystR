/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author soufanom
 */
public class RNetworkUtils {

    private static final Logger LOGGER = LogManager.getLogger(RNetworkUtils.class);

    public static int doEnrichmentTest_KO01100(RConnection RC, String dbType, String fileNm) {
        try {
            String rCommand = "PerformKOEnrichAnalysis_KO01100(NA" + ", \"" + dbType + "\", \"" + fileNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "doMnetworkAnalysis");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("doEnrichmentTest_KO01100", e);
        }
        return 0;
    }

    public static void prepareKeggQueryJson(RConnection RC) {
        try {
            String rCommand = "PrepareKeggQueryJson(NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "doMnetworkAnalysis");

            RC.voidEval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("prepareKeggQueryJson", rse);
        }
    }

    //parameter for network
    public static int prepareNetworkData(RConnection RC) {
        try {
            String rCommand = "PrepareNetworkData(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            LOGGER.error("prepareNetworkData", e);
        }
        return 0;
    }

    public static String getShortestPaths(RConnection RC, String from, String to) {
        try {
            String rCommand = "GetShortestPaths(\"" + from + "\", \"" + to + "\")";
            String nms = RC.eval(rCommand).asString();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getShortestPaths", rse);
        }
        return null;
    }

    public static int[] searchNetDB(RConnection RC, String dbType, String dbName, Double minScore) {
        try {
            String useExp = "FALSE";
            String rCommand = "SearchNetDB(NA" + ", \"" + dbType + "\", \"" + dbName + "\", " + useExp + ", " + minScore + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "doMnetworkAnalysis");

            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("searchNetDB", rse);
        }
        return new int[0];
    }

    public static int[] performDSPC(RConnection RC) {
        try {
            String rCommand = "PerformDSPC(NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "doMnetworkAnalysis");

            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("performDSPC", rse);
        }
        return new int[0];
    }

    public static int[] createIGraph(RConnection RC) {
        try {
            String rCommand = "CreateGraph(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("createIGraph", rse);
        }
        return null;
    }

    public static int prepareNetwork(RConnection RC, String netNm, String jsonNm) {
        try {
            return RC.eval("PrepareNetwork(\"" + netNm + "\", \"" + jsonNm + "\")").asInteger();
        } catch (Exception rse) {
            LOGGER.error("prepareNetwork", rse);
        }
        return (0);
    }

    public static String[] getNetsNames(RConnection RC) {
        try {
            String rCommand = "GetNetsName();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getNetsNames", e);
        }
        return null;
    }

    public static String getNetsNamesString(RConnection RC) {
        try {
            String checkFunc = "exists('GetNetsNameString') && is.function(GetNetsNameString)";
            int exists = RC.eval(checkFunc).asInteger();
            if (exists != 1) {
                RC.voidEval(".load.scripts.on.demand('networks_enrich.Rc')");
                RC.voidEval (".load.scripts.on.demand('networks_view.Rc')");
                return null;
            }
            String rCommand = "GetNetsNameString();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getNetsNamesString", e);
        }
        return null;
    }

    //return the number of components
    public static String detectCommunities(RConnection RC, String method) {
        try {
            String rCommand = "FindCommunities(\"" + method + "\");";
            RCenter.recordRCommand(RC, rCommand);
            String coms = RC.eval(rCommand).asString();
            return coms;
        } catch (Exception e) {
            LOGGER.error("detectCommunities", e);
        }
        return "NA";
    }

    public static String[] getNodeIDs(RConnection RC) {
        try {
            String rCommand = "GetNodeIDs();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getNodeIDs", e);
        }
        return null;
    }

    public static String[] getNodeNames(RConnection RC) {
        try {
            String rCommand = "GetNodeNames();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getNodeNames", e);
        }
        return null;
    }

    public static int[] getNodeDegrees(RConnection RC) {
        try {
            String rCommand = "GetNodeDegrees();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
            LOGGER.error("getNodeDegrees", e);
        }
        return null;
    }

    public static double[] getNodeBetweenness(RConnection RC) {
        try {
            String rCommand = "GetNodeBetweenness();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asDoubles();
        } catch (Exception e) {
            LOGGER.error("getNodeBetweenness", e);
        }
        return null;
    }

    public static String getNodeEntrezIDs(RConnection RC, String unipID) {
        try {
            String rCommand = "GetNodeEntrezIDs(\"" + unipID + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getNodeEntrezIDs", e);
        }
        return null;
    }

    public static String getNodeEmblEntrezIDs(RConnection RC, String emblprotein) {
        try {
            String rCommand = "GetNodeEmblEntrezIDs(\"" + emblprotein + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getNodeEmblEntrezIDs", e);
        }
        return null;
    }

    public static int[] getNetsNodeNum(RConnection RC) {
        try {
            String rCommand = "GetNetsNodeNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
            LOGGER.error("getNetsNodeNum", e);
        }
        return null;
    }

    public static int[] getNetsEdgeNum(RConnection RC) {
        try {
            String rCommand = "GetNetsEdgeNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
            LOGGER.error("getNetsEdgeNum", e);
        }
        return null;
    }

    public static int[] getNetsQueryNum(RConnection RC) {
        try {
            String rCommand = "GetNetsQueryNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
            LOGGER.error("getNetsQueryNum", e);
        }
        return null;
    }

    public static int doNetEnrichmentTest(RConnection RC, String fileNm, String libNm, String IDs, String vismode) {
        try {
            String rCommand = "PerformNetEnrichment(NA, \"" + fileNm + "\", \"" + libNm + "\", \"" + IDs + "\", \"" + vismode + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("doNetEnrichmentTest", e);
        }
        return 0;
    }

    public static String excludeNodes(RConnection RC, String nodeIDs, String fileNm) {
        try {
            String rCommand = "ExcludeNodes(\"" + nodeIDs + "\", \"" + fileNm + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("excludeNodes", rse);
        }
        return null;
    }

    public static String updateNetworkLayout(RConnection RC, String algo, String jsonNm) {
        try {
            return RC.eval("UpdateNetworkLayout(\"" + algo + "\", \"" + jsonNm + "\")").asString();
        } catch (Exception rse) {
            LOGGER.error("updateNetworkLayout", rse);
            return null;
        }
    }

    public static void exportNetwork(RConnection RC, String name) {
        try {
            RC.voidEval("ExportNetwork(\"" + name + "\")");
        } catch (Exception rse) {
            LOGGER.error("exportNetwork", rse);
        }
    }

    public static String extractModule(RConnection RC, String nodeIDs) {
        try {
            String rCommand = "ExtractModule(\"" + nodeIDs + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("extractModule", rse);
        }
        return null;
    }

    public static int updateIntegPathwayAnalysis(RConnection RC, String qlist, String fileNm, String topoOpt, String enrichOpt, String libOpt, String visMode) {
        try {
            String rCommand = "UpdateIntegPathwayAnalysis(NA" + ", \"" + qlist + "\", \"" + fileNm + "\", \"" + topoOpt + "\", \"" + enrichOpt + "\", \"" + libOpt + "\", \"" + visMode + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            LOGGER.error("updateIntegPathwayAnalysis", e);
        }
        return 0;
    }

    public static void prepareSubnetDownload(RConnection RC, String name) {
        try {
            String rCommand = "PrepareSubnetDownloads(\"" + name + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("prepareSubnetDownload", rse);
        }
    }

    public static int[] buildMinConnectedNet(RConnection RC) {
        try {
            String rCommand = "GetMinConnectedGraphs(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("buildMinConnectedNet", rse);
        }
        return null;
    }

    public static int[] filterNetByCor(RConnection RC, double minPval, double minQval, double negCoeff1, double negCoeff2, double posCoeff1, double posCoeff2) {
        try {
            String rCommand = "FilterNetByCor(" + minPval + ", " + minQval + ", " + negCoeff1 + ", " + negCoeff2 + ", " + posCoeff1 + ", " + posCoeff2 + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("filterNetByCor", rse);
        }
        return null;
    }

    public static int[] filterBipartiNet(RConnection RC, String ndType, double dgr, double btw) {
        try {
            String rCommand = "FilterBipartiNet(NA" + ", \"" + ndType + "\", " + dgr + ", " + btw + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            LOGGER.error("filterBipartiNet", rse);
        }
        return null;
    }

    public static double getMaxRawPVal(RConnection RC) {
        try {
            return RC.eval("GetMaxRawPVal(NA)").asDouble();
        } catch (Exception e) {
            LOGGER.error("getMaxRawPVal", e);
        }
        return 0;
    }

    public static double getMinNegCoeff(RConnection RC) {
        try {
            return RC.eval("GetMinNegCoeff(NA)").asDouble();
        } catch (Exception e) {
            LOGGER.error("getMinNegCoeff", e);
        }
        return 0;
    }

    public static double getMaxNegCoeff(RConnection RC) {
        try {
            return RC.eval("GetMaxNegCoeff(NA)").asDouble();
        } catch (Exception e) {
            LOGGER.error("getMaxNegCoeff", e);
        }
        return 0;
    }

    public static double getMinPosCoeff(RConnection RC) {
        try {
            return RC.eval("GetMinPosCoeff(NA)").asDouble();
        } catch (Exception e) {
            LOGGER.error("getMinPosCoeff", e);
        }
        return 0;
    }

    public static double getMaxPosCoeff(RConnection RC) {
        try {
            return RC.eval("GetMaxPosCoeff(NA)").asDouble();
        } catch (Exception e) {
            LOGGER.error("getMaxPosCoeff", e);
        }
        return 0;
    }

}
