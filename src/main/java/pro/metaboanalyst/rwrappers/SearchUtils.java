/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author Jeff
 */
public class SearchUtils {

    private static final Logger LOGGER = LogManager.getLogger(SearchUtils.class);

    public static void searchLibrary(RConnection RC, String query, String type) {
        try {
            String rCommand = "SearchMsetLibraries(NA" + ", \"" + query + "\",\"" + type + "\");";
            RC.eval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            LOGGER.error("searchLibrary", e);
        }
    }

    public static String[] searchMsetByName(RConnection RC, String query) {
        try {
            String rCommand = "SearchMsetLibByName(\"" + query + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("searchMsetByName", e);
        }
        return null;
    }

    public static void crossReferenceAPI(RConnection RC, String query_type) {
        try {
            String rCommand = "CrossReferencingAPI(NA" + ", \"" + query_type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("crossReferenceAPI", e);
        }
    }

    public static void crossReferenceExactGeneral(RConnection RC, String query_type) {
        try {
            String rCommand = "CrossReferencing(NA" + ", \"" + query_type + "\", T, T, T, T, T, T" + ");";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            LOGGER.error("crossReferenceExactGeneral", e);
        }
    }

    public static void crossReferenceExact(SessionBean1 sb, String query_type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CrossReferencing(NA" + ", \"" + query_type + "\");";
            RC.voidEval(rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Sanity Check");

            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            LOGGER.error("crossReferenceExact", e);
        }
    }

    public static void crossReferenceExactLipid(SessionBean1 sb, String query_type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CrossReferencing(NA" + ", \"" + query_type + "\", lipid = T);";
            RC.voidEval(rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Sanity Check");
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            LOGGER.error("crossReferenceExactLipid", e);
        }
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static String[] getNameMapTable(RConnection RC) {
        try {
            String rCommand = "CreateMappingResultTable(NA)";
            String[] resTable = RC.eval(rCommand).asStrings();
            if (resTable.length == 1) {
                if (resTable[0].equals("")) {
                    return null;
                }
            }
            RCenter.recordRCommand(RC, rCommand);
            return resTable;
        } catch (Exception e) {
            LOGGER.error("getNameMapTable", e);
        }
        return null;
    }

    public static String[] getGeneNameMapTable(RConnection RC) {
        try {
            //String rCommand = "GetGeneMappingResultTable(NA)";
            String rCommand = "GetKeggEntryMappingTable(NA)";
            String[] resTable = RC.eval(rCommand).asStrings();
            // RCenter.recordRCommand(RC, rCommand);
            return resTable;
        } catch (Exception e) {
            LOGGER.error("getGeneNameMapTable", e);
        }
        return null;
    }

    // Prepares table matchings of query genes with KEGG orthologs
    public static String[] getNetworkGeneNameMapTable(RConnection RC) {
        try {
            String rCommand = "GetNetworkGeneMappingResultTable(NA)";
            String[] resTable = RC.eval(rCommand).asStrings();
            RCenter.recordRCommand(RC, rCommand);
            return resTable;
        } catch (Exception e) {
            LOGGER.error("getNetworkGeneNameMapTable", e);
        }
        return null;
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static int getNameMapRowNumber(RConnection RC) {
        try {
            return RC.eval("GetHitsRowNumber(NA)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getNameMapRowNumber", e);
        }
        return 0;
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static int getGeneNameMapRowNumber(RConnection RC) {
        try {
            return RC.eval("GetGeneHitsRowNumber(NA)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getGeneNameMapRowNumber", e);
        }
        return 0;
    }

    public static void performDetailSearch(RConnection RC, String query) {
        try {
            String rCommand = "PerformDetailMatch(NA, \"" + query + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("performDetailSearch", e);
        }
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static int getCanListRowNumber(RConnection RC) {
        try {
            return RC.eval("GetCanListRowNumber(NA)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getCanListRowNumber", e);
        }
        return 0;
    }

    public static String[] getCandidateList(RConnection RC) {
        try {
            String rCommand = "GetCandidateList(NA);";
            String[] rawResults = RC.eval(rCommand).asStrings();

            RCenter.recordRCommand(RC, rCommand);
            return rawResults;
        } catch (Exception e) {
            LOGGER.error("getCandidateList", e);
        }
        return null;
    }

    public static int setCandidate(RConnection RC, String qNm, String cNm) {
        try {
            String rCommand = "SetCandidate(NA" + ", \"" + qNm + "\", \"" + cNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            LOGGER.error("setCandidate", e);
            return -1;
        }
    }

    public static String getQuery(RConnection RC, int inx) {
        try {
            return (RC.eval("GetQuery(NA" + ", " + inx + ")").asString());
        } catch (Exception e) {
            LOGGER.error("getQuery", e);
        }
        return null;
    }

}
