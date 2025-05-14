/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author jianguox
 */
public class RIntegUtils {

    public static int performGeneMapping(RConnection RC, String geneList, String org, String idType) {
        try {
            String rCommand = "PerformGeneMapping(NA, \"" + geneList + "\", \"" + org + "\", \"" + idType + "\");";

            // for local run, need to get the list from a txt file, not from web interface
            String rcmd1 = "geneListFile<-\"replace_with_your_file_name\"";
            RCenter.recordRCommand(RC, rcmd1);
            String rcmd2 = "geneList<-readChar(geneListFile, file.info(geneListFile)$size)";
            RCenter.recordRCommand(RC, rcmd2);
            String rcmd3 = "PerformGeneMapping(NA, geneList, \"" + org + "\", \"" + idType + "\");";
            RCenter.recordRCommand(RC, rcmd3);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performCmpdMapping(RConnection RC, String cmpdList, String org, String idType) {
        try {
            String rCommand = "PerformCmpdMapping(NA" + ", \"" + cmpdList + "\", \"" + org + "\", \"" + idType + "\");";
            // for local run, need to get the list from a txt file, not from web interface
            String rcmd1 = "cmpdListFile<-\"replace_with_your_file_name\"";
            RCenter.recordRCommand(RC, rcmd1);
            String rcmd2 = "cmpdList<-readChar(cmpdListFile, file.info(cmpdListFile)$size)";
            RCenter.recordRCommand(RC, rcmd2);
            String rcmd3 = "PerformCmpdMapping(NA, cmpdList, \"" + org + "\", \"" + idType + "\");";
            RCenter.recordRCommand(RC, rcmd3);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performIntegPathwayAnalysis(RConnection RC, String topoOpt, String enrichOpt, String libOpt, String integOpt) {
        try {
            String rCommand = "PerformIntegPathwayAnalysis(NA" + ", \"" + topoOpt + "\", \"" + enrichOpt  +"\", \"" + libOpt + "\", \"" + integOpt + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }
    
    public static int createIntegPathResults(RConnection RC) {
        try {
            String rCommand = "CreateIntegMatchingTable(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int removeCmpd(RConnection RC, int inx) {
        try {
            String rCommand = "RemoveCmpd(NA" + ", " + inx + ");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int removeGene(RConnection RC, int inx) {
        try {
            String rCommand = "RemoveGene(NA" + ", " + inx + ");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int prepareIntegData(RConnection RC) {
        try {
            String rCommand = "PrepareIntegData(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getPathMapColNames(RConnection RC) {
        try {
            String rCommand = "GetPathMapColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPathMapRowNames(RConnection RC) {
        try {
            String rCommand = "GetPathMapRowNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getIntegResColNames(RConnection RC) {
        try {
            String rCommand = "GetIntegResultColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getIntegResMatrix(RConnection RC) {
        try {
            String rCommand = "GetIntegResultMatrix(NA)";
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getIntegPathNames(RConnection RC) {
        try {
            String rCommand = "GetIntegResultPathNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getIntegPathIDs(RConnection RC) {
        try {
            String rCommand = "GetIntegResultPathIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
    
    public static int definePvalMethod(RConnection RC, String pmeth){
        String rCommand = "DefineIntegPvalueMethod(NA, method = \"" + pmeth + "\")";
        String rCommand2 = "DefineIntegPvalueMethod(mSetObj, method = \"" + pmeth + "\")";
        try {
            int res = RC.eval(rCommand).asInteger();
            RCenter.recordRCommand(RC, rCommand2);
            return res;
        } catch (REXPMismatchException | RserveException rse) {
            System.out.println(rse);
        }
        RCenter.recordRCommand(RC, rCommand);
        return 0;
    }
    
    public static int performIntegNetworkAnal(RConnection RC, String netOpt){        
        try {
            String rCommand = "Prepare4IntegNetwork(NA,\"" + netOpt + "\")";
            String rCommand2 = "Prepare4IntegNetwork(mSetObj)";
            int res = RC.eval(rCommand).asInteger();
            RCenter.recordRCommand(RC, rCommand2);
            return res;
        } catch (REXPMismatchException | RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }    
    public static int performTarIntegNetAnal(RConnection RC, String netOpt){        
        try {
            String rCommand = "Prepare4TarIntegNetwork(NA,\"" + netOpt + "\")";
            String rCommand2 = "Prepare4TarIntegNetwork(mSetObj)";
            int res = RC.eval(rCommand).asInteger();
            RCenter.recordRCommand(RC, rCommand2);
            return res;
        } catch (REXPMismatchException | RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }
}
