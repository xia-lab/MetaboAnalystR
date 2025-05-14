/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author Jeff
 */
public class REnrichUtils {

    private static final Logger LOGGER = LogManager.getLogger(REnrichUtils.class);

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static boolean hypergeomTest(RConnection RC) {
        try {
            String rCommand = "CalculateHyperScore(NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception e) {
            LOGGER.error("hypergeomTest", e);
        }
        return false;
    }

    public static String[] getORAColorBar(RConnection RC) {
        try {
            return RC.eval("GetORA.colorBar(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getORAColorBar", e);
        }
        return null;
    }

    public static String[] getORARowNames(RConnection RC) {
        try {
            return RC.eval("GetORA.rowNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getORARowNames", e);
        }
        return null;
    }

    public static double[][] getORAMat(RConnection RC) {
        try {
            return RC.eval("GetORA.mat(NA)").asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getORAMat", e);
        }
        return null;
    }

    public static String[] getEnrichPieNames(RConnection RC) {
        try {
            return RC.eval("GetEnrichPieNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getEnrichPieNames", e);
        }
        return null;
    }

    public static String[] getEnrichPieColors(RConnection RC) {
        try {
            return RC.eval("GetEnrichPieColors(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getEnrichPieColors", e);
        }
        return null;
    }

    public static double[][] getEnrichPieHits(RConnection RC) {
        try {
            return RC.eval("GetEnrichPieHits(NA)").asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getEnrichPieHits", e);
        }
        return null;
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void doSSPTest(RConnection RC) {
        try {
            String rCommand = "CalculateSSP(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("doSSPTest", e);
        }
    }

    public static String[] getSSP_HMDB(RConnection RC) {
        try {
            return RC.eval("GetSSP.HMDB(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSSP_HMDB", e);
        }
        return null;
    }

    public static String[] getSSPRefConcs(RConnection RC) {
        try {
            return RC.eval("GetSSP.RefConcs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSSPRefConcs", e);
        }
        return null;
    }

    public static String[] getSSPStates(RConnection RC) {
        try {
            return RC.eval("GetSSP.States(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSSPStates", e);
        }
        return null;
    }

    public static String[] getSSPdetails(RConnection RC) {
        try {
            return RC.eval("GetSSP.Details(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSSPdetails", e);
        }
        return null;
    }

    public static String[] getSSPNames(RConnection RC) {
        try {
            return RC.eval("GetSSP.Names(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSSPNames", e);
        }
        return null;
    }

    public static String[] getSSPConcs(RConnection RC) {
        try {
            return RC.eval("GetSSP.Concs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSSPConcs", e);
        }
        return null;
    }

    public static int performGlobalTest(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CalculateGlobalTestScore(NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            //here we use microservice
            rCommand = ".prepare.globaltest.score(NA)";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
                String rCommand_micro = "dat.in <- qs::qread(\"dat.in.qs\");"
                        + "dat.in$my.res <- dat.in$my.fun();"
                        + "qs::qsave(dat.in, file=\"dat.in.qs\");";
                RCenter.recordRCommand(RC, rCommand_micro);
            }
            rCommand = ".save.globaltest.score(NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("performGlobalTest", e);
            return 0;
        }
    }

    public static String[] getQEAColorBar(RConnection RC) {
        try {
            return RC.eval("GetQEA.colorBar(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getQEAColorBar", e);
        }
        return null;
    }

    public static String[] getQEARowNames(RConnection RC) {
        try {
            return RC.eval("GetQEA.rowNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getQEARowNames", e);
        }
        return null;
    }

    public static double[][] getQEAMat(RConnection RC) {
        try {
            return RC.eval("GetQEA.mat(NA)").asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getQEAMat", e);
        }
        return null;
    }

    public static String[] getHTMLMetSet(RConnection RC, String msetNm) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetHTMLMetSet(NA, \"" + msetNm + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getHTMLMetSet", e);
        }
        return null;
    }

    public static String[] getIntegHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetIntegHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getIntegHTMLPathSet", e);
        }
        return null;
    }

    public static String[] getHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getHTMLPathSet", e);
        }
        return null;
    }

    public static String[] getMummichogHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetMummichogHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getMummichogHTMLPathSet", e);
        }
        return null;
    }

    public static String getMetSetName(RConnection RC, int setInx) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetMetSetName(NA, " + setInx + ")";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getMetSetName", e);
        }
        return null;
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static boolean doPathOraTest(SessionBean1 sb, String topoCode, String method) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CalculateOraScore(NA, \"" + topoCode + "\", \"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("doPathOraTest", e);
        }
        return false;
    }

    public static String[] getORApathNames(RConnection RC) {
        try {
            return RC.eval("GetORA.pathNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getORApathNames", e);
        }
        return null;
    }

    public static String[] getORAKeggIDs(RConnection RC) {
        try {
            return RC.eval("GetORA.keggIDs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getORAKeggIDs", e);
        }
        return null;
    }

    public static String[] getORASMPDBIDs(RConnection RC) {
        try {
            return RC.eval("GetORA.smpdbIDs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getORASMPDBIDs", e);
        }
        return null;
    }

    public static boolean doPathQeaTest(SessionBean1 sb, String topoCode, String method) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand2 = "CalculateQeaScore(NA" + ", \"" + topoCode + "\", \"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand2);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand2, "paBn_proceed");

            //here we use microservice
            String rCommand = ".prepare.qea.score(NA" + ", \"" + topoCode + "\", \"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
                String rCommand_micro = "dat.in <- qs::qread(\"dat.in.qs\");"
                        + "dat.in$my.res <- dat.in$my.fun();"
                        + "qs::qsave(dat.in, file=\"dat.in.qs\");";
                RCenter.recordRCommand(RC, rCommand_micro);

                rCommand = ".save.qea.score(NA)";
                RCenter.recordRCommand(RC, rCommand);
                JavaRecord.recordRCommandFunctionInfo(RC, rCommand2, "paBn_proceed");

                return RC.eval(rCommand).asInteger() == 1;
            }

        } catch (Exception e) {
            LOGGER.error("doPathQeaTest", e);
        }
        return false;
    }

    public static String[] getQEApathNames(RConnection RC) {
        try {
            return RC.eval("GetQEA.pathNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getQEApathNames", e);
        }
        return null;
    }

    public static String[] getQEAKeggIDs(RConnection RC) {
        try {
            return RC.eval("GetQEA.keggIDs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getQEAKeggIDs", e);
        }
        return null;
    }

    public static String[] getQEASMPDBIDs(RConnection RC) {
        try {
            return RC.eval("GetQEA.smpdbIDs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getQEASMPDBIDs", e);
        }
        return null;
    }

    public static String plotCmpdConcRange(RConnection RC, String nm, String format, int dpi) {
        try {
            String rCommand = "PlotConcRange(NA, \"" + nm + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("plotCmpdConcRange", rse);
            return null;
        }
    }

    public static void plotEnrichPieChart(SessionBean1 sb, String enrichType, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotEnrichPieChart(NA" + ", \"" + enrichType + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            if (enrichType.equals("ora")) {
                sb.addGraphicsCMD("ora_pie", rCommand);
                sb.addGraphicsMapLink("ora_pie", "/Secure/enrichment/OraView.xhtml");

            } else {
                sb.addGraphicsCMD("qea_pie", rCommand);
                sb.addGraphicsMapLink("qea_pie", "/Secure/enrichment/QeaView.xhtml");

            }

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("plotEnrichPieChart", rse);
        }
    }

    public static void plotORA(SessionBean1 sb, String imgOpt, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotORA(NA" + ", \"" + imgName + "\", \"" + imgOpt + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            sb.addGraphicsCMD("ora", rCommand);
            sb.addGraphicsMapLink("ora", "/Secure/enrichment/OraView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("plotORA", rse);
        }
    }

    public static void plotQEA(SessionBean1 sb, String imgOpt, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotQEA.Overview(NA" + ", \"" + imgName + "\", \"" + imgOpt + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            sb.addGraphicsCMD("qea", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("plotQEA", rse);
        }
    }

    public static void plotEnrichmentDotPlot(SessionBean1 sb, String enrichType, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotEnrichDotPlot(NA" + ", \"" + enrichType + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Enrichment");

            if (enrichType.equals("ora")) {
                sb.addGraphicsCMD("ora_dot", rCommand);
                sb.addGraphicsMapLink("ora_dot", "/Secure/enrichment/OraView.xhtml");

            } else {
                sb.addGraphicsCMD("qea_dot", rCommand);
                sb.addGraphicsMapLink("qea_dot", "/Secure/enrichment/QeaView.xhtml");

            }

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("plotEnrichmentDotPlot", rse);
        }
    }

    public static String getIsLipid(RConnection RC) {
        try {
            return (RC.eval("GetIsLipids(NA)").asString());
        } catch (Exception e) {
            LOGGER.error("getIsLipid", e);
        }
        return null;
    }

    public static void plotPeaks(SessionBean1 sb, String imgName, String algOpt, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPeaks2Paths(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            if (algOpt.equals("mummichog")) {
                sb.addGraphicsCMD("peaks_to_paths", rCommand);
                sb.addGraphicsMapLink("peaks_to_paths", "/Secure/mummichog/MummiResultView.xhtml");

            } else {
                sb.addGraphicsCMD("peaks_to_paths_gsea", rCommand);
                sb.addGraphicsMapLink("peaks_to_paths_gsea", "/Secure/mummichog/GseaResultView.xhtml");

            }
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("plotPeaks", rse);
        }
    }

    public static void plotPSEAIntegPaths(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPSEAIntegPaths(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "performPeaks2Fun");

            sb.addGraphicsCMD("integ_peaks", rCommand);
            sb.addGraphicsMapLink("integ_peaks", "/Secure/mummichog/IntegMumResultView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("plotIntegPeaks", rse);
        }
    }

    public static double getDefaultPvalCutoff(RConnection RC) {
        try {
            String rCommand = "GetDefaultPvalCutoff(NA)";
            String rCommand2 = "pvalue <- GetDefaultPvalCutoff(NA)";
            RCenter.recordRCommand(RC, rCommand2);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand2, "InitLibrary");

            return RC.eval(rCommand).asDouble();
        } catch (Exception e) {
            LOGGER.error("getDefaultPvalCutoff", e);
        }
        return -1;
    }

    public static String getMummiMSMode(RConnection RC) {
        try {
            String rCommand = "GetMummiMode(NA)";
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "InitLibrary");

            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getMummiMSMode", rse);
            return null;
        }
    }

    public static String getMummiFormat(RConnection RC) {
        try {
            String rCommand = "GetMummiDataType(NA)";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getMummiFormat", rse);
            return null;
        }
    }

    public static void prepareSifDownload(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PrepareSifDownloads(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            LOGGER.error("prepareSifDownload", rse);
        }
    }

    public static String plotQeaMset(SessionBean1 sb, String nm, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotQEA.MetSet(NA" + ", \"" + nm + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("plotQeaMset", rse);
            return null;
        }
    }

    public static int readAdductData(RConnection RC, String adductList) {
        try {
            String rCommand = "Read.AdductData(NA" + ", \"" + adductList + "\",);";
            // for local run, need to get the list from a txt file, not from web interface
            String rcmd1 = "adductListFile<-\"replace_with_your_file_name\"";
            RCenter.recordRCommand(RC, rcmd1);
            String rcmd2 = "adductList<-readChar(adductListFile, file.info(adductListFile)$size)";
            RCenter.recordRCommand(RC, rcmd2);
            String rcmd3 = "Read.AdductData(NA, adductList);";
            RCenter.recordRCommand(RC, rcmd3);
            //System.out.println(rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            LOGGER.error("readAdductData", e);
        }
        return 0;
    }

    public static boolean setupMummichogPvalFromPercent(RConnection RC, double pval) {
        try {
            String rCommand = "SetMummichogPvalFromPercent(NA, " + pval + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("setupMummichogPvalFromPercent", e);
        }
        return false;
    }

    public static boolean setupMummichogPval(RConnection RC, double pval) {
        try {
            String rCommand = "SetMummichogPval(NA, " + pval + ")";
            String rCommand2 = "SetMummichogPval(NA, pvalue)";
            RCenter.recordRCommand(RC, rCommand2);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "performPeaks2Fun");

            return RC.eval(rCommand).asInteger() > 0;
        } catch (Exception e) {
            LOGGER.error("setupMummichogPval", e);
        }
        return false;
    }

    public static boolean convertTableToPeakList(RConnection RC) {
        try {
            String rCommand = "PreparePeakTable4PSEA(NA)";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "performPeaks2Fun");

            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("convertTableToPeakList", e);
        }
        return false;
    }

    public static boolean performMummichog(RConnection RC, String libOpt) {
        try {
            String rCommand = "PerformMummichog(NA, \"" + libOpt + "\",)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("performMummichog", e);
        }
        return false;
    }

    public static boolean performGSEA(RConnection RC, String libOpt) {
        try {
            String rCommand = "PerformGSEA(NA, \"" + libOpt + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("performGSEA", e);
        }
        return false;
    }

    //peak set enrichment analysis (integrate mummichog and GSEA)
    public static boolean performPSEA(RConnection RC, String libOpt, String libVer, int minLib) {
        try {
            //show per num in R command
            String rCommand = "PerformPSEA(NA, \"" + libOpt + "\", \"" + libVer + "\", " + minLib + " , " + 100 + ")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "performPeaks2Fun");

            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception e) {
            LOGGER.error("performPSEA", e);
        }
        return false;
    }

    public static double[][] getMummiMat(RConnection RC) {
        try {
            return RC.eval("GetMummiResMatrix(NA)").asDoubleMatrix();
        } catch (Exception e) {
            LOGGER.error("getMummiMat", e);
        }
        return null;
    }

    public static String[] getMummiPathNames(RConnection RC) {
        try {
            return RC.eval("GetMummiResRowNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getMummiPathNames", e);
        }
        return null;
    }

    public static String[] getMummiKeggIDs(RConnection RC) {
        try {
            return RC.eval("GetMummi.keggIDs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getMummiKeggIDs", e);
        }
        return null;
    }

    public static int doHeatmapMummichogTest(RConnection RC, String nm, String libType, String ids) {
        try {
            String rCommand = "doHeatmapMummichogTest(NA, \"" + nm + "\",\"" + libType + "\", \"" + ids + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());

        } catch (Exception e) {
            LOGGER.error("doHeatmapMummichogTest", e);
        }
        return 0;
    }

    public static int doHeatmapPathwayTest(RConnection RC, String nm, String libType, String ids) {
        try {
            String rCommand = "PerformNetEnrichment(NA, \"" + nm + "\", \"" + libType + "\", \"" + ids + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());

        } catch (Exception e) {
            LOGGER.error("doHeatmapPathwayTest", e);
        }
        return 0;
    }

    public static int createHeatmapJson(RConnection RC, String libOpt, String libVer,
            int minLib, String fileNm, String filt, String version) {
        try {
            String rCommand = "CreateHeatmapJson(NA, \"" + libOpt + "\", \"" + libVer + "\", "
                    + minLib + " , \"" + fileNm + "\", \"" + filt + "\", \"" + version + "\")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "performPeaks2Fun");

            return (RC.eval(rCommand).asInteger());

        } catch (Exception e) {
            LOGGER.error("createHeatmapJson", e);
        }
        return 0;
    }

    public static int createListHeatmapJson(RConnection RC, String libOpt, String libVer, int minLib, String fileNm, String filt, String version) {
        try {
            String rCommand = "CreateListHeatmapJson(NA, \"" + libOpt + "\", \"" + libVer + "\", " + minLib + " ,  \"" + fileNm + "\", \"" + filt + "\", \"" + version + "\")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "performPeaks2Fun");

            return (RC.eval(rCommand).asInteger());

        } catch (Exception e) {
            LOGGER.error("createListHeatmapJson", e);
        }
        return 0;
    }

    public static String getCurrencyMsg(RConnection RC) {
        try {
            return RC.eval("GetCurrencyMsg(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("getCurrencyMsg", e);
        }
        return null;
    }

    public static String getAdductMsg(RConnection RC) {
        try {
            return RC.eval("GetAdductMsg(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("getAdductMsg", e);
        }
        return null;
    }

    public static String getECMsg(RConnection RC) {
        try {
            return RC.eval("GetECMsg(NA").asString();
        } catch (Exception e) {
            LOGGER.error("getECMsg", e);
        }
        return null;
    }

    public static boolean computePathHeatmap(RConnection RC, String libOpt, String fileNm, String type) {
        try {
            String rCommand = "ComputePathHeatmap(NA, \"" + libOpt + "\", \"" + fileNm + "\", \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "paBn_proceed");

            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("computePathHeatmap", e);
        }
        return false;
    }

    public static int getPeakDataLevels(RConnection RC) {
        try {
            String rCommand = "length(levels(mSet$dataSet$cls))";
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("getPeakDataLevels", e);
        }
        return 0;
    }

    public static int checkMumExists(RConnection RC, String type) {
        try {
            String rCommand = "CheckMumExists(NA, \"" + type + "\")";
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("checkMumExists", e);
        }
        return 0;
    }

}
