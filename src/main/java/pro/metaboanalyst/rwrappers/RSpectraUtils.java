/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author qiang
 */
public class RSpectraUtils {

    private static final Logger LOGGER = LogManager.getLogger(RSpectraUtils.class);

    /// 1. Plotting ---- A Bunch of Plotting functions below    
    //  1.1 PLOT TIC of the specific file
    public static String plotSingleTIC(SessionBean1 sb, String fileName, int imageNm, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String imgName;
            String rcmd = "plotSingleTIC(\"" + fileName + "\"," + imageNm + ",\"" + format + "\"," + dpi + "," + width + ")";
            sb.addGraphicsCMD("raw_spec_stic_" + fileName, rcmd);
            sb.addGraphicsMapLink("raw_spec_stic_" + fileName, "/Secure/spectra/SpectraProcess.xhtml");

            imgName = RC.eval(rcmd).asString();
            return imgName;
        } catch (REXPMismatchException | RserveException e) {

        }
        return null;
    }

    // 1.2 PLOT summary of specific feature - MS spec module
    public static String plotMSfeature(SessionBean1 sb, int featureNm, String format, int dpi, String width) {
        /// Note: This function is not using the image count (imageNm) because the fixed format of images at spectral report module
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            RConnection RC = sb.getRConnection();
            //(FeatureNM, format = "png", dpi = 72, width = NA)            
            String featureimageNM = RC.eval("plotMSfeature(" + featureNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return featureimageNM;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotMSfeature", e);
        }
        return null;
    }

    // 1.3 PLOT (Update) EIC of a certain feature
    public static String plotXIC(RConnection RC, int featureNm, String format, int dpi, String width) {
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String featureimageNM2 = RC.eval("PlotXICUpdate(" + featureNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return featureimageNM2;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotXIC", e);
        }
        return null;
    }

    // 1.4 PLOT TIC of all files
    public static String plotTICs(RConnection RC, int imageNm, String format, int dpi, String width) {
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String imageName = RC.eval("plotTICs(" + imageNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return imageName;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotTICs", e);
        }
        return null;
    }

    // 1.5 PLOT BPI of all files
    public static String plotBPIs(RConnection RC, int imageNm, String format, int dpi, String width) {
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String imageName = RC.eval("plotBPIs(" + imageNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return imageName;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotBPIs", e);
        }
        return null;
    }

    // 1.6 PLOT Result of Retention Time correction of all files
    public static String plotRTcor(RConnection RC, int imageNm, String format, int dpi, String width) {
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String imageName = RC.eval("PlotSpectraRTadj(" + imageNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return imageName;
        } catch (Exception e) {
            LOGGER.error("plotRTcor", e);
        }
        return null;
    }

    // 1.7 PLOT Corrected BPI of all files
    public static String plotBPIcor(RConnection RC, int imageNm, String format, int dpi, String width) {
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String imageName = RC.eval("PlotSpectraBPIadj(" + imageNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return imageName;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotBPIcor", e);
        }
        return null;
    }

    public static String plotIntenStats(RConnection RC, int imageNm, String format, int dpi, String width) {
        try {
            if (!"png".equals(format)) {
                dpi = 72;
            }
            String imageName = RC.eval("PlotSpectraInsensityStatics(" + imageNm + ",\"" + format + "\"," + dpi + "," + width + ")").asString();
            return imageName;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotIntenStats", e);
        }
        return null;
    }

    //2. Utilities
    //2.1 ID convert (mz@rt --> FTID)
    public static int mzrt2ID(RConnection RC, String mzrt) {
        try {

            int FTID = RC.eval("mzrt2ID(\"" + mzrt + "\")").asInteger();
            return FTID;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("mzrt2ID", e);
        }
        return 0;
    }

    public static int mzrt2ID2(RConnection RC, String mzrt) {
        try {

            int FTID = RC.eval("mzrt2ID2(\"" + mzrt + "\")").asInteger();
            return FTID;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("mzrt2ID2", e);
        }
        return 0;
    }

    public static int convertFTno2Num(RConnection RC, int FTno) {
        try {

            int FTnum = RC.eval("FTno2Num(" + FTno + ")").asInteger();
            return FTnum;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("FTno2Num", e);
        }
        return 0;
    }

    public static String[] readAdductList(RConnection RC, String polarity) {

        try {
            String[] AdductList = RC.eval("readAdductsList(\"" + polarity + "\")").asStrings();
            return AdductList;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("readAdductList", e);
        }

        return null;
    }

    public static String retrieveModeInfo(RConnection RC) {

        try {
            String modeInfo = RC.eval("retrieveModeInfo()").asString();
            return modeInfo;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("retrieveModeInfo", e);
            return null;
        }

    }

    //triggered by clicking on data point in box plot (3d pca)
    public static String plotSingleXIC(SessionBean1 sb, String sample, String feature, boolean showlabel) {
        String showl = "FALSE";
        RConnection RC = sb.getRConnection();
        try {
            if (showlabel) {
                showl = "TRUE";
            }
            String imgName;
            //System.out.println("plotSingleXIC(NA" + ", " + sample + ",\"" + feature + "\")");
            String rcmd = "plotSingleXIC_pro(NA" + ", " + sample + ",\"" + feature + "\", showlabel = " + showl + ", format = \"png\")";
            sb.addGraphicsCMD("raw_spec_sxic_" + sample, rcmd);
            sb.addGraphicsMapLink("raw_spec_sxic_" + sample, "/Secure/spectra/SpectraProcess.xhtml");

            imgName = RC.eval("plotSingleXIC(NA" + ", " + sample + ",\"" + feature + "\", showlabel = " + showl + ")").asString();
            return imgName;
        } catch (REXPMismatchException | RserveException e) {
            LOGGER.error("plotSingleXIC", e);
        }
        return null;
    }

    public static void cleanEIClayer(RConnection RC, int featureNum) {
        try {
            RC.voidEval("cleanEICLayer(" + featureNum + ");");
        } catch (Exception e) {
            LOGGER.error("cleanEICLayer", e);

        }
    }

    public static int getMalariaRawData(RConnection RC, String homedir) {
        try {
            int res = RC.eval("getMalariaRawData(\"" + homedir + "\");").asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("getMalariaRawData", e);
            return 0;
        }
    }

    public static int getCOVIDRawData(RConnection RC, String homedir) {
        try {
            int res = RC.eval("getCOVIDRawData(\"" + homedir + "\");").asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("getCOVIDRawData", e);
            return 0;
        }
    }

    public static int getBloodRawData(RConnection RC, String homedir) {
        try {
            int res = RC.eval("getBloodRawData(\"" + homedir + "\");").asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("getBloodRawData", e);
            return 0;
        }
    }

    public static String[] getResSummaryMsg(RConnection RC) {
        try {
            return RC.eval("PerformResultSummary(mSet = NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getResSummaryMsg", e);
        }
        return null;
    }

    public static String extractHMDBCMPD(RConnection RC, String formula, int featureOrder) {
        //PerformExtractHMDBCMPD
        try {
            return RC.eval("PerformExtractHMDBCMPD(formula =\"" + formula + "\", featureOrder = " + featureOrder + ")").asString();
        } catch (Exception e) {
            LOGGER.error("extractHMDBCMPD", e);
        }
        return null;
    }

    public static boolean ensureDataExits(RConnection RC, String fileNM) {
        try {
            int res = RC.eval("checkdataexits(fileNM =\"" + fileNM + "\")").asInteger();
            if (res == 1) {
                return true;
            }
        } catch (Exception e) {
            LOGGER.error("ensureDataExits", e);
        }
        return false;
    }

    public static boolean generateParamFile(RConnection RC) {
        try {
            int res = RC.eval("GenerateParamFile()").asInteger();
            if (res == 1) {
                return true;
            }
        } catch (Exception e) {
            LOGGER.error("GenerateParamFile", e);
        }
        return false;
    }

    public static boolean processMSMSuploadSingleSpec(RConnection RC, String spectrum) {

        try {
            String rCommand = "processMSMSupload(NA, \"" + spectrum + "\")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            if (res == 1) {
                return true;
            }
        } catch (Exception e) {
            LOGGER.error("processMSMSupload", e);
        }
        return false;
    }

    public static int setMS2DBOpt(RConnection RC, String option) {

        try {
            String rCommand = "setMS2DBOpt(NA, \"" + option + "\")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("setMS2DBOpt", e);
        }
        return 0;
    }

    public static boolean performMS2searchSingle(RConnection RC, double ppm1, double ppm2, String frgdb_path, String db_path, String database,
            int similarity_meth, double precMZ, double sim_cutoff, String ionMode, String unit1, String unit2) {

        try {
            String rCommand = "performMS2searchSingle(NA, " + ppm1 + "," + ppm2 + ",\"" + db_path + "\", \"" + frgdb_path + "\", \"" + database + "\", "
                    + similarity_meth + "," + precMZ + ", " + sim_cutoff
                    + ",\"" + ionMode + "\",\"" + unit1 + "\",\"" + unit2 + "\")";
            String rCommand2 = "performMS2searchSingle(NA, " + ppm1 + "," + ppm2 + ",\"YOUR_MSMS_DATABASE\", \"YOUR_FRAGMENTATION_DATABASE\", \"" + database + "\", "
                    + similarity_meth + "," + precMZ + ", " + sim_cutoff
                    + ",\"" + ionMode + "\",\"" + unit1 + "\",\"" + unit2 + "\")";
            RCenter.recordRCommand(RC, rCommand2);
            int res = RC.eval(rCommand).asInteger();
            if (res == 1) {
                return true;
            }
        } catch (Exception e) {
            LOGGER.error("performMS2searchSingle", e);
        }
        return false;
    }

    public static boolean performMS2searchBatch(RConnection RC, double ppm1, double ppm2, String db_path, String frgDB_path, String database,
            int similarity_meth, double precMZ, double sim_cutoff, String ionMode, String unit1, String unit2, int ncores) {

        try {
            String rCommand = "performMS2searchBatch(NA, " + ppm1 + "," + ppm2 + ",\"" + db_path + "\", \"" + frgDB_path + "\", \"" + database + "\", "
                    + similarity_meth + "," + precMZ + ", " + sim_cutoff
                    + ",\"" + ionMode + "\",\"" + unit1 + "\",\"" + unit2 + "\", " + ncores + ")";
            String rCommand2 = "performMS2searchBatch(NA, " + ppm1 + "," + ppm2 + ",\"YOUR_MSMS_DATABASE\", \"YOUR_FRAGMENTATION_DATABASE\", \"" + database + "\", "
                    + similarity_meth + "," + precMZ + ", " + sim_cutoff
                    + ",\"" + ionMode + "\",\"" + unit1 + "\",\"" + unit2 + "\", " + ncores + ")";
            RCenter.recordRCommand(RC, rCommand2);
            int res = RC.eval(rCommand).asInteger();
            if (res == 1) {
                return true;
            }
        } catch (Exception e) {
            LOGGER.error("performMS2searchBatch", e);
        }
        return false;
    }

    public static int PlotMS2SummarySingle(RConnection RC, String Opt, String imagenm, int dpi, String format, String width, String height) {
        try {
            String rCommand = "PlotMS2SummarySingle(NA, \"" + imagenm + "\", " + Opt + ", " + dpi + ", \"" + format + "\", " + width + ", " + height + ")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("PlotMS2SummarySingle", e);
            return 0;
        }
    }

    public static String[] GetMSMSCompoundNames_single(RConnection RC, int idx) {
        try {
            String[] res = RC.eval("GetMSMSCompoundNames_single(NA," + idx + ")").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSCompoundNames_single", e);
        }
        return null;
    }

    public static String[] GetMSMSFormulas_single(RConnection RC, int idx) {
        try {
            String[] res = RC.eval("GetMSMSFormulas_single(NA," + idx + ")").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSFormulas_single", e);
        }
        return null;
    }

    public static String[] GetMSMSSmiles_single(RConnection RC, int idx) {
        try {
            String[] res = RC.eval("GetMSMSSmiles_single(NA," + idx + ")").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSSmiles_single", e);
        }
        return null;
    }

    public static String[] GetMSMSInchiKeys_single(RConnection RC, int idx) {
        try {
            String[] res = RC.eval("GetMSMSInchiKeys_single(NA," + idx + ")").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSInchiKeys_single", e);
        }
        return null;
    }

    public static double[] GetMSMSSimScores_single(RConnection RC, int idx) {
        try {
            double[] res = RC.eval("GetMSMSSimScores_single(NA," + idx + ")").asDoubles();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSSmiles_single", e);
        }
        return null;
    }

    public static double[] GetMSMSDot_single(RConnection RC, int idx) {
        try {
            double[] res = RC.eval("GetMSMSDot_single(NA," + idx + ")").asDoubles();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSDot_single", e);
        }
        return null;
    }

    public static double[] GetMSMSPrecs_single(RConnection RC, int idx) {
        try {
            double[] res = RC.eval("GetMSMSPrecs_single(NA," + idx + ")").asDoubles();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSPrecs_single", e);
        }
        return null;
    }

    public static double[] GetMSMSPrecMZvec_msp(RConnection RC) {
        try {
            double[] res = RC.eval("GetMSMSPrecMZvec_msp(NA)").asDoubles();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSPrecMZvec_msp", e);
        }
        return null;
    }

    public static int plotMirror(RConnection RC, int featureidx, double precMZ,
            double ppm, String imageNM,
            double dpi, String format, int width, int height) {
        try {
            String rCommand = "plotMirror(NA, " + featureidx + "," + precMZ
                    + ", " + ppm + ", \"" + imageNM + "\", " + dpi + ", \"" + format + "\", " + width
                    + ", " + height + ")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("plotMirror", e);
        }
        return 0;
    }

    public static int PerformMirrorPlottingWeb(RConnection RC, String fragDB_path, String featurelabel, int result_num, int sub_idx,
            double ppm, String imageNM,
            double dpi, String format, int width, int height) {
        try {
            int res = RC.eval("PerformMirrorPlottingWeb(NA, \"" + fragDB_path + "\", \"" + featurelabel + "\"," + result_num
                    + ", " + sub_idx + ", " + ppm + ", \"" + imageNM + "\", " + dpi + ", \"" + format + "\", " + width
                    + ", " + height + ")").asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("PerformMirrorPlottingWeb", e);
        }
        return 0;
    }

    public static int SaintyCheckMSPfile(RConnection RC, String filename, String format_type) {
        try {
            String rCommand = "SaintyCheckMSPfile(NA, " + "\"" + filename + "\", \"" + format_type + "\", TRUE)";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("SaintyCheckMSPfile", e);
        }
        return 0;
    }

    public static int FetchExampleMSP(RConnection RC, String URL) {
        try {
            int res = RC.eval("FetchExampleMSP(\"" + URL + "\")").asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("FetchExampleMSP", e);
        }
        return 0;
    }

    public static String[] GetMSPSanityCheckMsg(RConnection RC) {
        try {
            String[] res = RC.eval("GetMSPSanityCheckMsg(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSPSanityCheckMsg", e);
        }
        return null;
    }

    public static String[] GetAllPrecMZs(RConnection RC) {
        try {
            String[] res = RC.eval("GetAllPrecMZs(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetAllPrecMZs", e);
        }
        return null;
    }

    public static String[] GetIncludedPrecMZs(RConnection RC) {
        try {
            String[] res = RC.eval("GetIncludedPrecMZs(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetIncludedPrecMZs", e);
        }
        return null;
    }

    public static String[] GetNonIncludedPrecMZs(RConnection RC) {
        try {
            String[] res = RC.eval("GetNonIncludedPrecMZs(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetNonIncludedPrecMZs", e);
        }
        return null;
    }

    public static String[] GetAllPrecMZRTs(RConnection RC) {
        try {
            String[] res = RC.eval("GetAllPrecMZRTs(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetAllPrecMZRTs", e);
        }
        return null;
    }

    public static String[] GetIncludedPrecMZRTs(RConnection RC) {
        try {
            String[] res = RC.eval("GetIncludedPrecMZRTs(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetIncludedPrecMZRTs", e);
        }
        return null;
    }

    public static String[] GetNonIncludedPrecMZRTs(RConnection RC) {
        try {
            String[] res = RC.eval("GetNonIncludedPrecMZRTs(NA)").asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetNonIncludedPrecMZRTs", e);
        }
        return null;
    }

    // GetCountsOfIncludedIons
    public static int GetCountsOfIncludedIons(RConnection RC) {
        try {
            String rCommand = "GetCountsOfIncludedIons(NA)";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetCountsOfIncludedIons", e);
        }
        return 0;
    }

    // DataUpdatefromInclusionList
    public static int DataUpdatefromInclusionList(RConnection RC, String included_str) {
        try {
            String rCommand = "DataUpdatefromInclusionListPro(NA, \"" + included_str + "\")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("DataUpdatefromInclusionList", e);
        }
        return 0;
    }

    public static int[] SummarizeCMPDResults(RConnection RC, double ucutoff, double lcutoff) {
        try {
            int[] res = RC.eval("SummarizeCMPDResults(NA, " + ucutoff + "," + lcutoff + ")").asIntegers();
            return res;
        } catch (Exception e) {
            LOGGER.error("SummarizeCMPDResults", e);
        }
        return null;
    }

    public static int Setcurrentmsmsidx(RConnection RC, int idx) {
        try {
            String rCommand = "Setcurrentmsmsidx(NA, " + idx + ")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("Setcurrentmsmsidx", e);
        }
        return 0;
    }

    //GetMSMSFeatureLabel
    public static String GetMSMSFeatureLabel(RConnection RC, int idx) {
        try {
            String res = RC.eval("GetMSMSFeatureLabel(NA, " + idx + ")").asString();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetMSMSFeatureLabel", e);
        }
        return null;
    }

    // Updatecurrentmsmsidx
    public static int Updatecurrentmsmsidx(RConnection RC, String label) {
        try {
            String rCommand = "Updatecurrentmsmsidx(NA,\"" + label + "\")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("Updatecurrentmsmsidx", e);
        }
        return 0;
    }

    // PerformSWATHDesignDetection
    public static int PerformSWATHDesignDetection(RConnection RC, String file) {
        try {
            String rCommand = "PerformSWATHDesignDetection(NA,\"" + file + "\")";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "prepareDIASpec");

            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("PerformSWATHDesignDetection", e);
        }
        return 0;
    }

    public static double[] GetSWATHDesginLow(RConnection RC) {
        try {
            double[] res = RC.eval("GetSWATHDesginLow(NA)").asDoubles();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetSWATHDesginLow", e);
        }
        return null;
    }

    public static double[] GetSWATHDesginUp(RConnection RC) {
        try {
            double[] res = RC.eval("GetSWATHDesginUp(NA)").asDoubles();
            return res;
        } catch (Exception e) {
            LOGGER.error("GetSWATHDesginUp", e);
        }
        return null;
    }

    //exportSwathDesign
    public static int exportSwathDesign(RConnection RC, String lowmzs, String upmzs) {
        try {
            int res = RC.eval("exportSwathDesign(" + lowmzs + ", " + upmzs + ")").asInteger();
            return res;
        } catch (Exception e) {
            LOGGER.error("exportSwathDesign", e);
        }
        return 0;
    }

    //saveCurrentSession
    public static void saveCurrentSession(RConnection RC, String path) {
        try {
            RC.voidEval("saveCurrentSession(\"" + path + "\")");
        } catch (Exception e) {
            LOGGER.error("saveCurrentSession", e);
        }
    }

    //saveParams4Processing
    public static void saveParams4Processing(RConnection RC, double ppm_val1, double ppm_val2,
            String database_path, String fragmentDB_pth,
            String msmsDBOpt, int simlarity_meth, double precMZ,
            double simi_cutoff, String ionMode, String unit1, String unit2, String Path) {
        try {
            RC.voidEval("saveParams4Processing(" + ppm_val1 + ","
                    + ppm_val2 + ", \""
                    + database_path + "\",\""
                    + fragmentDB_pth + "\",\""
                    + msmsDBOpt + "\", "
                    + simlarity_meth + ", "
                    + precMZ + ", "
                    + simi_cutoff + ", \""
                    + ionMode + "\", \""
                    + unit1 + "\", \""
                    + unit2 + "\", \""
                    + Path + "\")");
        } catch (Exception e) {
            LOGGER.error("saveParams4Processing", e);
        }
    }

    public static void cancelMS2Job(RConnection RC, String Path) {
        System.out.println("Cancel this MS2 job!");
        try {
            RC.voidEval("cancelMS2Job(\"" + Path + "\")");
        } catch (Exception e) {
            LOGGER.error("cancelMS2Job", e);
        }
    }

    //createSLURMBash
    public static void createSLURMBash(RConnection RC, String path) {
        try {
            RC.voidEval("createSLURMBash(\"" + path + "\")");
        } catch (Exception e) {
            LOGGER.error("createSLURMBash", e);
        }
    }

    public static int readProgressSec(RConnection RC, String path) {
        try {
            int progress = RC.eval("readProgressSec(\"" + path + "\")").asInteger();
            return progress;
        } catch (Exception e) {
            LOGGER.error("readProgressSec", e);
        }
        return 0;
    }

    // loadMS2ResultmSet
    public static void loadMS2ResultmSet(RConnection RC) {
        try {
            RC.voidEval("loadMS2ResultmSet()");
        } catch (Exception e) {
            LOGGER.error("loadMS2ResultmSet", e);
        }
    }

    public static boolean validateGoogleDriveURL(RConnection RC, String url) {

        try {
            String rCommand = "ValidateGoogleDriveURL(\"" + url + "\");";
            RCenter.recordRCommand(RC, rCommand);
            boolean res = RC.eval(rCommand).asInteger() == 1;
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("validateGoogleDriveURL 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("validateGoogleDriveURL 2 ", ex);
        }

        return false;
    }

    public static int ExtractGoogleDriveURL(RConnection RC, String url) {

        try {
            String rCommand = "ExtractGoogleDriveURL(\"" + url + "\");";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("ExtractGoogleDriveURL 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("ExtractGoogleDriveURL 2 ", ex);
        }

        return 0;
    }

    public static int CheckMetadataMatching(RConnection RC) {
        try {
            String rCommand = "CheckMetadataMatching(NA);";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("CheckMetadataMatching 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("CheckMetadataMatching 2 ", ex);
        }
        return 0;
    }

    public static String GetMissingFiles(RConnection RC) {
        try {
            String rCommand = "GetMissingFiles(NA);";
            RCenter.recordRCommand(RC, rCommand);
            String res = RC.eval(rCommand).asString();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("GetMissingFiles 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("GetMissingFiles 2 ", ex);
        }
        return "Metadata not mached to the spectra files detected";
    }

    public static boolean GetFileTotalSizeBool(RConnection RC) {
        try {
            String rCommand = "GetFileTotalSizeBool(NA);";
            RCenter.recordRCommand(RC, rCommand);
            boolean res = RC.eval(rCommand).asInteger() == 1;
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("GetFileTotalSizeBool 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("GetFileTotalSizeBool 2 ", ex);
        }
        return false;
    }

    public static String[] GetAllSpectraFiles(RConnection RC) {
        try {
            String rCommand = "GetAllSpectraFiles(NA);";
            RCenter.recordRCommand(RC, rCommand);
            String[] res = RC.eval(rCommand).asStrings();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("GetAllSpectraFiles 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("GetAllSpectraFiles 2 ", ex);
        }
        return null;
    }

    //GetAllSpectraGroups
    public static String[] GetAllSpectraGroups(RConnection RC) {
        try {
            String rCommand = "GetAllSpectraGroups(NA);";
            RCenter.recordRCommand(RC, rCommand);
            String[] res = RC.eval(rCommand).asStrings();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("GetAllSpectraGroups 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("GetAllSpectraGroups 2 ", ex);
        }
        return null;
    }

    //GetSampleNMsofGroup
    public static String[] GetSampleNMsofGroup(RConnection RC, String group_nm) {
        try {
            String rCommand = "GetSampleNMsofGroup(NA, \"" + group_nm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            String[] res = RC.eval(rCommand).asStrings();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("GetSampleNMsofGroup 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("GetSampleNMsofGroup 2 ", ex);
        }
        return null;
    }

    //GetFileSizesofSpectra
    public static double GetFileSizesofSpectra(RConnection RC, String spectra_nm) {
        try {
            String rCommand = "GetFileSizesofSpectra(NA, \"" + spectra_nm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            double res = RC.eval(rCommand).asDouble();
            return res;
        } catch (REXPMismatchException ex) {
            LOGGER.error("GetFileSizesofSpectra 1 ", ex);
        } catch (RserveException ex) {
            LOGGER.error("GetFileSizesofSpectra 2 ", ex);
        }
        return 0;
    }

    // Download the smallest MS2 example
    public static void DownloadAnMS2File(RConnection RC) {
        try {
            RC.voidEval("DownloadAnMS2File(NA)");
        } catch (Exception e) {
            LOGGER.error("DownloadAnMS2File", e);
        }
    }

}
