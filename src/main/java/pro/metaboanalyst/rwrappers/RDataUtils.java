/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import java.io.File;
import java.util.List;
import java.util.StringTokenizer;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.models.ComponentBean;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.SampleBean;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.RList;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.rosuda.REngine.REXP;

/**
 * Create a dataSet list object mSet$dataSet$cls : class information
 * mSet$dataSet$orig : original data mSet$dataSet$msg : message when processing
 * raw data dataSet$status : boolean value indicates whether acceptable or not
 * dataSet$proc: processed data mSet$dataSet$norm : normalized data
 *
 * @author Jeff
 */
public class RDataUtils {

    private static final Logger LOGGER = LogManager.getLogger(RDataUtils.class);

    /*
     * data type: list, conc, specbin, pktable, nmrpeak, mspeak, msspec
     * anal type: stat, pathora, pathqea, msetora, msetssp, msetqea, map, peaksearch
     *
     * */
    public static boolean initDataObjects(RConnection RC, String dataType, String analType, boolean isPaired) {
        try {
            String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ", 150)";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (RserveException rse) {
            LOGGER.error("initDataObjects", rse);
            return false;
        }
    }

    public static boolean initDataObjectsMulti(RConnection RC, String dataType, String[] analTypeArr, boolean isPaired) {
        try {
            String analType = "multi";
            RC.assign("moduleNms.vec", analTypeArr);

            String rCommand = "InitDataObjectsMulti(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (RserveException rse) {
            LOGGER.error("initDataObjects", rse);
            return false;
        } catch (REngineException ex) {
            java.util.logging.Logger.getLogger(RDataUtils.class.getName()).log(Level.SEVERE, null, ex);
            return false;
        }
    }

    public static boolean prepareSpec4Switch(RConnection RC) {
        try {
            String rCommand = "PrepareSpec4Switch()";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (RserveException rse) {
            LOGGER.error("prepareSpec4Switch", rse);
            return false;
        }
    }

    public static boolean updateDataObjects(RConnection RC, String dataType, String analType, boolean isPaired) {
        try {
            String rCommand = "UpdateDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (RserveException rse) {
            LOGGER.error("updateDataObjects", rse);
            return false;
        }
    }

    // Used to initialize a plan for raw data processing procedures
    public static boolean initDataPlan(RConnection RC) {
        try {

            String rCommand = "plan <<- InitializaPlan()";
            RC.voidEval(rCommand);
            // RCenter.recordRCommand(RC, rCommand); // Don't do the record here!
            return true;

        } catch (RserveException rse) {
            LOGGER.error("initDataPlan", rse);
            return false;
        }
    }

    // Used to initialize a plan for raw data processing procedures
    public static boolean setPrimaryUser(RConnection RC) {
        try {

            String rCommand = "primary.user <<- TRUE";
            RC.voidEval(rCommand);
            // RCenter.recordRCommand(RC, rCommand); // Don't do the record here!
            return true;

        } catch (RserveException rse) {
            LOGGER.error("setPrimaryUser", rse);
            return false;
        }
    }

    // Used to define the plan for raw data processing procedures
    public static boolean planProcessDefine(RConnection RC, String usrNm, String fileNms, String meth, ApplicationBean1 ab, String homedir) {

        String rCommandOpt, rCommandProcess, rCommandEXC, rCommandFileNm;
        if ("auto".equals(meth)) {
            rCommandOpt = "plan <- running.plan(plan,\n"
                    + "data_folder_QC <- \"upload/QC/\",\n"
                    + "mSet <- PerformROIExtraction(data_folder_QC, rt.idx = 0.9, plot = F, running.controller=rc),\n"
                    + "param_initial <- SetPeakParam(),\n"
                    + "best_parameters <- PerformParamsOptimization(mSet, param = param_initial, ncore = 8,running.controller=rc),\n"
                    + "data_folder_Sample <- \"upload/\",\n"
                    + "plotSettings1 <- SetPlotParam(Plot=TRUE),\n"
                    + "param <- best_parameters,\n"
                    + "mSet <- ImportRawMSData(path = data_folder_Sample,plotSettings = plotSettings1,running.controller=rc),\n"
                    + "mSet <- PerformPeakProfiling(mSet, Params = param, plotSettings = plotSettings1,running.controller=rc),\n"
                    + "annParams <- SetAnnotationParam(polarity = \"negative\", mz_abs_add = 0.015),\n"
                    + "mSet <- PerformPeakAnnotation(mSet, annParams,running.controller=rc),\n"
                    + "mSet <- FormatPeakList(mSet, annParams, filtIso =F, filtAdducts = FALSE, missPercent = 1))";

            rCommandProcess = "opt";

        } else { //"customized"
            rCommandOpt = "plan <- running.plan(plan,\n"
                    + "data_folder_Sample <- \"upload/\",\n"
                    + "param <- SetPeakParam(),\n"
                    + "plotSettings1 <- SetPlotParam(Plot=TRUE),\n"
                    + "mSet <- ImportRawMSData(path = data_folder_Sample, plotSettings = plotSettings1, running.controller=rc),\n"
                    + "mSet <- PerformPeakProfiling(mSet, param, plotSettings = plotSettings1, running.controller=rc),\n"
                    + "annParams <- SetAnnotationParam(polarity = \"negative\", mz_abs_add = 0.015),\n"
                    + "mSet <- PerformPeakAnnotation(mSet, annParams, running.controller=rc),\n"
                    + "mSet <- FormatPeakList(mSet, annParams, filtIso =F, filtAdducts = FALSE, missPercent = 1))";

            rCommandProcess = "default";

        }

        rCommandEXC = "ExecutePlan(plan)";
        rCommandFileNm = "rawfilenms <<-" + fileNms;

        if (ab.shouldUseScheduler()) {
            RDataUtils.createScriptForScheduler(RC, usrNm, rCommandOpt, rCommandProcess, fileNms, ab.getRscriptsHomePath());
        } else {
            String bashPath = RCenter.getBashFullPath(RC);
            String rScriptHome = ab.getRscriptsHomePath();
            DataUtils.runRawSpecScript(RC, rScriptHome, bashPath, homedir, usrNm, rCommandOpt, rCommandProcess, fileNms);
        }

        return true;
    }

    // Detecting whether the paramters have changed or not
    public static int paramChangingDetection(RConnection RC, String param0_path, String usrNm) throws REXPMismatchException {
        try {
            String rCommand = "verifyParam(\'" + param0_path + "\', \'" + usrNm + "\')";
            //RCenter.recordRCommand(RC, rCommand);
            //System.out.println(rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("paramChangingDetection", e);
        }
        return 0;
    }

    public static int demoPlanProcessDefine(RConnection RC, RConnection RC2, String usrNm, String fileNms, String meth, ApplicationBean1 ab, String homedir, boolean controller) throws REXPMismatchException {
        String fileusrNM = "c(\"QC_PREFA02.mzML\",\"QC_PREFB02.mzML\",\"CD-9WOBP.mzML\",\"CD-6KUCT.mzML\",\"CD-77FXR.mzML\",\"CD-9OS5Y.mzML\",\"HC-9SNJ4.mzML\",\"HC-AMR37.mzML\",\"HC-9X47O.mzML\",\"HC-AUP8B.mzML\")";

        //System.out.println(meth);
        int pid = -1;
        if (fileNms.length() == fileusrNM.length() && "auto".equals(meth) && controller) {

            String source_path = ab.getAutoCache();
            DataUtils.copyDir(source_path, homedir);

            try {
                // RDataUtils.createScriptForScheduler(RC, usrNm, rCommandOpt, rCommandProcess, fileNms);

                String rCommand = "FastRunningShow_auto(\"" + homedir + "\")";
                //System.out.println(rCommand);
                pid = RC2.eval("Sys.getpid()").asInteger();
                RC2.voidEvalDetach(rCommand);

            } catch (RserveException ex) {
                LOGGER.error("demoPlanProcessDefine-auto", ex);
            }

            return pid;

        } else if (fileusrNM.length() == fileNms.length() && "customized".equals(meth) && controller) {

            String source_path = ab.getCustomizedCache();
            DataUtils.copyDir(source_path, homedir);

            try {
                // RDataUtils.createScriptForScheduler(RC, usrNm, rCommandOpt, rCommandProcess, fileNms);

                String rCommand = "FastRunningShow_customized(\"" + homedir + "\")";
                //System.out.println(rCommand);
                pid = RC2.eval("Sys.getpid()").asInteger();
                RC2.voidEvalDetach(rCommand);

            } catch (RserveException ex) {
                //Logger.getLogger(RDataUtils.class.getName()).log(Level.SEVERE, null, ex);
                LOGGER.error("demoPlanProcessDefine-custom", ex);
            }

            return pid;

        } else { // run the customized/auto case when parameters changed! & Deal with unexpectation !

            //System.out.println("Real run for example data, why?");
            if (!controller || fileusrNM.length() != fileNms.length()) {
                //System.out.println("Because parameters changed !");

                // Copy all the caches first 
                String source_path;
                if ("customized".equals(meth)) {
                    source_path = ab.getCustomizedCache();
                } else if ("auto".equals(meth)) {
                    source_path = ab.getAutoCache();
                } else {
                    source_path = null;
                }

                DataUtils.copyDir(source_path, homedir);

            } else {
                // System.out.println("Because unexpectation happened !");
            }
            RDataUtils.planProcessDefine(RC, usrNm, fileNms, meth, ab, homedir);

            return 0;
        }
    }

    public static boolean updateParams(RConnection RC, String PKmeth, String RTmeth, String polarity,
            double ppm, double min_peakwidth,
            double max_peakwidth, double mzdiff,
            double snthresh, double noise, double prefilter,
            double value_prefilter, double bw, double minFranction,
            double minSamples, double Maxfeatures, double integrate,
            double extra, double span, double profstep, double fwhm,
            double sigma, double steps, double max,
            double perc_fwhm, double mz_abs_iso, double max_charge, double max_iso,
            double corr_eic_th, double mz_abs_add,
            String adducts,
            boolean rmConts,
            boolean rmblanks) {

        // Convert boolean as String for R
        String rmConts_String;
        if (rmConts) {
            rmConts_String = "TRUE";
        } else {
            rmConts_String = "FALSE";
        }

        String rmblanks_string;
        if (rmblanks) {
            rmblanks_string = "TRUE";
        } else {
            rmblanks_string = "FALSE";
        }

        String rCommand = "SetPeakParam(Peak_method = \"" + PKmeth + "\", "
                + "mzdiff = " + mzdiff + ","
                + "snthresh = " + snthresh + ","
                + "bw = " + bw + ","
                + "ppm = " + ppm + ","
                + "min_peakwidth = " + min_peakwidth + ","
                + "max_peakwidth = " + max_peakwidth + ","
                + "noise = " + noise + ","
                + "prefilter = " + prefilter + ","
                + "value_of_prefilter = " + value_prefilter + ","
                + "fwhm = " + fwhm + ","
                + "steps = " + steps + ","
                + "sigma = " + sigma + ","
                + "profStep = " + profstep + ","
                + "minFraction = " + minFranction + ","
                + "minSamples = " + minSamples + ","
                + "maxFeatures = " + Maxfeatures + ","
                + "max = " + max + ","
                + "extra = " + extra + ","
                + "span = " + span + ","
                + "integrate = " + integrate + ","
                + "RT_method = \"" + RTmeth + "\","
                + "polarity = \"" + polarity + "\","
                + "perc_fwhm = " + perc_fwhm + ","
                + "mz_abs_iso = " + mz_abs_iso + ","
                + "max_charge = " + max_charge + ","
                + "max_iso = " + max_iso + ","
                + "corr_eic_th = " + corr_eic_th + ","
                + "mz_abs_add = " + mz_abs_add + ","
                + "adducts = \"" + adducts + "\","
                + "rmConts = " + rmConts_String + ", "
                + "BlankSub = " + rmblanks_string + ")";

        try {
            RC.voidEval(rCommand);
        } catch (RserveException ex) {
            // Logger.getLogger(RDataUtils.class.getName()).log(Level.SEVERE, null, ex);
            LOGGER.error("updateParams", ex);
        }
        RCenter.recordRCommand(RC, rCommand);

        return false;
    }

    // used to check the centriod or not
    public static boolean checkCentroid(RConnection RC, String filepath) throws REXPMismatchException {
        try {
            String rCommand = "CentroidCheck(\'" + filepath + "\')";
            //RCenter.recordRCommand(RC, rCommand);

            return RC.eval(rCommand).asString().equals("TRUE");
        } catch (RserveException rse) {
            LOGGER.error("checkCentroid", rse);
            return false;
        }
    }

    public static int createScriptForScheduler(RConnection RC, String guestName, String planString, String planString2, String fileNms, String source_path) {
        try {
            planString = planString.replace("\"", "\'");
            //planString2 = planString2.replace("\"", "\'");
            fileNms = fileNms.replace("\"", "\'");
            String rCommand = "CreateRawRscriptPro(\"" + guestName + "\", \"" + planString + "\", \"" + planString2 + "\",\"" + fileNms + "\",\"" + source_path + "\")";

            /* TODO: OUTPUT REAL R code*/
            if ("".equals(planString2)) {

                String rCommand2 = "data_folder_Sample <- \"REPLACE WITH YOUR Data folder/\"";
                String rCommand3 = "param <- SetPeakParam(\"REPLACE WITH YOUR PARAM SETTINGS\")";
                String rCommand4 = "plotSettings1 <- SetPlotParam(Plot=T)";
                String rCommand5 = "plotSettings2 <- SetPlotParam(Plot=T)";
                String rCommand6 = "rawData <- ImportRawMSData(data_folder_Sample,plotSettings = plotSettings1)";
                String rCommand7 = "mSet <- PerformPeakProfiling(rawData,param,plotSettings = plotSettings2)";
                String rCommand8 = "annParams <- SetAnnotationParam(\"REPLACE WITH YOUR PARAM SETTINGS\")";
                String rCommand9 = "annotPeaks <- PerformPeakAnnotation(mSet, annParams)";
                String rCommand10 = "maPeaks <- FormatPeakList(annotPeaks, annParams, filtIso =F, filtAdducts = FALSE,missPercent = 1)";

                //RCenter.recordRCommand(RC, rCommand1);
                RCenter.recordRCommand(RC, rCommand2);
                RCenter.recordRCommand(RC, rCommand3);
                RCenter.recordRCommand(RC, rCommand4);
                RCenter.recordRCommand(RC, rCommand5);
                RCenter.recordRCommand(RC, rCommand6);
                RCenter.recordRCommand(RC, rCommand7);
                RCenter.recordRCommand(RC, rCommand8);
                RCenter.recordRCommand(RC, rCommand9);
                RCenter.recordRCommand(RC, rCommand10);

            } else {

                String rCommand02 = "data_folder_QC <- \"REPLACE WITH YOUR QC Data folder/\"";
                String rCommand04 = "raw_data <- PerformDataTrimming(data_folder_QC,rt.idx = 0.9,plot = F)";
                String rCommand05 = "param_initial <- SetPeakParam(\"REPLACE WITH YOUR PARAM SETTINGS\")";
                String rCommand06 = "param_optimized <- PerformParamsOptimization(raw_data, param = param_initial, ncore = 8)";

                String rCommand2 = "data_folder_Sample <- \"REPLACE WITH YOUR Data folder/\"";
                String rCommand3 = "param <- SetPeakParam(\"REPLACE WITH YOUR PARAM SETTINGS\")";
                String rCommand4 = "plotSettings1 <- SetPlotParam(Plot=T)";
                String rCommand5 = "plotSettings2 <- SetPlotParam(Plot=T)";
                String rCommand6 = "rawData <- ImportRawMSData(data_folder_Sample,plotSettings = plotSettings1)";
                String rCommand7 = "mSet <- PerformPeakProfiling(rawData,param,plotSettings = plotSettings2)";
                String rCommand8 = "annParams <- SetAnnotationParam(\"REPLACE WITH YOUR PARAM SETTINGS\")";
                String rCommand9 = "annotPeaks <- PerformPeakAnnotation(mSet, annParams)";
                String rCommand10 = "maPeaks <- FormatPeakList(annotPeaks, annParams, filtIso =F, filtAdducts = FALSE,missPercent = 1)";

                RCenter.recordRCommand(RC, rCommand02);
                RCenter.recordRCommand(RC, rCommand04);
                RCenter.recordRCommand(RC, rCommand05);
                RCenter.recordRCommand(RC, rCommand06);

                //RCenter.recordRCommand(RC, rCommand1);
                RCenter.recordRCommand(RC, rCommand2);
                RCenter.recordRCommand(RC, rCommand3);
                RCenter.recordRCommand(RC, rCommand4);
                RCenter.recordRCommand(RC, rCommand5);
                RCenter.recordRCommand(RC, rCommand6);
                RCenter.recordRCommand(RC, rCommand7);
                RCenter.recordRCommand(RC, rCommand8);
                RCenter.recordRCommand(RC, rCommand9);
                RCenter.recordRCommand(RC, rCommand10);

            }

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("createScriptForScheduler", e);
        }
        return 0;
    }

    public static String[][] initialPlatformParams(RConnection RC, String platform) throws REXPMismatchException {

        try {

            String rCommand = "names(SetPeakParam(platform = \"" + platform + "\"))";
            String rCommand2 = "unname(unlist(SetPeakParam(platform = \"" + platform + "\")))";

            //RDataUtils.createScriptForScheduler(RC, sb.getCurrentUser().getName(),rCommand);
            String[] params_name = RC.eval(rCommand).asStrings();
            String[] params_value = RC.eval(rCommand2).asStrings();
            String[][] param_arrray = {params_name, params_value};

            //RCenter.recordRCommand(RC, rCommand);
            return param_arrray;

        } catch (RserveException rse) {
            LOGGER.error("initialPlatformParams", rse);
            return null;

        }

    }

    public static int generatePeakList(RConnection RC, String userPath) {
        int res = -10;
        try {
            String rCommand = "GeneratePeakList(\"" + userPath + "\")";
            RCenter.recordRCommand(RC, rCommand);
            //RC.voidEval(rCommand);
            res = RC.eval(rCommand).asInteger();
            return (res);
        } catch (Exception rse) {
            LOGGER.error("generatePeakList", rse);
        }
        return res;
    }

    public static int generateAsariPeakList(RConnection RC, String userPath) {
        int res = -10;
        try {
            String rCommand = "generateAsariPeakList(\"" + userPath + "\")";
            RCenter.recordRCommand(RC, rCommand);
            //RC.voidEval(rCommand);
            res = RC.eval(rCommand).asInteger();
            return (res);
        } catch (Exception rse) {
            LOGGER.error("generateAsariPeakList", rse);
        }
        return res;
    }

    public static String plotMSData(RConnection RC, String FileName, double mzl, double mzh, double rtl, double rth, int res, String dimen) {
        // , mz.range, dimension="3D", res=100
        try {
            String rCommand = "PerformDataInspect(\"" + FileName
                    + "\", rt.range = c(" + rtl + "," + rth
                    + "), mz.range = c(" + mzl + "," + mzh
                    + ") , dimension= \"" + dimen
                    + "\", res = " + res + ")";
            RCenter.recordRCommand(RC, rCommand);
            String ImageNM = RC.eval(rCommand).asString();
            return ImageNM;
        } catch (Exception rse) {
            LOGGER.error("plotMSData", rse);
        }
        return null;
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readTextData(RConnection RC, String filePath, String format, String lblType) {
        try {
            String rCommand = "Read.TextData(NA, \"" + filePath + "\", \"" + format + "\", \"" + lblType + "\");";
            String rCommand2 = "Read.TextData(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\", \"" + lblType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readTextData", rse);
            return false;
        }
    }

    public static boolean readTextDataTs(RConnection RC, String filePath, String format) {
        try {
            String rCommand = "Read.TextDataTs(NA, \"" + filePath + "\", \"" + format + "\");";
            String rCommand2 = "Read.TextDataTs(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readTextDataTs", rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readTextDataDose(RConnection RC, String filePath, String format, String lblType) {
        try {
            String rCommand = "Read.TextDataDose(NA, \"" + filePath + "\", \"" + format + "\", \"" + lblType + "\");";
            String rCommand2 = "Read.TextDataDose(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\", \"" + lblType + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readTextDataDose", rse);
            return false;
        }
    }

    public static boolean readMetaData(RConnection RC, String filePath) {
        try {
            String rCommand = "ReadMetaData(NA, \"" + filePath + "\");";
            String rCommand2 = "ReadMetaData(NA, Replacing_with_your_file_path);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readMetaData", rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readPeakListData(RConnection RC, String filePath) {
        try {
            String rCommand = "Read.PeakListData(NA, \"" + filePath + "\");";
            String rCommand2 = "Read.PeakListData(NA, \"" + "Replacing_with_your_file_path" + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readPeakListData", rse);
            return false;
        }
    }

    public static String retrieveOmicsType(RConnection RC) {
        try {
            String rCommand = "retrieveOmicsType();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("retrieveOmicsType", rse);
            return null;
        }
    }

    public static String getExposomeInfo(RConnection RC, String ft_label, int subidx) {
        try {
            String rCommand = "getExposomeInfo(NA, \"" + ft_label + "\", " + subidx + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getExposomeInfo", rse);
            return null;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readMzTabData(RConnection RC, String filePath, String identifier) {
        try {
            String rCommand = "Read.mzTab(NA, \"" + filePath + "\", \"" + identifier + "\");";
            String rCommand2 = "Read.mzTab(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + identifier + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readMzTabData", rse);
            return false;
        }
    }

    public static boolean validateMetabolon(RConnection RC, String filePath) {
        try {
            String rCommand = "ValidateMetabolonData(\"" + filePath + "\");";
            String rCommand2 = "ValidateMetabolonData(\"" + "Replacing_with_your_file_path" + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("ValidateMetabolonData", rse);
            return false;
        }
    }

    public static String[] getMetabolonMetaFactors(RConnection RC, String filePath) {
        String[] res = {};
        try {
            String rCommand = "ExtractMetabolonMetaFactors(NA, \"" + filePath + "\");";
            String rCommand2 = "ExtractMetabolonMetaFactors(\"" + "Replacing_with_your_file_path" + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception rse) {
            LOGGER.error("ExtractMetabolonMetaFactors", rse);
            return res;
        }
    }

    public static String[] getMetabolonCMPDIDs(RConnection RC, String filePath) {
        String[] res = {};
        try {
            String rCommand = "ExtractMetabolonCompoundIDs(NA, \"" + filePath + "\");";
            String rCommand2 = "ExtractMetabolonCompoundIDs(\"" + "Replacing_with_your_file_path" + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception rse) {
            LOGGER.error("ExtractMetabolonCompoundIDs", rse);
            return res;
        }
    }

    public static String[] getquickMetabolonMetaFactors(RConnection RC) {
        String[] res = {};
        try {
            String rCommand = "GetMetabolonMetaFactor(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception rse) {
            LOGGER.error("GetMetabolonMetaFactor", rse);
            return res;
        }
    }

    public static String[] getquickMetabolonCMPDIDs(RConnection RC) {
        String[] res = {};
        try {
            String rCommand = "GetMetabolonCMPDIDs(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asStrings());
        } catch (Exception rse) {
            LOGGER.error("GetMetabolonCMPDIDs", rse);
            return res;
        }
    }

    public static boolean parseMetabolonData(RConnection RC, String metafactor, String featureID) {
        try {
            String rCommand = "ReadMetabolonSheets(NA, \"" + metafactor + "\",\"" + featureID + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("FormatMetabolonSheets", rse);
            return false;
        }
    }

    public static boolean ValidateMetaFactor2Level(RConnection RC, String metafactor) {
        try {
            String rCommand = "ValidateMetaFactor2Level(NA, \"" + metafactor + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("ValidateMetaFactor2Level", rse);
            return false;
        }
    }

    public static boolean readMzMineTable(RConnection RC, String filePath, String format) {
        try {
            String rCommand = ".format_mzmine_pktable(NA, \"" + filePath + "\", \"" + format + "\");";
            String rCommand2 = ".format_mzmine_pktable(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readMzMineTable", rse);
            return false;
        }
    }

    public static boolean getMetabolomicsWorkbenchData(RConnection RC, String StudyID) {
        try {
            String rCommand = "GetNMDRStudy(NA, \"" + StudyID + "\");";
            String rCommand2 = "GetNMDRStudy(NA, \"" + "Replace_with_Study_ID" + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("getMetabolomicsWorkbenchData", rse);
            return false;
        }
    }

    public static boolean readMetabolomicsWorkbenchData(RConnection RC, String filePath, String format, String lblType) {
        try {
            String rCommand = "Read.TextData(NA, \"" + filePath + "\", \"" + format + "\", \"" + lblType + "\", nmdr = TRUE);";
            String rCommand2 = "Read.TextData(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\", \"" + lblType + "\", nmdr = TRUE);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readMetabolomicsWorkbenchData", rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean setInstrumentParams(SessionBean1 sb, double instrumentOpt, String msModeOpt, String primaryIon, double rtFrac) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "UpdateInstrumentParameters(NA, " + instrumentOpt + ", \"" + msModeOpt + "\", \"" + primaryIon + "\", " + rtFrac + ");";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performPeaks2Fun");

            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("setInstrumentParams", rse);
            return false;
        }
    }

    //zip data is from stream, not from file
    public static boolean readZipData(SessionBean1 sb, String inPath, String outPath, String dataType, String homeDir) {
        try {

            RConnection RC = sb.getRConnection();
            //first try System and R approach
            String rCommand = "UnzipUploadedFile(\"" + inPath + "\", \"" + outPath + "\");";
            boolean unzipOK = RC.eval(rCommand).asInteger() == 1;

            String rCommand2 = "UnzipUploadedFile(\"" + "Replacing_with_your_file_path" + "\", \"upload\");";
            RCenter.recordRCommand(RC, rCommand);

            if (!unzipOK) {
                System.out.println("Java unzip failed, using System-R  ===============");
                //Abslute path for Java
                unzipOK = DataUtils.unzipData(homeDir + File.separator + inPath, homeDir + File.separator + outPath);
            } else {
                System.out.println("==== Java unzip successful  ===============");
            }

            if (!unzipOK) {
                sb.addMessage("Error", "Failed to unzip your file. Please make sure it is in standard zip format!");
                return false;
            }
            if (dataType.equals("nmrpeak") || dataType.equals("mspeak")) {
                String rCommand3 = "Read.PeakList(NA" + ", \"" + outPath + "\");";
                RCenter.recordRCommand(RC, rCommand3);
                return ((RC.eval(rCommand3)).asInteger() == 1);
            }
            return true;
        } catch (Exception rse) {
            LOGGER.error("readZipData", rse);
            return false;
        }
    }

    public static String getExamplePath(RConnection RC) {
        try {
            String rCommand = "GetExampleDataPath();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getExamplePath", rse);
            return null;
        }
    }

    //zip example data is from stream, not from file
    public static boolean readZipExampleData(SessionBean1 sb, String inPath, String outPath, String dataType, String homeDir) {
        try {

            RConnection RC = sb.getRConnection();
            //first try System and R approach
            String rCommand = "UnzipUploadedFile(\"" + inPath + "\", \"" + outPath + "\");";
            boolean unzipOK = RC.eval(rCommand).asInteger() == 1;

            String rCommand2 = "UnzipUploadedFile(\"" + "Replacing_with_your_file_path" + "\", \"upload\");";
            RCenter.recordRCommand(RC, rCommand);

            if (!unzipOK) {
                System.out.println("Java unzip failed, using System-R  ===============");
                //Abslute path for Java
                unzipOK = DataUtils.unzipData(homeDir + File.separator + inPath, homeDir + File.separator + outPath);
            } else {
                System.out.println("==== Java unzip successful  ===============");
            }

            if (!unzipOK) {
                sb.addMessage("Error", "Failed to unzip your file. Please make sure it is in standard zip format!");
                return false;
            }
            if (dataType.equals("nmrpeak") || dataType.equals("mspeak")) {
                String rCommand3 = "Read.PeakList(NA" + ", \"" + outPath + "\");";
                RCenter.recordRCommand(RC, rCommand3);
                return ((RC.eval(rCommand3)).asInteger() == 1);
            }
            return true;
        } catch (Exception rse) {
            LOGGER.error("readZipExampleData", rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readBatchDataTB(RConnection RC, String fileName, String format, String missingEstt) {
        try {
            String rCommand = "Read.BatchDataTB(NA" + ", \"" + fileName + "\", \"" + format + "\",\"" + missingEstt + "\");";
            String rCommand2 = "Read.BatchDataTB(NA" + ", \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\",\"" + missingEstt + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return !RC.eval(rCommand).asString().equalsIgnoreCase("F");
        } catch (Exception rse) {
            LOGGER.error("readBatchDataTB", rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readBatchDataBC(RConnection RC, String fileName, String format, String label, String missingEstt) {
        try {
            String rCommand = "Read.BatchDataBC(NA" + ", \"" + fileName + "\", \"" + format + "\", \"" + label + "\",\"" + missingEstt + "\");";
            String rCommand2 = "Read.BatchDataBC(NA" + ", \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\", \"" + label + "\",\"" + missingEstt + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return !RC.eval(rCommand).asString().equalsIgnoreCase("F");
        } catch (Exception rse) {
            LOGGER.error("readBatchDataBC", rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean resetBatchData(RConnection RC) {
        try {
            String rCommand = "ResetBatchData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return !RC.eval(rCommand).asString().equalsIgnoreCase("F");
        } catch (Exception rse) {
            LOGGER.error("resetBatchData", rse);
            return false;
        }
    }

    public static boolean performBatchCorrection(RConnection RC, String imgName, String method, String center) {
        try {
            String rCommand = "PerformBatchCorrection(NA, \"" + imgName + "\"" + ", \"" + method + "\"" + ", \"" + center + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString().equals("T");
        } catch (Exception e) {
            LOGGER.error("performBatchCorrection", e);
        }
        return false;
    }

    public static boolean readDPDataTB(RConnection RC, String fileName, String format) {
        try {
            String rCommand = "Read.duplicatesDataTable(NA" + ", \"" + fileName + "\", \"" + format + "\");";
            String rCommand2 = "Read.duplicatesDataTable(NA" + ", \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return !RC.eval(rCommand).asString().equalsIgnoreCase("F");
        } catch (Exception rse) {
            LOGGER.error("readDPDataTB", rse);
            return false;
        }
    }

    public static int performDuplicateEstimation(RConnection RC, String format, String method, boolean smoothed) {
        try {
            String smoother = "FALSE";
            if (smoothed) {
                smoother = "TRUE";
            }

            String rCommand = "performDuplicateEstimation(NA, \"" + format + "\"" + ", \"" + method + "\"" + ", " + smoother + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("performDuplicateEstimation", e);
            return 0;
        }
    }

    public static int mergeFeatureDuplicates(RConnection RC, String format, String method) {
        try {
            String rCommand = "MergeFeatureDuplicates(NA, \"" + format + "\", \"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("MergeFeatureDuplicates", e);
        }
        return 1;
    }

    public static String[] getBatchNames(RConnection RC) {
        try {
            String rCommand = "GetAllBatchNames(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            LOGGER.error("getBatchNames", rse);
        }
        return null;
    }

    public static boolean processPeakList(RConnection RC, double binwidth) {
        try {
            String rCommand = "GroupPeakList(NA" + ", " + binwidth + "," + 10 + ");\n"
                    + "SetPeakList.GroupValues(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (Exception e) {
            LOGGER.error("processPeakList", e);
            return false;
        }
    }

    public static boolean processPeakList(RConnection RC, double binwidth, double bw) {
        try {
            String rCommand = "GroupPeakList(NA" + ", " + binwidth + "," + bw + ");\n"
                    + "SetPeakList.GroupValues(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (Exception e) {
            LOGGER.error("processPeakList-bw", e);
            return false;
        }
    }

    public static String[] getPeaklistProcMessage(RConnection RC) {
        try {
            return RC.eval("c(mSet$msgSet$read.msg, mSet$msgSet$proc.msg)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getPeaklistProcMessage", e);
        }
        return null;
    }

    public static String getNewSampleNames(RConnection RC) {
        try {
            String rCommand = "GetNewSampleNames(NA)";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getNewSampleNames", e);
            return null;
        }
    }

    public static String getCmpdInfo(RConnection RC, String id) {
        try {
            String rCommand = "GetMatchingDetails(NA, \"" + id + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getCmpdInfo", e);
            return null;
        }
    }

    //------------Sanity Check------------------
    public static int sanityCheckData(RConnection RC) {
        try {
            String rCommand = "SanityCheckData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("sanityCheckData", rse);
        }
        return 0;
    }

    //for sanity check on meta check page
    public static int sanityCheckMeta(RConnection RC, int init) {
        try {
            String rCommand = "SanityCheckMeta(NA, " + init + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("sanityCheckMeta", rse);
        }
        return 0;
    }

    public static boolean sanityCheckMummichogData(RConnection RC) {
        try {
            String rCommand = "SanityCheckMummichogData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("sanityCheckMummichogData", rse);
        }
        return false;
    }

    public static String[] getSanityCheckMessage(RConnection RC) {
        try {
            return RC.eval("mSet$msgSet$check.msg").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSanityCheckMessage", e);
        }
        return null;
    }

    public static String[] getSanityCheckMetaMessage(RConnection RC) {
        try {
            return RC.eval("mSet$msgSet$metacheck.msg").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSanityCheckMetaMessage", e);
        }
        return null;
    }

    public static String[] getSanityCheckRawMessage(RConnection RC) {
        try {
            String rCommand = "mesObj$messageSum";
            // RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getSanityCheckRawMessage", e);
        }
        return null;
    }

    public static boolean sanityCheckMetaData(RConnection RC, String dataNm) {
        try {
            String rCommand = "SanityCheckIndData(NA" + ", \"" + dataNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("sanityCheckMetaData", rse);
        }
        return false;
    }

    public static void sanityCheckRawSpectra() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public static String getMissingTestMsg(RConnection RC) {
        try {
            String rCommand = "GetMissingTestMsg(NA)";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("GetMissingTestMsg", e);
            return null;
        }
    }

    /**
     * Plot a lollipop chart summarising the percentage of non-missing values in
     * every sample (wrapper for the R function `PlotMissingDistr`).
     *
     * @param sb active SessionBean1 (provides the RServe connection)
     * @param imgName base file name (no extension) for the graphic
     * @param format image format (e.g. "png", "tiff", "pdf", "svg")
     * @param dpi dots-per-inch resolution
     *
     * @return 1 on success, 0 on failure
     */
    public static int plotMissingDistr(SessionBean1 sb,
            String imgName,
            String format,
            int dpi) {
        try {
            RConnection RC = sb.getRConnection();

            // R call: let width/height default by passing NA
            String rCommand = "PlotMissingDistr(NA, \"" + imgName + "\", \""
                    + format + "\", " + dpi + ", width=NA)";

            RCenter.recordRCommand(RC, rCommand);                 // log the call
            sb.addGraphicsCMD("qc_miss", rCommand);       // track graphic

            return RC.eval(rCommand).asInteger();                 // run & return
        } catch (Exception e) {
            LOGGER.error("PlotMissingDistr", e);
        }
        return 0; // failure
    }

    public static void sanityCheckRawSpectra(RConnection RC) {

        try {

            String rCommand = "mesObj<-SanityCheckRawSpectra()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

        } catch (Exception rse) {
            LOGGER.error("sanityCheckRawSpectra", rse);
        }

    }

    public static String[] getMetaSanityCheckMessage(RConnection RC, String dataNm) {
        try {
            String rCommand = "GetMetaSanityCheckMsg(NA" + ", \"" + dataNm + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getMetaSanityCheckMessage", e);
        }
        return null;
    }

    public static String[] getMetaPathSanityCheckMessage(RConnection RC, String dataNm) {
        try {
            String rCommand = "GetMetaPathSanityCheckMsg(NA" + ", \"" + dataNm + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getMetaPathSanityCheckMessage", e);
        }
        return null;
    }

    public static String[] getMSSpectraProcessingMessage(RConnection RC) {
        try {
            return RC.eval("c(mSet$msgSet$read.msg, mSet$msgSet$xset.msg)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getMSSpectraProcessingMessage", e);
        }
        return null;
    }

    public static String getUploadErrorMessage(RConnection RC) {
        try {
            return RC.eval("c(mSet$msgSet$read.msg)").asString();
        } catch (Exception e) {
            LOGGER.error("getUploadErrorMessage", e);
        }
        return null;
    }

    public static String getCurrentMsg(RConnection RC) {
        try {
            return RC.eval("GetCurrentMsg()").asString();
        } catch (Exception e) {
            LOGGER.error("getCurrentMsg", e);
        }
        return null;
    }

    public static String printCurrentClsInfo(RConnection RC) {
        try {
            return RC.eval("PrintCurrentCls()").asString();
        } catch (Exception e) {
            LOGGER.error("PrintCurrentCls", e);
        }
        return null;
    }

    //----------------Data normalization-------------------
    //normalize the data
    public static int normalizeData(SessionBean1 sb, String rowNorm, String transNorm,
            String scaleNorm, String refName, boolean includeRatio, int ratioNum) {
        try {
            RConnection RC = sb.getRConnection();
            String ratioCMD = "ratio=FALSE, ratioNum=20";
            if (includeRatio) {
                ratioCMD = "ratio=TRUE, ratioNum=" + ratioNum;
            }
            String rCommand;
            if (refName == null) {
                rCommand = "Normalization(NA" + ", \"" + rowNorm + "\", \"" + transNorm + "\", \"" + scaleNorm + "\", " + ratioCMD + ")";
            } else {
                rCommand = "Normalization(NA" + ", \"" + rowNorm + "\", \"" + transNorm + "\", \"" + scaleNorm + "\", \"" + refName + "\", " + ratioCMD + ")";
            }
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Normalization");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("normalizeData", e);
            return 0;
        }
    }

    public static int setRatioOption(SessionBean1 sb, String opt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "setRatioOption(NA, \"" + opt + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Normalization");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("setRatioOption", e);
            return 0;
        }

    }

    public static int getFeatureNum(RConnection RC) {
        try {
            String rCommand = "getFeatureNum(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("getFeatureNum", e);
            return 0;
        }

    }

    public static void quantileNormalizeData(RConnection RC) {
        try {
            String rCommand = "QuantileNormalize()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("quantileNormalizeData", e);
        }
    }

    public static void saveRCommands(RConnection RC) {
        try {
            String rCommand = "SaveRCommands()";
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("saveRCommands", e);
        }
    }

    //plot a boxplot and density for each compound
    public static void plotNormSummaryGraph(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotNormSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Normalization");

            sb.addGraphicsCMD("norm", rCommand);
            sb.addGraphicsMapLink("norm", "/Secure/process/NormalizationView.xhtml");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("plotNormSummaryGraph", e);
        }
    }

    //plot a boxplot and density for each compound
    public static void plotSampleNormSummaryGraph(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSampleNormSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("snorm", rCommand);
            sb.addGraphicsMapLink("snorm", "/Secure/process/NormalizationView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "Normalization");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("plotSampleNormSummaryGraph", e);
        }
    }

    //---------------Methods for access Data information-------------
    //get data information
    public static void initPrenormData(RConnection RC) {
        try {
            String rCommand = "PreparePrenormData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("initPrenormData", e);
        }
    }

    public static void setPeakFormat(RConnection RC, String type) {
        try {
            String rCommand = "SetPeakFormat(NA, \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setPeakFormat", e);
        }
    }

    public static String GetPeakFormat(RConnection RC) {
        try {
            return RC.eval("GetPeakFormat(NA)").asString();
        } catch (Exception e) {
            LOGGER.error("GetPeakFormat", e);
        }
        return null;
    }

    public static void setRT(RConnection RC, String rt) {
        try {
            String rCommand = "SetRTincluded(NA" + ", \"" + rt + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setRT", e);
        }
    }

    public static String[] getOrigSampleNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetOrigSmplNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getOrigSampleNames", e);
        }
        return null;
    }

    public static String[] getUniqueMetaNames(RConnection RC, String metaname) {
        try {
            String[] names = RC.eval("GetUniqueMetaNames(NA" + ", \"" + metaname + "\")").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getUniqueMetaNames", e);
        }
        return null;
    }

    public static String[] getPrenormSampleNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetPrenormSmplNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getPrenormSampleNames", e);
        }
        return null;
    }

    public static int getPrenormFeatureNum(RConnection RC) {
        try {
            return RC.eval("GetPrenormFeatureNum(NA)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getPrenormFeatureNum", e);
        }
        return 0;
    }

    public static int validateFeatureName(RConnection RC, String featNm) {
        try {
            String rCommand = "ValidateFeatureName(NA" + ", \"" + featNm + "\")";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("validateFeatureName", rse);
        }
        return 0;
    }

    public static String[] getPrenormFeatureNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetPrenormFeatureNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getPrenormFeatureNames", e);
        }
        return null;
    }

    public static String[] getNameCheckMsgs(RConnection RC) {
        try {
            return RC.eval("GetNameCheckMsgs(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getNameCheckMsgs", e);
        }
        return null;
    }

    public static void setCachexiaTestSet(RConnection RC, String used) {
        String rCommand = "SetCachexiaSetUsed(NA" + ", " + used + ")";
        try {
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setCachexiaTestSet", e);
        }
    }

    //pass sample specific information to R for normalization
    //note: only need ordered double values
    public static void setDesignType(RConnection RC, String design) {
        String rCommand = "SetDesignType(NA" + ", \"" + design + "\")";
        RCenter.recordRCommand(RC, rCommand);
        try {
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setDesignType", e);
        }
    }

    //pass sample specific information to R for normalization
    //note: only need ordered double values
    public static void setSampleNormFactor(RConnection RC, List<SampleBean> norms) {
        String rcmd = "norm.vec<-c(";
        for (int i = 0; i < norms.size() - 1; i++) {
            rcmd = rcmd + norms.get(i).getAdjust() + ",";
        }
        rcmd = rcmd + norms.get(norms.size() - 1).getAdjust() + ");";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            LOGGER.error("setSampleNormFactor", e);
        }
    }

    //pass sample specific information to R for normalization
    //note: only need ordered double values
    public static void setSampleGroups(RConnection RC, List<SampleBean> norms, String metadata) {
        String rcmd = "grp.vec<-c(\"";
        for (int i = 0; i < norms.size() - 1; i++) {
            rcmd = rcmd + norms.get(i).getGroup() + "\", \"";
        }
        rcmd = rcmd + norms.get(norms.size() - 1).getGroup() + "\"); UpdateSampleGroups(NA" + ", \"" + metadata + "\")";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            LOGGER.error("setSampleGroups", e);
        }
    }

    public static void updateMetaType(RConnection RC, String metadata, String type) {
        String rcmd = "UpdateMetaType(NA" + ", \"" + metadata + "\", \"" + type + "\")";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            LOGGER.error("updateMetaType", e);
        }
    }

    public static String GetPrimaryType(RConnection RC, String analysisVar) {
        try {
            String rCommand = "GetPrimaryType(\"" + analysisVar + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void updateMetaOrder(RConnection RC, String metadata, String[] metaVec) throws REngineException {
        RC.assign("meta.ord.vec", metaVec);
        String rcmd = "UpdateMetaOrder(NA" + ", \"" + metadata + "\")";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            LOGGER.error("updateMetaOrder", e);
        }
    }

    public static void setCompVarNumbers(RConnection RC, List<ComponentBean> comps) {
        String rcmd = "comp.var.nums<-c(";
        for (int i = 0; i < comps.size() - 1; i++) {
            rcmd = rcmd + comps.get(i).getVarNum() + ",";
        }
        rcmd = rcmd + comps.get(comps.size() - 1).getVarNum() + ");";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            LOGGER.error("setCompVarNumbers", e);
        }
    }

    public static void updateGraphSettings(RConnection RC, String[] cols, int[] sps) {
        try {
            String colCmd = "colVec<-" + DataUtils.createStringVector(cols);
            RCenter.recordRCommand(RC, colCmd, true);

            StringBuilder sb = new StringBuilder();
            for (int s : sps) {
                sb.append(s);
                sb.append(",");
            }
            String res = sb.toString();
            //trim the last comma
            res = res.substring(0, res.length() - 1);
            String shpCmd = "shapeVec<-c(" + res + ")";
            RCenter.recordRCommand(RC, shpCmd, true);
            String updateCmd = "UpdateGraphSettings(NA, colVec, shapeVec)";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = colCmd + "; " + shpCmd + "; " + updateCmd;
            RC.eval(rcmd);
        } catch (Exception e) {
            LOGGER.error("updateGraphSettings", e);
        }
    }

    public static String[] getNormFeatureNames(RConnection RC) {
        try {
            return RC.eval("colnames(mSet$dataSet$norm)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getNormFeatureNames", e);
        }
        return null;
    }

    public static int getAovSigNum(RConnection RC) {
        try {
            return RC.eval("GetAovSigNum(NA)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getAovSigNum", e);
        }
        return 0;
    }

    public static int getOrigFeatureNumber(RConnection RC) {
        try {
            return RC.eval("length(mSet$dataSet$url.var.nms)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getOrigFeatureNumber", e);
        }
        return 0;
    }

    public static int getProcFeatureNumber(RConnection RC) {
        try {
            return RC.eval("mSet$dataSet$proc.feat.num").asInteger();
        } catch (Exception e) {
            LOGGER.error("getProcFeatureNumber", e);
        }
        return 0;
    }

    public static int getFiltFeatureNumber(RConnection RC) {
        try {
            return RC.eval("ncol(mSet$dataSet$filt)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getFiltFeatureNumber", e);
        }
        return 0;
    }

    public static int getNormFeatureNumber(RConnection RC) {
        try {
            return RC.eval("ncol(mSet$dataSet$norm)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getNormFeatureNumber", e);
        }
        return 0;
    }

    //save Normalized data
    public static void saveAllData(RConnection RC) {
        try {
            String rCommand = "SaveTransformedData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("saveAllData", e);
        }
    }

    //deselect certain compounds from further consideration
    // maxAllowed -1 all, 1 use empirical control 
    public static int filterVariable(SessionBean1 sb, String doQC, int rsd, String varFilter, int varCutoff, String intFilter, int intCutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FilterVariable(NA" + ", \"" + doQC + "\", " + rsd + ", \"" + varFilter + "\", " + varCutoff + ", \"" + intFilter + "\", " + intCutoff + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Filtering");

            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            LOGGER.error("filterVariable", e);
            return 0;
        }
    }

    public static int updateData(RConnection RC, String[] featVec, String[] smplVec, String[] useVec, String ordGrp) {
        try {
            String featCmd = "feature.nm.vec <- " + DataUtils.createStringVector(featVec);
            RCenter.recordRCommand(RC, featCmd);
            String smplCmd = "smpl.nm.vec <- " + DataUtils.createStringVector(smplVec);
            RCenter.recordRCommand(RC, smplCmd);
            String grpCmd = "grp.nm.vec <- " + DataUtils.createStringVector(useVec);
            RCenter.recordRCommand(RC, grpCmd);
            String updateCmd = "UpdateData(NA, " + ordGrp + ")";
            RCenter.recordRCommand(RC, updateCmd);
            String rcmd = featCmd + "; " + smplCmd + "; " + grpCmd + ";" + updateCmd;
            return RC.eval(rcmd).asInteger();
        } catch (Exception e) {
            LOGGER.error("updateData", e);
        }
        return 0;
    }

    public static int updateMetaData(RConnection RC, String[] grpVec) {
        try {
            String grpCmd = "meta.nm.vec <- " + DataUtils.createStringVector(grpVec);
            RCenter.recordRCommand(RC, grpCmd);
            String updateCmd = "UpdateMetaData(NA)";
            RCenter.recordRCommand(RC, updateCmd);
            String rcmd = grpCmd + ";" + updateCmd;
            return RC.eval(rcmd).asInteger();
        } catch (Exception e) {
            LOGGER.error("updateMetaData", e);
        }
        return 0;
    }

    public static void setCurrMapData(RConnection RC, String[] currVec) {
        try {
            String currCmd = "curr.vec <- " + DataUtils.createStringVector(currVec);
            RCenter.recordRCommand(RC, currCmd, true);
            String updateCmd = "Setup.MapData(NA" + ", curr.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = currCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            LOGGER.error("setCurrMapData", e);
        }
    }

    public static void setAdductData(RConnection RC, String[] addVec) {
        try {
            String currCmd = "add.vec <- " + DataUtils.createStringVector(addVec);
            RCenter.recordRCommand(RC, currCmd, true);
            String updateCmd = "Setup.AdductData(NA" + ", add.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = currCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            LOGGER.error("setAdductData", e);
        }
    }

    public static int performCurrencyMapping(RConnection RC) {
        try {
            String rCommand = "PerformCurrencyMapping(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();

        } catch (Exception e) {
            LOGGER.error("performCurrencyMapping", e);
        }
        return 0;
    }

    public static int performAdductMapping(RConnection RC, String mode) {
        try {
            String rCommand = "PerformAdductMapping(NA, \"" + mode + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();

        } catch (Exception e) {
            LOGGER.error("performAdductMapping", e);
        }
        return 0;
    }

    public static void removeVariableByPercent(SessionBean1 sb, double perc) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "RemoveMissingPercent(NA" + ", percent=" + perc + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performMissingImpute");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("removeVariableByPercent", e);
        }
    }

    // final sanity check to set up dataSet$proc; return feature number
    public static int replaceMin(RConnection RC) {
        try {
            String rCommand = "ReplaceMin(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("replaceMin", e);
        }
        return -1;
    }

    // final sanity check to set up dataSet$proc
    public static void imputeVariable(SessionBean1 sb, String method) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ImputeMissingVar(NA" + ", method=\"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performMissingImpute");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("imputeVariable", e);
        }
    }

    public static String getProcZipMessage(RConnection RC) {
        try {
            return RC.eval("procZipErrMsg").asString();
        } catch (Exception e) {
            LOGGER.error("getProcZipMessage", e);
        }
        return null;
    }

    public static int getGroupNumber(RConnection RC) {
        try {
            return RC.eval("GetGroupNumber(NA)").asInteger();
        } catch (Exception e) {
            LOGGER.error("getGroupNumber", e);
        }
        return 0;
    }

    public static String[] getPathNames(RConnection RC) {
        try {
            return RC.eval("GetPathNames(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getPathNames", e);
        }
        return null;
    }

    public static String[] getMatchedCmpdNames(RConnection RC) {
        try {
            return RC.eval("GetMatchedCompounds(NA)").asStrings();
        } catch (Exception e) {
            LOGGER.error("getMatchedCmpdNames", e);
        }
        return null;
    }

    public static String[] getErrorMsgs(RConnection RC) {
        try {
            return RC.eval("GetErrMsg()").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getErrorMsgs", rse);
            return new String[]{"Unknown Error Occurred"};
        }
    }

    //should be in the same directory format specify sample in row or column
    public static void addErrMsg(RConnection RC, String msg) {
        try {
            String rCommand = "AddErrMsg(\"" + msg + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("addErrMsg", rse);
        }
    }

    public static String[] getSSPReferences(RConnection RC, String nm) {
        try {
            return RC.eval("GetSSP.Refs(NA, \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getSSPReferences", rse);
            return null;
        }
    }

    public static String[] getSSPConcs(RConnection RC, String nm) {
        try {
            return RC.eval("GetSSP.RefConc(NA, \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getSSPConcs", rse);
            return null;
        }
    }

    public static String[] getSSP_Pmids(RConnection RC, String nm) {
        try {
            //  System.out.println(RC.eval("GetSSP.Pmids("+inx+")").asStrings());
            return RC.eval("GetSSP.Pmids(NA" + ", \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getSSP_Pmids", rse);
            return null;
        }
    }

    public static String[] getSSP_Notes(RConnection RC, String nm) {
        try {
            return RC.eval("GetSSP.Notes(NA" + ", \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getSSP_Notes", rse);
            return null;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readMsetLibData(RConnection RC, String fileName) {
        try {
            String rCommand = "Setup.UserMsetLibData(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("readMsetLibData", rse);
        }
        return false;
    }

    public static String getMsetLibCheckMsg(RConnection RC) {
        try {
            return RC.eval("GetMsetLibCheckMsg(NA)").asString();
        } catch (Exception rse) {
            LOGGER.error("getMsetLibCheckMsg", rse);
            return null;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readKEGGRefLibData(RConnection RC, String fileName) {
        try {
            String rCommand = "Setup.KEGGReferenceMetabolome(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);

            //System.out.println("======here is res=====" + RC.eval(rCommand).asInteger());
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("readKEGGRefLibData", rse);
        }
        return false;
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readHMDBRefLibData(RConnection RC, String fileName) {
        try {
            String rCommand = "Setup.HMDBReferenceMetabolome(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("readHMDBRefLibData", rse);
        }
        return false;
    }

    public static String getRefLibCheckMsg(RConnection RC) {
        try {
            return RC.eval("GetRefLibCheckMsg(NA)").asString();
        } catch (Exception rse) {
            LOGGER.error("getRefLibCheckMsg", rse);
            return null;
        }
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void setMapData(RConnection RC, String[] qvec) {
        try {
            String grpCmd = "cmpd.vec<-" + DataUtils.createStringVector(qvec);
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "Setup.MapData(NA" + ", cmpd.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = grpCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            LOGGER.error("setMapData", e);
        }
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void updateMapData(SessionBean1 sb, String[] qvec) {
        try {
            RConnection RC = sb.getRConnection();
            String grpCmd = "ssp.cmpds<-" + DataUtils.createStringVector(qvec);
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "Update.MapData(NA" + ", ssp.cmpds);";
            RCenter.recordRCommand(RC, updateCmd);
            sb.recordRCommandFunctionInfo(grpCmd, "SSP");
            sb.recordRCommandFunctionInfo(updateCmd, "SSP");

            String rcmd = grpCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            LOGGER.error("updateMapData", e);
        }
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void setConcData(RConnection RC, double[] concVec) {
        try {

            StringBuilder sb = new StringBuilder();
            for (double s : concVec) {
                sb.append(s);
                sb.append(",");
            }
            String res = sb.toString();
            //trim the last comma
            res = res.substring(0, res.length() - 1);
            String grpCmd = "conc.vec<-c(" + res + ")";
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "Setup.ConcData(NA" + ", conc.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = grpCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            LOGGER.error("setConcData", e);
        }
    }

    //to set up the single sample concentration data for SSP analysis
    public static void setupSspData(RConnection RC, String[] nmVec, double[] concVec, String type) {
        try {
            setMapData(RC, nmVec);
            setConcData(RC, concVec);
            setBiofluidType(RC, type);
        } catch (Exception e) {
            LOGGER.error("setupSspData", e);
        }
    }

    public static void setBiofluidType(RConnection RC, String type) {
        try {
            String rCommand = "Setup.BiofluidType(NA" + ", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setBiofluidType", e);
        }
    }

    //set current metabolite set library for search
    public static void setCurrentMsetLib(SessionBean1 sb, String libType, int excludeNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetCurrentMsetLib(NA" + ", \"" + libType + "\", " + excludeNum + ");";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Enrichment");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setCurrentMsetLib", e);
        }
    }

    //set current metabolite set library for search
    public static void setCurrentMsetLib(RConnection RC, String libType) {
        try {
            String rCommand = "SetCurrentMsetLib(NA" + ", \"" + libType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setCurrentMsetLib", e);
        }
    }

    //use filter or not
    public static void setMetabolomeFilter(SessionBean1 sb, boolean useFilter) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetMetabolomeFilter(NA" + ", " + (useFilter ? "T" : "F") + ");";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Enrichment");
            sb.recordRCommandFunctionInfo(rCommand, "paBn_proceed");

            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setMetabolomeFilter", e);
        }
    }

    //first col is compound name, 2nd is concentration values, return as a Hashmap
    //with cmpd name as key and concentration as value
    public static boolean setSspData(RConnection RC, String content, String type, String sep) {

        if (sep == null) {
            sep = System.getProperty("line.separator");
        }
        StringTokenizer st = new StringTokenizer(content, sep);

        try {
            ArrayList<String> nmVec = new ArrayList();
            ArrayList<Double> concVec = new ArrayList();
            while (st.hasMoreTokens()) {
                String line = st.nextToken();
                line = DataUtils.cleanString(line);
                if (line.length() == 0) { //empty line
                    continue;
                }
                String[] eles = line.split("\t");
                if (eles.length != 2) {
                    RDataUtils.addErrMsg(RC, "Each row must have two elements: compound and its concentration value, separated by a tab!");
                    return false;
                }
                String name = DataUtils.cleanString(eles[0]);
                double conc = Double.parseDouble(DataUtils.cleanString(eles[1]));
                nmVec.add(name);
                concVec.add(conc);
            }
            double[] concArray = new double[concVec.size()];
            for (int i = 0; i < concArray.length; i++) {
                concArray[i] = concVec.get(i);
            }
            setupSspData(RC, nmVec.toArray(new String[0]), concArray, type);
            return true;
        } catch (NumberFormatException e) {
            LOGGER.error("setSample", e);
            RDataUtils.addErrMsg(RC, "Some concentration values are not numeric!");
            return false;
        }
    }

    public static boolean containMissing(RConnection RC) {
        try {
            String rCommand = "ContainMissing(NA)";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("containMissing", rse);
        }
        return false;
    }

    public static boolean setPathLib(SessionBean1 sb, String path, String libVersion) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "";
            if (path.startsWith("smpdb")) {
                path = path.split("-")[1];
                rCommand = "SetSMPDB.PathLib(NA, \"" + path + "\")";
            } else {
                rCommand = "SetKEGG.PathLib(NA, \"" + path + "\", \"" + libVersion + "\")";
            }
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "paBn_proceed");
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("setPathLib", rse);
        }
        return false;
    }

    public static String[] getKeggPathLibNames(RConnection RC) {
        try {
            return RC.eval("GetKEGG.PathNames(NA)").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getKeggPathLibNames", rse);
            return null;
        }
    }

    public static List<SampleBean> createOrigSampleBeans(RConnection RC, String metaname, boolean withNA) {
        String[] allSamples = RDataUtils.getOrigSampleNames(RC);
        String[] allGrps = RDataUtils.getOrigSmplGroupNamesPerMeta(RC, metaname);
        int samSize = allSamples.length;
        List<SampleBean> samNABeans = new ArrayList();
        if (withNA) {
            samNABeans.add(new SampleBean("<Not set>", "NA")); // the first one is NA
        }
        for (int i = 0; i < samSize; i++) {
            samNABeans.add(new SampleBean(allSamples[i], allGrps[i]));
        }
        return samNABeans;
    }

    public static int getSampleSize(RConnection RC) {
        try {
            String rCommand = "GetOrigSmplSize(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("getOrigSmplSize", rse);
        }
        return 0;
    }

    public static String[] getOrigSmplGroupNamesPerMeta(RConnection RC, String metaname) {
        try {
            String rCommand = "GetOrigSmplGroupNamesPerMeta(NA, \"" + metaname + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getOrigSmplGroupNamesPerMeta", rse);
        }
        return null;
    }

    //one per sample
    public static String[] getPrenormSmplGroupNames(RConnection RC) {
        try {
            String rCommand = "GetPrenormSmplGroupNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getPrenormSmplGroupNames", rse);
        }
        return null;
    }

    //just unique group names
    public static String[] getPrenormGroupNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetPrenormClsNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getPrenormGroupNames", e);
        }
        return null;
    }

    public static String[] getGroupNames(RConnection RC, String expFac) {
        try {
            String rCommand = "GetGroupNames(NA, \"" + expFac + "\")";
            RCenter.recordRCommand(RC, rCommand);
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getGroupNames", rse);
        }
        return null;
    }

    public static String[] getMetaInfo(RConnection RC) {
        try {
            String rCommand = "GetMetaInfo(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getMetaInfo", rse);
        }
        return null;
    }

    public static String[] getNormGroupNames(RConnection RC) {
        try {
            String rCommand = "GetNormGroupNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getNormGroupNames", rse);
        }
        return null;
    }

    public static String[] getQCFeatureNames(RConnection RC) {
        try {
            String rCommand = "GetQCCompoundNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getQCFeatureNames", rse);
        }
        return null;
    }

    public static boolean isDataContainNegative(RConnection RC) {
        try {
            String rCommand = "IsDataContainsNegative(NA);";
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("isDataContainNegative", rse);
        }
        return false;
    }

    public static boolean isSmallSampleSize(RConnection RC) {
        try {
            String rCommand = "IsSmallSmplSize(NA);";
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("isSmallSampleSize", rse);
        }
        return false;
    }

    public static int getMinGroupSize(RConnection RC) {
        try {
            String rCommand = "GetMinGroupSize(NA);";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("getMinGroupSize", rse);
        }
        return 0;
    }

    public static int setPvalFromPercent(RConnection RC, double pct) {
        try {
            String rCommand = "SetPvalFromPercent(NA," + pct + ");";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("setPvalFromPercent", rse);
        }
        return 0;
    }

    public static String getErrMsg(RConnection RC) {
        String msg = "<table>";
        if (RC != null) {
            String[] msgs = getErrorMsgs(RC);
            msg = msg + "<tr><th> Possible causes of error (last one being the most relevant): </th></tr>";
            if (msgs.length > 0) {
                for (String msg1 : msgs) {
                    msg = msg + "<tr><td align=\"left\">" + msg1 + "</td></tr>";
                }
            } else {
                msg = msg + "An unknown error occurred, please notify web admin for help.";
            }
        } else {
            msg = "An unknown error occurred.";
        }
        msg = msg + "</table>";
        return msg;
    }

    public static String[] getNewSampleNameVec(RConnection RC) {
        try {
            return RC.eval("GetNewSampleNameVec(NA)").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getNewSampleNameVec", rse);
            return null;
        }
    }

    public static double[] getNewSampleProbs(RConnection RC) {
        try {
            return RC.eval("GetNewSampleProbs(NA)").asDoubles();
        } catch (Exception rse) {
            LOGGER.error("getNewSampleProbs", rse);
            return null;
        }
    }

    public static String[] getNewSampleGrps(RConnection RC) {
        try {
            return RC.eval("GetNewSampleGrps(NA)").asStrings();
        } catch (Exception rse) {
            LOGGER.error("getNewSampleGrps", rse);
            return null;
        }
    }

    public static boolean setPeakEnrichMethod(SessionBean1 sb, String algOpt, String version) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetPeakEnrichMethod(NA" + ", \"" + algOpt + "\", \"" + version + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "performPeaks2Fun");

            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("setPeakEnrichMethod", rse);
        }
        return false;
    }

    public static void setOrganism(SessionBean1 sb, String orgCode) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetOrganism(NA" + ", \"" + orgCode + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "paBn_proceed");

            RC.voidEval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("setOrganism", rse);
        }
    }

    public static int setSelectedDataNames(RConnection RC, String[] nmVec) {
        try {
            RC.assign("nm.vec", nmVec);
            String rcmd = "nm.vec <- " + DataUtils.convertArrayToVecInR(nmVec);
            RCenter.recordRCommand(RC, rcmd);

            RCenter.recordRCommand(RC, "SelectMultiData(mSetObj)");
            return RC.eval("SelectMultiData(NA)").asInteger();
        } catch (Exception rse) {
            LOGGER.error("setSelectedDataNames", rse);
        }
        return 0;
    }

    public static int prepareUpsetData(SessionBean1 sb, String fileName) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PrepareUpsetData(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Upset Diagram");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("prepareUpsetData", e);
        }
        return 0;
    }

    public static int readRawMeta(RConnection RC, String fileName) {
        try {
            String rCommand = "ReadRawMeta(\"" + fileName + "\");"; // No need to record this function.            
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("readRawMeta", e);
        }
        return 0;
    }

    public static int removeSelectedMeta(RConnection RC, String meta) {
        try {
            String rCommand = "RemoveSelectedMeta(NA" + ", \"" + meta + "\");"; // No need to record this function.            
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("removeSelectedMeta", e);
        }
        return 0;
    }

    public static String[] getMetaGroupNames(RConnection RC, String dataName) {
        try {
            String rCommand = "GetMetaGroupNames(NA" + ", \"" + dataName + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getMetaGroupNames", rse);
        }
        return null;
    }

    public static String[] setSelectedMetaMetaInfo(RConnection RC, String dataName, String meta0, String meta1, String isBlock) {
        try {
            String cmd = "SetSelectedMetaMetaInfo(\"" + dataName + "\", \"" + meta0 + "\", \"" + meta1 + "\", " + isBlock + ")";
            RCenter.recordRCommand(RC, cmd);
            return RC.eval(cmd).asStrings();
        } catch (Exception rse) {
            LOGGER.error("setSelectedMetaMetaInfo", rse);
            return null;
        }
    }

    public static void setUserPathForScheduler(RConnection RC, String path) {
        try {
            String rCommand = "SetUserPath(\"" + path + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("setUserPathForScheduler", e);
        }
    }

    public static String genPathwayJSON(RConnection RC, String name) {
        try {
            String rCommand = "GeneratePathwayJSON(\"" + name + "\")";
            //String imgName = RC.eval(rCommand).asString();
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("genPathwayJSON", e);
        }
        return null;
    }

    public static String[] getPartialFiles(RConnection RC, String naviString) {
        try {
            String rCommand = "GetFilesToBeSaved(\"" + naviString + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            LOGGER.error("getPartialFiles", rse);
        }
        return null;
    }

    public static String getPathForScheduler(RConnection RC) {
        try {
            String rCommand = "GetCurrentPathForScheduler()";
            //String imgName = RC.eval(rCommand).asString();
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("getPathForScheduler", e);
        }
        return null;
    }

    public static String[] getRawFileNames(RConnection RC) {
        try {
            String rCommand = "GetRawFileNms()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception e) {
            LOGGER.error("getRawFileNames", e);
        }
        return null;
    }

    public static String[] getRawClassNames(RConnection RC) {
        try {
            return RC.eval("GetRawClassNms()").asStrings();
        } catch (Exception e) {
            LOGGER.error("getRawClassNames", e);
        }
        return null;
    }

    public static boolean updateFeatureName(RConnection RC, String oldNm, String newNm) {
        try {
            String rCommand = "UpdateFeatureName(NA" + ", \"" + oldNm + "\", \"" + newNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("updateFeatureName", rse);
        }
        return false;
    }

    public static boolean updateMetaColName(RConnection RC, String oldNm, String newNm) {
        try {
            String rCommand = "UpdateMetaColName(NA" + ", \"" + oldNm + "\", \"" + newNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("updateMetaColName", rse);
        }
        return false;
    }

    public static boolean updateMetaLevels(RConnection RC, String metaNm, String[] metaLvls) {
        try {
            RC.assign("meta.lvls", metaLvls);
            String rcmd = "meta.lvls <- " + DataUtils.convertArrayToVecInR(metaLvls);
            RCenter.recordRCommand(RC, rcmd);

            String rCommand = "UpdateMetaLevels(NA" + ", \"" + metaNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            LOGGER.error("updateMetaLevels", rse);
        }
        return false;
    }

    public static void CoerceTable(RConnection RC, String oriFile, String oriFormat, String targetModule) {
        try {
            String rCommand = "TableFormatCoerce(" + "\"" + oriFile + "\", \"" + oriFormat + "\", \"" + targetModule + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.eval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("CoerceTable", rse);
        }
    }

    public static void saveFilesInclusion(SessionBean1 sb, String files, int number) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "spectraInclusion(\'" + files + "\', " + number + ");";
            //RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "prepareSpecProc");

            RC.voidEval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("saveFilesInclusion", rse);
        }
    }

    public static RList readFilesInclusion(RConnection RC) {
        RList res = null;
        try {
            String rCommand = "getSpectraInclusion();";
            //RCenter.recordRCommand(RC, rCommand);
            res = RC.eval(rCommand).asList();
        } catch (Exception rse) {
            LOGGER.error("readFilesInclusion", rse);
        }

        return res;
    }

    public static int centroidMSData(RConnection RC, String fileNM, String outDir) {
        int res = 0;
        try {
            String rCommand = "centroidMSData(\"" + fileNM + "\", \"" + outDir + "\");";
            RCenter.recordRCommand(RC, rCommand);
            res = RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("centroidMSData", rse);
        }
        return res;
    }

    public static int update3DPCA(RConnection RC, int num) {
        int res = 0;
        try {
            String rCommand = "updateSpectra3DPCA(" + num + ");";
            RCenter.recordRCommand(RC, rCommand);
            res = RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("update3DPCA", rse);
        }
        return res;
    }

    public static boolean checkDataGenericFormat(RConnection RC) {
        boolean res = false;
        try {
            String rCommand = "checkDataGenericFormat();";
            RCenter.recordRCommand(RC, rCommand);
            int res0 = RC.eval(rCommand).asInteger();
            if (res0 == 1) {
                res = true;
            }
        } catch (Exception rse) {
            LOGGER.error("checkDataGenericFormat", rse);
        }
        return res;
    }

    public static String[] getMetaDataGroups(RConnection RC) {
        try {
            String rCommand = "colnames(mSet$dataSet$meta.info)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getMetaDataGroups", e);
        }
        return null;
    }

    public static String[] getMetaDataStatus(RConnection RC) {
        try {
            String rCommand = "unname(mSet$dataSet$meta.status)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getMetaDataStatus", e);
        }
        return null;
    }

    public static int setMetaTypes(RConnection RC, String[] metaTypes) {
        int res = 0;
        try {
            String updateCmd = "SetMetaTypes(NA, "
                    + DataUtils.createStringVector(metaTypes) + ");";
            RCenter.recordRCommand(RC, updateCmd);
            res = RC.eval(updateCmd).asInteger();
        } catch (Exception e) {
            LOGGER.error("setMetaTypes", e);
        }

        return res;
    }

    public static int removeEntry(RConnection RC, String rowID) {
        try {
            String rCommand = "RemoveEntry(NA, \"" + rowID + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int setDataTypeOfMeta(RConnection RC) {
        int res = 0;
        try {
            String updateCmd = "SetDataTypeOfMeta(NA);";
            RCenter.recordRCommand(RC, updateCmd);
            res = RC.eval(updateCmd).asInteger();
        } catch (Exception e) {
            LOGGER.error("setDataTypeOfMeta", e);
        }

        return res;
    }

    public static String[] getMetaByCol(RConnection RC, String metaname) {
        try {
            String[] names = RC.eval("GetMetaByCol(NA" + ", \"" + metaname + "\")").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getMetaByCol", e);
        }
        return null;
    }

    public static String[] getMetaTypes(RConnection RC) {
        try {
            String[] names = RC.eval("GetMetaTypes(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getMetaTypes", e);
        }
        return null;
    }

    public static String[] getSampleNames(RConnection RC) {
        try {
            String rCommand = "GetSampleNames(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("getSampleNames", e);
        }
        return null;
    }

    //plot a meta-data correlation heatmap for meta-data check
    public static int plotMetaCorrHeatmap(SessionBean1 sb, String corOpt, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotMetaCorrHeatmap(NA, \"" + corOpt + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Metadata Heatmap");

            sb.addGraphicsCMD("metaCorrHeatmap", rCommand);
            sb.addGraphicsMapLink("metaCorrHeatmap", "/Secure/multifac/MetaDataView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("plotMetaCorrHeatmap", e);
        }
        return 0;
    }

    public static int plotMetaHeatmap(SessionBean1 sb, String viewOpt, String clustSelOpt, String distOpt, String clustOpt, String colOpt, String includeRowNames, String imgName, String format, int dpi) {
        int res = 0;
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotMetaHeatmap(NA" + ",  \"" + clustSelOpt + "\", \"" + distOpt + "\", \"" + clustOpt + "\", \"" + colOpt + "\",  " + includeRowNames + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Metadata Heatmap");

            String rCommand2 = "PlotStaticMetaHeatmap(NA" + ", \"" + viewOpt + "\", \"" + clustSelOpt + "\", \"" + distOpt + "\", \"" + clustOpt + "\", \"" + colOpt + "\",  " + includeRowNames + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            sb.recordRCommandFunctionInfo(rCommand2, "Metadata Heatmap");

            sb.addGraphicsCMD("metaHeatmap", rCommand2);
            sb.addGraphicsMapLink("metaHeatmap", "/Secure/multifac/MetaDataView.xhtml");

            res = RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("plotMetaHeatmap", e);
        }
        return res;
    }

    public static String checkOptiLCMS(RConnection RC, String version) {
        try {
            if (RC.eval("as.character(utils::packageVersion(\"OptiLCMS\"))").asString().equals(version)) {
                //System.out.println("Your OptiLCMS version is latest, NO need to be udpated!");
                return "TRUE";
            } else {
                //System.out.println("Your OptiLCMS version is too old, updating required!");
                return "FALSE";
            }
        } catch (Exception e) {
            // System.out.println("Your OptiLCMS is borken, re-installation required!");
            LOGGER.error("checkOptiLCMS", e);
            return "FALSE";
        }
    }

    public static int updateOptiLCMS(RConnection RC) {
        try {
            //System.out.println("RC2 is " + RC);
            //RC.voidEval("install.packages(\"https://www.dropbox.com/s/mzgcq1fwmtk2zxk/OptiLCMS.tar.gz\", repos = NULL, method = \"wget\")");
            //install.packages("https://www.dropbox.com/s/mzgcq1fwmtk2zxk/OptiLCMS.tar.gz", repos = NULL, method = "wget")
            return 1;
        } catch (Exception e) {
            LOGGER.error("updateOptiLCMS", e);
        }
        return 0;
    }

    public static void resetMemoise(RConnection RC) {
        try {
            String rCommand = "ResetMemoise();";
            RC.voidEval(rCommand);

        } catch (Exception e) {
            LOGGER.error("updateOptiLCMS", e);
        }
    }

    public static String generateColorArray(RConnection RC, int n, String color, String filenm) {
        try {
            String rCommand = "gg_color_hue_covid(" + n + ", \"" + color + "\", \"" + filenm + "\");";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("generateColorArray", e);
        }
        return "NA";
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

    public static String generatecoviddata(RConnection RC, String platformVec,
            String bloodVec, String polarityVec, String countryVec, String populationVec, String CompariVec) {
        try {
            String rCommand = "dataprocessing(\"" + platformVec + "\",\"" + bloodVec + "\",\"" + polarityVec + "\",\"" + countryVec + "\",\"" + populationVec + "\",\"" + CompariVec + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            LOGGER.error("dataprocessing", e);
        }

        return "NA";
    }

    public static boolean recordPartialLinkinDocker(RConnection RC, String partialID, int JobID, String guestfolder, String time) {

        String rcmd1 = "library(RSQLite); library(DBI); con <- dbConnect(SQLite(), \"/home/jobs/jobDB.sqlite\"); ";
        String rcmd2 = "dbSendQuery(con, \"INSERT INTO jobs (jobID, guestfolder, key, time) VALUES ("
                + JobID + ", \'" + guestfolder + "\', \'" + partialID + "\' , \'" + time + "\');\"); ";
        String rcmd3 = "dbDisconnect(con)";

        try {
            String rcmd = rcmd1 + rcmd2 + rcmd3;
            RC.eval(rcmd);
            return true;
        } catch (Exception rse) {
            LOGGER.error("recordPartialLinkinDocker", rse);
            return false;
        }
    }

    public static String checkParitialLinkinDocker(RConnection RC, String partialID, int JobID) {

        String rcmd1 = "library(RSQLite); library(DBI); con <- dbConnect(SQLite(), \"/home/jobs/jobDB.sqlite\"); ";
        String rcmd2 = "resx <- dbGetQuery(con, \"Select guestfolder from jobs where key=\'" + partialID + "\';\");  as.character(resx$guestfolder)[1]";
        String rcmd3 = "dbDisconnect(con)";

        try {
            String res;
            res = RC.eval(rcmd1 + rcmd2).asString();
            RC.eval(rcmd3);
            System.out.println("checkParitialLinkinDocker res ===> " + res);
            return res;
        } catch (Exception rse) {
            LOGGER.error("checkParitialLinkinDocker", rse);
            return "";
        }
    }

    public static int generateSLURMbash(RConnection RC, String anaType, String newcmds, String temp_path, String home_dir, String scriptPath, String sh4slurm) {
        try {
            String rCommand = "generateSLURMbash(\"" + anaType + "\", \"" + newcmds + "\", \"" + temp_path + "\", \"" + home_dir + "\", \"" + scriptPath + "\", " + sh4slurm + ")";
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }

        return 0;
    }

    public static String[] getOrgPathLbl(RConnection RC) {
        try {
            String[] names = RC.eval("GetOrgPathLbl(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getOrgPathLbl", e);
        }
        return null;
    }

    public static String[] getOrgPathVal(RConnection RC) {
        try {
            String[] names = RC.eval("GetOrgPathVal(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getOrgPathVal", e);
        }
        return null;
    }

    public static String[] getOrgMummichogLbl(RConnection RC) {
        try {
            String[] names = RC.eval("GetOrgMummichogLbl(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getOrgMummichogLbl", e);
        }
        return null;
    }

    public static String[] getOrgMummichogVal(RConnection RC) {
        try {
            String[] names = RC.eval("GetOrgMummichogVal(NA)").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getOrgMummichogVal", e);
        }
        return null;
    }

    public static boolean AsariPlanProcessDefine(RConnection RC, String usrNm, String fileNms,
            boolean ms1, ApplicationBean1 ab, String homedir, String param_str, String source_path) {

        String RString;
        String asariStr = "asari process " + param_str;
        // 

        if (ms1) {
            System.out.println("AsariPlanProcessDefine ==== ms1");
            String rCommandOpt = """
                                 data_folder_Sample <- 'upload/';
                                 plotSettings1 <- SetPlotParam(Plot=TRUE);
                                 mSet <- ImportRawMSData(path = data_folder_Sample, plotSettings = plotSettings1);
                                 """;

            RString = "print(getwd());" + rCommandOpt;
        } else {
            System.out.println("AsariPlanProcessDefine ==== ms2");
            String rCommandOpt = """
                                 data_folder_Sample <- 'upload/';
                                 plotSettings1 <- SetPlotParam(Plot=TRUE);
                                 mSet <- ImportRawMSData(path = data_folder_Sample, plotSettings = plotSettings1);
                                 """;

            RString = "print(getwd());" + rCommandOpt;
        }
        System.out.println("asariStr ==== asariStr ---> " + asariStr);

        if (ab.shouldUseScheduler()) {
            RDataUtils.createScriptAsariForScheduler(RC, usrNm, RString, asariStr, fileNms, source_path);
        } else {
            String bashPath = RCenter.getBashFullPath(RC);
            String rScriptHome = ab.getRscriptsHomePath();
            DataUtils.runRawSpecScriptAsari(RC, rScriptHome, bashPath, homedir, usrNm, RString, asariStr, fileNms);
        }

        return true;
    }

    public static int createScriptAsariForScheduler(RConnection RC, String guestName, String RString, String asari_str, String fileNms, String source_path) {
        try {

            RString = RString.replace("\"", "\'");
            //planString2 = planString2.replace("\"", "\'");
            fileNms = fileNms.replace("\"", "\'");
            String rCommand = "CreateRawRscript4AsariPro(\"" + guestName + "\", \"" + RString + "\", \"" + asari_str + "\",\"" + fileNms + "\",\"" + source_path + "\")";

            /* TODO: OUTPUT REAL R code*/
            if ("".equals(asari_str)) {

                String rCommand2 = "data_folder_Sample <- \"REPLACE WITH YOUR Data folder/\"";
                String rCommand3 = "param <- SetPeakParam(\"REPLACE WITH YOUR PARAM SETTINGS\")";
                //RCenter.recordRCommand(RC, rCommand1);
                RCenter.recordRCommand(RC, rCommand2);
                RCenter.recordRCommand(RC, rCommand3);

            } else {

            }

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("createScriptAsariForScheduler", e);
        }
        return 0;
    }

    public static int createMS2ScriptForScheduler(RConnection RC, String guestName, String planString, String mode) {
        try {
            planString = planString.replace("\"", "\'");
            System.out.println("===== createMS2ScriptForScheduler===== planString ---> " + planString);
            String rCommand = "CreateMS2RawRscriptPro(\"" + guestName + "\", \"" + planString + "\", \"" + mode + "\")";
            System.out.println("===== createMS2ScriptForScheduler===== planString ---> " + planString);
            /* TODO: OUTPUT REAL R code*/
            if ("".equals(mode)) {

            } else {

            }

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("createMS2ScriptForScheduler", e);
        }
        return 0;
    }

    public static int addFeatureToReport(RConnection RC, String id, String imageName) {
        try {
            String rCommand = "AddFeatureToReport(NA, \"" + id + "\",\"" + imageName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int addDoseFeatureToReport(RConnection RC, String id, String imageName) {
        try {
            String rCommand = "AddDoseFeatureToReport(NA, \"" + id + "\",\"" + imageName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int loadRscriptsOnDemand(RConnection RC, String module) {
        try {
            String rCommand = "LoadRscriptsOnDemand(\"" + module + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int setAnalType(RConnection RC, String analType) {
        try {
            String rCommand = "SetAnalType(NA,\"" + analType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;

    }

    public static String[] setSelectedMetaInfo(RConnection RC, String dataName, String meta0, String meta1, String block, boolean record, boolean execute) {
        try {
            String rCommand = "SetSelectedMetaInfo(\"" + dataName + "\",\"" + meta0 + "\", \"" + meta1 + "\", \"" + block + "\")";
            RCenter.recordRCommand(RC, rCommand);

            if (!execute) {
                return null;
            }
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            LOGGER.error("setSelectedMetaInfo", rse);
            return null;
        }

    }

    public static int recordspecjob2local(RConnection RC, String email, String jobID, String status, String folder) {
        String rCommand = "dt<-read.csv(\"/data/glassfish/projects/data/all_slurm_jobs.csv\", header = TRUE);"
                + "dt2 <- data.frame(jobid=" + jobID + ", emailed=FALSE, email='" + email + "', folder='" + folder + "', wfstatus='UNCOMPLETE'); "
                + "dt <- rbind(dt, dt2);"
                + "write.csv(dt, file = \"/data/glassfish/projects/data/all_slurm_jobs.csv\", row.names = FALSE)";
        try {
            RC.voidEval(rCommand);
        } catch (RserveException ex) {
            java.util.logging.Logger.getLogger(RDataUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return 0;
    }

    public static Map<String, Object> getSummaryNormResults(RConnection RC, String selectedContrast, String[] normOpts) {
        try {
            // 1) Call your R function to populate mSetObj$summaryNormResults
            RC.assign("selectedNormOpts", normOpts);

            String rCommand = "SummarizeNormResults(NA, \"" + selectedContrast + "\")";
            RC.eval(rCommand);

            // 2) Now retrieve mSetObj$summaryNormResults
            REXP result = RC.eval("mSet$summaryNormResults");

            if (result == null || result.isNull()) {
                System.err.println("mSet$summaryNormResults is null or empty.");
                return null;
            }

            // Check that it is an R list
            if (!result.isList()) {
                System.err.println("mSet$summaryNormResults is not a list structure in R.");
                return null;
            }

            // Convert to RList
            RList outerList = result.asList();
            String[] outerNames = outerList.keys(); // The keys are your normOpts

            Map<String, Object> summaryMap = new LinkedHashMap<>();

            // Loop through each normalization option
            for (int i = 0; i < outerNames.length; i++) {
                String normOpt = outerNames[i]; // e.g. "Log2", "VST", etc.
                REXP subVal = outerList.at(i);

                // Each 'subVal' should also be a list (the named list: numDE, pcaPermInfo, etc.)
                if (subVal != null && subVal.isList()) {
                    RList subList = subVal.asList();
                    String[] subNames = subList.keys();

                    Map<String, Object> detailMap = new LinkedHashMap<>();

                    for (int j = 0; j < subNames.length; j++) {
                        String key = subNames[j];  // e.g. "numDE", "pcaPermInfo"
                        REXP val = subList.at(j);

                        // Parse each field as needed
                        if (val == null || val.isNull()) {
                            detailMap.put(key, null);
                        } else if (val.isString()) {
                            // Single string
                            detailMap.put(key, val.asString());
                        } else if (val.isNumeric()) {
                            // Could be a single numeric or an array
                            // Check if length > 1 => numeric vector
                            if (val.length() == 1) {
                                detailMap.put(key, val.asDouble());
                            } else {
                                // store as an array of doubles
                                double[] arr = val.asDoubles();
                                detailMap.put(key, arr);
                            }
                        } else if (val.isVector()) {
                            // Could handle other vector types similarly
                            // e.g. parse as Strings, etc.
                            // For simplicity, store raw
                            detailMap.put(key, val);
                        } else {
                            // For other complex objects or lists, handle as needed
                            detailMap.put(key, val);
                        }
                    }

                    summaryMap.put(normOpt, detailMap);
                } else {
                    // If subVal is not a list, store it directly
                    summaryMap.put(normOpt, subVal);
                }
            }

            return summaryMap;
        } catch (Exception e) {
            System.err.println("Error in getSummaryNormResults: " + e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    public static String getFactors(RConnection RC) {
        try {
            String rCommand = "GetFactors(NA)";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getFactors", rse);
        }
        return null;
    }

    public static String getPrimaryInfo(RConnection RC) {
        try {
            String rCommand = "GetPrimaryInfo(NA)";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getPrimaryInfo", rse);
        }
        return null;
    }

    public static void getViewData(RConnection RC, String dataName) {
        try {
            String rCommand = "GetViewData(\"" + dataName + "\");";
            RC.voidEval(rCommand);

        } catch (Exception e) {
            LOGGER.error("updateOptiLCMS", e);
        }
        return;

    }

    public static String[] getMetaDataCol(RConnection RC, String colnm) {
        try {
            String rcmd = "GetMetaDataCol(NA, \"" + colnm + "\")";
            return RC.eval(rcmd).asStrings();
        } catch (Exception rse) {
            LOGGER.error("GetMetaDataCol", rse);
            return null;
        }
    }

    public static String[] getMetaTypes(RConnection RC, String colNm) {
        try {
            String[] names = RC.eval("GetMetaTypes(\"" + colNm + "\")").asStrings();
            return names;
        } catch (Exception e) {
            LOGGER.error("getMetaTypes", e);
        }
        return null;
    }

    public static String getImageDesc(RConnection RC, String type) {
        try {
            System.out.println("GetImageDesc(NA, \"" + type + "\")");
            return RC.eval("GetImageDesc(NA, \"" + type + "\")").asString();
        } catch (Exception rse) {
            LOGGER.error("getImageDesc", rse);
            return null;
        }
    }

    public static void updateRawJobStatusByFolder(RConnection RC, String folderName, String newStatus) {
        try {
            String dataPath = "/data/glassfish/projects/data/all_slurm_jobs.csv";

            // Ensure the file exists
            String checkCommand = "if(!file.exists(\"" + dataPath + "\")) {"
                    + "write.csv(data.frame(jobid=0, emailed=T, email='', folder='', wfstatus=''), "
                    + "file = \"" + dataPath + "\", row.names = FALSE)}";
            RC.voidEval(checkCommand);

            // Update status for the matching folder name
            String updateCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE);"
                    + "dt$wfstatus[dt$folder == \"" + folderName + "\"] <- \"" + newStatus + "\";"
                    + "write.csv(dt, file = \"" + dataPath + "\", row.names = FALSE)";
            RC.voidEval(updateCommand);

            System.out.println("Updated status for folder: " + folderName);
        } catch (RserveException e) {
            e.printStackTrace();
        }
    }

    public static boolean checkDetailsTablePerformed(RConnection RC, String type) {
        try {
            String rCommand = "CheckDetailsTablePerformed(\'" + type + "\')";
            //RCenter.recordRCommand(RC, rCommand);

            return RC.eval(rCommand).asString().equals("TRUE");
        } catch (RserveException rse) {
            LOGGER.error("CheckDetailsTablePerformed", rse);
        } catch (REXPMismatchException ex) {
            java.util.logging.Logger.getLogger(RDataUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    public static boolean readTextDataDoseWithMeta(RConnection RC, String filePath, String metaPath, String format, String lblType) {
        try {
            String rCommand = "Read.TextDataDoseWithMeta(NA, \"" + filePath + "\", \"" + metaPath + "\", \"" + format + "\", \"" + lblType + "\");";
            String rCommand2 = "Read.TextDataDoseWithMeta(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + "Replacing_with_your_metadata_file_path" + "\", \"" + format + "\", \"" + lblType + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            LOGGER.error("readTextDataDose", rse);
            return false;
        }
    }

    public static String[] getEnrResColNames(RConnection RC, String enrType) {
        try {
            String rCommand = "GetEnrResultColNames(\"" + enrType + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getEnrResSetIDs(RConnection RC, String enrType) {
        try {
            String rCommand = "GetEnrResSetIDs(\"" + enrType + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getEnrResSetNames(RConnection RC, String enrType) {
        try {
            String rCommand = "GetEnrResSetNames(\"" + enrType + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMatchIntegNames(RConnection RC) {
        try {
            String rCommand = "GetMatchIntegNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMatchIntegColNames(RConnection RC) {
        try {
            String rCommand = "GetMatchIntegColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMatchIntegSetIDs(RConnection RC, int inx) {
        try {
            String rCommand = "GetMatchIntegSetIDs(" + inx + ")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getSetIDLinks(RConnection RC, String type) {
        try {
            String rCommand = "GetSetIDLinks(\"" + type + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getEnrResMatrix(RConnection RC, String enrType) {
        try {
            String rCommand = "GetEnrResultMatrix(\"" + enrType + "\")";
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getEnrHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetEnrHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            LOGGER.error("GetEnrHTMLPathSet", e);
        }
        return null;
    }

    public static String getEnrFunType(RConnection RC, String key) {
        try {
            String rCommand = "GetEnrFunType(\"" + key + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getEnrFunType", rse);
        }
        return null;
    }

    public static String getEnrQueryType(RConnection RC, String key) {
        try {
            String rCommand = "GetEnrQueryType(\"" + key + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getEnrQueryType", rse);
        }
        return null;
    }

    public static int plotMetaPCA(SessionBean1 sb, RConnection RC, String pcaName, int dpi, String type) {
        try {
            String rCommand = "PlotMetaPCA(NA,\"" + pcaName + "\",\"" + dpi + "\", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);

            sb.addGraphicsCMD("qc_meta_pca", rCommand);
            sb.addGraphicsMapLink("qc_meta_pca", "/Secure/metastat/MetaQCView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("plotMetaPCA", e);
        }
        return 0;
    }

    public static int plotMetaDensity(SessionBean1 sb, RConnection RC, String densityName, int dpi, String type) {
        try {
            String rCommand = "PlotMetaDensity(NA,\"" + densityName + "\",\"" + dpi + "\", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);

            sb.addGraphicsCMD("qc_meta_density", rCommand);

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            LOGGER.error("plotMetaDensity", e);
        }
        return 0;
    }

    public static String getMetaProcMsg(RConnection RC) {
        try {
            String rCommand = "GetMetaProcMsg()";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getMetaProcMsg", rse);
        }
        return null;
    }

    public static void toggleMetaRes(RConnection RC, String type) {
        try {
            String rCommand = "ToggleMetaRes(NA,\"" + type + "\");";
            RC.voidEval(rCommand);

        } catch (Exception e) {
            LOGGER.error("toggleMetaRes", e);
        }
        return;

    }
}
