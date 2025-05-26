/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import java.util.Arrays;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RserveException;
import pro.metaboanalyst.controllers.dose.DoseResponseModel;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author soufanom
 */
public class RDoseUtils {

    private static final Logger LOGGER = LogManager.getLogger(DataUtils.class);

    public static int performDoseDEAnal(RConnection RC) {
        try {
            //System.out.println("DE22==============");
            //String robustTrend = robustTrendBool ? "T" : "F";

            String rCommand = "PerformDoseDEAnal();";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "DE Analysis");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("performDoseDEAnal", rse);
            return 0;
        }
    }

    public static int prepareSignificantItems(RConnection RC, double sigLevel, double fcLevel, boolean fdrFilterPass, boolean wtTest, double wtSigLevel) {
        try {
            // Convert Java boolean to R boolean representation
            String fdrFilterPassR = fdrFilterPass ? "TRUE" : "FALSE";
            String wtTestR = wtTest ? "TRUE" : "FALSE";

            String rCommand = "PrepareSigDRItems(NA, " + sigLevel + "," + fcLevel + "," + fdrFilterPassR + "," + wtTestR + ", " + wtSigLevel + ");";
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            System.out.println(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int prepareDataForDoseResponse(RConnection RC) {
        try {
            String rCommand = "PrepareDataForDoseResponse(NA);";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int prepareBmdEnrich(RConnection RC, String fileNm, String scale, String geneDB, String organism) {
        try {
            String rCommand = "PreparePODJSON(fileNm = \"" + fileNm + "\", doseScale = \"" + scale + "\", geneDB = \"" + geneDB + "\", org = \"" + organism + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performModelFit(RConnection RC, String[] models) {
        try {
            RC.assign("models", models);
            String rCommand = "PerformDRFit(NA);";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performAPIModelFit(RConnection RC, String[] models, String cpus) {
        try {
            RC.assign("models", models);
            RC.assign("cpus", cpus);
            String rCommand = "PerformAPIDRFit();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int plotDRModelBar(SessionBean1 sb, RConnection RC, String plotName, int dpi, String type) {
        try {
            String rCommand = "PlotDRModelBars(NA, \"" + plotName + "\", " + dpi + ", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            sb.addGraphicsCMD("dr_barplot", rCommand);
            sb.addGraphicsMapLink("dr_barplot", "/Secure/dose/ModelFitView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int plotDoseVolcano(SessionBean1 sb, RConnection RC, String plotName, int dpi, String type) {
        try {
            String rCommand = "PlotDoseVolcano(NA, \"" + plotName + "\", " + dpi + ", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Sig. analysis");

            sb.addGraphicsCMD("dose_volcano", rCommand);
            sb.addGraphicsMapLink("dose_volcano", "/Secure/dose/SigFeatureView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int plotDRHistogram(SessionBean1 sb, RConnection RC, String plotName, int dpi, String type, String units, String scale) {
        try {
            String rCommand = "PlotDRHistogram(NA, \"" + plotName + "\", " + dpi + ", \"" + type + "\", \"" + units + "\", \"" + scale + "\");";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            sb.addGraphicsCMD("dr_histogram", rCommand);
            sb.addGraphicsMapLink("dr_histogram", "/Secure/dose/ModelFitView.xhtml");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getDRFitResAdverseTypeColumn(RConnection RC) {
        try {
            String rCommand = "GetFitResultAdverseType()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDRFitResModelNmColumn(RConnection RC) {
        try {
            String rCommand = "GetFitResultModelNms()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDRFitResColNames(RConnection RC) {
        try {
            String rCommand = "GetFitResultColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int computeDoseLimma(RConnection RC, double pThresh, double fcLevel, boolean fdrBool) {
        try {

            String rCommand = "ComputeDoseLimmaResTable(NA, " + pThresh + ", " + fcLevel + ", \"" + fdrBool + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static double[][] getDoseDEMat(RConnection RC) {
        try {
            String rCommand = "GetDoseDEMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getDoseDnMat(RConnection RC) {
        try {
            String rCommand = "GetDoseDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            //System.out.println(rse);
        }
        return null;
    }

    public static String[] getDoseDnIDs(RConnection RC) {
        try {
            String rCommand = "GetDoseDnIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getDoseUpMat(RConnection RC) {
        try {
            String rCommand = "GetDoseUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDoseUpIDs(RConnection RC) {
        try {
            String rCommand = "GetDoseUpIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getDoseUnsigMat(RConnection RC) {
        try {
            String rCommand = "GetDoseUnsigMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDoseUnsigIDs(RConnection RC) {
        try {
            String rCommand = "GetDoseUnsigIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getDRFitResMatrix(RConnection RC) {
        try {
            String rCommand = "GetFitResultMatrix();";
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String plotSelectedFeatureDRCurve(SessionBean1 sb, String geneId, String geneName, String model,
            double b, double c, double d, double e, double Bmdl, double Bmd, double Bmdu, String transDose, int dpi, String format) {
        try {
            String rCommand = "PlotMetaboliteDRCurve(NA, \"" + geneId + "\",\"" + geneName + "\",\"" + model + "\"," + b + "," + c + "," + d + "," + e + "," + Bmdl + "," + Bmd + "," + Bmdu + ",\"" + transDose + "\"," + dpi + ",\"" + format + "\")";
            RCenter.recordRCommand(sb.getRConnection(), rCommand);
            sb.addGraphicsCMD("Metabolite_" + geneId + "_" + model, rCommand);
            sb.addGraphicsMapLink("Metabolite_" + geneId + "_" + model, "/Secure/dose/ModelFitView.xhtml");

            //System.out.println("Metabolite_" + geneId + "======cmd");
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void recordPlotSelectedFeatureDRCurve(SessionBean1 sb, String geneId, String geneName, String model,
            double b, double c, double d, double e, double Bmdl, double Bmd, double Bmdu, String transDose, int dpi, String format) {
        try {
            String rCommand = "PlotMetaboliteDRCurve(NA, \"" + geneId + "\",\"" + geneName + "\",\"" + model + "\"," + b + "," + c + "," + d + "," + e + "," + Bmdl + "," + Bmd + "," + Bmdu + ",\"" + transDose + "\"," + dpi + ",\"" + format + "\")";
            //RCenter.recordRCommand(sb.getRConnection(), rCommand);
            sb.addGraphicsCMD("Metabolite_" + geneId + "_" + model, rCommand);
            sb.addGraphicsMapLink("Metabolite_" + geneId + "_" + model, "/Secure/dose/ModelFitView.xhtml");

            //System.out.println("Metabolite_" + geneId + "======cmd");
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static double[][] getDRItemSelectData(RConnection RC) {
        try {
            String rCommand = "GetItemSelectData(NA)";
            double[][] data = RC.eval(rCommand).asDoubleMatrix();
            return data;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getDRItemSelectDose(RConnection RC) {
        try {
            String rCommand = "GetItemSelectDose(NA)";
            double[] dose = RC.eval(rCommand).asDoubles();
            return dose;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getDRItemSelectDataMean(RConnection RC) {
        try {
            String rCommand = "GetItemSelectDataMean(NA)";
            double[][] data = RC.eval(rCommand).asDoubleMatrix();
            return data;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDRItemSelectDataMeanColNms(RConnection RC) {
        try {
            String rCommand = "GetItemSelectDataMeanColNms(NA)";
            String[] colNms = RC.eval(rCommand).asStrings();
            return colNms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDRItemSelectItems(RConnection RC) {
        try {
            String rCommand = "GetItemSelectItems(NA)";
            String[] items = RC.eval(rCommand).asStrings();
            return items;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int prepareDrcFitRObject(RConnection RC, DoseResponseModel model) {
        try {

            RC.assign("drc.res", REXP.createDoubleMatrix(model.getDrcRes()));
            RC.assign("drc.res.colnms", model.getDrcResColNms());
            RC.assign("drc.res.modelnms", model.getDrcModelNms());
            RC.assign("drc.res.invstatus", model.getDrcInvStatus());
            RC.assign("drc.res.gene.ids", model.getDrcResGeneIds());
            String rCommand = "InitDrcFitObj();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static int performBMDAnal(RConnection RC, String bmdOption, String numsds, String ctrlMode) {
        try {
            RC.assign("bmd.pass.option", bmdOption);
            RC.assign("num.sds", numsds);
            RC.assign("ctrl.mode", ctrlMode);
            String rCommand = "PerformBMDCalc(NA);";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getDRRes(RConnection RC) {
        try {
            String rCommand = "GetDRRes();";
            String[] res = RC.eval(rCommand).asStrings();
            return res;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getDRFitResFeatureIDs(RConnection RC) {
        try {
            String rCommand = "GetFitResultFeatureIDs()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int performDrcResFilter(RConnection RC, String option, String pval, String[] models) {
        try {
            RC.assign("fit.select", option);
            RC.assign("lof.pval", pval);
            RC.assign("models", models);
            String rCommand = "FilterDRFit(NA);";
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Curve Fitting");

            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static int prepareItemSelect(RConnection RC, DoseResponseModel model) {
        try {

            RC.assign("data.select", REXP.createDoubleMatrix(model.getData()));
            RC.assign("data.mean", REXP.createDoubleMatrix(model.getDataMean()));
            RC.assign("dose", model.getDose());
            RC.assign("item", model.getItems());
            RC.assign("data.mean.colnames", model.getDataMeanColNms());
            String rCommand = "InitItemSelect();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static int doDREnrichmentTest(RConnection RC, String scale, double xMin, double xMax, String fileNm, String libNm, String organism) {
        try {
            String rCommand = "PreparePODJSON(fileNm = \"" + fileNm + "\", doseScale = \"" + scale + "\", xMin = " + xMin + ", xMax = " + xMax + ", geneDB = \"" + libNm + "\", org = \"" + organism + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int getNumDoses(RConnection RC) {
        try {
            String rCommand = "GetNumberDoses();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getDoseDERows(RConnection RC) {
        try {
            String rCommand = "GetDoseDERows();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static String[] getDoseDEColumns(RConnection RC) {
        try {
            String rCommand = "GetDoseDEColumns();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static int performContModelFit(RConnection RC, String[] models) {
        try {
            RC.assign("models", models);
            RCenter.recordRCommand(RC, "models <<- " + Arrays.toString(models));
            String rCommand = "PerformContDRFit(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performDrcResFilterCont(RConnection RC, String option, String pval, String[] models) {
        try {
            RC.assign("fit.select", option);
            RC.assign("lof.pval", pval);
            RC.assign("models", models);
            RCenter.recordRCommand(RC, "fit.select <<- \"" + option + "\"");
            RCenter.recordRCommand(RC, "lof.pval <<- \"" + pval + "\"");
            RCenter.recordRCommand(RC, "models <<- " + Arrays.toString(models));
            String rCommand = "FilterDRFitCont(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            System.out.println(e);
        }
        return 0;
    }

    public static int performDoseDEAnal(RConnection RC, String analysisMeta, String[] adjustedVar) {
        try {
            //System.out.println("DE22==============");
            //String robustTrend = robustTrendBool ? "T" : "F";
            RC.assign("adj.vec", adjustedVar);
            RCenter.recordRCommand(RC, "adj.vec <- " + DataUtils.convertArrayToVecInR(adjustedVar));
            String rCommand = "PerformDoseDEAnal(NA,\"" + analysisMeta + "\");";
            RCenter.recordRCommand(RC, rCommand);

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            LOGGER.error("performDoseDEAnal", rse);
            return 0;
        }
    }

    public static int computeContDoseLimma(RConnection RC, double pThresh, double coef_thresh, boolean fdrBool) {
        try {

            String rCommand = "ComputeContDoseLimmaResTable(NA, " + pThresh + ", " + coef_thresh + ", \"" + fdrBool + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performContBMDAnal(RConnection RC, String bmdOption, String numsds, String ctrlMode) {
        try {
            RC.assign("bmd.pass.option", bmdOption);
            RC.assign("num.sds", numsds);
            RC.assign("ctrl.mode", ctrlMode);
            RCenter.recordRCommand(RC, "bmd.pass.option <<- \"" + bmdOption + "\"");
            RCenter.recordRCommand(RC, "num.sds <<- \"" + numsds + "\"");
            RCenter.recordRCommand(RC, "ctrl.mode <<- \"" + ctrlMode + "\"");
            String rCommand = "PerformContBMDCalc(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }
}
