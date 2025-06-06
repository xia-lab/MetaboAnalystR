/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author jianguox
 */
public class PowerUtils {

    public static void initPowerAnal(RConnection RC, String clsOpts) {
        try {
            String rCommand = "InitPowerAnal(NA" + ", \"" + clsOpts + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double performPowerProfile(RConnection RC, double fdr, int smplSize) {
        try {
            String rCommand = "PerformPowerProfiling(NA" + ", " + fdr + ", " + smplSize + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asDouble();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static void plotPowerStatDiagnostics(SessionBean1 sb, String imgNm, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPowerStat(NA" + ", \"" + imgNm + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("power_stat", rCommand);
            sb.addGraphicsMapLink("power_stat", "/Secure/utils/PowerCurveView.xhtml");

            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static double[] plotPowerProfile(SessionBean1 sb, double fdr, int smplSize, String imgNm, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPowerProfile(NA" + ", " + fdr + ", " + smplSize + ", \"" + imgNm + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("power_profile", rCommand);
            sb.addGraphicsMapLink("power_profile", "/Secure/utils/PowerCurveView.xhtml");

            return RC.eval(rCommand).asDoubles();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int[] getPowerValueX(RConnection RC) {
        try {
            String rCommand = "GetPowerValuesX(NA)";
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }
}
