/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author Jeff
 */
public class SigVarSelect {

    public static int initSAM(SessionBean1 sb, String method, String paired, String equalVar, double delta, String imgName) {
        try {
            //Note: paried and equalVar mimic T and F, should not quote
            String rCommand = "SAM.Anal(NA, \"" + method + "\", " + paired + ", " + equalVar + ", " + delta + ", \"" + imgName + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "SAM");

            //No plotting, for record only
            String rCommand2 = "PlotSAM.Cmpd(NA" + ", \"" + imgName + "\", \"png\", 72, width=NA)";
            sb.addGraphicsCMD("sam_imp", rCommand2);
            sb.addGraphicsMapLink("sam_imp", "/Secure/analysis/SAMView.xhtml");

            RCenter.recordRCommand(RC, rCommand2);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand2, "SAM");

            //use microservice
            rCommand = ".prepare.sam.anal(NA, \"" + method + "\", " + paired + ", " + equalVar + ", " + delta + ", \"" + imgName + "\", 72)";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            rCommand = ".save.sam.anal(NA)";
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "SAM");

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static void plotSAM_FDR(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotSAM.FDR(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("sam_view", rCommand);
            sb.addGraphicsMapLink("sam_view", "/Secure/analysis/SAMView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "SAM");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //plot SAM image and set up sig.cmpd object so that SAM member page can use
    public static int plotSAM_Cmpd(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSAM.Cmpd(NA, \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            sb.addGraphicsCMD("sam_imp", rCommand);
            sb.addGraphicsMapLink("sam_imp", "/Secure/analysis/SAMView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            //use microservice
            rCommand = ".prepare.sam.cmpd(NA, \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            return 1;
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static String[] getSAMSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMSigRowNames(NA)";
            String[] names = sb.getRConnection().eval(rCommand).asStrings();
            return names;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getSAMSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMSigColNames(NA)";
            String[] names = sb.getRConnection().eval(rCommand).asStrings();
            return names;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getSAMSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMSigMat(NA)";
            double[][] sigvals = sb.getRConnection().eval(rCommand).asDoubleMatrix();
            return sigvals;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double getSAMSuggestedDelta(SessionBean1 sb) {
        try {
            String rCommand = "GetSuggestedSAMDelta(NA)";
            double delta = sb.getRConnection().eval(rCommand).asDouble();
            return delta;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static double[] getSAMDeltaRange(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMDeltaRange(NA)";
            double[] deltas = sb.getRConnection().eval(rCommand).asDoubles();
            return deltas;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int initEBAM(SessionBean1 sb, String paired, String equalVar, String nonPar, double A0, double delta, String imgA0, String imgSig) {
        try {
            String rCommand = "EBAM.Init(NA, " + paired + ", " + equalVar + ", " + nonPar + ", " + A0 + ", " + delta + ", \"" + imgA0 + "\", \"" + imgSig + "\", " + 72 + ")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "EBAM");
            sb.addGraphicsCMD("ebam_view", rCommand);
            sb.addGraphicsMapLink("ebam_view", "/Secure/analysis/EBAMView.xhtml");

            String rCommand2 = "PlotEBAM.Cmpd(NA" + ", \"" + imgSig + "\", \"png\", 72, width=NA)";
            RCenter.recordRCommand(RC, rCommand2);
            sb.addGraphicsCMD("ebam_imp", rCommand2);
            sb.addGraphicsMapLink("ebam_imp", "/Secure/analysis/EBAMView.xhtml");

            JavaRecord.recordRCommandFunctionInfo(RC, rCommand2, "EBAM");

            //use microservice
            rCommand = ".prepare.ebam.init(NA, " + paired + ", " + equalVar + ", " + nonPar + ", " + A0 + ", " + delta + ", \"" + imgA0 + "\", \"" + imgSig + "\", " + 72 + ")";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            rCommand = ".save.ebam.init(NA)";

            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    //plot EBAM images and set up sig.cmpd object so the member page can use 
    public static int plotEBAM_Cmpd(SessionBean1 sb, String imgName, String format, int dpi, double delta) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand2 = "PlotEBAM.Cmpd(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand2);
            sb.addGraphicsCMD("ebam_imp", rCommand2);
            sb.addGraphicsMapLink("ebam_imp", "/Secure/analysis/EBAMView.xhtml");

            //use microservice
            String rCommand = ".prepare.ebam.cmpd(NA, \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            if (RC.eval(rCommand).asInteger() == 1) {
                RCenter.performRserveMicro(sb.getCurrentUser().getHomeDir());
            }
            return 1;
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static double getEBAMSuggestedA0(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSuggestedA0(NA)";
            return sb.getRConnection().eval(rCommand).asDouble();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String[] getEBAMSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getEBAMSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getEBAMSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }
}
