/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import pro.metaboanalyst.workflows.JavaRecord;

/**
 *
 * @author Jeff
 */
public class Clustering {

    public static void plotClustTree(SessionBean1 sb, String imgName, String format, int dpi, String smplDist, String clstDist) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotHCTree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + smplDist + "\", \"" + clstDist + "\")";
            sb.addGraphicsCMD("tree", rCommand);
            sb.addGraphicsMapLink("tree", "/Secure/analysis/TreeView.xhtml");

            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Dendrogram");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String dataOpt, String scaleOpt,
            String smplDist, String clstDist, String colors, int fzCol, int fzRow, double annoFz, double annoHeight,
            int unitCol, int unitRow, String rowV, String colV, String drawBorder, String grpAve, String showLegend,
            String showAnnotLegend, String showColNames, String showRowNames, int maxFeatureNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotHeatMap(NA" + ", \"" + imgName + "\", \""
                    + format + "\", " + dpi + ", width=NA, \"" + dataOpt + "\", \"" + scaleOpt + "\", \""
                    + smplDist + "\", \"" + clstDist + "\",\"" + colors + "\", " + fzCol + "," + fzRow + ", " + annoFz + "," + annoHeight + "," 
                    + unitCol + ", " + unitRow + ", " + rowV + ", " + colV + ", NULL, " + drawBorder + ", " + grpAve + ", " 
                    + showLegend + ", " + showAnnotLegend + ",  "
                    + showColNames + "," + showRowNames + "," + maxFeatureNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "Heatmap");

            sb.addGraphicsCMD("heatmap", rCommand);
            sb.addGraphicsMapLink("heatmap", "/Secure/analysis/HeatmapView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSubHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String dataOpt, String scaleOpt, String smplDist,
            String clstDist, String colors, int fzCol, int fzRow, double annoFz, double annoHeight, int unitCol, int unitRow, String method, int num, String rowV, String colV, String drawBorder, String grpAve, String showLegend, String showAnnotLegend, String showColNames, String showRowNames) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSubHeatMap(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + dataOpt + "\", \"" + scaleOpt + "\", \"" + smplDist + "\", \"" + clstDist + "\",\"" + colors + "\", " + fzCol + "," + fzRow + ", " + annoFz + "," + annoHeight + "," + unitCol + ", " + unitRow + ", \"" + method + "\", " + num + ", " + rowV + ", " + colV + ", " + drawBorder + ", " + grpAve + ", " + showLegend + ", " + showAnnotLegend + ", " + showColNames + "," + showRowNames + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("heatmap", rCommand);
            sb.addGraphicsMapLink("heatmap", "/Secure/analysis/HeatmapView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotKmeans(SessionBean1 sb, String imgName, String format, int dpi, int clustNum, String colPal, String kmFacet) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Kmeans.Anal(NA" + ", " + clustNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "K-means");

            String rCommand2 = "PlotKmeans(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + colPal + "\", \"" + kmFacet + "\")";
            RCenter.recordRCommand(RC, rCommand2);
            sb.recordRCommandFunctionInfo(rCommand2, "K-means");

            sb.addGraphicsCMD("km", rCommand2);
            sb.addGraphicsMapLink("km", "/Secure/analysis/KMView.xhtml");

            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotKmeansPCA(SessionBean1 sb, String imgName, String format, int dpi, String colPal, String label) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotClustPCA(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + colPal + "\", \"km\", \"" + label + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "K-means");

            sb.addGraphicsCMD("km_pca", rCommand);
            sb.addGraphicsMapLink("km_pca", "/Secure/analysis/KMView.xhtml");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int getKMClusterNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("length(mSet$analSet$kmeans$size)").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String getKMClusterMembers(SessionBean1 sb, int clNo) {
        try {
            String rCommand = "GetKMClusterMembers(NA" + ", " + clNo + ")";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String getClassLabel(SessionBean1 sb, int inx) {
        try {
            String rCommand = "GetClassLabel(NA" + ", " + inx + ")";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void plotSOM(SessionBean1 sb, String imgName, String format, int dpi, int clx, int cly, String init, String nb, String colPal, String somFacet) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SOM.Anal(NA" + ", " + clx + "," + cly + ",\"" + init + "\"," + "\"" + nb + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.recordRCommandFunctionInfo(rCommand, "SOM");

            String rCommand2 = "PlotSOM(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + colPal + "\", \"" + somFacet + "\")";
            RCenter.recordRCommand(RC, rCommand2);
            sb.addGraphicsCMD("som", rCommand2);
            sb.addGraphicsMapLink("som", "/Secure/analysis/SOMView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand2, "SOM");

            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotSOMPCA(SessionBean1 sb, String imgName, String format, int dpi, String colPal, String label) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotClustPCA(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + colPal + "\", \"som\", \"" + label + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("som_pca", rCommand);
            sb.addGraphicsMapLink("som_pca", "/Secure/analysis/SOMView.xhtml");

            sb.recordRCommandFunctionInfo(rCommand, "SOM");

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int getSOMXdimension(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("mSet$analSet$som$xdim").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int getSOMYdimension(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("mSet$analSet$som$ydim").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String getSOMClusterMembers(SessionBean1 sb, int xord, int yord) {
        try {
            String rCommand = "GetSOMClusterMembers(NA" + ", " + xord + "," + yord + ")";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
}
