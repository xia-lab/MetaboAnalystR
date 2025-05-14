/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.controllers.general.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author Jeff
 */
public class RGraphUtils {

    private static final Logger LOGGER = LogManager.getLogger(RGraphUtils.class);

    public static String plotKEGGPath(SessionBean1 sb, String pathName, String width, String height, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            sb.setCurrentPathName(pathName);
            String rCommand = "PlotKEGGPath(NA" + ", \"" + pathName + "\"," + width + ", " + height + ", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("plot_kegg_graph", rCommand);
            //sb.addGraphicsMapLink("plot_kegg_graph", "/Secure/mummichog/GseaResultView.xhtml");

            String name = RC.eval(rCommand).asString();
            return name;
        } catch (Exception rse) {
            LOGGER.error("plotKEGGPath", rse);
            return null;
        }
    }

    public static void plotPathSummary(SessionBean1 sb, String showGrid, String imgName, String format, int dpi, double xlim, double ylim, String nextpage) {
        try {
            RConnection RC = sb.getRConnection();
            String xlimValue;
            String ylimValue;
            if (xlim == 0) {
                xlimValue = "NA";
            } else {
                xlimValue = xlim + "";
            }
            if (ylim == 0) {
                ylimValue = "NA";
            } else {
                ylimValue = ylim + "";
            }
            String rCommand = "PlotPathSummary(NA, " + showGrid + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, "
                    + xlimValue + ", " + ylimValue + " )";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("path_view", rCommand);
            if (nextpage.equals("smpdbpathview")) {
                sb.addGraphicsMapLink("path_view_smpdbpathview", "/Secure/pathway/SMPDBResultView.xhtml");
            } else if (nextpage.equals("integview")) {
                sb.addGraphicsMapLink("path_view_integview", "/Secure/pathinteg/IntegResultView.xhtml");
            } else {
                sb.addGraphicsMapLink("path_view_pathview", "/Secure/pathway/PathResultView.xhtml");
            }
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("plotPathSummary", rse);
        }
    }

    public static boolean drawMetPAGraph(RConnection RC, String imgName, double width, double height, double zoomPercent) {
        try {
            String rCommand = "RerenderMetPAGraph(NA" + ", \"" + imgName + "\"," + width + ", " + height + ", " + zoomPercent + ")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return (res == 1);
        } catch (Exception rse) {
            LOGGER.error("drawMetPAGraph", rse);
        }
        return false;
    }

    public static String getOverviewImgMap(RConnection RC) {
        try {
            String rCommand = "GetCircleInfo(NA)";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            LOGGER.error("getOverviewImgMap", rse);
            return null;
        }
    }

    public static String getCurrentImg(RConnection RC) {
        try {
            return RC.eval("GetCurrentImg(NA)").asString();
        } catch (Exception rse) {
            LOGGER.error("getCurrentImg", rse);
            return null;
        }
    }

    public static String getConvertFullPath(RConnection RC) {
        try {
            return RC.eval("GetConvertFullPath()").asString();
        } catch (Exception rse) {
            LOGGER.error("getConvertFullPath", rse);
            return null;
        }
    }

}
