package pro.metaboanalyst.utils;

import org.rosuda.REngine.Rserve.RConnection;
//import pro.metaboanalyst.agents.RPlotCustomizationAgent;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.utils.JavaRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RDataUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(RDataUtils.class);

    public static void plotNormSummaryGraphAI(SessionBean1 sb,
            String imgName,
            String format,
            int dpi,
            String userRequest) {
        try {
            RConnection RC = sb.getRConnection();

            // 1) Get the AI customizer and customize the plot based on user request
            //RPlotCustomizationAgent aiCustomizer = new RPlotCustomizationAgent();
            //String customizedFunction = aiCustomizer.customizePlot("PlotNormSummary", userRequest);

            // 2) Write the customized function to a temporary file
            String tempFile = "PlotNormSummaryAI.R";
            String rCommand = String.format(
                "source('%s')", tempFile
            );

            // 3) Record & dispatch
            System.out.println("[R CMD] " + rCommand);
            RCenter.recordRCommand(RC, rCommand);
            JavaRecord.recordRCommandFunctionInfo(RC, rCommand, "Normalization");
            sb.addGraphicsCMD("norm", rCommand);
            sb.addGraphicsMapLink("norm", "/Secure/process/NormalizationView.xhtml");

            // 4) Execute the plot with the customized function
            String plotCommand = String.format(
                "PlotNormSummaryAI(NA, \"%s\", \"%s\", %d, width=NA)",
                imgName, format, dpi
            );
            RC.voidEval(plotCommand);

        } catch (Exception e) {
            LOGGER.error("plotNormSummaryGraphAI", e);
        }
    }
} 