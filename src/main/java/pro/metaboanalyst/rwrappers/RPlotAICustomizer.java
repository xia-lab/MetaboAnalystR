package pro.metaboanalyst.rwrappers;

import pro.metaboanalyst.llm.GoogleAIClient;
import java.io.IOException;
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class that uses AI to customize R plot parameters and generate
 * optimal visualization settings.
 */
public class RPlotAICustomizer implements Serializable {

    private static final Logger LOGGER = Logger.getLogger(RPlotAICustomizer.class.getName());
    private final GoogleAIClient aiClient;

    public RPlotAICustomizer() throws IOException {
        this.aiClient = new GoogleAIClient();
    }

    /**
     * Generates AI-customized parameters for the normalization summary plot.
     * The AI analyzes the data context and suggests optimal visualization
     * parameters.
     *
     * @param dataContext Information about the data being plotted
     * @return R code string with customized parameters
     */
    public String customizeNormSummaryPlot(String dataContext) {
        try {
            String prompt = String.format(
                    "You’re generating styling overrides for the R function `PlotNormSummaryAI`.  Given this data context:\n\n%s\n\n"
                    + "Produce only valid R assignment statements (separated by semicolons) that set any of the following parameters:\n"
                    + "  density_col      <- <color string>\n"
                    + "  boxplot_col      <- <color string>\n"
                    + "  density_lwd      <- <numeric line width>\n"
                    + "  boxplot_lwd      <- <numeric line width>\n"
                    + "  label_cex        <- <numeric font scaling>\n"
                    + "  title_cex        <- <numeric title scaling>\n"
                    + "  margin_mar       <- c(<bottom>, <left>, <top>, <right>)\n\n"
                    + "Optionally, you can also suggest a `width=…` value to adjust figure dimensions.  \n"
                    + "Return your answer as a single comma-separated string of assignments, with no extra text.",
                    dataContext
            );

            String aiResponse = aiClient.generateText(prompt);
            System.out.println("aiResponse: " + aiResponse);
            return aiResponse;
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error generating AI customization", e);
            return ""; // Return empty string to fall back to default parameters
        }
    }

    /**
     * Extracts R parameters from the AI response. This method parses the AI
     * response to extract valid R code parameters.
     */
    private String parseAIResponse(String aiResponse) {
        // Extract the R code block
        Pattern pattern = Pattern.compile("```R\\s*(.*?)\\s*```", Pattern.DOTALL);
        Matcher matcher = pattern.matcher(aiResponse);

        if (matcher.find()) {
            String rCode = matcher.group(1).trim();

            // Convert the R list format to individual parameter assignments
            StringBuilder params = new StringBuilder();

            // Split by lines and process each parameter
            String[] lines = rCode.split("\n");
            for (String line : lines) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) {
                    continue;
                }

                // Remove trailing comma if present
                if (line.endsWith(",")) {
                    line = line.substring(0, line.length() - 1);
                }

                // Extract parameter name and value
                String[] parts = line.split("=", 2);
                if (parts.length == 2) {
                    String paramName = parts[0].trim();
                    String value = parts[1].trim();

                    // Remove comments from value
                    int commentIndex = value.indexOf("#");
                    if (commentIndex > 0) {
                        value = value.substring(0, commentIndex).trim();
                    }

                    // Add the parameter assignment
                    params.append(paramName).append(" <- ").append(value).append("\n");
                }
            }
            System.out.println(params.toString());
            return params.toString();
        }

        return "";
    }

    /**
     * Customizes a plot based on the plot type and user prompt
     *
     * @param plotType The type of plot to customize
     * @param prompt The user's customization instructions
     * @return The R code parameters for customization
     * @throws IOException If there's an error communicating with the AI service
     */
    public String customizePlot(String plotType, String prompt) throws IOException {
        // Get data context based on plot type
        String dataContext = getDataContext(plotType);

        // Construct the prompt for the AI
        String fullPrompt = String.format(
                "I need to customize a %s plot in R. Here's the data context:\n%s\n\n"
                + "User's customization request: %s\n\n"
                + "Please provide R code parameters for customization in this format:\n"
                + "```R\n"
                + "list(\n"
                + "  color_scheme = \"viridis\", # Or \"magma\", \"plasma\", \"cividis\" for perceptually uniform color schemes\n"
                + "  fig_width = 10,  # Adjust as needed based on screen resolution and number of samples\n"
                + "  fig_height = 7, # Adjust as needed\n"
                + "  title_size = 14,\n"
                + "  axis_label_size = 12,\n"
                + "  axis_tick_size = 10,\n"
                + "  boxplot_fill_color = \"lightgray\", # Or a specific color like \"#E0E0E0\"\n"
                + "  boxplot_line_color = \"black\",\n"
                + "  density_line_color = \"blue\",  # Or a color from the viridis palette for consistency\n"
                + "  density_fill_color = \"lightblue\", # Or a color from the viridis palette\n"
                + "  density_alpha = 0.5 # Transparency of the density fill\n"
                + ")\n"
                + "```\n"
                + "Please provide only the R code block with the parameters.",
                plotType, dataContext, prompt
        );

        // Get AI response
        String aiResponse = aiClient.generateText(fullPrompt);
        System.out.println("aiResponse: " + aiResponse);

        // Parse the response to extract R parameters
        return parseAIResponse(aiResponse);
    }

    /**
     * Gets the data context for a specific plot type
     *
     * @param plotType The type of plot
     * @return A string describing the data context
     */
    private String getDataContext(String plotType) {
        switch (plotType) {
            case "Normalization Summary":
                return "This plot shows the distribution of data before and after normalization. "
                        + "It includes density plots and boxplots for both pre-normalized and normalized data. "
                        + "The plot helps visualize how normalization affects the data distribution.";

            case "Sample Summary":
                return "This plot shows the distribution of samples before and after normalization. "
                        + "It includes density plots and boxplots for both pre-normalized and normalized data. "
                        + "The plot helps identify any sample-specific effects of normalization.";

            case "PCA":
                return "This is a Principal Component Analysis plot showing the projection of samples "
                        + "onto the first two principal components. It helps visualize sample clustering "
                        + "and identify potential outliers.";

            case "PLS-DA":
                return "This is a Partial Least Squares Discriminant Analysis plot showing the projection "
                        + "of samples onto the first two components. It helps visualize class separation "
                        + "and identify important features.";

            case "Heatmap":
                return "This is a heatmap showing the expression levels of features across samples. "
                        + "It helps identify patterns and clusters in the data.";

            default:
                return "This is a general plot that may include multiple visualization elements.";
        }
    }
}
