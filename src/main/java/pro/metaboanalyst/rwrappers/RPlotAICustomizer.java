package pro.metaboanalyst.rwrappers;

import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import pro.metaboanalyst.agents.RPlotCustomizationAgent;

/**
 * A utility class that uses AI to customize R plot parameters and generate
 * optimal visualization settings.
 */
@ApplicationScoped
public class RPlotAICustomizer {
    private static final Logger LOGGER = Logger.getLogger(RPlotAICustomizer.class.getName());
    
    @Inject
    private RPlotCustomizationAgent agent;

    /**
     * Customizes a plot based on the plot type and user prompt
     *
     * @param originalFunction The original R function to customize
     * @param prompt The user's customization instructions
     * @return The modified R function
     */
    public String customizePlot(String originalFunction, String prompt) {
        try {
            // Use the agent to generate modified R function
            String modifiedFunction = agent.customizePlot(originalFunction, prompt);
            LOGGER.info("Generated modified function: " + modifiedFunction);
            return modifiedFunction;
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error generating AI customization", e);
            throw new RuntimeException("Failed to customize plot: " + e.getMessage());
        }
    }
}
