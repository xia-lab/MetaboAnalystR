package pro.metaboanalyst.visualization;

import java.io.IOException;
import java.io.Serializable;
import jakarta.annotation.ManagedBean;
import jakarta.faces.context.FacesContext;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.ArrayList;
import java.util.List;
import pro.metaboanalyst.chat.Message;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RPlotAICustomizer;

@Named("rPlotCustomizationBean")
@ViewScoped
public class RPlotCustomizationBean implements Serializable {

    @Inject
    private SessionBean1 sb;
    private String plotType;
    private String prompt;
    private String aiResponse;
    private RPlotAICustomizer aiCustomizer;
    private String welcomeMsg = "<p>Describe how you want to customize the plot. For example:</p>\n"
            + "\n"
            + "<ul>\n"
            + "  <li>Change colors (e.g., <code>Use a blue color scheme</code>)</li>\n"
            + "  <li>Adjust text size (e.g., <code>Make labels larger</code>)</li>\n"
            + "  <li>Modify layout (e.g., <code>Increase spacing between plots</code>)</li>\n"
            + "  <li>Change dimensions (e.g., <code>Make the plot wider</code>)</li>\n"
            + "  <li>Add features (e.g., <code>Add a grid to the background</code>)</li>\n"
            + "</ul>";

    public RPlotCustomizationBean() {
        try {
            aiCustomizer = new RPlotAICustomizer();
        } catch (IOException e) {
            // Log error and handle initialization failure
            e.printStackTrace();
        }
    }

    private List<Message> messages;

    public List<Message> getMessages() {
        if (messages == null) {
            messages = new ArrayList<>();
            messages.add(new Message("Assistant", welcomeMsg));
        }
        return messages;
    }

    public void setMessages(List<Message> messages) {
        this.messages = messages;
    }

    public String getWelcomeMsg() {
        return welcomeMsg;
    }

    public void setWelcomeMsg(String welcomeMsg) {
        this.welcomeMsg = welcomeMsg;
    }

    public String getPlotType() {
        return plotType;
    }

    /**
     * Sets the plot type based on the current context This is called when the
     * dialog is opened
     */
    public void setPlotType() {
        // Get the current view ID to determine which plot we're customizing
        String viewId = FacesContext.getCurrentInstance().getViewRoot().getViewId();

        // Map view IDs to plot types
        if (viewId.contains("norm")) {
            plotType = "Normalization Summary";
        } else if (viewId.contains("pca")) {
            plotType = "PCA";
        } else if (viewId.contains("plsda")) {
            plotType = "PLS-DA";
        } else if (viewId.contains("heatmap")) {
            plotType = "Heatmap";
        } else {
            plotType = "Sample Summary"; // Default
        }
    }

    public String getPrompt() {
        return prompt;
    }

    public void setPrompt(String prompt) {
        this.prompt = prompt;
    }

    public String getAiResponse() {
        return aiResponse;
    }

    public void setAiResponse(String aiResponse) {
        this.aiResponse = aiResponse;
    }

    public void applyCustomization() {
        try {
            if (prompt == null || prompt.trim().isEmpty()) {
                sb.addMessage("error", "Please provide customization instructions");
                return;
            }

            String response = aiCustomizer.customizePlot(plotType, prompt);
            aiResponse = response;

            // Here you would apply the customization to your R plot
            // This would involve calling the appropriate R function with the custom parameters
        } catch (IOException e) {
            sb.addMessage("error", "Error communicating with AI service: " + e.getMessage());

        } catch (Exception e) {
            sb.addMessage("error", "Error applying customization: " + e.getMessage());

        }
    }
}
