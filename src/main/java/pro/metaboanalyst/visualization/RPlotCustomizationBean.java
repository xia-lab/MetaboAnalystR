package pro.metaboanalyst.visualization;

import java.io.IOException;
import java.io.Serializable;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import pro.metaboanalyst.agents.RPlotCustomizationAgent;
import pro.metaboanalyst.chat.Message;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;

@Named("rPlotCustomizationBean")
@ViewScoped
public class RPlotCustomizationBean implements Serializable {

    @Inject
    private SessionBean1 sb;

    @Inject
    private ApplicationBean1 ab;

    @Inject
    private RPlotCustomizationAgent aiCustomizer;

    private String plotType = "";
    private String prompt;
    private String aiResponse;
    private String welcomeMsg = "<p>Describe how you want to customize the plot. For example:</p>\n"
            + "\n"
            + "<ul>\n"
            + "  <li>Change colors (e.g., <code>Use a blue color scheme</code>)</li>\n"
            + "  <li>Adjust text size (e.g., <code>Make labels larger</code>)</li>\n"
            + "  <li>Modify layout (e.g., <code>Increase spacing between plots</code>)</li>\n"
            + "  <li>Change dimensions (e.g., <code>Make the plot wider</code>)</li>\n"
            + "  <li>Add features (e.g., <code>Add a grid to the background</code>)</li>\n"
            + "</ul>";

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
            String source = sb.getImgSource();
            if (source.equals("norm")) {
                plotType = "PlotNormSummary";
            } else if (source.equals("volcano")) {
                plotType = "PlotVolcano";
            }

            String response = aiCustomizer.customizePlot(plotType, prompt);
            aiResponse = response;
            System.out.println("AIRESPONSE:" + aiResponse);
        } catch (Exception e) {
            sb.addMessage("error", "Error applying customization: " + e.getMessage());
        }
    }

    public String getPreviewImage() {
        String plotSource = sb.getImgSource();
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(plotSource) + "dpi72.png";
    }
}
