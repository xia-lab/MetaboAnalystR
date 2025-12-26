package pro.metaboanalyst.agents;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import jakarta.inject.Named;
import java.util.ArrayList;
import java.util.List;
import jakarta.enterprise.context.SessionScoped;
import com.vladsch.flexmark.html.HtmlRenderer;
import com.vladsch.flexmark.parser.Parser;
import com.vladsch.flexmark.util.data.MutableDataSet;
import com.vladsch.flexmark.ext.tables.TablesExtension;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.SessionBean1;

@Named("myBot")
@SessionScoped
public class OmicsBot implements Serializable {

    @Inject
    private GeminiService geminiService;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private FAQLoader faqLoader;

    private GeminiConversationClient geminiClient;
    private List<Message> messages;
    private String promptMsg = "";
    private String code = "metabolomics";
    
    // Current user's selected model (Starts with 3.0 Flash)
    private String currentModel = GeminiService.MODEL_PRIMARY;
    
    private String systemInstruction = "You are a helpful assistant specialized in bioinformatics and omics data analysis. "
            + "Answer questions clearly and concisely. If relevant information is provided in the RELEVANT KNOWLEDGE BASE section, "
            + "prioritize that information in your response. Always provide accurate, scientifically sound advice. "
            + " You specialize in metabolomics, mass spectrometry data analysis, and metabolite identification using MetaboAnalyst.";

    private String welcomeMsg = ""; 

    public void initAssistant() {
        if (messages != null && messages.size() > 20) {
            List<Message> recentMessages = new ArrayList<>(messages.subList(messages.size() - 20, messages.size()));
            messages = recentMessages;
        } else {
            messages = new ArrayList<>();
        }

        cleanResource();

        // Pass service to the client wrapper
        geminiClient = new GeminiConversationClient(code, geminiService);

        String displayModelName = geminiService.getModelDisplayName(currentModel);
        welcomeMsg = "I am MetaboAnalyst Assistant using " + displayModelName + ". Type your question and click on 'Send' to start. Please be patient ....";
        messages.add(new Message("Assistant", welcomeMsg));
    }

    public void modifyModel() {
        // Toggle between 3.0 Flash and 2.5 Flash
        if (currentModel.equals(GeminiService.MODEL_PRIMARY)) {
            currentModel = GeminiService.MODEL_ALTERNATIVE;
        } else {
            currentModel = GeminiService.MODEL_PRIMARY;
        }
        initAssistant();
    }
    
    public String getModelName() {
        return geminiService.getModelDisplayName(currentModel);
    }
    
    public String getAltMdlName() {
        if (currentModel.equals(GeminiService.MODEL_PRIMARY)) {
             return geminiService.getModelDisplayName(GeminiService.MODEL_ALTERNATIVE);
        } else {
             return geminiService.getModelDisplayName(GeminiService.MODEL_PRIMARY);
        }
    }

    public void sendUserQuery() {
        if (promptMsg.trim().length() == 0) {
            sb.addMessage("error", "Please enter your question first");
            return;
        }

        if (geminiClient == null) {
            geminiClient = new GeminiConversationClient(code, geminiService);
        }

        getMessages().add(new Message("User", promptMsg));

        try {
            List<FAQLoader.FAQEntry> relevantFAQs = faqLoader.findRelevantFAQs(code, promptMsg, 5);
            
            // Generate answer using the Service + Adaptive Fallback
            String answer = geminiClient.sendMessage(promptMsg, systemInstruction, relevantFAQs, currentModel);

            String regex = "【\\d+(:\\d+)?+†source】";
            String res = answer.replaceAll(regex, "");
            res = getHtmlContent(res);

            messages.add(new Message("Assistant", res));

            if (messages.size() > 30) {
                messages.subList(0, messages.size() - 30).clear();
            }

        } catch (Exception e) {
            System.err.println("OmicsBot: Error in sendMsg: " + e.getMessage());
            e.printStackTrace();
            messages.add(new Message("Assistant", "I apologize, but I encountered an error processing your request. Please try again."));
        }
        promptMsg = "";
    }

    public void cleanResource() {
        if (geminiClient != null) {
            geminiClient.clearHistory();
            geminiClient = null;
        }
    }

    public String getHtmlContent(String text) {
        MutableDataSet options = new MutableDataSet();
        options.set(Parser.EXTENSIONS, java.util.Arrays.asList(TablesExtension.create()));
        Parser parser = Parser.builder(options).build();
        HtmlRenderer renderer = HtmlRenderer.builder(options).build();
        return renderer.render(parser.parse(text));
    }
    
    // Standard Getters/Setters
    public List<Message> getMessages() { if (messages == null) initAssistant(); return messages; }
    public void setMessages(List<Message> messages) { this.messages = messages; }
    public String getPromptMsg() { return promptMsg; }
    public void setPromptMsg(String promptMsg) { this.promptMsg = promptMsg; }
    public String getWelcomeMsg() { return welcomeMsg; }
    public void setWelcomeMsg(String welcomeMsg) { this.welcomeMsg = welcomeMsg; }
    public String getCode() { return code; }
    public void setCode(String code) { this.code = code; }
}