/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
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

/**
 *
 * @author jeffxia
 */
@Named("myBot")
@SessionScoped
public class OmicsBot implements Serializable {

    private String code = "metabolomics";
    private String welcomeMsg = "I am MetaboAnalyst Assistant using Gemini 2.5 Flash. Type your question and click on 'Send' to start. Please be patient ....";

    private String promptMsg = "";

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private FAQLoader faqLoader;

    private GeminiConversationClient geminiClient;

    private List<Message> messages;

    private String modelName = "gemini-2.5-flash-lite";
    public String getModelName() {
        return modelName;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
    }
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

    public void OmicsBot() {
        messages = new ArrayList<>();
        messages.add(new Message("Assistant", welcomeMsg));
    }

    public void initAssistant() {
        // Memory optimization: Limit message history to last 30 messages before reset
        if (messages != null && messages.size() > 20) {
            List<Message> recentMessages = new ArrayList<>(messages.subList(messages.size() - 20, messages.size()));
            messages = recentMessages;
        } else {
            messages = new ArrayList<>();
        }

        if ("na".equals(code)) {
            promptMsg = "";
            welcomeMsg = "Welcome! Please first choose your Omics Assistant to start";
            messages.add(new Message("Assistant", welcomeMsg));
            return;
        }

        cleanResource();

        // Initialize Gemini client for the selected domain
        geminiClient = new GeminiConversationClient(getDomainName(code));

        switch (code) {
            case "transcriptomics" ->
                welcomeMsg = "I am ExpressAnalyst Assistant using Gemini 2.5 Flash. Type your question and click on 'Send' to start. Please be patient ....";
            case "metabolomics" ->
                welcomeMsg = "I am MetaboAnalyst Assistant using Gemini 2.5 Flash. Type your question and click on 'Send' to start. Please be patient ....";
            case "microbiomics" ->
                welcomeMsg = "I am MicrobiomeAnalyst Assistant using Gemini 2.5 Flash. Type your question and click on 'Send' to start. Please be patient ....";
            case "microrna" ->
                welcomeMsg = "I am miRNet Assistant using Gemini 2.5 Flash. Type your question and click on 'Send' to start. Please be patient ....";
            default ->
                welcomeMsg = "I am OmicsNet/OmicsAnalyst Assistant using Gemini 2.5 Flash. Type your question and click on 'Send' to start. Please be patient ....";
        }
        messages.add(new Message("Assistant", welcomeMsg));
    }

    public String getPromptMsg() {
        return promptMsg;
    }

    public void setPromptMsg(String promptMsg) {
        this.promptMsg = promptMsg;
    }

    public String getWelcomeMsg() {
        return welcomeMsg;
    }

    public void setWelcomeMsg(String welcomeMsg) {
        this.welcomeMsg = welcomeMsg;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public void cleanResource() {
        if (geminiClient != null) {
            geminiClient.clearHistory();
            System.out.println("Gemini conversation history cleared");
            geminiClient = null;
        } else {
            System.out.println("GeminiClient is null, resource already cleaned");
        }
    }

    // Updated sendMessage method for JSF integration
    public void sendMsg() {

        if (promptMsg.trim().length() == 0) {
            sb.addMessage("error", "Please enter your question first");
            return;
        }

        if (geminiClient == null) {
            geminiClient = new GeminiConversationClient(getDomainName(code));
        }

        // Add the user's message to the chat
        getMessages().add(new Message("User", promptMsg));

        try {
            // Get system instruction for this domain
            String systemInstruction = getSystemInstruction(code);

            // Find relevant FAQs
            List<FAQLoader.FAQEntry> relevantFAQs = faqLoader.findRelevantFAQs(
                    getDomainName(code), promptMsg, 4);

            String answer = geminiClient.sendMessage(promptMsg, systemInstruction, relevantFAQs, modelName);

            // Clean up citations (in case Gemini adds any)
            String regex = "【\\d+(:\\d+)?+†source】";
            String res = answer.replaceAll(regex, "");

            // Convert markdown to HTML
            res = getHtmlContent(res);

            // Add the response from Gemini to the chat
            messages.add(new Message("Assistant", res));

            // Memory optimization: Keep only last 50 messages to prevent unbounded growth
            if (messages.size() > 30) {
                messages.subList(0, messages.size() - 30).clear();
            }

        } catch (Exception e) {
            System.err.println("OmicsBot: Error in sendMsg: " + e.getMessage());
            e.printStackTrace();
            messages.add(new Message("Assistant", "I apologize, but I encountered an error processing your request. Please try again."));
        }

        // Clear the prompt message for the next input
        promptMsg = "";
    }

    /**
     * Gets the domain name from the code.
     */
    private String getDomainName(String code) {
        return switch (code) {
            case "transcriptomics" ->
                "transcriptomics";
            case "metabolomics" ->
                "metabolomics";
            case "microbiomics" ->
                "microbiomics";
            case "microrna" ->
                "microrna";
            default ->
                "multiomics";
        };
    }

    /**
     * Gets the system instruction for each domain.
     */
    private String getSystemInstruction(String code) {
        String baseInstruction = "You are a helpful assistant specialized in bioinformatics and omics data analysis. "
                + "Answer questions clearly and concisely. If relevant information is provided in the RELEVANT KNOWLEDGE BASE section, "
                + "prioritize that information in your response. Always provide accurate, scientifically sound advice.";

        return switch (code) {
            case "transcriptomics" ->
                baseInstruction + " You specialize in RNA sequencing, gene expression analysis, and transcriptomics using ExpressAnalyst.";
            case "metabolomics" ->
                baseInstruction + " You specialize in metabolomics, mass spectrometry data analysis, and metabolite identification using MetaboAnalyst.";
            case "microbiomics" ->
                baseInstruction + " You specialize in microbiome analysis, 16S rRNA sequencing, and metagenomic data analysis using MicrobiomeAnalyst.";
            case "microrna" ->
                baseInstruction + " You specialize in microRNA analysis, miRNA-target interactions, and network analysis using miRNet.";
            default ->
                baseInstruction + " You specialize in multi-omics integration, network analysis, and systems biology using OmicsNet and OmicsAnalyst.";
        };
    }

    public String getHtmlContent(String text) {
        // Configure Flexmark with table support
        MutableDataSet options = new MutableDataSet();
        options.set(Parser.EXTENSIONS, java.util.Arrays.asList(TablesExtension.create()));

        Parser parser = Parser.builder(options).build();
        HtmlRenderer renderer = HtmlRenderer.builder(options).build();
        return renderer.render(parser.parse(text)); // Render HTML from markdown
    }

    public void modifyModel() {
        if (modelName.equals("gemini-2.5-flash")) {
            modelName = "gemini-2.5-flash-lite";
        } else {
            modelName = "gemini-2.5-flash";
        }
        initAssistant();
    }

}
