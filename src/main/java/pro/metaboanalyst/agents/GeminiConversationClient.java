package pro.metaboanalyst.agents;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Wrapper that manages conversation history (Stateful).
 * Delegates actual API calls to the shared GeminiService (Stateless).
 */
public class GeminiConversationClient implements Serializable {

    private final GeminiService geminiService;
    private final List<ConversationMessage> conversationHistory;
    private final String domain;

    private static class ConversationMessage implements Serializable {
        String role;  
        String text;
        ConversationMessage(String role, String text) { this.role = role; this.text = text; }
    }

    // Constructor now requires the Service
    public GeminiConversationClient(String domain, GeminiService service) {
        this.domain = domain;
        this.geminiService = service;
        this.conversationHistory = new ArrayList<>();
    }

    public String sendMessage(String userMessage, String systemInstruction, List<FAQLoader.FAQEntry> relevantFAQs, String modelName) {
        try {
            StringBuilder promptBuilder = new StringBuilder();

            // 1. System Instruction
            promptBuilder.append(systemInstruction).append("\n\n");

            // 2. FAQ Context
            if (relevantFAQs != null && !relevantFAQs.isEmpty()) {
                promptBuilder.append("RELEVANT KNOWLEDGE BASE:\n\n");
                for (int i = 0; i < relevantFAQs.size(); i++) {
                    promptBuilder.append("Q").append(i + 1).append(": ").append(relevantFAQs.get(i).getQuestion()).append("\n");
                    promptBuilder.append("A").append(i + 1).append(": ").append(relevantFAQs.get(i).getAnswer()).append("\n\n");
                }
                promptBuilder.append("---\n\n");
            }

            // 3. History
            if (!conversationHistory.isEmpty()) {
                promptBuilder.append("CONVERSATION HISTORY:\n");
                int historyToShow = Math.min(conversationHistory.size(), 6);
                for (int i = conversationHistory.size() - historyToShow; i < conversationHistory.size(); i++) {
                    ConversationMessage msg = conversationHistory.get(i);
                    String role = msg.role.equals("user") ? "User" : "Assistant";
                    promptBuilder.append(role).append(": ").append(msg.text).append("\n");
                }
                promptBuilder.append("\n---\n\n");
            }

            promptBuilder.append("USER'S NEW QUESTION:\n").append(userMessage);

            // 4. CALL SHARED SERVICE
            String assistantResponse = geminiService.generateText(promptBuilder.toString(), modelName);

            // 5. Update History
            conversationHistory.add(new ConversationMessage("user", userMessage));
            conversationHistory.add(new ConversationMessage("model", assistantResponse));

            if (conversationHistory.size() > 20) {
                conversationHistory.subList(0, conversationHistory.size() - 20).clear();
            }

            return assistantResponse;

        } catch (Exception e) {
            //System.err.println("GeminiConversationClient: Error: " + e.getMessage());
            e.printStackTrace();
            String errorMsg = e.getMessage();
            if (errorMsg != null && (errorMsg.contains("Rate limit") || errorMsg.contains("429"))) {
                return "I apologize, but the service is currently overloaded. Please try again in a moment.";
            }
            return "I apologize, but I encountered an error processing your request.";
        }
    }

    public void clearHistory() {
        conversationHistory.clear();
    }
    
    public int getHistorySize() {
        return conversationHistory.size();
    }

    public String getDomain() {
        return domain;
    }
}