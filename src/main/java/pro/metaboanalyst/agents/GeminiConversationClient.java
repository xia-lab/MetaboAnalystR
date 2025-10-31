package pro.metaboanalyst.agents;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Wrapper around GoogleAIClient that manages conversation history and FAQ context.
 * This class maintains state for a single conversation session.
 */
public class GeminiConversationClient implements Serializable {

    private final GoogleAIClient googleClient;
    private final List<ConversationMessage> conversationHistory;
    private final String domain;

    /**
     * Simple container for conversation history.
     */
    private static class ConversationMessage implements Serializable {
        String role;  // "user" or "model"
        String text;

        ConversationMessage(String role, String text) {
            this.role = role;
            this.text = text;
        }
    }

    /**
     * Creates a new conversation client for a specific domain.
     *
     * @param domain The domain (e.g., "metabolomics", "transcriptomics")
     */
    public GeminiConversationClient(String domain) {
        this.googleClient = new GoogleAIClient();
        this.conversationHistory = new ArrayList<>();
        this.domain = domain;
    }

    /**
     * Sends a message and returns the response, maintaining conversation history.
     *
     * @param userMessage The user's message
     * @param systemInstruction System instruction for the assistant
     * @param relevantFAQs List of relevant FAQ entries for RAG
     * @param modelName
     * @return The assistant's response
     */
    public String sendMessage(String userMessage, String systemInstruction, List<FAQLoader.FAQEntry> relevantFAQs, String modelName) {
        try {
            // Build the complete prompt with FAQ context
            StringBuilder promptBuilder = new StringBuilder();

            // Add system instruction
            promptBuilder.append(systemInstruction).append("\n\n");

            // Add FAQ context if available
            if (!relevantFAQs.isEmpty()) {
                promptBuilder.append("RELEVANT KNOWLEDGE BASE:\n\n");
                for (int i = 0; i < relevantFAQs.size(); i++) {
                    FAQLoader.FAQEntry faq = relevantFAQs.get(i);
                    promptBuilder.append("Q").append(i + 1).append(": ").append(faq.getQuestion()).append("\n");
                    promptBuilder.append("A").append(i + 1).append(": ").append(faq.getAnswer()).append("\n\n");
                }
                promptBuilder.append("---\n\n");
            }

            // Add conversation history context (last 3 exchanges = 6 messages)
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

            // Add current user message
            promptBuilder.append("USER'S NEW QUESTION:\n").append(userMessage);

            // Call Gemini API using the simple generateText method
            String assistantResponse = googleClient.generateText(promptBuilder.toString(), modelName);

            // Add to conversation history
            conversationHistory.add(new ConversationMessage("user", userMessage));
            conversationHistory.add(new ConversationMessage("model", assistantResponse));

            // Memory optimization: Keep only last 20 messages (10 exchanges)
            // We only show last 6 messages in prompts anyway
            if (conversationHistory.size() > 20) {
                conversationHistory.subList(0, conversationHistory.size() - 20).clear();
            }

            return assistantResponse;

        } catch (Exception e) {
            System.err.println("GeminiConversationClient: Error generating response: " + e.getMessage());
            e.printStackTrace();

            // Provide user-friendly error messages based on error type
            String errorMsg = e.getMessage();
            if (errorMsg != null && errorMsg.contains("Rate limit exceeded")) {
                return "I apologize, but the service is currently experiencing high traffic. The system has tried multiple times but couldn't complete your request. Please wait a moment and try again.";
            } else if (errorMsg != null && (errorMsg.contains("429") || errorMsg.contains("Too Many Requests"))) {
                return "I apologize, but the service is temporarily overloaded. Please wait a few seconds and try again.";
            } else {
                return "I apologize, but I encountered an error processing your request. Please try again.";
            }
        }
    }

    /**
     * Clears the conversation history (useful for starting a new conversation).
     */
    public void clearHistory() {
        conversationHistory.clear();
    }

    /**
     * Gets the current conversation history size.
     */
    public int getHistorySize() {
        return conversationHistory.size();
    }

    /**
     * Gets the domain for this conversation.
     */
    public String getDomain() {
        return domain;
    }
}
