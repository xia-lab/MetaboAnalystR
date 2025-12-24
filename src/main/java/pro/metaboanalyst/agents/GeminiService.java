package pro.metaboanalyst.agents;

import com.google.genai.Client;
import com.google.genai.types.GenerateContentResponse;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Named;
import java.io.Serializable;

/**
 * APPLICATION SCOPED SERVICE
 * Features:
 * 1. Connection Pooling (Single Client instance).
 * 2. Adaptive Model Selection (Auto-switches 3.0 Preview -> GA).
 * 3. Rate Limit Handling (Exponential Backoff).
 */
@Named
@ApplicationScoped
public class GeminiService implements Serializable {

    private Client client;
    
    // Security Note: Ideally move this to System.getenv in production
    private static final String GOOGLE_API_KEY = "AIzaSyDybLk7hiEHTiz2s8hAIVXgp37ojAX0qnw";
    
    // --- CENTRALIZED CONFIGURATION ---
    // Primary: Future-proof name. Code automatically falls back to "-preview" if this 404s.
    public static final String MODEL_PRIMARY = "gemini-3-flash"; 
    
    // Alternative: The stable 2.5 model (as requested)
    public static final String MODEL_ALTERNATIVE = "gemini-2.5-flash"; 

    // Retry configuration
    private static final int MAX_RETRIES = 3;
    private static final long INITIAL_BACKOFF_MS = 1000;

    @PostConstruct
    public void init() {
        System.out.println("GeminiService: Initializing shared Google GenAI Client...");
        try {
            this.client = Client.builder()
                    .apiKey(GOOGLE_API_KEY)
                    .build();
            System.out.println("GeminiService: Client initialized successfully.");
        } catch (Exception e) {
            System.err.println("GeminiService: Failed to initialize client: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    public String getModelDisplayName(String modelId) {
        if (MODEL_PRIMARY.equals(modelId)) return "Gemini 3 Flash";
        if ("gemini-3-flash-preview".equals(modelId)) return "Gemini 3 Flash (Preview)";
        if (MODEL_ALTERNATIVE.equals(modelId)) return "Gemini 2.5 Flash";
        return modelId;
    }

    /**
     * Generates text with "Self-Adaptive" logic.
     * If 'gemini-3-flash' is requested but returns 404 (Not Found), 
     * it automatically retries with 'gemini-3-flash-preview'.
     */
    public String generateText(String prompt, String requestedModel) {
        if (this.client == null) {
            throw new RuntimeException("Gemini Client is not initialized.");
        }

        // 1. Determine Candidates
        // If requesting Primary, try Standard -> Preview. Otherwise just try requested.
        String[] candidates;
        if (MODEL_PRIMARY.equals(requestedModel)) {
            candidates = new String[]{MODEL_PRIMARY, "gemini-3-flash-preview"};
        } else {
            candidates = new String[]{requestedModel};
        }

        RuntimeException lastException = null;

        // 2. Try candidates in order
        for (String modelToTry : candidates) {
            try {
                return generateTextInternal(prompt, modelToTry);
            } catch (Exception e) {
                lastException = new RuntimeException(e);
                String msg = e.getMessage();
                
                // If the error is "Model Not Found" (404), try the next candidate (Preview)
                boolean isModelMissing = msg != null && (msg.contains("404") || msg.contains("not found") || msg.contains("not supported"));
                
                if (isModelMissing) {
                    System.out.println("GeminiService: Model '" + modelToTry + "' not found. Falling back to next candidate...");
                    continue; 
                } else {
                    // If it's Auth error or Rate Limit (after retries), stop immediately.
                    throw lastException;
                }
            }
        }
        throw lastException;
    }

    /**
     * Internal execution loop handles Rate Limiting (429) for a specific model.
     */
    private String generateTextInternal(String prompt, String modelName) {
        int attempt = 0;
        long backoffMs = INITIAL_BACKOFF_MS;

        while (attempt < MAX_RETRIES) {
            try {
                GenerateContentResponse response = client.models.generateContent(
                        modelName,
                        prompt,
                        null
                );
                return response.text();

            } catch (Exception e) {
                attempt++;
                String errorMsg = e.getMessage();
                
                boolean isRateLimit = errorMsg != null && 
                        (errorMsg.contains("429") || 
                         errorMsg.contains("Too Many Requests") || 
                         errorMsg.contains("Resource exhausted"));

                if (isRateLimit && attempt < MAX_RETRIES) {
                    System.err.println("GeminiService: Rate limit on " + modelName + " (attempt " + attempt + "). Backing off " + backoffMs + "ms");
                    try { Thread.sleep(backoffMs); } catch (InterruptedException ie) { Thread.currentThread().interrupt(); }
                    backoffMs *= 2; 
                } else {
                    // Throw immediately so the outer loop can decide (Switch model or Fail)
                    throw new RuntimeException(errorMsg, e);
                }
            }
        }
        throw new RuntimeException("Failed to generate text after " + MAX_RETRIES + " attempts.");
    }
    
    // Convenience default
    public String generateText(String prompt) {
        return generateText(prompt, MODEL_PRIMARY);
    }
    
    @PreDestroy
    public void cleanup() {
        System.out.println("GeminiService: Shutting down.");
    }
}