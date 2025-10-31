package pro.metaboanalyst.agents;

import com.google.genai.Client;
import com.google.genai.types.GenerateContentResponse;

/**
 * A client class for interacting with Google's Generative AI API. This class
 * provides basic functionality to generate text using Google's AI models.
 * Includes retry logic with exponential backoff for rate limiting (429 errors).
 */
public class GoogleAIClient {

    private final Client client;
    private static final String GOOGLE_API_KEY = "AIzaSyDybLk7hiEHTiz2s8hAIVXgp37ojAX0qnw";

    // Retry configuration
    private static final int MAX_RETRIES = 3;
    private static final long INITIAL_BACKOFF_MS = 1000; // 1 second

    /**
     * Creates a new GoogleAIClient instance. Note: You need to set up Google
     * Cloud credentials before using this class. Set the GOOGLE_API_KEY
     * environment variable with your API key.
     */
    public GoogleAIClient() {
        //String apiKey = System.getenv("GOOGLE_API_KEY");
        String apiKey = GOOGLE_API_KEY;

        this.client = Client.builder()
                .apiKey(apiKey)
                .build();
    }

    public String generateText(String prompt) {
        return generateText(prompt, "gemini-2.5-flash");
    }

    /**
     * Generates text based on the provided prompt using Google's AI model.
     * Includes retry logic with exponential backoff for rate limiting.
     *
     * @param prompt The text prompt to generate content from
     * @param modelName
     * @return The generated text response
     */
    public String generateText(String prompt, String modelName) {
        int attempt = 0;
        long backoffMs = INITIAL_BACKOFF_MS;

        while (attempt < MAX_RETRIES) {
            try {
                GenerateContentResponse response = client.models.generateContent(
                        modelName,
                        prompt,
                        null // No additional config needed for basic text generation
                );
                return response.text();

            } catch (Exception e) {
                attempt++;
                String errorMsg = e.getMessage();

                // Check if it's a rate limiting error (429)
                boolean isRateLimitError = errorMsg != null
                        && (errorMsg.contains("429")
                        || errorMsg.contains("Too Many Requests")
                        || errorMsg.contains("Resource exhausted"));

                if (isRateLimitError && attempt < MAX_RETRIES) {
                    System.err.println("GoogleAIClient: Rate limit hit (attempt " + attempt + "/" + MAX_RETRIES + "). Retrying in " + backoffMs + "ms...");

                    try {
                        Thread.sleep(backoffMs);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        throw new RuntimeException("Retry interrupted: " + ie.getMessage(), ie);
                    }

                    // Exponential backoff: 1s, 2s, 4s
                    backoffMs *= 2;

                } else {
                    // Not a rate limit error, or max retries exceeded
                    if (isRateLimitError) {
                        throw new RuntimeException("Rate limit exceeded after " + MAX_RETRIES + " retries. Please try again in a few moments.", e);
                    } else {
                        throw new RuntimeException("Error generating text: " + errorMsg, e);
                    }
                }
            }
        }

        // Should never reach here, but just in case
        throw new RuntimeException("Failed to generate text after " + MAX_RETRIES + " attempts");
    }

}
