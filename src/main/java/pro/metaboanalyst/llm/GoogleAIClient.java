package pro.metaboanalyst.llm;

import com.google.genai.Client;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.Content;
import com.google.genai.types.Part;
import java.io.IOException;

/**
 * A client class for interacting with Google's Generative AI API.
 * This class provides basic functionality to generate text using Google's AI models.
 */
public class GoogleAIClient {
    private final Client client;
    private static final String MODEL_NAME = "gemini-2.0-flash-001";

    /**
     * Creates a new GoogleAIClient instance.
     * Note: You need to set up Google Cloud credentials before using this class.
     * Set the GOOGLE_API_KEY environment variable with your API key.
     */
    public GoogleAIClient() {
        String apiKey = System.getenv("GOOGLE_API_KEY");
        if (apiKey == null || apiKey.isEmpty()) {
            throw new IllegalStateException("GOOGLE_API_KEY environment variable is not set");
        }
        
        this.client = Client.builder()
            .apiKey(apiKey)
            .build();
    }

    /**
     * Generates text based on the provided prompt using Google's AI model.
     *
     * @param prompt The text prompt to generate content from
     * @return The generated text response
     */
    public String generateText(String prompt) {
        try {
            GenerateContentResponse response = client.models.generateContent(
                MODEL_NAME,
                prompt,
                null  // No additional config needed for basic text generation
            );
            return response.text();
        } catch (Exception e) {
            throw new RuntimeException("Error generating text: " + e.getMessage(), e);
        }
    }

    /**
     * Example usage of the GoogleAIClient.
     */
    public static void main(String[] args) {
        try {
            GoogleAIClient client = new GoogleAIClient();
            String response = client.generateText("What is the capital of France?");
            System.out.println("Response: " + response);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
} 