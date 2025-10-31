package pro.metaboanalyst.agents;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.ejb.Singleton;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Singleton that loads FAQ files lazily on first use.
 * FAQs are used for Retrieval-Augmented Generation (RAG) in the chatbot.
 */
@Singleton
public class FAQLoader {

    private List<FAQEntry> metaboanalystFAQs;
    private boolean initialized = false;
    // Path relative to webapp root (not classpath)
    private static final String METABOANALYST_FAQ_PATH = "/resources/libs/faqs/metaboanalyst_faqs.json";

    /**
     * Represents a single FAQ entry with question and answer.
     */
    public static class FAQEntry {
        private String question;
        private String answer;

        public FAQEntry(String question, String answer) {
            this.question = question;
            this.answer = answer;
        }

        public String getQuestion() {
            return question;
        }

        public String getAnswer() {
            return answer;
        }

        @Override
        public String toString() {
            return "FAQEntry{Q='" + question.substring(0, Math.min(50, question.length())) + "...', A=" + answer.length() + " chars}";
        }
    }

    /**
     * Lazy initialization - loads FAQs on first use.
     * Gets ServletContext from SessionBean1 which is available at runtime.
     */
    private synchronized void ensureInitialized() {
        if (initialized) {
            return;
        }

        System.out.println("FAQLoader: Initializing and loading FAQs...");
        metaboanalystFAQs = loadFAQsFromWebapp(METABOANALYST_FAQ_PATH);
        System.out.println("FAQLoader: Loaded " + metaboanalystFAQs.size() + " MetaboAnalyst FAQs");
        initialized = true;
    }

    /**
     * Loads FAQ entries from webapp resources directory.
     */
    private List<FAQEntry> loadFAQsFromWebapp(String webappPath) {
        List<FAQEntry> faqs = new ArrayList<>();
        InputStream is = null;

        try {
            // Try multiple approaches to find the file
            System.out.println("FAQLoader: Attempting to load from path: " + webappPath);

            // Approach 1: ServletContext via Thread (works when FacesContext available)
            try {
                jakarta.servlet.ServletContext servletContext =
                    (jakarta.servlet.ServletContext) jakarta.faces.context.FacesContext
                        .getCurrentInstance()
                        .getExternalContext()
                        .getContext();
                is = servletContext.getResourceAsStream(webappPath);
                if (is != null) {
                    System.out.println("FAQLoader: Found via ServletContext");
                }
            } catch (Exception e) {
                System.out.println("FAQLoader: FacesContext not available: " + e.getMessage());
            }

            // Approach 2: Try as web resource (remove leading slash and try variations)
            if (is == null) {
                String[] variants = {
                    webappPath,
                    webappPath.substring(1), // Remove leading /
                    "webapp" + webappPath,
                    "src/main/webapp" + webappPath
                };

                for (String variant : variants) {
                    File file = new File(variant);
                    System.out.println("FAQLoader: Trying file: " + file.getAbsolutePath());
                    if (file.exists()) {
                        is = new FileInputStream(file);
                        System.out.println("FAQLoader: Found via filesystem at: " + variant);
                        break;
                    }
                }
            }

            if (is == null) {
                System.err.println("FAQLoader: Could not find FAQ file at " + webappPath);
                System.err.println("FAQLoader: Current directory: " + new File(".").getAbsolutePath());
                return faqs;
            }

            // Parse JSON
            ObjectMapper mapper = new ObjectMapper();
            JsonNode rootNode = mapper.readTree(is);

            if (rootNode.isArray()) {
                for (JsonNode node : rootNode) {
                    String question = node.has("Question") ? node.get("Question").asText() : "";
                    String answer = node.has("Answer") ? node.get("Answer").asText() : "";
                    if (!question.isEmpty() && !answer.isEmpty()) {
                        faqs.add(new FAQEntry(question, answer));
                    }
                }
            }

            System.out.println("FAQLoader: Successfully parsed " + faqs.size() + " FAQ entries");

        } catch (Exception e) {
            System.err.println("FAQLoader: Error loading FAQs: " + e.getMessage());
            e.printStackTrace();
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (Exception e) {
                    // Ignore
                }
            }
        }
        return faqs;
    }

    /**
     * Finds relevant FAQs based on keyword matching.
     *
     * @param domain The domain (e.g., "metabolomics", "transcriptomics")
     * @param question The user's question
     * @param topN Number of top FAQs to return
     * @return List of relevant FAQ entries
     */
    public List<FAQEntry> findRelevantFAQs(String domain, String question, int topN) {
        ensureInitialized();

        if (metaboanalystFAQs == null || metaboanalystFAQs.isEmpty()) {
            System.out.println("FAQLoader.findRelevantFAQs: No FAQs loaded");
            return new ArrayList<>();
        }

        System.out.println("FAQLoader.findRelevantFAQs: Domain=" + domain + ", Total FAQs=" + metaboanalystFAQs.size());

        // Normalize question for keyword matching
        String normalizedQuestion = question.toLowerCase().trim();
        System.out.println("FAQLoader.findRelevantFAQs: Searching for: " + normalizedQuestion);

        // Score each FAQ based on keyword overlap
        Map<FAQEntry, Integer> scores = new HashMap<>();
        for (FAQEntry faq : metaboanalystFAQs) {
            int score = calculateRelevanceScore(normalizedQuestion, faq);
            if (score > 0) {
                scores.put(faq, score);
            }
        }

        System.out.println("FAQLoader.findRelevantFAQs: Found " + scores.size() + " FAQs with score > 0");

        // Sort by score and return top N
        List<FAQEntry> results = scores.entrySet().stream()
                .sorted((e1, e2) -> e2.getValue().compareTo(e1.getValue()))
                .limit(topN)
                .map(Map.Entry::getKey)
                .toList();

        System.out.println("FAQLoader.findRelevantFAQs: Returning " + results.size() + " top FAQs");
        for (int i = 0; i < results.size(); i++) {
            System.out.println("  " + (i+1) + ". " + results.get(i).getQuestion().substring(0, Math.min(60, results.get(i).getQuestion().length())) + "...");
        }

        return results;
    }

    /**
     * Calculates relevance score based on keyword overlap.
     */
    private int calculateRelevanceScore(String question, FAQEntry faq) {
        String faqQuestion = faq.getQuestion().toLowerCase();
        String faqAnswer = faq.getAnswer().toLowerCase();

        // Remove punctuation and split into words
        String cleanQuestion = question.replaceAll("[^a-z0-9\\s-]", " ");
        String[] keywords = cleanQuestion.split("\\s+");
        int score = 0;

        for (String keyword : keywords) {
            keyword = keyword.trim();
            if (keyword.length() <= 2) continue; // Changed from 3 to 2

            // Higher weight for matches in FAQ question
            if (faqQuestion.contains(keyword)) {
                score += 3;
            }
            // Lower weight for matches in FAQ answer
            if (faqAnswer.contains(keyword)) {
                score += 1;
            }
        }

        // Bonus: if the entire question phrase appears in the FAQ question
        if (faqQuestion.contains(question)) {
            score += 20;
        }

        return score;
    }

}
