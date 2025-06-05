package pro.metaboanalyst.agents;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.List;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.stats.UnivBean;
import pro.metaboanalyst.llm.GoogleAIClient;
import pro.metaboanalyst.rwrappers.UniVarTests;

@Named
@SessionScoped
public class RPlotCustomizationAgent implements Serializable {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(RPlotCustomizationAgent.class.getName());

    private static final String SYSTEM_PROMPT = "You are an expert in R programming and data visualization. "
            + "Update the R plotting function below according to the user's request while preserving core analytical logic. "
            + "Return ONLY the modified R code or a concise status message when no change is required.";

    @Inject
    private ApplicationBean1 appBean;

    @Inject
    private SessionBean1 sb;

    @Inject
    private UnivBean ub;

    private transient GoogleAIClient aiClient;
    private static String RSCRIPT_DIR;
    private final List<ChatMessage> chatHistory = new ArrayList<>();

    @PostConstruct
    private void init() {
        if (appBean == null) {
            LOG.severe("ApplicationBean1 not injected – cannot resolve R script path");
            return;
        }
        RSCRIPT_DIR = appBean.getRscriptsHomePath() + "/XiaLabPro/R/plotting";
        aiClient = new GoogleAIClient();
        LOG.info(() -> "Plot script directory = " + RSCRIPT_DIR);
    }

    public String customizePlot(String functionName, String userRequest) {
        if (RSCRIPT_DIR == null) {
            return err("R script directory not initialised");
        }

        // 1) Read original .R file
        String original = readFunctionCode(functionName).orElse(null);
        if (original == null) {
            return err("Function '" + functionName + "' not found");
        }

        // 2) Build chat history + SYSTEM_PROMPT + original + userRequest
        StringBuilder historyBuilder = new StringBuilder();
        for (ChatMessage msg : chatHistory) {
            historyBuilder.append("[").append(msg.role).append("]\n")
                    .append(msg.content).append("\n\n");
        }

        String prompt = historyBuilder.toString()
                + SYSTEM_PROMPT + "\n\nHere is the original R function:\n\n"
                + original
                + "\n\nUser request: " + userRequest;

        // 3) Record user message in history, call AI
        chatHistory.add(new ChatMessage("user", userRequest));
        String modifiedRFunction = aiClient.generateText(prompt);
        chatHistory.add(new ChatMessage("model", modifiedRFunction));

        // 4) Strip any ``` fences
        modifiedRFunction = modifiedRFunction
                .replaceAll("^```[a-zA-Z]*\\s*", "")
                .replaceAll("\\s*```$", "");

        // 5) **New**: Rename the top‐level function definition to functionName + "AI" **
        //    We look for:    functionName <- function
        //    and replace it with:   functionNameAI <- function
        String aiFuncName = functionName + "AI";
        // Regex: \bfunctionName\s*<-\s*function
        String pattern = "\\b" + functionName + "\\s*<-\\s*function";
        modifiedRFunction = modifiedRFunction.replaceFirst(pattern, aiFuncName + " <- function");

        // 6) Send to R
        RConnection mainConn = sb.getRConnection();
        try {
            mainConn.eval(modifiedRFunction);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "R evaluation failed", ex);
        }

        // 7) (Optional) If your downstream code expects to call plotVolcanoAI or whatever,
        //    you can still trigger the AI‐named function here. Otherwise, just return the code.
        if (ub.getLabelOpt().equals("all")) {
            UniVarTests.plotVolcanoAI(
                    sb,
                    sb.getNewImage("volcano"),
                    ub.getPlotLbl(),
                    ub.getPlotTheme(),
                    "png",
                    72,
                    -1
            );
        } else {
            UniVarTests.plotVolcanoAI(
                    sb,
                    sb.getNewImage("volcano"),
                    ub.getPlotLbl(),
                    ub.getPlotTheme(),
                    "png",
                    72,
                    sb.getVolcanoLabelNum()
            );
        }

        // 8) Return the actual R code string that defined functionNameAI()
        return modifiedRFunction;
    }

    private Optional<String> readFunctionCode(String functionName) {
        String fileName = Character.toUpperCase(functionName.charAt(0)) + functionName.substring(1) + ".R";
        Path file = Path.of(RSCRIPT_DIR, fileName);
        if (!Files.exists(file)) {
            return Optional.empty();
        }
        try {
            String content = Files.readString(file);
            int start = content.indexOf(functionName + " <- function");
            if (start < 0) {
                return Optional.empty();
            }
            int depth = 0, end = start;
            for (int i = start; i < content.length(); i++) {
                char c = content.charAt(i);
                if (c == '{') {
                    depth++;
                } else if (c == '}' && --depth == 0) {
                    end = i + 1;
                    break;
                }
            }
            return Optional.of(content.substring(start, end));
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, "Failed to read R function", ex);
            return Optional.empty();
        }
    }

    private static String err(String msg) {
        return "Error: " + msg;
    }

    private static class ChatMessage {

        private final String role;
        private final String content;

        public ChatMessage(String role, String content) {
            this.role = role;
            this.content = content;
        }

        public String getRole() {
            return role;
        }

        public String getContent() {
            return content;
        }
    }
}
