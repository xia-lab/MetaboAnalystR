package pro.metaboanalyst.agents;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.stats.UnivBean;
import pro.metaboanalyst.llm.GoogleAIClient;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.rwrappers.RCenter;

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
        
        // Initialize the AI client
        try {
            aiClient = new GoogleAIClient();
        } catch (Exception e) {
            LOG.severe("Failed to initialize GoogleAIClient: " + e.getMessage());
        }
    }

    public String customizePlot(String key, String functionName, String userRequest) {
        if (RSCRIPT_DIR == null) {
            return err("R script directory not initialised");
        }

        if (aiClient == null) {
            return err("AI client not initialized");
        }

        // Resolve alias if needed and ensure function is loaded in R
        String resolvedFuncName = functionName;
        if (FUNCTION_MAPPINGS.containsKey(functionName)) {
            FunctionMapping mapping = FUNCTION_MAPPINGS.get(functionName);
            resolvedFuncName = mapping.rFuncName;
            try {
                String lazyLoader = ".load.scripts.on.demand('" + mapping.rScriptFile + "')";
                RConnection RC = sb.getRConnection();
                RC.eval("if (!exists('" + resolvedFuncName + "')) " + lazyLoader);
            } catch (Exception ex) {
                LOG.log(Level.WARNING, "Lazy loading failed", ex);
            }
        }

        // Fetch R function code
        String original = readFunctionCode(resolvedFuncName).orElse(null);
        if (original == null) {
            return err("Function '" + resolvedFuncName + "' not found");
        }

        // Assemble chat prompt
        StringBuilder historyBuilder = new StringBuilder();
        for (ChatMessage msg : chatHistory) {
            historyBuilder.append("[").append(msg.role).append("]\n")
                    .append(msg.content).append("\n\n");
        }

        String prompt = historyBuilder.toString()
                + SYSTEM_PROMPT + "\n\nHere is the original R function:\n\n"
                + original + "\n\nUser request: " + userRequest;

        chatHistory.add(new ChatMessage("user", userRequest));
        String modifiedRFunction = aiClient.generateText(prompt);
        chatHistory.add(new ChatMessage("model", modifiedRFunction));

        // Remove code fences and rename function
        modifiedRFunction = modifiedRFunction
                .replaceAll("^```[a-zA-Z]*\\s*", "")
                .replaceAll("\\s*```$", "");

        String aiFuncName = functionName + "AI";

        // Case 1: Try to rename existing assignment
        String pattern = "\\b" + functionName + "\\s*<-\\s*function";
        if (modifiedRFunction.matches("(?s).*" + pattern + ".*")) {
            modifiedRFunction = modifiedRFunction.replaceFirst(pattern, aiFuncName + " <- function");
        } else if (modifiedRFunction.trim().startsWith("function")) {
            // Case 2: AI returned anonymous function, we prepend assignment
            modifiedRFunction = aiFuncName + " <- " + modifiedRFunction;
        }

        // Evaluate modified function in R
        try {
            System.out.println(modifiedRFunction);
            sb.getRConnection().eval(modifiedRFunction);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "R evaluation failed", ex);
        }

        String origRCommand = sb.getGraphicsMap().get(key);
        executeAICustomizedPlot(origRCommand);

        return modifiedRFunction;
    }

    private static final Map<String, FunctionMapping> FUNCTION_MAPPINGS = Map.of(
            "PlotVolcano", new FunctionMapping("my.plot.volcano", "util_volcano.Rc"),
            "plotVennDiagram", new FunctionMapping("my.plot.venn", "util_venn.Rc"),
            "Plot3D", new FunctionMapping("my.plot.scatter3d", "util_3dscatter.Rc")
    );

    private Optional<String> readFunctionCode(String rFuncName) {
        try {
            String rCmd = "paste(deparse(" + rFuncName + "), collapse='\\n')";
            String code = sb.getRConnection().eval(rCmd).asString();
            return Optional.of(code);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Failed to fetch R function from RConnection", ex);
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

    private static class FunctionMapping {

        final String rFuncName;
        final String rScriptFile;

        FunctionMapping(String rFuncName, String rScriptFile) {
            this.rFuncName = rFuncName;
            this.rScriptFile = rScriptFile;
        }
    }

    public void executeAICustomizedPlot(String rCommand) {
        try {
            // Add AI suffix to the R function name
            String modifiedRCommand = rCommand.replaceFirst("(\\w+)\\(", "$1AI(");

            // Get R connection and execute the modified command
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, modifiedRCommand);
            RC.voidEval(modifiedRCommand);
        } catch (Exception e) {
            LOG.severe("Error in executeAICustomizedPlot: " + e.getMessage());
        }
    }
}
