package pro.metaboanalyst.agents;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.llm.GoogleAIClient;
import pro.metaboanalyst.rwrappers.RCenter;

/**
 * Dynamically rewrites &amp; executes R plotting functions through an LLM.
 * <p>
 * New features:
 * <ul>
 *   <li>Accepts an <strong>optional list of helper functions</strong> that are
 *       appended to the LLM prompt so the model has full context.</li>
 *   <li>Strips <code>```r</code> fences, handles multi-function responses
 *       and ensures <em>every</em> top-level assignment is renamed with the
 *       <code>AI</code> suffix.</li>
 * </ul>
 */
@Named
@SessionScoped
public class RPlotCustomizationAgent implements Serializable {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(RPlotCustomizationAgent.class.getName());

    private static final String SYSTEM_PROMPT =
            "You are an expert in R programming and data visualization. "
          + "Update the R plotting function below according to the user's request "
          + "while preserving core analytical logic. "
          + "Return ONLY the modified R code or a concise status message when no change is required.";

    @Inject
    private transient ApplicationBean1 appBean;

    @Inject
    private transient SessionBean1 sb;

    private transient GoogleAIClient aiClient;
    private static String RSCRIPT_DIR;
    private final List<ChatMessage> chatHistory = new ArrayList<>();

    /*───────────────────────────────────────────────────────────────────────────
     *  LIFE-CYCLE
     *─────────────────────────────────────────────────────────────────────────*/
    @PostConstruct
    private void init() {
        if (appBean == null) {
            LOG.severe("ApplicationBean1 not injected – cannot resolve R script path");
            return;
        }
        RSCRIPT_DIR = appBean.getRscriptsHomePath() + "/XiaLabPro/R/plotting";
        try {
            aiClient = new GoogleAIClient();
        } catch (Exception e) {
            LOG.severe("Failed to initialise GoogleAIClient: " + e.getMessage());
        }
    }

    /*───────────────────────────────────────────────────────────────────────────
     *  PUBLIC API
     *─────────────────────────────────────────────────────────────────────────*/

    /** Back-compat wrapper: call with no helpers. */
    public String customizePlot(String key, String functionName, String userRequest) {
        return customizePlot(key, functionName, Collections.emptyList(), userRequest);
    }

    /**
     * Generate a customised plot by rewriting the specified R function
     * (plus any helper routines) via the LLM, then evaluating the result.
     *
     * @param key          graphics-map key (used to replay the command)
     * @param functionName name of the main R routine to edit
     * @param helpers      extra R functions that the main routine relies on
     * @param userRequest  natural-language instructions from the UI
     * @return the modified R code (with <code>AI</code> suffixes) or an error
     *         message suitable for display
     */
    public String customizePlot(String key,
                                String functionName,
                                List<String> helpers,
                                String userRequest) {

        /*── sanity checks ───────────────────────────────────────────────────*/
        if (RSCRIPT_DIR == null) {
            return err("R script directory not initialised");
        }
        if (aiClient == null) {
            return err("AI client not initialised");
        }

        /*── 0. resolve aliases for the MAIN routine (lazy load if needed) ──*/
        String resolvedMain = functionName;
        if (FUNCTION_MAPPINGS.containsKey(functionName)) {
            FunctionMapping m = FUNCTION_MAPPINGS.get(functionName);
            resolvedMain = m.rFuncName;
            try {
                String lazyLoader = ".load.scripts.on.demand('" + m.rScriptFile + "')";
                RConnection rc = sb.getRConnection();
                rc.eval("if (!exists('" + resolvedMain + "')) " + lazyLoader);
            } catch (Exception ex) {
                LOG.log(Level.WARNING, "Lazy loading failed", ex);
            }
        }

        /*── 1. pull source code for main routine ───────────────────────────*/
        String originalMain = readFunctionCode(resolvedMain).orElse(null);
        if (originalMain == null) {
            return err("Function '" + resolvedMain + "' not found");
        }

        /*── 2. pull helper sources (if any) ────────────────────────────────*/
        StringBuilder helperBlocks = new StringBuilder();
        for (String helper : helpers) {
            String resolvedHelper =
                    FUNCTION_MAPPINGS.getOrDefault(helper, new FunctionMapping(helper, null))
                                     .rFuncName;
            readFunctionCode(resolvedHelper).ifPresent(code -> {
                helperBlocks.append("\n\n# Helper: ").append(resolvedHelper)
                            .append("\n").append(code);
            });
        }

        /*── 3. assemble prompt incl. chat history ─────────────────────────*/
        StringBuilder history = new StringBuilder();
        for (ChatMessage m : chatHistory) {
            history.append('[').append(m.role).append("]\n")
                   .append(m.content).append("\n\n");
        }

        String prompt = history
                + SYSTEM_PROMPT
                + "\n\nHere is the original R code the app is running:\n\n"
                + originalMain
                + helperBlocks
                + "\n\nUser request:\n" + userRequest;

        chatHistory.add(new ChatMessage("user", userRequest));

        /*── 4. call the model ──────────────────────────────────────────────*/
        String llmOut = aiClient.generateText(prompt);
        chatHistory.add(new ChatMessage("model", llmOut));

        /*── 5. sanitise: strip fences, rename assignments, enforce suffix ─*/
        llmOut = llmOut.replaceAll("(?s)```[rR]?\\s*", "")
                       .replaceAll("(?s)```\\s*$", "");

        final String aiSuffix = "AI";
        final String aiMain   = functionName + aiSuffix;

        // rename every *top-level* assignment with the original main name
        llmOut = llmOut.replaceAll(
                "(?m)^\\s*"
              + Pattern.quote(functionName)
              + "\\s*<-\\s*function",
                aiMain + " <- function");

        // if still anonymous (no "<- function" anywhere) prepend assignment
        if (!llmOut.matches("(?s).*<-\\s*function.*")) {
            llmOut = aiMain + " <- " + llmOut.trim();
        }

        /*── 6. evaluate the code in R ──────────────────────────────────────*/
        try {
            sb.getRConnection().eval(llmOut);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "R evaluation failed", ex);
            return err("R evaluation failed: " + ex.getMessage());
        }

        /*── 7. replay the graphics command using the AI version ───────────*/
        String origCmd = sb.getGraphicsMap().get(key);
        executeAICustomizedPlot(origCmd, aiSuffix);

        return llmOut;  // for logging / debugging
    }

    /*───────────────────────────────────────────────────────────────────────────
     *  INTERNAL HELPERS
     *─────────────────────────────────────────────────────────────────────────*/

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

    private void executeAICustomizedPlot(String originalCmd, String aiSuffix) {
        if (originalCmd == null || originalCmd.isBlank()) {
            LOG.warning("No graphics command found for key");
            return;
        }
        try {
            String modified = originalCmd.replaceFirst("(\\w+)\\(", "$1" + aiSuffix + "(");
            System.out.println(modified);
            RConnection rc = sb.getRConnection();
            RCenter.recordRCommand(rc, modified);
            rc.voidEval(modified);
        } catch (Exception e) {
            LOG.severe("Error in executeAICustomizedPlot: " + e.getMessage());
        }
    }

    private static String err(String msg) { return "Error: " + msg; }

    /*───────────────────────────────────────────────────────────────────────────
     *  SUPPORT CLASSES
     *─────────────────────────────────────────────────────────────────────────*/
    private record ChatMessage(String role, String content) {}

    private static final Map<String, FunctionMapping> FUNCTION_MAPPINGS = Map.of(
            "PlotVolcano",     new FunctionMapping("my.plot.volcano",   "util_volcano.Rc"),
            "plotVennDiagram", new FunctionMapping("my.plot.venn",      "util_venndiagram.Rc"),
            "Plot3D",          new FunctionMapping("my.plot.scatter3d", "util_scatter3d.Rc")
    );

    private record FunctionMapping(String rFuncName, String rScriptFile) {}
}
