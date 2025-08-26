package pro.metaboanalyst.agents;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
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
 * <li>Accepts an <strong>optional list of helper functions</strong> that are
 * appended to the LLM prompt so the model has full context.</li>
 * <li>Strips <code>```r</code> fences, handles multi-function responses and
 * ensures <em>every</em> top-level assignment is renamed with the
 * <code>AI</code> suffix.</li>
 * </ul>
 */
@Named
@SessionScoped
public class RPlotCustomizationAgent implements Serializable {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(RPlotCustomizationAgent.class.getName());

    private static final String SYSTEM_PROMPT
            = "You are an expert R programmer specializing in data visualization for MetaboAnalyst. "
            + "\n\nYour task is to modify R plotting functions according to user requests while maintaining compatibility with the existing codebase."
            + "\n\nREQUIREMENTS:"
            + "\n1. Return ONLY valid R code without markdown fences or explanatory text"
            + "\n2. Preserve all core analytical logic and data processing steps"
            + "\n3. Maintain existing function signatures when possible; if new parameters are needed, make them optional with sensible defaults"
            + "\n4. Ensure all helper functions are updated consistently with the main function"
            + "\n5. Use standard R plotting parameter conventions (e.g., pch values: 0=square, 1=circle, 2=triangle, 5=diamond)"
            + "\n6. Preserve all existing styling, legends, and layout logic unless specifically requested to change"
            + "\n\nIf no changes are needed, return: \"No modification required.\""
            + "\n\nFocus on making minimal, targeted changes that fulfill the user's request without breaking existing functionality.";

    @Inject
    private transient ApplicationBean1 appBean;

    @Inject
    private transient SessionBean1 sb;

    private transient GoogleAIClient aiClient;
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
        try {
            aiClient = new GoogleAIClient();
        } catch (Exception e) {
            LOG.severe("Failed to initialise GoogleAIClient: " + e.getMessage());
        }
    }

    /*───────────────────────────────────────────────────────────────────────────
     *  PUBLIC API
     *─────────────────────────────────────────────────────────────────────────*/
    /**
     * Back-compat wrapper: call with no helpers.
     */
    public String customizePlot(String key, String functionName, String userRequest) {
        return customizePlot(key, functionName, Collections.emptyList(), userRequest);
    }

    /**
     * Generate a customised plot by rewriting the specified R function (plus
     * any helper routines) via the LLM, then evaluating the result.
     *
     * @param key graphics-map key (used to replay the command)
     * @param functionName name of the main R routine to edit
     * @param helpers extra R functions that the main routine relies on
     * @param userRequest natural-language instructions from the UI
     * @return the modified R code (with <code>AI</code> suffixes) or an error
     * message suitable for display
     */
    public String customizePlot(String key,
            String functionName,
            List<String> helpers,
            String userRequest) {

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
            String resolvedHelper
                    = FUNCTION_MAPPINGS.getOrDefault(helper, new FunctionMapping(helper, null)).rFuncName;
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

        System.out.println(prompt + "\n");

        chatHistory.add(new ChatMessage("user", userRequest));

        /*── 4. call the model ──────────────────────────────────────────────*/
        String llmOut = aiClient.generateText(prompt);
        chatHistory.add(new ChatMessage("model", llmOut));

        // debugging
        System.out.println("Model Response: \n" + llmOut);

        /*── 5. sanitize + suffix main & helpers + rewrite call-sites ─────────────*/
        llmOut = llmOut
                .replaceAll("(?s)```[rR]?\\s*", "")
                .replaceAll("(?s)```\\s*$", "")
                .trim();

        final String aiSuffix = "AI";
        final String aiMain = functionName + aiSuffix;

        /* 5a) Detect helper blocks of the form:
 *   # Helper: <Name>
 *   function(...) { ... }
 * and helpers already assigned:
 *   <Name>AI <- function(...) { ... }
         */
        Pattern hdrPat = Pattern.compile("(?m)^\\s*#\\s*Helper:\\s*([\\w\\.]+)\\s*\\n\\s*function\\b");
        Pattern asgPat = Pattern.compile("(?m)^\\s*([\\w\\.]+)" + Pattern.quote(aiSuffix) + "\\s*<-\\s*function\\b");

        LinkedHashMap<String, String> helperMap = new LinkedHashMap<>(); // base -> target

        Matcher mh = hdrPat.matcher(llmOut);
        while (mh.find()) {
            String raw = mh.group(1); // may already include AI
            String base = raw.endsWith(aiSuffix) ? raw.substring(0, raw.length() - aiSuffix.length()) : raw;
            helperMap.put(base, base + aiSuffix);
        }

        Matcher ma = asgPat.matcher(llmOut);
        while (ma.find()) {
            String base = ma.group(1); // already without AI (we matched <Name>AI)
            helperMap.putIfAbsent(base, base + aiSuffix);
        }

        /* Also honor any helpers passed in explicitly */
        for (String h : helpers) {
            String base = FUNCTION_MAPPINGS.getOrDefault(h, new FunctionMapping(h, null)).rFuncName;
            helperMap.putIfAbsent(base, base + aiSuffix);
        }

        /* 5b) Normalize helper headers to include assignment to AI name */
        for (Map.Entry<String, String> e : helperMap.entrySet()) {
            String baseQ = Pattern.quote(e.getKey());
            String tgt = e.getValue();
            llmOut = llmOut.replaceAll(
                    "(?m)^\\s*#\\s*Helper:\\s*" + baseQ + "\\s*\\n\\s*function\\b",
                    "# Helper: " + tgt + "\n" + tgt + " <- function"
            );
        }

        /* 5c) Ensure the MAIN is assigned to <functionName>AI */
        if (llmOut.startsWith("function")) {
            llmOut = aiMain + " <- " + llmOut;
        } else {
            llmOut = llmOut.replaceFirst("(?ms)^(\\s*)([\\w\\.]+)\\s*<-\\s*function\\b",
                    "$1" + aiMain + " <- function");
        }

        /* 5d) Rewrite ALL call-sites to helpers INSIDE the code:
 *    base(  ->  baseAI(
 * Guard against namespaced calls (pkg::base()), and token-boundary on the left.
         */
        for (Map.Entry<String, String> e : helperMap.entrySet()) {
            String base = e.getKey();
            String tgt = e.getValue();

            // (?<![\\w\\.:])  = not preceded by word char, dot, or colon (avoids foo.bar( and pkg::foo()
            // \\s*\\(          = actual call
            String callPat = "(?<![\\w\\.:])" + Pattern.quote(base) + "\\s*\\(";
            llmOut = llmOut.replaceAll(callPat, tgt + "(");
        }

        /* Optional: if the main ever self-calls via its original R name (rare), rewrite too.
    * For example, if resolvedMain appears as a call token inside body.
         */
        String resolvedMainBase = resolvedMain; // from step 0
        if (resolvedMainBase != null && !resolvedMainBase.isBlank()) {
            String callPat = "(?<![\\w\\.:])" + Pattern.quote(resolvedMainBase) + "\\s*\\(";
            llmOut = llmOut.replaceAll(callPat, aiMain + "(");
        }

        // <---------------- NEW ADDITION ------------------------------------->
        // debugging: show the final transformed code
        System.out.println("Final transformed code:\n" + llmOut);

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
            String rCmd = "paste(deparse(" + rFuncName + "), collapse='\\n')";  // returns the code in the R session as txt for the LLM, e.g. r code for my.plot.volcano
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
            String modified = originalCmd.replaceFirst("([\\w.]+)\\(", "$1" + aiSuffix + "("); // added . in regex
            System.out.println(modified);
            RConnection rc = sb.getRConnection();
            RCenter.recordRCommand(rc, modified);
            rc.voidEval(modified);
        } catch (Exception e) {
            LOG.severe("Error in executeAICustomizedPlot: " + e.getMessage());
        }
    }

    private static String err(String msg) {
        return "Error: " + msg;
    }

    /*───────────────────────────────────────────────────────────────────────────
     *  SUPPORT CLASSES
     *─────────────────────────────────────────────────────────────────────────*/
    private record ChatMessage(String role, String content) {

    }

    private static final Map<String, FunctionMapping> FUNCTION_MAPPINGS = Map.of(
            "PlotVolcano", new FunctionMapping("my.plot.volcano", "util_volcano.Rc"),
            "plotVennDiagram", new FunctionMapping("my.plot.venn", "util_venndiagram.Rc"),
            "Plot3D", new FunctionMapping("my.plot.scatter3d", "util_scatter3d.Rc")
    );

    private record FunctionMapping(String rFuncName, String rScriptFile) {

    }
}
