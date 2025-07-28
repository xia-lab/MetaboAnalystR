/*package pro.metaboanalyst.agents;

import com.t4a.api.JavaMethodAction;
import com.t4a.api.ActionRisk;
import com.t4a.annotations.Action;
import com.t4a.annotations.Agent;
import com.t4a.processor.GeminiV2ActionProcessor;
import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.Dependent;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import pro.metaboanalyst.controllers.general.ApplicationBean1;

/**
 * RPlotCustomizationAgent — concise version that:
 * <ul>
 *   <li>Disables Shell and HTTP loaders via system properties</li>
 *   <li>Implements a single Java‑method action "customizePlot"</li>
 *   <li>Sends exactly one prompt to GeminiV2ActionProcessor, passing <code>this</code>
 *       so no auto‑prediction lookup is required (avoids <code>AIAction null</code> NPE)</li>
 * </ul>
 *
@Named
@Dependent
@Agent(groupName = "plotting", groupDescription = "Customise R plots via Gemini")
public class RPlotCustomizationAgentt4a implements JavaMethodAction, Serializable {

    static {
        // Hard‑disable loaders that cause NPEs when YAML is missing / empty
        System.setProperty("shell.actions.enabled", "false");
        System.setProperty("http.actions.enabled",  "false");
    }

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(RPlotCustomizationAgentt4a.class.getName());

    private static final String SYSTEM_PROMPT = "You are an expert in R programming and data visualisation. " +
            "Update the R plotting function below according to the user's request while preserving core analytical logic. " +
            "Return ONLY the modified R code or a concise status message when no change is required.";

    @Inject
    private transient ApplicationBean1 appBean;

    private transient GeminiV2ActionProcessor processor;
    private static String RSCRIPT_DIR;

    // -------------------------------------------------- lifecycle ---------

    @PostConstruct
    private void init() {
        if (appBean == null) {
            LOG.severe("ApplicationBean1 not injected – cannot resolve R script path");
            return;
        }
        RSCRIPT_DIR = appBean.getRscriptsHomePath() + "/XiaLabPro/R/plotting";
        LOG.info(() -> "Plot script directory = " + RSCRIPT_DIR);
    }

    // ------------------------ JavaMethodAction metadata -------------------

    @Override public Class<?> getActionClass() { return getClass(); }
    @Override public String getActionName()  { return "customizePlot"; }

    // ------------------------------- action -------------------------------

    @Action(description = "Modify an R plotting function per user request", riskLevel = ActionRisk.LOW)
    public String customizePlot(String functionName, String userRequest) {
        if (RSCRIPT_DIR == null) return err("R script directory not initialised");
        ensureProcessor();

        String original = readFunctionCode(functionName).orElse(null);
        if (original == null) return err("Function '%s' not found".formatted(functionName));

        String prompt = SYSTEM_PROMPT + "\n\nHere is the original R function:\n\n" + original +
                "\n\nUser request: " + userRequest;
        try {
            // Pass THIS action explicitly to bypass prediction lookup
            return String.valueOf(processor.processSingleAction(prompt, this));
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Gemini processing failed", ex);
            return err("Gemini error: " + ex.getMessage());
        }
    }

    // -------------------------------- helpers -----------------------------

    private void ensureProcessor() {
        if (processor == null) processor = new GeminiV2ActionProcessor();
    }

    private Optional<String> readFunctionCode(String functionName) {
        String fileName = Character.toUpperCase(functionName.charAt(0)) + functionName.substring(1) + ".R";
        Path file = Path.of(RSCRIPT_DIR, fileName);
        if (!Files.exists(file)) return Optional.empty();
        try {
            String content = Files.readString(file);
            int start = content.indexOf(functionName + " <- function");
            if (start < 0) return Optional.empty();
            int depth = 0, end = start;
            for (int i = start; i < content.length(); i++) {
                char c = content.charAt(i);
                if (c == '{') depth++; else if (c == '}' && --depth == 0) { end = i + 1; break; }
            }
            return Optional.of(content.substring(start, end));
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, "Failed to read R function", ex);
            return Optional.empty();
        }
    }

    private static String err(String msg) { return "Error: " + msg; }
}
*/