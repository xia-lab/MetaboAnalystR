package pro.metaboanalyst.agents;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RCenter;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

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
    private transient ApplicationBean1 ab;

    @Inject
    private transient SessionBean1 sb;

    private transient GoogleAIClient aiClient;
    private final List<ChatMessage> chatHistory = new ArrayList<>();

    /*───────────────────────────────────────────────────────────────────────────
     *  LIFE-CYCLE
     *─────────────────────────────────────────────────────────────────────────*/
    @PostConstruct
    private void init() {
        if (ab == null) {
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

        //System.out.println(prompt + "\n");

        chatHistory.add(new ChatMessage("user", userRequest));

        /*── 4. call the model ──────────────────────────────────────────────*/
        String llmOut = aiClient.generateText(prompt);
        chatHistory.add(new ChatMessage("model", llmOut));

        // debugging
        //System.out.println("Model Response: \n" + llmOut);

        /*── 5. sanitize + suffix main & helpers + rewrite call-sites ─────────────*/
        llmOut = llmOut
                .replaceAll("(?s)```[rR]?\\s*", "")
                .replaceAll("(?s)```\\s*$", "")
                .trim();

        final String aiSuffix = "AI";
        final String aiMain = functionName + aiSuffix;

        /* Detect helpers:
 *  A) Header style:   # Helper: <Name>\nfunction(...)
 *  B) AI-assigned:    <Name>AI <- function(...)
 *  C) BASE-assigned:  <Name>    <- function(...)     (incl. leading dots)
         */
        Pattern hdrPat = Pattern.compile("(?m)^\\s*#\\s*Helper:\\s*([\\w\\.]+)\\s*\\n\\s*function\\b");
        Pattern asgAIPat = Pattern.compile("(?m)^\\s*([\\w\\.]+)" + Pattern.quote(aiSuffix) + "\\s*<-\\s*function\\b");
        Pattern asgBasePat = Pattern.compile("(?m)^\\s*([\\w\\.]+)\\s*<-\\s*function\\b");

        LinkedHashMap<String, String> helperMap = new LinkedHashMap<>(); // base -> target

// (A) header-tagged helpers
        Matcher mh = hdrPat.matcher(llmOut);
        while (mh.find()) {
            String raw = mh.group(1);
            String base = raw.endsWith(aiSuffix) ? raw.substring(0, raw.length() - aiSuffix.length()) : raw;
            helperMap.put(base, base + aiSuffix);
        }

// (B) helpers already assigned with AI
        Matcher ma = asgAIPat.matcher(llmOut);
        while (ma.find()) {
            String base = ma.group(1);
            if (!base.equals(functionName) && !base.equals(resolvedMain)) {
                helperMap.putIfAbsent(base, base + aiSuffix);
            }
        }

// (explicit) helpers provided by Java mapping
        for (String h : helpers) {
            String base = FUNCTION_MAPPINGS.getOrDefault(h, new FunctionMapping(h, null)).rFuncName;
            if (base != null && !base.isBlank()) {
                helperMap.putIfAbsent(base, base + aiSuffix);
            }
        }

// (C) any other top-level function assignments (incl. leading-dot helpers)
        Matcher mb = asgBasePat.matcher(llmOut);
        while (mb.find()) {
            String name = mb.group(1);
            if (name.endsWith(aiSuffix)) {
                continue;                 // already AI
            }
            if (name.equals(functionName) || name.equals(resolvedMain)) {
                continue; // skip main
            }
            helperMap.putIfAbsent(name, name + aiSuffix);
        }

        /* Normalize header helpers to include AI assignment */
        for (Map.Entry<String, String> e : helperMap.entrySet()) {
            String baseQ = Pattern.quote(e.getKey());
            String tgt = e.getValue();
            llmOut = llmOut.replaceAll(
                    "(?m)^\\s*#\\s*Helper:\\s*" + baseQ + "\\s*\\n\\s*function\\b",
                    "# Helper: " + tgt + "\n" + tgt + " <- function"
            );
        }

        /* Convert any BASE helper assignments to AI assignments */
        for (Map.Entry<String, String> e : helperMap.entrySet()) {
            String baseQ = Pattern.quote(e.getKey());
            String tgt = e.getValue();
            llmOut = llmOut.replaceAll(
                    "(?m)^\\s*" + baseQ + "\\s*<-\\s*function\\b",
                    tgt + " <- function"
            );
        }

        /* Ensure the MAIN is assigned to <functionName>AI */
        if (llmOut.startsWith("function")) {
            llmOut = aiMain + " <- " + llmOut;
        } else {
            llmOut = llmOut.replaceFirst("(?ms)^(\\s*)([\\w\\.]+)\\s*<-\\s*function\\b",
                    "$1" + aiMain + " <- function");
        }

        /* Rewrite ALL helper call-sites and dynamic lookups to AI names */
        for (Map.Entry<String, String> e : helperMap.entrySet()) {
            String base = e.getKey();
            String tgt = e.getValue();

            // calls: base(  -> baseAI(   (avoid pkg::base and foo.bar)
            String callPat = "(?<![\\w\\.:])" + Pattern.quote(base) + "\\s*\\(";
            llmOut = llmOut.replaceAll(callPat, tgt + "(");

            // exists('base') / exists("base") -> exists('baseAI')
            llmOut = llmOut.replaceAll("\\bexists\\s*\\(\\s*'" + Pattern.quote(base) + "'\\s*\\)", "exists('" + tgt + "')");
            llmOut = llmOut.replaceAll("\\bexists\\s*\\(\\s*\"" + Pattern.quote(base) + "\"\\s*\\)", "exists(\"" + tgt + "\")");

            // get('base') / get("base") -> get('baseAI')
            llmOut = llmOut.replaceAll("\\bget\\s*\\(\\s*'" + Pattern.quote(base) + "'\\s*", "get('" + tgt + "'");
            llmOut = llmOut.replaceAll("\\bget\\s*\\(\\s*\"" + Pattern.quote(base) + "\"\\s*", "get(\"" + tgt + "\"");
        }

        /* Canonicalize main variants (dotted/undotted, case) in defs & calls */
        llmOut = llmOut.replaceFirst(
                "(?mis)^\\s*[\\.]?" + Pattern.quote(functionName + aiSuffix) + "\\s*<-\\s*function\\b",
                aiMain + " <- function"
        );
        llmOut = llmOut.replaceAll(
                "(?i)(?<![\\w\\.:])\\.?" + Pattern.quote(functionName + aiSuffix) + "\\s*\\(",
                aiMain + "("
        );

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
        //System.out.println("Final transformed code:\n" + llmOut);

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

        try {
            Path saved = saveAIScriptToFile(key, functionName, llmOut, userRequest);
            sb.addMessage("info", "Saved AI script, you can generate high generation image now.");
        } catch (Exception e) {
            sb.addMessage("error", "Failed to persist AI script: " + e.getMessage());
        }

        return llmOut;  // for logging / debugging
    }

    /*───────────────────────────────────────────────────────────────────────────
     *  INTERNAL HELPERS
     *─────────────────────────────────────────────────────────────────────────*/
    private Optional<String> readFunctionCode(String rFuncName) {
        try {
            RConnection rc = sb.getRConnection();
            String nm = rFuncName; // expected R symbol name
            String nmQ = rQuote(nm);

            // 1) Does the function exist (anywhere on the search path)?
            boolean exists = rc.eval("exists('" + nmQ + "', inherits=TRUE)").asInteger() == 1;

            // 2) If missing, attempt lazy load from mapped script
            if (!exists) {
                String script = R_SCRIPT_MAP.get(nm);
                if (script != null) {
                    String spQ = rQuote(script);
                    String lazyLoadR
                            = "local({\n"
                            + "  nm <- '" + nmQ + "'\n"
                            + "  p  <- '" + spQ + "'\n"
                            + "  # If 'p' is relative, prepend rpath; else use as-is\n"
                            + "  full <- if (grepl('^(/|[A-Za-z]:)', p)) p else file.path(rpath, p)\n"
                            + "  if (file.exists(full)) {\n"
                            + "    if (grepl('\\\\.Rc$', full)) {\n"
                            + "      try(compiler::loadcmp(full), silent=TRUE)\n"
                            + "    } else {\n"
                            + "      try(source(full, encoding='UTF-8'), silent=TRUE)\n"
                            + "    }\n"
                            + "  }\n"
                            + "  exists(nm, inherits=TRUE)\n"
                            + "})";
                    exists = rc.eval(lazyLoadR).asInteger() == 1;
                }
            }

            if (!exists) {
                LOG.warning("Function not found even after lazy load: " + nm);
                return Optional.empty();
            }

            // 3) Robust source capture (works for byte-compiled fns)
            String dumpR
                    = "local({ nm <- '" + nmQ + "'; paste(capture.output(dump(nm, file='')), collapse='\\n') })";
            String code = rc.eval(dumpR).asString();
            return Optional.ofNullable(code);

        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Failed to fetch R function from RConnection", ex);
            return Optional.empty();
        }
    }

    private static String rQuote(String s) {
        return s == null ? "" : s.replace("\\", "\\\\").replace("'", "\\'");
    }
    private static final Map<String, String> R_SCRIPT_MAP = Map.ofEntries(
            Map.entry(".plot.pca.pair.meta", "rscripts/MetaboAnalystR/R/util_pcapair.Rc")
    );

    private void executeAICustomizedPlot(String originalCmd, String aiSuffix) {
        if (originalCmd == null || originalCmd.isBlank()) {
            LOG.warning("No graphics command found for key");
            return;
        }
        try {
            // extract the first called function (start of the command)
            Matcher m = Pattern.compile("(?m)^\\s*([\\w.]+)\\s*\\(").matcher(originalCmd);
            if (!m.find()) {
                sb.addMessage("warn", "Could not detect plot function; using the default plot.");
                RConnection rc = sb.getRConnection();
                RCenter.recordRCommand(rc, originalCmd);
                rc.voidEval(originalCmd);
                return;
            }
            String func = m.group(1);
            String aiFunc = func + aiSuffix;

            RConnection rc = sb.getRConnection();
            boolean hasAI = rc.eval("exists('" + aiFunc + "')").asInteger() == 1;

            String cmdToRun;
            if (hasAI) {
                cmdToRun = originalCmd.replaceFirst("(?m)^\\s*" + Pattern.quote(func) + "\\s*\\(", aiFunc + "(");
            } else {
                sb.addMessage("warn", "AI customization not available for " + func + "; showing the default plot.");
                cmdToRun = originalCmd;
            }

            //System.out.println(cmdToRun);
            RCenter.recordRCommand(rc, cmdToRun);
            rc.voidEval(cmdToRun);

        } catch (Exception e) {
            LOG.severe("Error in executeAICustomizedPlot: " + e.getMessage());
            sb.addMessage("warn", "AI plot execution failed; using the default plot.");
            try {
                RConnection rc = sb.getRConnection();
                RCenter.recordRCommand(rc, originalCmd);
                rc.voidEval(originalCmd);
            } catch (Exception inner) {
                LOG.severe("Fallback plot also failed: " + inner.getMessage());
            }
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

    private Path saveAIScriptToFile(String source, String plotType, String aiCode, String prompt) throws Exception {
        if (aiCode == null || aiCode.isBlank()) {
            throw new IllegalArgumentException("Empty AI code");
        }
        if (ab == null) {
            throw new IllegalStateException("Application bean 'ab' not injected");
        }
        if (sb == null || sb.getCurrentUser() == null) {
            throw new IllegalStateException("Session/user not available");
        }

        // Target dir (must already exist)
        Path outDir = Path.of(sb.getCurrentUser().getHomeDir());
        if (!Files.isDirectory(outDir)) {
            throw new IllegalStateException("Target directory not found: " + outDir);
        }

        // Stable filename (no timestamp)
        String safePlot = (plotType == null ? "UnknownPlot" : plotType).replaceAll("[^A-Za-z0-9._-]", "_");
        String fileName = safePlot + "AI.R";
        Path outFile = outDir.resolve(fileName);

        // Sanitize code
        String code = aiCode
                .replaceAll("(?s)```[rR]?\\s*", "")
                .replaceAll("(?s)```\\s*$", "")
                .replace("\r\n", "\n").replace("\r", "\n")
                .trim();
        if (!code.endsWith("\n")) {
            code += "\n";
        }

        // Date (America/Toronto)
        ZoneId tz = ZoneId.of("America/Toronto");
        String savedAt = ZonedDateTime.now(tz).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss z"));

        // Provenance header (now includes date)
        String header
                = "# --- AI customized plot -------------------------------------\n"
                + "# Date: " + savedAt + "\n"
                + "# Prompt: " + (prompt == null ? "" : prompt.replaceAll("[\\r\\n]+", " ").trim()) + "\n"
                + "# -------------------------------------------------------------\n\n";

        // Atomic write
        Path tmp = Files.createTempFile(outDir, ".tmp_ai_", ".R");
        Files.writeString(tmp, header + code, StandardCharsets.UTF_8, StandardOpenOption.TRUNCATE_EXISTING);
        try {
            Files.move(tmp, outFile, StandardCopyOption.ATOMIC_MOVE);
        } catch (AtomicMoveNotSupportedException e) {
            Files.move(tmp, outFile, StandardCopyOption.REPLACE_EXISTING);
        }
        return outFile;
    }

    public void resetToDefault(String key) {
        try {
            // 0) Switch UI back to default rendering
            sb.setGraphTypeOptAI("default");

            // 1) Resolve plotType & R main function (same logic you use elsewhere)
            String plotType = RPlotCustomizationBean.getGRAPHICS_CMD_TO_R_FUNC().get(key);
            if (plotType == null && key != null) {
                // best-effort contains-match
                final String src = key.toLowerCase(Locale.ROOT);
                String bestKey = null;
                for (String k : RPlotCustomizationBean.getGRAPHICS_CMD_TO_R_FUNC().keySet()) {
                    if (k != null && src.contains(k.toLowerCase(Locale.ROOT))) {
                        if (bestKey == null || k.length() > bestKey.length()) {
                            bestKey = k;
                        }
                    }
                }
                if (bestKey != null) {
                    plotType = RPlotCustomizationBean.getGRAPHICS_CMD_TO_R_FUNC().get(bestKey);
                }
            }
            if (plotType == null) {
                sb.addMessage("warn", "Could not determine plot function to reset.");
                return;
            }

            // 2) Compute AI symbol names to remove
            final String aiSuffix = "AI";
            final String mainBase = FUNCTION_MAPPINGS.getOrDefault(plotType, new FunctionMapping(plotType, null)).rFuncName;
            final String mainAI = plotType + aiSuffix; // NOTE: you name the MAIN by UI name+AI
            // Helper bases (from your HELPERS map if you have one)
            List<String> helperBases = RPlotCustomizationBean.getHELPERS().getOrDefault(plotType, List.of());
            // Also consider that helpers may be dotted names; remove both dotted/undotted AI variants
            List<String> aiSymbols = new ArrayList<>();
            aiSymbols.add(mainAI);
            for (String hb : helperBases) {
                String base = FUNCTION_MAPPINGS.getOrDefault(hb, new FunctionMapping(hb, null)).rFuncName;
                if (base == null || base.isBlank()) {
                    base = hb;
                }
                aiSymbols.add(base + aiSuffix);
                if (!base.startsWith(".")) {
                    aiSymbols.add("." + base + aiSuffix);
                }
            }

            // 3) Remove AI symbols from the R session (don’t touch the originals)
            RConnection rc = sb.getRConnection();
            String vec = aiSymbols.stream()
                    .map(s -> "'" + s.replace("'", "\\'") + "'")
                    .reduce((a, b) -> a + "," + b).orElse("");
            rc.voidEval(
                    "local({ nm <- c(" + vec + "); "
                    + "  nm <- nm[nm %in% ls(envir=.GlobalEnv, all.names=TRUE)]; "
                    + "  if (length(nm)) rm(list=nm, envir=.GlobalEnv); "
                    + "})"
            );

            // 4) Reload the original main from disk (if you have a mapping)
            FunctionMapping fm = FUNCTION_MAPPINGS.get(plotType);
            if (fm != null && fm.rScriptFile() != null) {
                String fileQ = fm.rScriptFile().replace("'", "\\'");
                // Use your lazy loader if available; otherwise load compiled or source
                rc.voidEval(
                        "if (exists('.load.scripts.on.demand')) "
                        + "  try(.load.scripts.on.demand('" + fileQ + "'), silent=TRUE) "
                        + "else {"
                        + "  full <- if (grepl('^(/|[A-Za-z]:)', '" + fileQ + "')) '" + fileQ + "' else file.path(rpath, '" + fileQ + "');"
                        + "  if (file.exists(full)) {"
                        + "    if (grepl('\\\\.Rc$', full)) try(compiler::loadcmp(full), silent=TRUE) else try(source(full, encoding='UTF-8'), silent=TRUE);"
                        + "  }"
                        + "}"
                );
            }

            // 5) Delete the saved AI script file (optional but recommended so “AI-generated” won’t reuse)
            try {
                Path outDir = Path.of(sb.getCurrentUser().getHomeDir());
                Path aiFile = outDir.resolve(deriveAIScriptFileNameStable(plotType)); // e.g. PlotSAM.CmpdAI.R
                Files.deleteIfExists(aiFile);
            } catch (Exception ignore) {
                // non-fatal: script might not exist
            }

            //replot
            String rcmd = sb.getGraphicsMap().get(key);
            try {
                RCenter.recordRCommand(sb.getRConnection(), rcmd);
                sb.getRConnection().voidEval(rcmd);
            } catch (Exception ignore) {

            }

            sb.addMessage("info", "Restored default plotting function for: " + plotType);

        } catch (Exception ex) {
            LOG.log(Level.SEVERE, "Reset failed", ex);
            sb.addMessage("error", "Reset failed: " + ex.getMessage());
        }
    }

    private String deriveAIScriptFileNameStable(String plotType) {
        String safePlot = (plotType == null ? "UnknownPlot" : plotType)
                .replaceAll("[^A-Za-z0-9._-]", "_");
        return safePlot + "AI.R";          // e.g., PlotSAM.CmpdAI.R
    }

}
