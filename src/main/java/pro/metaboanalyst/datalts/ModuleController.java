package pro.metaboanalyst.datalts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;
import pro.metaboanalyst.utils.DataUtils;

@SessionScoped
@Named("moduleController")
public class ModuleController implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private DatasetController dc;

    private Map<String, Boolean> nodeVisibility = new HashMap<>();

    private final List<String> untargetedDatas = Arrays.asList("spec", "specbin", "pktable", "nmrpeak", "mspeak");
    private final List<String> regresAnals = Arrays.asList("pathway", "enrich");
    private final List<String> compatibleDatas = Arrays.asList("conc", "spec", "specbin", "pktable", "nmrpeak", "mspeak", "mass_table");
    private final List<String> compatibleAnals = Arrays.asList("stat", "roc", "raw", "power", "mummichog", "pathqea", "msetqea", "pathway", "enrich", "dose");
    private final List<String> compatibleListAnals = Arrays.asList("pathora", "msetora", "pathway", "enrich");
    private final List<String> targetedAnals = Arrays.asList("pathora", "pathqea", "msetora", "msetqea", "pathway", "enrich");

    public ModuleController() {
        // defaults: everything visible
        nodeVisibility.put("spectraProcessing", true);
        nodeVisibility.put("peakAnnotation", true);
        nodeVisibility.put("functionalAnalysis", true);
        nodeVisibility.put("functionalMetaAnalysis", true);
        nodeVisibility.put("statisticalAnalysis1", true);
        nodeVisibility.put("statisticalAnalysis2", true);
        nodeVisibility.put("biomarkerAnalysis", true);
        nodeVisibility.put("doseResponse", true);
        nodeVisibility.put("statMeta", true);
        nodeVisibility.put("enrichment", true);
        nodeVisibility.put("pathway", true);
        nodeVisibility.put("network", true);
        nodeVisibility.put("causalAnalysis", true);
    }

    private void disableAllModules() {
        nodeVisibility.put("spectraProcessing", false);
        nodeVisibility.put("peakAnnotation", false);
        nodeVisibility.put("functionalAnalysis", false);
        nodeVisibility.put("functionalMetaAnalysis", false);
        nodeVisibility.put("statisticalAnalysis1", false);
        nodeVisibility.put("statisticalAnalysis2", false);
        nodeVisibility.put("biomarkerAnalysis", false);
        nodeVisibility.put("doseResponse", false);
        nodeVisibility.put("statMeta", false);
        nodeVisibility.put("enrichment", false);
        nodeVisibility.put("pathway", false);
        nodeVisibility.put("network", false);
        nodeVisibility.put("causalAnalysis", false);
    }

    public Map<String, Boolean> getNodeVisibility() {
        return nodeVisibility;
    }

    public void toggleNodeVisibility(String nodeTitle, boolean show) {
        nodeVisibility.put(nodeTitle, show);
    }

    public boolean isVisible(String nodeTitle) {
        return nodeVisibility.getOrDefault(nodeTitle, false);
    }

    public boolean visible(String nodeTitle) {   // <- alias for EL
        return isVisible(nodeTitle);
    }

    public void checkAvailableModules() {
        DatasetRow ds = dc.getSelected();
        if (ds == null) {
            return;
        }
        disableAllModules();

        // ---- Facts about this dataset ----
        final String dt = (ds.getDataType() == null) ? "" : ds.getDataType().toLowerCase();
        final String mod = (ds.getModule() == null) ? "" : ds.getModule().toLowerCase();
        final boolean hasMeta = ds.isHasMetadata();
        final boolean isUntargeted = untargetedDatas.contains(dt);

        System.out.println(mod + "===========================mod");

        // Buckets from your provided lists
        final boolean isCompatible = compatibleAnals.contains(mod) || compatibleListAnals.contains(mod);
        final boolean isListAnal = compatibleListAnals.contains(mod);                 // ORA (or generic that can fork to ORA)
        final boolean isTargetedAnal = targetedAnals.contains(mod);                       // ORA/QEA/generics
        final boolean isQEA = "pathqea".equals(mod) || "msetqea".equals(mod);    // explicit QEA
        final boolean isGenericFunc = regresAnals.contains(mod);                         // "pathway" or "enrich" generic entry
        final boolean isCoreStats = "stat".equals(mod) || "roc".equals(mod) || "mf".equals(mod) || "dose".equals(mod);
        final boolean isMummichog = "mummichog".equals(mod);
        final boolean isRaw = "raw".equals(mod);
        System.out.println(isCoreStats + "===========================isCoreStats");

        // Helpers
        java.util.function.Consumer<String> on = k -> nodeVisibility.put(k, true);
        java.util.function.Consumer<String> off = k -> nodeVisibility.put(k, false);

        Runnable enableStatsBundle = () -> {
            on.accept("statisticalAnalysis1");
            on.accept("biomarkerAnalysis");
            if (hasMeta) {
                on.accept("statisticalAnalysis2");
            }
            on.accept("doseResponse");
        };

        Runnable maybeEnableFunctional = () -> {
            if (isUntargeted) {
                on.accept("functionalAnalysis");
            } else {
                off.accept("functionalAnalysis");
            }
        };

        // ------------ Module-specific visibility ------------
        if (isCoreStats) {
            // Core table analyses
            enableStatsBundle.run();
            maybeEnableFunctional.run();
        }else if (isRaw) {
            on.accept("spectraProcessing");
            on.accept("peakAnnotation");
            // no stats/functional until peak tables exist
        } else if (isMummichog) {
            // Untargeted network/annotation workflow
            maybeEnableFunctional.run();
            enableStatsBundle.run();
        } else if (isListAnal) {
            // ORA list-based: enrichment/pathway only
            on.accept("enrichment");
            on.accept("pathway");
            off.accept("functionalAnalysis");
        } else if (isQEA) {
            // QEA data+meta: enrichment/pathway + stats bundle
            on.accept("enrichment");
            on.accept("pathway");
            enableStatsBundle.run();
            maybeEnableFunctional.run();
        }  else if (isGenericFunc || isTargetedAnal) {
            // Generic "pathway"/"enrich" entry or other targeted keys:
            // Always show enrichment/pathway; if metadata is present we also expose stats bundle.
            on.accept("enrichment");
            on.accept("pathway");
        }

        // Final guard: functional tile only for untargeted data types
        if (!isUntargeted) {
            off.accept("functionalAnalysis");
        }
    }


    /*
    url1 = switch (num) {
            case 0 ->
                "/Secure/upload/PeakUploadView.xhtml";
            case 1 ->
                "/Secure/upload/MetaPathLoadView.xhtml";
            case 2 ->
                "/Secure/upload/EnrichUploadView.xhtml";
            case 3 ->
                "/Secure/upload/PathUploadView.xhtml";
            case 4 ->
                "/Secure/upload/JointUploadView.xhtml";
            case 5 ->
                "/Secure/upload/MnetUploadView.xhtml";
            case 6 ->
                "/Secure/upload/StatUploadView.xhtml";
            case 7 ->
                "/Secure/upload/MultifacUploadView.xhtml";
            case 8 ->
                "/Secure/upload/RocUploadView.xhtml";
            case 9 ->
                "/Secure/upload/MetaLoadView.xhtml";
            case 10 ->
                "/Secure/upload/PowerUploadView.xhtml";
            case 11 ->
                "/Secure/upload/DoseUploadView.xhtml";
            case 12 ->
                "/Secure/upload/MgwasUploadView.xhtml";
            case 14 ->
                "/Secure/upload/MS2UploadView.xhtml";
            default ->
                "/Secure/upload/StatUploadView.xhtml";
        };
     */
    public void openModuleRC() {
        FacesContext ctx = FacesContext.getCurrentInstance();
        try {
            String idxStr = ctx.getExternalContext().getRequestParameterMap().get("idx");
            int idx = Integer.parseInt(idxStr);

            DatasetRow ds = dc.getSelected();
            if (ds == null) {
                sb.addMessage("Error", "No dataset selected to load.");
                return;
            }
            if (ds.getFiles() == null || ds.getFiles().isEmpty()) {
                sb.addMessage("Error", "Selected dataset has no files.");
                return;
            }

            // --- Gather filenames by role ---
            String dataName = null, data2Name = null, metaName = null, listName = null, ms2Name = null, rawName = null;
            for (DatasetFile f : ds.getFiles()) {
                String role = f.getRole() == null ? "" : f.getRole().toLowerCase();
                System.out.println(role + "====role");
                switch (role) {
                    case "data" ->
                        dataName = f.getFilename();
                    case "data2" ->
                        data2Name = f.getFilename();
                    case "metadata" ->
                        metaName = f.getFilename();
                    case "list" ->
                        listName = f.getFilename();
                    case "ms2" ->
                        ms2Name = f.getFilename();
                    case "raw" ->
                        rawName = f.getFilename();
                }
            }

            // --- Map idx to desired module behavior ---
            enum InputMode {
                DATA_ONLY, DATA_PLUS_META, LIST_ONLY, JOINT_TWO_TABLES, MS2_ONLY, RAW_MODE, MGWAS_ONLY
            }
            String analType;
            String naviType;
            InputMode mode;

            switch (idx) {
                case 0 -> {
                    analType = "mummichog";
                    naviType = "mummichog";
                    mode = InputMode.DATA_ONLY;
                }          // PeakUploadView (mummichog)
                case 1 -> {
                    analType = "pathway";
                    naviType = "pathway";
                    mode = InputMode.DATA_PLUS_META;
                }      // MetaPathLoadView (QEA)
                case 2 -> {
                    analType = (sb.getUploadType().equals("list") ? "msetora" : "msetqea");
                    naviType = "enrich";
                    mode = (sb.getUploadType().equals("list") ? InputMode.LIST_ONLY : InputMode.DATA_ONLY);
                } // EnrichUploadView
                case 3 -> {
                    analType = (sb.getUploadType().equals("list") ? "pathora" : "pathqea");
                    naviType = "pathway";
                    mode = (sb.getUploadType().equals("list") ? InputMode.LIST_ONLY : InputMode.DATA_ONLY);
                } // PathUploadView
                case 4 -> {
                    analType = "pathinteg";
                    naviType = "pathinteg";
                    mode = InputMode.JOINT_TWO_TABLES;
                }    // JointUploadView
                case 5 -> {
                    analType = "mnet";
                    naviType = "mnet";
                    mode = InputMode.DATA_ONLY;
                }           // MnetUploadView
                case 6 -> {
                    analType = "stat";
                    naviType = "stat";
                    mode = InputMode.DATA_ONLY;
                }           // StatUploadView
                case 7 -> {
                    analType = "mf";
                    naviType = "mf";
                    mode = InputMode.DATA_PLUS_META;
                }      // MultifacUploadView (time/meta module) 
                case 8 -> {
                    analType = "roc";
                    naviType = "roc";
                    mode = InputMode.DATA_ONLY;
                }      // RocUploadView
                case 9 -> {
                    analType = "metadata";
                    naviType = "meta";
                    mode = (dataName != null ? InputMode.DATA_PLUS_META : InputMode.DATA_ONLY);
                } // MetaLoadView
                case 10 -> {
                    analType = "power";
                    naviType = "power";
                    mode = InputMode.DATA_ONLY;
                }      // PowerUploadView
                case 11 -> {
                    analType = "dose";
                    naviType = "dose";
                    mode = InputMode.DATA_ONLY;
                }      // DoseUploadView
                case 12 -> {
                    analType = "mgwas";
                    naviType = "mgwas";
                    mode = InputMode.MGWAS_ONLY;
                }    // MgwasUploadView
                case 14 -> {
                    analType = "ms2";
                    naviType = "ms2";
                    mode = InputMode.MS2_ONLY;
                }            // MS2UploadView
                default -> {
                    analType = "stat";
                    naviType = "stat";
                    mode = InputMode.DATA_ONLY;
                }
            }

            final String dataType = sb.getDataType() == null ? "" : sb.getDataType().toLowerCase();

            // Compatibility checks
            if (!compatibleDatas.contains(dataType) && mode != InputMode.LIST_ONLY && mode != InputMode.MS2_ONLY) {
                sb.addMessage("Error", "Data type “" + dataType + "” not compatible with this module.");
                return;
            }
            if (analType.equals("mummichog") && !untargetedDatas.contains(dataType)) {
                sb.addMessage("Error", "Mummichog requires untargeted peak data (spec/specbin/pktable/nmrpeak/mspeak).");
                return;
            }

            if (metaName != null && mode.equals(InputMode.DATA_ONLY)) {
                mode = InputMode.DATA_PLUS_META;
            }

            // Init R
            RConnection RC = sb.getRConnection();
            sb.setAnalType(analType);
            RDataUtils.initDataObjects(RC, sb.getDataType(), analType, sb.isPaired());

            // smart loader: mzTab vs text
            java.util.function.BiFunction<RConnection, String, Boolean> loadSmart = (rc, fname) -> {
                if (fname == null) {
                    return false;
                }
                final String lower = fname.toLowerCase();
                if (lower.endsWith(".mztab") || "mztab".equalsIgnoreCase(ds.getType())) {
                    return RDataUtils.readMzTabDataReload(rc, fname);
                } else {
                    return RDataUtils.readTextDataReload(rc, fname);
                }
            };

            boolean ok = true;

            switch (mode) {
                case DATA_ONLY -> {
                    if (dataName == null) {
                        sb.addMessage("Error", "No data file (role=data).");
                        return;
                    }
                    ok = loadSmart.apply(RC, dataName);

                    // optional meta for stat/roc/power/dose/multifac
                    if (ok && (analType.equals("stat") || analType.equals("roc") || analType.equals("power")
                            || analType.equals("dose") || analType.equals("mf"))) {
                        if (metaName != null && !metaName.isBlank()) {
                            RDataUtils.readMetaData(RC, metaName);
                        }
                    }

                    // Mummichog starts from peaks (like its upload bean)
                    // (Bean uses analType "mummichog"; keep ours consistent). :contentReference[oaicite:5]{index=5}
                }

                case DATA_PLUS_META -> {
                    // Pathway/Enrich: auto-detect ORA vs QEA

                    // multifac/time, roc, power, dose, metadata …
                    if (dataName != null) {
                        ok = loadSmart.apply(RC, dataName);
                    }
                    if (!ok) {
                        break;
                    }
                    if (metaName != null) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    } else if (analType.equals("roc") || analType.equals("power") || analType.equals("dose") || analType.equals("multifac")) {
                        sb.addMessage("Warn", "No metadata found; some analyses may be limited.");
                    }
                    // Time/multifac upload bean logs into “mf” and reads data+meta; we mirror that by requiring meta when present. :contentReference[oaicite:9]{index=9}

                }

                case LIST_ONLY -> {

                    // Align with beans: set upload type FIRST
                    sb.setUploadType("list"); // like EnrichUploadBean/PathUploadBean.  // :contentReference[oaicite:2]{index=2} :contentReference[oaicite:3]{index=3}

                    // Convert list file -> qVec and set map data in R
                    String[] qVec = DataUtils.readListFileToNames(sb.getCurrentUser().getHomeDir(), "datalist.csv");
                    if (qVec.length == 0) {
                        sb.addMessage("Error", "Your list file appears to be empty.");
                        return;
                    }
                    RDataUtils.setMapData(RC, qVec); // exactly like the upload beans.         // :contentReference[oaicite:4]{index=4} :contentReference[oaicite:5]{index=5}

                    // Choose the cross-reference routine. If you track lipid vs met feature type in state,
                    // reuse it here; otherwise default to the generic exact matcher (same as PathUploadBean).
                    // Example using a flag you maintain elsewhere:
                    boolean isLipid = "lipid".equalsIgnoreCase(sb.getFeatType());
                    if (isLipid) {
                        SearchUtils.crossReferenceExactLipid(sb, sb.getCmpdIDType());          // :contentReference[oaicite:6]{index=6}
                    } else {
                        SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());               // :contentReference[oaicite:7]{index=7} :contentReference[oaicite:8]{index=8}
                    }

                    // Refine the analysis type to the ORA variant like the beans do
                    if ("enrich".equals(analType)) {
                        analType = "msetora";   // Enrich ORA
                    } else {
                        analType = "pathora";   // Pathway ORA
                    }
                    sb.setAnalType(analType);
                }

                case JOINT_TWO_TABLES -> {
                    if (dataName == null || data2Name == null) {
                        sb.addMessage("Error", "Expected two data files (role=data and role=data2).");
                        return;
                    }
                    ok = loadSmart.apply(RC, dataName) && loadSmart.apply(RC, data2Name);
                    if (ok && metaName != null) {
                        ok = RDataUtils.readMetaData(RC, metaName) && ok;
                    }
                }

                case MS2_ONLY -> {
                    String toLoad = (ms2Name != null) ? ms2Name : dataName;
                    if (toLoad == null) {
                        sb.addMessage("Error", "Expected MS/MS or data table.");
                        return;
                    }
                    ok = RDataUtils.readTextDataReload(RC, toLoad);
                }

                case RAW_MODE -> {
                    if (rawName == null) {
                        sb.addMessage("Error", "No raw file (role=raw).");
                        return;
                    }
                    ok = RDataUtils.readTextDataReload(RC, rawName);
                }
            }

            if (!ok) {
                sb.addMessage("Error", "Failed to load the selected dataset for this module.");
                return;
            }

            // Load scripts (per analType) & init nav tree
            RDataUtils.loadRscriptsOnDemand(RC, sb.getAnalType());

            // Some modules have their own loaders/pages, but “SanityCheck” is a safe landing.
            // MetaPath/MetaLoad/Peak/Mnet beans implement their own upload workflows if used directly,
            // but here we’re resuming from a saved dataset, so the analysis tree is sufficient. 
            // (Refs: MetaPathLoadBean manages mixed ion files; MetaLoadBean handles metadata tables; PeakUploadBean runs mummichog; MnetLoadBean maps IDs.) 
            // :contentReference[oaicite:12]{index=12} :contentReference[oaicite:13]{index=13} :contentReference[oaicite:14]{index=14} :contentReference[oaicite:15]{index=15}
            // Choose a sensible nav root
            sb.initNaviTree(
                    switch (sb.getAnalType()) {
                case "msetora" ->
                    "enrich-ora";
                case "msetqea" ->
                    "enrich-qea";
                case "pathora" ->
                    "pathway-ora";
                case "pathqea" ->
                    "pathway-qea";
                case "multifac" ->
                    "multifac";
                default ->
                    naviType;
            }
            );

            switch (mode) {
                case DATA_ONLY -> {

                    DataUtils.doRedirectWithGrowl(
                            sb,
                            "/" + ab.getAppName() + "/Secure/process/SanityCheck.xhtml",
                            "info",
                            "Dataset loaded, please proceed with analysis!"
                    );
                }

                case DATA_PLUS_META -> {

                    DataUtils.doRedirectWithGrowl(
                            sb,
                            "/" + ab.getAppName() + "/Secure/process/SanityCheck.xhtml",
                            "info",
                            "Dataset loaded, please proceed with analysis!"
                    );
                }

                case LIST_ONLY -> {
                    DataUtils.doRedirectWithGrowl(
                            sb,
                            "/" + ab.getAppName() + "/Secure/process/NameMapView.xhtml",
                            "info",
                            "Dataset loaded, please proceed with analysis!"
                    );
                }

                case JOINT_TWO_TABLES -> {
                    DataUtils.doRedirectWithGrowl(
                            sb,
                            "/" + ab.getAppName() + "/Secure/process/NameMapView.xhtml",
                            "info",
                            "Dataset loaded, please proceed with analysis!"
                    );
                }

                case MS2_ONLY -> {

                }

                case RAW_MODE -> {

                }

                default -> {

                }
            }

            DataUtils.doRedirectWithGrowl(
                    sb,
                    "/" + ab.getAppName() + "/Secure/process/SanityCheck.xhtml",
                    "info",
                    "Dataset loaded, please proceed with analysis!"
            );

        } catch (Exception e) {
            sb.addMessage("Error", "Unable to open module: " + e.getMessage());
        }
    }

}
