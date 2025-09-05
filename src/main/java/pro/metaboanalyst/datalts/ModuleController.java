package pro.metaboanalyst.datalts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
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
        if (dc.getSelected() == null) {
            return;
        }
        disableAllModules();
        switch (dc.getSelected().getModule()) {
            case "stat" -> {
                nodeVisibility.put("functionalAnalysis", true);
                nodeVisibility.put("biomarkerAnalysis", true);
                nodeVisibility.put("statisticalAnalysis1", true);
                nodeVisibility.put("doseResponse", true);

            }
            case "dose" -> {
                nodeVisibility.put("functionalAnalysis", true);
                nodeVisibility.put("biomarkerAnalysis", true);
                nodeVisibility.put("statisticalAnalysis1", true);
                nodeVisibility.put("doseResponse", true);
                if (dc.getSelected().isHasMetadata()) {
                    nodeVisibility.put("statisticalAnalysis2", true);
                }
            }
            case "mf" -> {
                nodeVisibility.put("functionalAnalysis", true);
                nodeVisibility.put("biomarkerAnalysis", true);
                nodeVisibility.put("statisticalAnalysis1", true);
                nodeVisibility.put("doseResponse", true);
                nodeVisibility.put("statisticalAnalysis2", true);
            }
            case "roc" -> {
                nodeVisibility.put("functionalAnalysis", true);
                nodeVisibility.put("biomarkerAnalysis", true);
                nodeVisibility.put("statisticalAnalysis1", true);
                nodeVisibility.put("doseResponse", true);
                if (dc.getSelected().isHasMetadata()) {
                    nodeVisibility.put("statisticalAnalysis2", true);
                }
            }
            case "pathqea", "msetqea" -> {
                nodeVisibility.put("functionalAnalysis", true);
                nodeVisibility.put("biomarkerAnalysis", true);
                nodeVisibility.put("statisticalAnalysis1", true);
                nodeVisibility.put("doseResponse", true);
                nodeVisibility.put("enrichment", false);
                nodeVisibility.put("pathway", false);
                if (dc.getSelected().isHasMetadata()) {
                    nodeVisibility.put("statisticalAnalysis2", true);
                }
            }
            case "pathqora", "msetora" -> {
                nodeVisibility.put("enrichment", false);
                nodeVisibility.put("pathway", false);
            }
            case "mummichog" -> {
                nodeVisibility.put("functionalAnalysis", true);
                nodeVisibility.put("biomarkerAnalysis", true);
                nodeVisibility.put("statisticalAnalysis1", true);
                nodeVisibility.put("doseResponse", true);
                if (dc.getSelected().isHasMetadata()) {
                    nodeVisibility.put("statisticalAnalysis2", true);
                }
            }
            case "metadata" -> {
                nodeVisibility.put("statMeta", true);

            }
            case "raw" -> {
                nodeVisibility.put("spectraProcessing", true);
                nodeVisibility.put("peakAnnotation", true);
            }
            default -> {
            }
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
        var ctx = FacesContext.getCurrentInstance();
        try {
            String idxStr = ctx.getExternalContext().getRequestParameterMap().get("idx");
            int idx = Integer.parseInt(idxStr);

            // If you want to check visibility server-side as well (defense-in-depth):
            // if (!visible(keyFromIdx(idx))) { return; }
            // reuse your existing navigation logic:
            boolean res = true;
            DatasetRow ds = dc.getSelected();
            System.out.println("ds.getFiles()length=====" + ds.getFiles());
            String dataName = "";
            String metaName = "";

            for (DatasetFile f : ds.getFiles()) {
                String fname = f.getFilename();
                System.out.println(f.getRole() + "=============ds.getFiles()");
                if ("data".equalsIgnoreCase(f.getRole())) {
                    dataName = fname;
                }
                if ("metadata".equalsIgnoreCase(f.getRole())) {
                    metaName = fname;
                }
            }
            if (res) {
                RConnection RC = sb.getRConnection();
                String analType = sb.getAnalType();
                String naviType = analType;
                if (idx == 6) {
                    analType = "stat";
                    naviType = "stat";
                    RDataUtils.initDataObjects(RC, sb.getDataType(), analType, sb.isPaired());

                    res = RDataUtils.readTextDataReload(sb.getRConnection(), dataName);
                } else if (idx == 8) {
                    analType = "roc";
                    naviType = "roc";
                    RDataUtils.initDataObjects(RC, sb.getDataType(), analType, sb.isPaired());

                    res = RDataUtils.readTextDataReload(sb.getRConnection(), dataName);
                    if (!metaName.equals("")) {
                        boolean ok = RDataUtils.readMetaData(RC, metaName);
                    }

                }

                sb.setAnalType(analType);
                RDataUtils.loadRscriptsOnDemand(RC, analType);
                sb.initNaviTree("roc");
                DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/process/SanityCheck.xhtml", "info", "Dataset loaded, please proceed with analysis!");
            }

        } catch (Exception e) {

        }
    }
}
