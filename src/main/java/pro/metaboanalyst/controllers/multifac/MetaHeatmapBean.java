/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@RequestScoped
@Named("mhmBean")
public class MetaHeatmapBean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private final String pageID = "Metadata";

    private String metaDistOpt = "euclidean";
    private String metaViewOpt = "overview";
    private String metaClusterOpt = "ward.D";
    private String metaClusterSelOpt = "both";
    private String metaColorOpt = "bwm";
    private boolean includeRowNamesMeta = true;
    private boolean drawBordersMeta = false;
    private String corOpt = "pearson";

    public String getMetaViewOpt() {
        return metaViewOpt;
    }

    public void setMetaViewOpt(String metaViewOpt) {
        this.metaViewOpt = metaViewOpt;
    }

    public String getMetaClusterSelOpt() {
        return metaClusterSelOpt;
    }

    public void setMetaClusterSelOpt(String metaClusterSelOpt) {
        this.metaClusterSelOpt = metaClusterSelOpt;
    }

    public String getMetaClusterOpt() {
        return metaClusterOpt;
    }

    public void setMetaClusterOpt(String metaClusterOpt) {
        this.metaClusterOpt = metaClusterOpt;
    }

    public String getMetaColorOpt() {
        return metaColorOpt;
    }

    public void setMetaColorOpt(String metaColorOpt) {
        this.metaColorOpt = metaColorOpt;
    }

    public String getMetaDistOpt() {
        return metaDistOpt;
    }

    public void setMetaDistOpt(String metaDistOpt) {
        this.metaDistOpt = metaDistOpt;
    }

    public boolean isIncludeRowNamesMeta() {
        return includeRowNamesMeta;
    }

    public void setIncludeRowNamesMeta(boolean includeRowNamesMeta) {
        this.includeRowNamesMeta = includeRowNamesMeta;
    }

    public boolean isDrawBordersMeta() {
        return drawBordersMeta;
    }

    public void setDrawBordersMeta(boolean drawBordersMeta) {
        this.drawBordersMeta = drawBordersMeta;
    }

    public String getCorOpt() {
        return corOpt;
    }

    public void setCorOpt(String corOpt) {
        this.corOpt = corOpt;
    }

    public void doDefaultMetaHeatmap() {
        if (!sb.isAnalInit(pageID)) {
            jrd.record_metaOverviewBn_action(this);
            sb.addNaviTrack(pageID, "/Secure/multifac/MetaDataView.xhtml");
            int res = RDataUtils.plotMetaCorrHeatmap(sb,
                    "univariate", // corMethod
                    corOpt,
                    "default", // colorGradient
                    false, sb.getNewImage("metaCorrHeatmap"), "png", 150);
            RDataUtils.plotMetaHeatmap(sb, "overview", "both", "euclidean", "ward.D", "bwm",
                    includeRowNamesMeta ? "T" : "F", sb.getNewImage("metaHeatmap"), "png", 150);
        } else {
            if (wb.getFunctionInfos().get("Metadata Heatmap") != null) {
                try {
                    FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("Metadata Heatmap"));
                } catch (Exception ex) {

                }
            }
        }
    }

    public boolean metaOverviewBn_action() {
        jrd.record_metaOverviewBn_action(this);

        String ucMethod = "univariate";
        switch (corOpt) {
            case "u-pearson" -> {
                ucMethod = "univariate";
                corOpt = "pearson";
            }
            case "u-spearman" -> {
                ucMethod = "univariate";
                corOpt = "spearman";
            }
            case "u-kendall" -> {
                ucMethod = "univariate";
                corOpt = "kendall";
            }
            case "p-pearson" -> {
                ucMethod = "partial";
                corOpt = "pearson";
            }
            case "p-spearman" -> {
                ucMethod = "partial";
                corOpt = "spearman";
            }
            case "p-kendall" -> {
                ucMethod = "partial";
                corOpt = "kendall";
            }
            default -> {
            }
        }

        int res = RDataUtils.plotMetaCorrHeatmap(sb,
                ucMethod, // corMethod
                corOpt,
                "default", // colorGradient
                false, sb.getNewImage("metaCorrHeatmap"), "png", 150);
        if (res == 0) {
            sb.addMessage("Error", "Unknown error occured in the image generation!");
            return false;
        } else {
            return true;
        }

    }
}
