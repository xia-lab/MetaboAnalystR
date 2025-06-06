/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import jakarta.faces.event.PhaseId;
import jakarta.faces.event.ValueChangeEvent;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author xia
 */
@ViewScoped
@Named("hm2Bean")
public class HeatMap2Bean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;
    @JsonIgnore
    @Inject
    private WorkflowBean wb;
    @JsonIgnore
    @Inject
    private MultifacBean mfb;
   @JsonIgnore
    @Inject
    private JavaRecord jrd;
   
    private final String pageID = "Heatmap2";
    private boolean includeRowNames = false;
    private boolean showLegend = true;
    private boolean showAnnotLegend = true;
    private String scaleOpt = "row";
    private String dataOpt = "norm";
    private String fontSizeOpt = "8";
    private String fontSizeCol = "8";
    private String fontSizeRow = "8";
    private double annoHeight = 0.015;
    private double annoFz = 10;
    private int unitCol = 10;
    private int unitRow = 10;
    private boolean showColnm = true;
    private boolean showRownm = false;
    private final int maxFeatureNum = 5000; //overwrite heatmap 2000

    public String getFontSizeOpt() {
        return fontSizeOpt;
    }

    public void setFontSizeOpt(String fontSizeOpt) {
        this.fontSizeOpt = fontSizeOpt;
    }

    public String getFontSizeCol() {
        return fontSizeCol;
    }

    public void setFontSizeCol(String fontSizeCol) {
        this.fontSizeCol = fontSizeCol;
    }

    public String getFontSizeRow() {
        return fontSizeRow;
    }

    public void setFontSizeRow(String fontSizeRow) {
        this.fontSizeRow = fontSizeRow;
    }

    public double getAnnoHeight() {
        return annoHeight;
    }

    public void setAnnoHeight(double annoHeight) {
        this.annoHeight = annoHeight;
    }

    public double getAnnoFz() {
        return annoFz;
    }

    public void setAnnoFz(double annoFz) {
        this.annoFz = annoFz;
    }

    public int getUnitCol() {
        return unitCol;
    }

    public void setUnitCol(int unitCol) {
        this.unitCol = unitCol;
    }

    public int getUnitRow() {
        return unitRow;
    }

    public void setUnitRow(int unitRow) {
        this.unitRow = unitRow;
    }

    public boolean isShowColnm() {
        return showColnm;
    }

    public void setShowColnm(boolean showColnm) {
        this.showColnm = showColnm;
    }

    public boolean isShowRownm() {
        return showRownm;
    }

    public void setShowRownm(boolean showRownm) {
        this.showRownm = showRownm;
    }

    public String getScaleOpt() {
        return scaleOpt;
    }

    public void setScaleOpt(String scaleOpt) {
        this.scaleOpt = scaleOpt;
    }

    public String getDataOpt() {
        return dataOpt;
    }

    public void setDataOpt(String dataOpt) {
        this.dataOpt = dataOpt;
    }

    private String distOpt = "euclidean";

    public String getDistOpt() {
        return distOpt;
    }

    public void setDistOpt(String distOpt) {
        this.distOpt = distOpt;
    }

    private String clusterOpt = "ward.D";

    public String getClusterOpt() {
        return clusterOpt;
    }

    public void setClusterOpt(String clusterOpt) {
        this.clusterOpt = clusterOpt;
    }

    private String colorOpt = "bwn";

    public String getColorOpt() {
        return colorOpt;
    }

    public void setColorOpt(String colorOpt) {
        this.colorOpt = colorOpt;
    }

    private String[] selectedMetas;

    public String[] getSelectedMetas() {
        return selectedMetas;
    }

    public void setSelectedMetas(String[] selectedMetas) {
        this.selectedMetas = selectedMetas;
    }

    private String viewOpt = "overview";

    public String getViewOpt() {
        return viewOpt;
    }

    public void setViewOpt(String viewOpt) {
        this.viewOpt = viewOpt;
    }
    private int topThresh = 25;
    private String selectMethodOpt = "mean";
    private boolean useTopFeature = false;

    public boolean isUseTopFeature() {
        return useTopFeature;
    }

    public void setUseTopFeature(boolean useTopFeature) {
        this.useTopFeature = useTopFeature;
    }

    public int getTopThresh() {
        return topThresh;
    }

    public void setTopThresh(int topThresh) {
        this.topThresh = topThresh;
    }

    public String getSelectMethodOpt() {
        return selectMethodOpt;
    }

    public void setSelectMethodOpt(String selectMethodOpt) {
        this.selectMethodOpt = selectMethodOpt;
    }

    private String[] smplSortOpt;

    public String[] getSmplSortOpt() {
        return smplSortOpt;
    }

    public void setSmplSortOpt(String[] smplSortOpt) {
        this.smplSortOpt = smplSortOpt;
    }

    public boolean isIncludeRowNames() {
        return includeRowNames;
    }

    public void setIncludeRowNames(boolean includeRowNames) {
        this.includeRowNames = includeRowNames;
    }

    public boolean isShowAnnotLegend() {
        return showAnnotLegend;
    }

    public void setShowAnnotLegend(boolean showAnnotLegend) {
        this.showAnnotLegend = showAnnotLegend;
    }

    public boolean isShowLegend() {
        return showLegend;
    }

    public void setShowLegend(boolean showLegend) {
        this.showLegend = showLegend;
    }

    private boolean drawBorders = false;

    public boolean isDrawBorders() {
        return drawBorders;
    }

    public void setDrawBorders(boolean drawBorders) {
        this.drawBorders = drawBorders;
    }

    //set up default, but not actually perform action
    public void doDefaultHeatmap2() {

        if (wb.getFunctionInfos().get("Heatmap2") != null) {
            try {
                FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("Heatmap2"));
            } catch (Exception ex) {

            }
        }

        if (selectedMetas == null) {
            int metaNum = mfb.getMetaDataBeans().size();
            if (mfb.getMetaDataBeans().size() > 4) {
                metaNum = 4;
            }
            String[] meta = new String[metaNum];

            for (int i = 0; i < metaNum; i++) {
                meta[i] = mfb.getMetaDataBeans().get(i).getName();
            }
            selectedMetas = meta;
        }

        if (smplSortOpt == null) {
            String[] meta = new String[1];
            meta[0] = mfb.getMetaDataBeans().get(0).getName();
            smplSortOpt = meta;
            if (!smplSortOptList.contains(meta[0])) {
                smplSortOptList.add(meta[0]);
            }
        }

        //sb.addMessage("Info", "Specify parameters and click Submit to generate interactive heatmaps. It can take long for large data. Please be patient!");
        /*
        int fzCol = Integer.parseInt(fontSizeCol);
        int fzRow = Integer.parseInt(fontSizeRow);
        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/Heatmap2View.xhtml");
            TimeSeries.plotHeatMap2(sb, sb.getCurrentImage("heatmap2"), "norm", "row", "png", 150, "euclidean", "ward.D", "bwm", fzCol, fzRow, annoFz, annoHeight, unitCol, unitRow, "mean", 2000, smplSortOpt, "F", "F", "T", "T", "T", "F", selectedMetas);
        }
         */
    }

    ArrayList<String> smplSortOptList = new ArrayList();

    public ArrayList<String> getSmplSortOptList() {
        return smplSortOptList;
    }

    public void setSmplSortOptList(ArrayList<String> smplSortOptList) {
        this.smplSortOptList = smplSortOptList;
    }

    public String getPageID() {
        return pageID;
    }

    public int getMaxFeatureNum() {
        return maxFeatureNum;
    }

    public boolean hm2Bn_action() {
        jrd.record_hm2b_action(this);

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            return true;
        }

        for (String smplSortOpt1 : smplSortOpt) {
            if (!Arrays.asList(selectedMetas).contains(smplSortOpt1)) {
                sb.addMessage("Error", "Please make sure that the \"Sample arrangement\" metadata are included in \"Metadata in annotation\"!");
                return false;
            }
        }

        if (selectedMetas.length < 1) {
            sb.addMessage("Error", "Please select at least one meta-data for annotation!");
            return false;
        }

        for (String metaNm : smplSortOpt) {
            if (!Arrays.asList(selectedMetas).contains(metaNm)) { // if sample sort
                sb.addMessage("Error", "Please make sure to include the \"Sample arrangement\" metadata in \"Metadata in annotation\"!");
                return false;
            }
        }

        if (smplSortOpt.length > 4) { // if sample sort
            sb.addMessage("Error", "Only up to four metadata can be selected for \"Sample arrangement\".");
            return false;
        }

        if (useTopFeature) {
            if (RDataUtils.getNormFeatureNumber(sb.getRConnection()) <= topThresh) {
                sb.addMessage("Error", "The number of top features cannot be bigger than total feature number!");
                return false;
            }

            if (maxFeatureNum <= topThresh) {
                sb.addMessage("Error", "The number of top features cannot be bigger than max allowed feature number!");
                return false;
            }
            /*   
            if (viewOpt.equals("detail") && topThresh > 1000) {
                sb.addMessage("Warn", "Too many features for detail view (max 1000) - reset to 1000.");
                topThresh = 1000;
            }*/
        }
        int fzCol = Integer.parseInt(fontSizeCol);
        int fzRow = Integer.parseInt(fontSizeRow);
        String[] smplSortArray = smplSortOptList.toArray(String[]::new);
        int res = TimeSeries.plotHeatMap2(sb, sb.getNewImage("heatmap2"), dataOpt, scaleOpt, "png", 150, distOpt, clusterOpt, colorOpt, fzCol, fzRow, annoFz, annoHeight, unitCol, unitRow, selectMethodOpt, topThresh, smplSortArray,
                useTopFeature ? "T" : "F", drawBorders ? "T" : "F", showLegend ? "T" : "F", showAnnotLegend ? "T" : "F", showColnm ? "T" : "F", showRownm ? "T" : "F",
                selectedMetas, maxFeatureNum);
        if (res == 0) {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Failed to plot heatmap." + err);
        }
        return true;
    }

    public void changeListener(ValueChangeEvent event) {
        if (event.getPhaseId() != PhaseId.INVOKE_APPLICATION) {
            event.setPhaseId(PhaseId.INVOKE_APPLICATION);
            event.queue();
            return;
        }

        Object oldValue = event.getOldValue();
        Object newValue = event.getNewValue();
        String[] oldArray = (String[]) oldValue;
        String[] newArray = (String[]) newValue;

        if (oldArray.length > newArray.length) { // remove element
            for (String metaNm : oldArray) {
                if (!Arrays.asList(newArray).contains(metaNm)) {
                    smplSortOptList.remove(metaNm);
                }
            }
        } else {
            for (String metaNm : newArray) { // add element
                if (!Arrays.asList(oldArray).contains(metaNm)) {
                    smplSortOptList.add(metaNm);
                }
            }
        }
    }
}
