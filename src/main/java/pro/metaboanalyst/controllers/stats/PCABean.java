/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import jakarta.inject.Named;
import jakarta.faces.model.SelectItem;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.DetailsBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.multifac.LivePCABean;
import pro.metaboanalyst.rwrappers.ChemoMetrics;
import pro.metaboanalyst.rwrappers.TimeSeries;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("pcaBean")
public class PCABean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private DetailsBean dtb;

    private int pcaPairNum = 5;

    public String getStat4PCA() {
        return ChemoMetrics.getPCAStats(sb.getRConnection());
    }

    public String getPermAnovaTxt() {
        return ChemoMetrics.getPCAPermANOVAText(sb.getRConnection());
    }

    private String cexOpt = "na";

    public String getCexOpt() {
        return cexOpt;
    }

    public void setCexOpt(String cexOpt) {
        this.cexOpt = cexOpt;
    }

    public int getPcaPairNum() {
        return pcaPairNum;
    }

    public void setPcaPairNum(int pcaPairNum) {
        this.pcaPairNum = pcaPairNum;
    }

    //for feature label on loading plot
    private String loadOpt = "all";

    public String getLoadOpt() {
        return loadOpt;
    }

    public void setLoadOpt(String plsLoadOpt) {
        this.loadOpt = plsLoadOpt;
    }

    public SelectItem[] getPcaPCs() {
        int pcNums = ChemoMetrics.getMaxPCACompNumber(sb) - 2;
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 2;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }

    public SelectItem[] getPcaAllPCs() {
        int pcNums = ChemoMetrics.getMaxPCACompNumber(sb) - 1;
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 1;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }

    public String pcaPairBtn_action() {
        TimeSeries.plotPCAPairSummaryMeta(sb, sb.getCurrentImage("pca_pair"), "pca_pair", "png", 150, pcaPairNum, "NA", "NA");
        return null;
    }

    private boolean greyScale = false;

    public boolean isGreyScale() {
        return greyScale;
    }

    public void setGreyScale(boolean greyScale) {
        this.greyScale = greyScale;
    }

    private boolean diffShapes = true;

    public boolean isDiffShapes() {
        return diffShapes;
    }

    public void setDiffShapes(boolean diffShapes) {
        this.diffShapes = diffShapes;
    }

    private int pcaScreeNum = 5;

    public int getPcaScreeNum() {
        return pcaScreeNum;
    }

    public void setPcaScreeNum(int pcaScreeNum) {
        this.pcaScreeNum = pcaScreeNum;
    }

    public void pcaScreeBtn_action() {
        ChemoMetrics.plotPCAScree(sb, sb.getNewImage("pca_scree"), "png", 150, pcaScreeNum);
    }

    private boolean displayNames = false;

    public boolean isDisplayNames() {
        return displayNames;
    }

    public void setDisplayNames(boolean displayNames) {
        this.displayNames = displayNames;
    }

    private boolean displayFeatNames = true;

    public boolean isDisplayFeatNames() {
        return displayFeatNames;
    }

    public void setDisplayFeatNames(boolean displayFeatNames) {
        this.displayFeatNames = displayFeatNames;
    }

    private boolean displayConfs = true;

    public boolean isDisplayConfs() {
        return displayConfs;
    }

    public void setDisplayConfs(boolean displayConfs) {
        this.displayConfs = displayConfs;
    }

    private int pcaScoreX = 1;
    private int pcaScoreY = 2;

    public int getPcaScoreX() {
        return pcaScoreX;
    }

    public void setPcaScoreX(int pcaScoreX) {
        this.pcaScoreX = pcaScoreX;
    }

    public int getPcaScoreY() {
        return pcaScoreY;
    }

    public void setPcaScoreY(int pcaScoreY) {
        this.pcaScoreY = pcaScoreY;
    }

    public String pcaScore2dBtn_action() {
        if (pcaScoreX == pcaScoreY) {
            sb.addMessage("Error", "X and Y axes are of the same PC");
        } else {
            double conf = 0.95;
            if (!displayConfs) {
                conf = 0;
            }
            int showNames = 0;
            if (displayNames) {
                showNames = 1;
            }

            int useGreyScale = 0;
            if (greyScale) {
                useGreyScale = 1;
            }
            ChemoMetrics.plotPCA2DScore(sb, sb.getNewImage("pca_score2d"), "png", 150, pcaScoreX, pcaScoreY, conf, showNames, useGreyScale, cexOpt);

        }
        return null;
    }

    @JsonIgnore
    @Inject
    private LivePCABean lb;

    public String pcaScore2dBtn_meta_action() {
        if (pcaScoreX == pcaScoreY) {
            sb.addMessage("Error", "X and Y axes are of the same PC");
        } else {
            double conf = 0.95;
            if (!displayConfs) {
                conf = 0;
            }
            int showNames = 0;
            if (displayNames) {
                showNames = 1;
            }

            int useGreyScale = 0;
            if (greyScale) {
                useGreyScale = 1;
            }
            TimeSeries.plotPCA2DScoreMeta(sb, sb.getNewImage("pca_score2d_meta"), "png", 150, pcaScoreX, pcaScoreY, conf, showNames, useGreyScale, cexOpt, lb.getColOpt(), lb.getShapeOpt());

        }
        return null;
    }

    private int pcaScore3dX = 1;
    private int pcaScore3dY = 2;
    private int pcaScore3dZ = 3;
    private int rotationAngle = 40;

    public int getPcaScore3dX() {
        return pcaScore3dX;
    }

    public void setPcaScore3dX(int pcaScore3dX) {
        this.pcaScore3dX = pcaScore3dX;
    }

    public int getPcaScore3dY() {
        return pcaScore3dY;
    }

    public void setPcaScore3dY(int pcaScore3dY) {
        this.pcaScore3dY = pcaScore3dY;
    }

    public int getPcaScore3dZ() {
        return pcaScore3dZ;
    }

    public void setPcaScore3dZ(int pcaScore3dZ) {
        this.pcaScore3dZ = pcaScore3dZ;
    }

    public int getRotationAngle() {
        return rotationAngle;
    }

    public void setRotationAngle(int rotationAngle) {
        this.rotationAngle = rotationAngle;
    }

    public void pcaScore3dBtn_action(boolean onCurrentPage) {
        if (pcaScore3dX == pcaScore3dY || pcaScore3dX == pcaScore3dZ || pcaScore3dY == pcaScore3dZ) {
            sb.addMessage("Error", "Detected the same PC on two axes!");
        } else {
            //ChemoMetrics.PlotPCA3DScore(sb, sb.getNewImage("pca_score3d"), "png", 150, pcaScore3dX, pcaScore3dY, pcaScore3dZ, rotationAngle);
            ChemoMetrics.plotPCA3DScore(sb, sb.getNewImage("pca_score3d"), "json", 150, pcaScore3dX, pcaScore3dY, pcaScore3dZ);
            ChemoMetrics.plotPCA3DLoading(sb, sb.getNewImage("pca_loading3d"), "json", 150, pcaScore3dX, pcaScore3dY, pcaScore3dZ);
        }
    }

    private int pcaLoadX = 1;
    private int pcaLoadY = 2;

    public int getPcaLoadX() {
        return pcaLoadX;
    }

    public void setPcaLoadX(int pcaLoadX) {
        this.pcaLoadX = pcaLoadX;
    }

    public int getPcaLoadY() {
        return pcaLoadY;
    }

    public void setPcaLoadY(int pcaLoadY) {
        this.pcaLoadY = pcaLoadY;
    }

    private String loadPlotOpt = "scatter";

    public String getLoadPlotOpt() {
        return loadPlotOpt;
    }

    public void setLoadPlotOpt(String loadPlotOpt) {
        this.loadPlotOpt = loadPlotOpt;
    }

    public void pcaSaveViewBtn_action() {
        sb.addMessage("info", "The current view is save for report generation. Please proceed to <b>Download</b> page to generate reports.");
    }

    public void pcaLoadBtn_action() {
        if (pcaLoadX == pcaLoadY) {
            sb.addMessage("Error", "Detected the same PC on two axes!");
        } else {
            ChemoMetrics.updatePCALoadType(sb, loadOpt);
            ChemoMetrics.plotPCALoading(sb, sb.getNewImage("pca_loading"), "png", 150, pcaLoadX, pcaLoadY);
            dtb.update1CompModel("pca");
            if (loadOpt.equals("custom")) {
                sb.addMessage("info", "Please first click the points of interest and then re-gerenate the Splot in Image Dialog");
            } else {
                sb.addMessage("info", "You can now re-generate the plot using the Image Dialog");
            }
        }
    }

    private int pcaBiplotX = 1;
    private int pcaBiplotY = 2;

    public int getPcaBiplotX() {
        return pcaBiplotX;
    }

    public void setPcaBiplotX(int pcaBiplotX) {
        this.pcaBiplotX = pcaBiplotX;
    }

    public int getPcaBiplotY() {
        return pcaBiplotY;
    }

    public void setPcaBiplotY(int pcaBiplotY) {
        this.pcaBiplotY = pcaBiplotY;
    }
    private int pcaBiplotFeat = 10;

    public int getPcaBiplotFeat() {
        if (pcaBiplotFeat >= sb.getFeatureNumber()) {
            pcaBiplotFeat = sb.getFeatureNumber() - 1;
        }
        return pcaBiplotFeat;
    }

    public void setPcaBiplotFeat(int pcaBiplotFeat) {
        this.pcaBiplotFeat = pcaBiplotFeat;
    }

    public void pcaBiplotBtn_action() {
        if (pcaBiplotX == pcaBiplotY) {
            sb.addMessage("Error", "Detected the same PC on two axes!");
            return;
        }

        if (pcaBiplotFeat >= sb.getFeatureNumber()) {
            sb.addMessage("Error", "The value must be less than the total feature number in your data: " + sb.getFeatureNumber());
            return;
        }
        ChemoMetrics.plotPCABiplot(sb, sb.getNewImage("pca_biplot"), "png", 150, pcaBiplotX, pcaBiplotY, pcaBiplotFeat);

    }

    public void flipPCA() {
        ChemoMetrics.flipPCA(sb, flipOpt);
        pcaPairBtn_action();
        pcaScore2dBtn_action();
        pcaLoadBtn_action();
        pcaBiplotBtn_action();
        pcaScore3dBtn_action(false);
    }

    public void flipPCAMeta() {
        ChemoMetrics.flipPCA(sb, flipOpt);
        pcaScore2dBtn_meta_action();
        lb.pcaPairBtn_action();
        TimeSeries.initIPCA(sb.getRConnection(), sb.getCurrentImage("ipca_3d") + ".json", lb.getColOpt(), lb.getShapeOpt(), "blue");
    }

    private String flipOpt = "y";

    public String getFlipOpt() {
        return flipOpt;
    }

    public void setFlipOpt(String flipOtp) {
        this.flipOpt = flipOtp;
    }
}
