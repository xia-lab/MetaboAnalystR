/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import jakarta.inject.Named;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.models.ColorBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RGraphUtils;
import pro.metaboanalyst.rwrappers.RocUtils;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("graphBean")
public class GraphBean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private DoseResponseBean drb;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private RocAnalBean rcb;

    @JsonIgnore
    @Inject
    private SpectraProcessBean spb;

    @JsonIgnore
    @Inject
    private MetaPathStatBean mpsb;
    
    private static final Logger LOGGER = LogManager.getLogger(GenericControllers.class);

    public void updateColorScheme() {
        updateGraphSetting();
        sb.addMessage("info", "You can now update the images to see the effect.");
    }

    private void updateGraphSetting() {
        List<ColorBean> colorBeanLists = sb.getColorBeanLists();
        int grpNum = colorBeanLists.size();
        if (grpNum > 0) {
            String[] cols = new String[grpNum];
            int[] sps = new int[grpNum];
            for (int i = 0; i < grpNum; i++) {
                String col = colorBeanLists.get(i).getColorPopup();
                int sp = colorBeanLists.get(i).getShapeType();
                if (col == null || col.trim().isEmpty()) {
                    sb.addMessage("Warn:", "Some group does not have a color specified. Default color will be applied.");
                    col = "NA";
                }
                if (col != null && !col.isEmpty() && col.charAt(0) != '#') {
                    col = "#" + col;
                }
                cols[i] = col;
                if (sp < 0 | sp > 25) {
                    sb.addMessage("Warn:", "Some group shape is invalid. Default shape will be applied.");
                    sp = 0;
                }
                sps[i] = sp;
            }
            RDataUtils.updateGraphSettings(sb.getRConnection(), cols, sps);
        }
    }

    public void graphBn_action() {

        String key = sb.getImageSource();
        String imgName;
        //String mydpi = "150";
        String mydpi;
        String formatOpt = sb.getFormatOpt();
        if (formatOpt.equals("png") || formatOpt.equals("tiff")) {
            mydpi = sb.getDpiOpt() + "";
        }else{
            mydpi = "72"; //somehow, this value affect PDF and SVG, 72 works well
        }
        //System.out.println(key + "==========key");
        //String rcmd1 = sb.getGraphicsMap().get(key);
        // System.out.println(rcmd1 + "==========r");
        switch (key) {
            case "pathway" -> {
                String currentPathName = sb.getCurrentPathName();
                if (currentPathName == null) {
                    sb.addMessage("Error", "No command found for plotting the image!");
                    return;
                }
                imgName = RGraphUtils.plotKEGGPath(sb, currentPathName, sb.getSizeOpt(), sb.getSizeOpt(), formatOpt, mydpi);
            }
            case "mb" -> {
                String cmpdName = sb.getCurrentCmpdName();
                if (cmpdName == null) {
                    sb.addMessage("Error", "No command found for plotting the image!");
                    return;
                }
                imgName = TimeSeries.plotMBTimeProfile(sb, cmpdName, 100, formatOpt, mydpi + "");
            }
            case "metabolite_dr_curve" -> {
                imgName = drb.plotSelectedFeature(Integer.parseInt(mydpi), formatOpt);
            }
            case "cmpd" -> {
                String cmpdName = sb.getCurrentCmpdName();
                if (cmpdName == null) {
                    sb.addMessage("Error", "No command found for plotting the image!");
                    return;
                }

                mfb.setBoxMetaVersionNum(mfb.getBoxMetaVersionNum() + 1);

                imgName = UniVarTests.plotCmpdSummary(sb, cmpdName, "NA", "NA", mfb.getBoxMetaVersionNum(), formatOpt, mydpi + "");
            }
            case "roc.univ" -> {

                String cmpdName = rcb.getCurrentCmpd();
                if (cmpdName == null) {
                    sb.addMessage("Error", "No command found for plotting the image!");
                    return;
                }
                String isAUC = rcb.isShowCI() ? "T" : "F";
                String isOpt = rcb.isShowOptPoint() ? "T" : "F";
                String optMtd = rcb.getOptimalDD();
                String isPartial = rcb.isPartialRoc() ? "T" : "F";
                String measure = rcb.getUnivPerfOpt();
                double mycutoff = rcb.getUnivThresh();
                imgName = RocUtils.performUnivROC(sb, cmpdName, 100, formatOpt, mydpi, isAUC, isOpt, optMtd, isPartial, measure, mycutoff);
            }
            case "roc.boxplot" -> {

                String cmpdName = rcb.getCurrentCmpd();
                if (cmpdName == null) {
                    sb.addMessage("Error", "No command found for plotting the image!");
                    return;
                }
                String isOpt = rcb.isShowOptPoint() ? "T" : "F";
                imgName = RocUtils.plotUnivROCBP(sb, cmpdName, 100, formatOpt, mydpi, isOpt, "FALSE");
            }
            case "RTcor" -> {

                int figureNM = spb.getFigureCount();
                int figureNMNew = figureNM + 1;
                spb.setFigureCount(figureNMNew);
                imgName = spb.plotRTcor();
                spb.internalizeImage(imgName);
            }
            case "BPIcor" -> {
                int figureNM = spb.getFigureCount();
                int figureNMNew = figureNM + 1;
                spb.setFigureCount(figureNMNew);
                imgName = spb.plotBPIcor();
                spb.internalizeImage(imgName);
            }
            case "IntensitySpec" -> {
                int figureNM = spb.getFigureCount();
                int figureNMNew = figureNM + 1;
                spb.setFigureCount(figureNMNew);
                imgName = spb.plotIntenStats();
                spb.internalizeImage(imgName);
            }
            case "TICs" -> {

                int figureNM = spb.getFigureCount();
                int figureNMNew = figureNM + 1;
                spb.setFigureCount(figureNMNew);
                imgName = spb.plotTICs();
                spb.internalizeImage(imgName);
            }
            case "BPIs" -> {

                int figureNM = spb.getFigureCount();
                int figureNMNew = figureNM + 1;
                spb.setFigureCount(figureNMNew);
                imgName = spb.plotBPIs();
                spb.internalizeImage(imgName);
            }
            case "TIC" -> {

                int figureNM = spb.getFigureCount();
                int figureNMNew = figureNM + 1;
                spb.setFigureCount(figureNMNew);
                imgName = spb.plotTIC();
                spb.internalizeImage(imgName);
            }
            case "EIC_int" -> {

                imgName = spb.plotMSfeatureUpdate();
                spb.internalizeImage(imgName);
            }
            case "EICs" -> {
                String FeatureName = spb.plotXICUpdate();
                imgName = FeatureName;
                spb.internalizeImage(imgName);
            }
            case "EICg" -> {
                int dpi = 150;
                if (sb.getFormatOpt().equals("png")) {
                    dpi = sb.getDpiOpt();
                }
                String FeatureName = spb.plotXICUpdate();
                imgName = "EIC_" + FeatureName + "_group_" + dpi + "." + sb.getFormatOpt();
                spb.internalizeImage(imgName);
            }
            case "metapath" -> {

                imgName = mpsb.updatePlotPathMetaHigRes();
                //System.out.println(imgName);
            }

            default -> {
                String rcmd = sb.getGraphicsMap().get(key);
                /*
                if (key.equals("cls_test_roc")) {
                    rcmd = sb.getGraphicsMap().get("cls_roc");
                } else if (key.equals("cls_test_prob")) {
                    rcmd = sb.getGraphicsMap().get("cls_prob");
                }**/
                if (rcmd == null) {
                    sb.addMessage("Error", "No command found for plotting the image!");
                    return;
                }
                if (key.equals("covariate_plot")) {
                    rcmd = rcmd.replace("default", mfb.getCovStyleOpt());
                }
                rcmd = rcmd.replace("png", formatOpt);
                rcmd = rcmd.replace("150", mydpi + "");
                rcmd = rcmd.replace("width=NA", "width=" + sb.getSizeOpt());
                imgName = sb.getCurrentImage(key);
                imgName = imgName.replaceAll("\\/", "_");
                imgName = imgName + "dpi" + mydpi + "." + formatOpt;
                try {
                    RConnection RC = sb.getRConnection();
                    RCenter.recordRCommand(RC, rcmd);
                    RC.voidEval(rcmd);
                } catch (Exception e) {
                    // e.printStackTrace();
                    LOGGER.error("graphBn_action", e);
                }
            }
        }

        String imgDownloadTxt = "<b>Download the image: </b> <a target='_blank' href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + File.separator + imgName + "\"><b>" + imgName + "</b></a>";
        sb.setImgDownloadTxt(imgDownloadTxt);
    }

    private final List<String> shapeColImgs = Arrays.asList(new String[]{"pca_pair", "pca_score2d", "pca_score2d_meta", "pls_pair", "pls_score2d",
        "spls_pair", "spls_score2d", "opls_score2d", "cmpd", "roc.boxplot", "tree", "heatmap", "mb"});

    public boolean isRenderStatus() {
        String key = sb.getImageSource();
        return key != null && shapeColImgs.contains(key);
    }
}
