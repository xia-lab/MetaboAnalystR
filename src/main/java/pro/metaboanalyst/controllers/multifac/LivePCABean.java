/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.List;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetaDataBean;
import pro.metaboanalyst.rwrappers.ChemoMetrics;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@ViewScoped
@Named("livePcaBean")
public class LivePCABean implements Serializable {

    @Inject
    private SessionBean1 sb;

    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private final String pageID = "iPCA";
    private int pcaPairNum = 5;
    private String colOpt;
    private String expOpt = "score";
    private String shapeOpt;

    public int getPcaPairNum() {
        return pcaPairNum;
    }

    public void setPcaPairNum(int pcaPairNum) {
        this.pcaPairNum = pcaPairNum;
    }

    public String getColOpt() {
        if (colOpt == null) {
            colOpt = mfb.getMetaDataBeans().get(0).getName();
        }
        return colOpt;
    }

    public void setColOpt(String colOpt) {
        this.colOpt = colOpt;
    }

    public String getExpOpt() {
        return expOpt;
    }

    public void setExpOpt(String expOpt) {
        this.expOpt = expOpt;
    }

    public String getShapeOpt() {
        if (shapeOpt == null) {
            List<MetaDataBean> beans = mfb.getMetaDataBeans();
            for (int i = 1; i < beans.size(); i++) {
                if (!beans.get(i).getParam().equals("cont")) {
                    shapeOpt = mfb.getMetaDataBeans().get(i).getName();
                    break;
                }
            }
        }
        return shapeOpt;
    }

    public void setShapeOpt(String shapeOpt) {
        this.shapeOpt = shapeOpt;
    }

    public void initPCA3D() {

        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/LivePCAView.xhtml");
            ChemoMetrics.initPCA(sb);

            TimeSeries.plotPCAPairSummaryMeta(sb, sb.getNewImage("pca_pair_meta"), "pca_pair_meta", "png", 150, pcaPairNum, getColOpt(), getShapeOpt());
            TimeSeries.initIPCA(sb.getRConnection(), sb.getNewImage("ipca_3d") + ".json", colOpt, shapeOpt, "blue");
        } else {

            if (wb.getFunctionInfos().get("PCA 3D") != null) {
                try {
                    FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("PCA 3D"));
                } catch (Exception ex) {

                }
            }
        }
        wb.getCalledWorkflows().add("iPCA");
    }

    public void updatePCA3D() {
        sb.addNaviTrack(pageID, "/Secure/multifac/LivePCAView.xhtml");
        ChemoMetrics.initPCA(sb);
        TimeSeries.plotPCAPairSummaryMeta(sb, sb.getNewImage("pca_pair_meta"), "pca_pair_meta", "png", 150, pcaPairNum, getColOpt(), getShapeOpt());
        TimeSeries.initIPCA(sb.getRConnection(), sb.getNewImage("ipca_3d") + ".json", colOpt, shapeOpt, "blue");

    }

    public String pcaPairBtn_action() {
        List<MetaDataBean> beans = mfb.getMetaDataBeans();
        for (int i = 0; i < beans.size(); i++) {
            String type = beans.get(i).getParam();
            String name = beans.get(i).getName();
            if (name.equals(shapeOpt)) {
                if (type.equals("cont")) {
                    sb.addMessage("Error", "Continuous meta-data can not be used for data point shapes.");
                    return null;
                }
            }
        }

        TimeSeries.plotPCAPairSummaryMeta(sb, sb.getNewImage("pca_pair_meta"), "pca_pair_meta", "png", 150, pcaPairNum, colOpt, shapeOpt);
        return null;
    }
}
