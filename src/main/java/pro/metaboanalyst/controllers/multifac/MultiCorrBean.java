/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;

import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.rwrappers.UniVarTests;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@ViewScoped
@Named("mCorrBean")
public class MultiCorrBean implements Serializable {

    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @Inject
    private MultifacBean mfb;
    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private final String pageID = "Correlations";

    private String covFeatures;
    private String ptnMetaCov;
    private String ptnFeature = "";
    private String tgtType = "metaNm";
    private String covType = "none";
    private String ptnMeta = "";
    private String ptnDistMeasure = "pearson";
    private String[] covMetas = new String[0];

    public String getCovFeatures() {
        return covFeatures;
    }

    public void setCovFeatures(String covFeatures) {
        this.covFeatures = covFeatures;
    }

    public String getPtnMetaCov() {
        return ptnMetaCov;
    }

    public void setPtnMetaCov(String ptnMetaCov) {
        this.ptnMetaCov = ptnMetaCov;
    }

    public String getPtnFeature() {
        return ptnFeature;
    }

    public void setPtnFeature(String ptnFeature) {
        this.ptnFeature = ptnFeature;
    }

    public String getTgtType() {
        return tgtType;
    }

    public void setTgtType(String ptnType) {
        this.tgtType = ptnType;
    }

    public String getCovType() {
        return covType;
    }

    public void setCovType(String ptnTypeCov) {
        this.covType = ptnTypeCov;
    }

    public String getPtnMeta() {
        if (ptnMeta.equals("")) {
            ptnMeta = mfb.getAnalysisMetaOpts()[0].getValue().toString();
        }
        return ptnMeta;
    }

    public void setPtnMeta(String ptnMeta) {
        this.ptnMeta = ptnMeta;
    }

    public String getPtnDistMeasure() {
        return ptnDistMeasure;
    }

    public void setPtnDistMeasure(String ptnDistMeasure) {
        this.ptnDistMeasure = ptnDistMeasure;
    }

    public String[] getCovMetas() {
        return covMetas;
    }

    public void setCovMetas(String[] covMetas) {
        this.covMetas = covMetas;
    }

    public void doDefaultCorr() {

        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/PartialCorrView.xhtml");
            //do something here
        } else {
            if (wb.getFunctionInfos().get("corBtn_action") != null) {
                try {
                    FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("corBtn_action"));
                } catch (Exception ex) {

                }
            }
        }
    }

    public String corBtn_action() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_corBtn_action(this);
            return null;
        }
        String imgName = sb.getNewImage("ptn");
        RConnection RC = sb.getRConnection();

        String tgtNm;

        if (covMetas == null) {
            String[] meta = new String[1];
            meta[0] = mfb.getMetaDataBeans().get(1).getName();
            covMetas = meta;
        }
        jrd.record_corBtn_action(this);
        if (tgtType.equals("featNm")) {
            if (ptnFeature.isEmpty()) {
                sb.addMessage("Error", "Please specify a feature of interest!");
                return null;
            }
            tgtNm = ptnFeature;
        } else {
            for (String covMeta : covMetas) {
                if (covMeta.equals(ptnMeta)) {
                    sb.addMessage("Error", "Please make sure covariate is not the same as target.");
                    return null;
                }
            }
            tgtNm = ptnMeta;
        }

        if (TimeSeries.performCorrAnal(sb, ptnDistMeasure, tgtType, tgtNm, covMetas)) {
            UniVarTests.plotMatchedFeatures(sb, imgName, tgtType, "png", 150);
            mfb.setCorrPerformed(true);
        } else {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
        }
        return null;
    }
}
