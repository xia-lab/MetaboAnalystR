/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetaDataBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.List;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@SessionScoped
@Named("lmBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class LimmaBean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private MultifacBean tb;

    private String analysisMeta;
    private String analysisMeta2 = "NA";
    private String primaryType;

    private String[] adjustedMeta = new String[0];
    private String referenceGroupFromAnalysisMeta = "NA";
    private String contrastFromAnalysisMeta = "anova";
    private String covPThresh = "0.05";
    @JsonIgnore
    private SelectItem[] referenceGroupFromAnalysisMetaOpts;
    @JsonIgnore
    private SelectItem[] contrastFromAnalysisMetaOpts;
    private String blockFac = "NA";
    private String covStyleOpt = "default";
    private String covPvalType = "fdr";

    public String getCovPvalType() {
        return covPvalType;
    }

    public void setCovPvalType(String covPvalType) {
        this.covPvalType = covPvalType;
    }

    public String getAnalysisMeta() {
        if (analysisMeta == null) {
            List<MetaDataBean> beans = tb.getMetaDataBeans();
            analysisMeta = beans.get(0).getName();
            primaryType = RDataUtils.GetPrimaryType(sb.getRConnection(), analysisMeta);
        }
        return analysisMeta;
    }

    public void setAnalysisMeta(String analysisMeta) {
        this.analysisMeta = analysisMeta;
    }

    public String getCovPThresh() {
        return covPThresh;
    }

    public void setCovPThresh(String covPThresh) {
        this.covPThresh = covPThresh;
    }

    public String getPrimaryType() {
        return primaryType;
    }

    public void setPrimaryType(String primaryType) {
        this.primaryType = primaryType;
    }

    public String[] getAdjustedMeta() {
        if (adjustedMeta == null) {
            MultifacBean mf = (MultifacBean) DataUtils.findBean("multifacBean");
            adjustedMeta = new String[1];
            adjustedMeta[0] = mf.getAnalysisMetaOpts()[0].getValue().toString();
        }
        return adjustedMeta;
    }

    public void setAdjustedMeta(String[] adjustedMeta) {
        this.adjustedMeta = adjustedMeta;
    }

    public String getReferenceGroupFromAnalysisMeta() {
        if (referenceGroupFromAnalysisMeta.equals("NA")) {
            referenceGroupFromAnalysisMeta = getReferenceGroupFromAnalysisMetaOpts()[0].getValue().toString();
        }
        return referenceGroupFromAnalysisMeta;
    }

    public void setReferenceGroupFromAnalysisMeta(String referenceGroupFromAnalysisMeta) {
        this.referenceGroupFromAnalysisMeta = referenceGroupFromAnalysisMeta;
    }

    public SelectItem[] getReferenceGroupFromAnalysisMetaOpts() {
        String[] grpNames = RDataUtils.getUniqueMetaNames(sb.getRConnection(), analysisMeta);
        referenceGroupFromAnalysisMetaOpts = new SelectItem[grpNames.length];
        for (int i = 0; i < grpNames.length; i++) {
            referenceGroupFromAnalysisMetaOpts[i] = new SelectItem(grpNames[i], grpNames[i]);
        }
        return referenceGroupFromAnalysisMetaOpts;
    }

    public String getContrastFromAnalysisMeta() {
        return contrastFromAnalysisMeta;
    }

    public void setContrastFromAnalysisMeta(String contrastFromAnalysisMeta) {
        this.contrastFromAnalysisMeta = contrastFromAnalysisMeta;
    }

    public SelectItem[] getContrastFromAnalysisMetaOpts() {
        String[] grpNames = RDataUtils.getUniqueMetaNames(sb.getRConnection(), analysisMeta);
        contrastFromAnalysisMetaOpts = new SelectItem[grpNames.length + 1];
        contrastFromAnalysisMetaOpts[0] = new SelectItem("anova", "All contrasts (ANOVA-style)");
        for (int i = 0; i < grpNames.length; i++) {
            contrastFromAnalysisMetaOpts[i + 1] = new SelectItem(grpNames[i], grpNames[i]);
        }
        return contrastFromAnalysisMetaOpts;
    }

    public String getBlockFac() {
        return blockFac;
    }

    public void setBlockFac(String blockFac) {
        this.blockFac = blockFac;
    }

    public String getCovStyleOpt() {
        return covStyleOpt;
    }

    public void setCovStyleOpt(String covStyleOpt) {
        this.covStyleOpt = covStyleOpt;
    }

    public String getAnalysisMeta2() {
        return analysisMeta2;
    }

    public void setAnalysisMeta2(String analysisMeta2) {
        this.analysisMeta2 = analysisMeta2;
    }

    public void analysisMetaChangeListener() {
        primaryType = RDataUtils.GetPrimaryType(sb.getRConnection(), analysisMeta);
        if (primaryType.equals("disc")) {
            String[] grpNames = RDataUtils.getUniqueMetaNames(sb.getRConnection(), analysisMeta);
            referenceGroupFromAnalysisMeta = grpNames[0];
            tb.setDisableTwofac(false);
        }else{
             tb.setDisableTwofac(true);
        }
         
        if (tb.getCompDesign().equals("nest")) {

            setSelectedMeta();
        }

    }

    public void covScatterButton_action() {
        JavaRecord.record_covScatterButton_action(this);
        double sigThresh = Double.parseDouble(covPThresh);
        String newName = sb.getNewImage("covariate_plot") + "dpi72";
        String imgName = newName + ".png";
        String covJsonName = newName + ".json";
        int res = 0;
        if (analysisMeta.equals(blockFac)) {
            sb.addMessage("Error", "Please make sure blocking factor is not the same as primary meta-data.");
            return;
        }

        for (String adjustedMeta1 : getAdjustedMeta()) {
            if (adjustedMeta1.equals(analysisMeta)) {
                sb.addMessage("Error", "Please make sure primary data is not also selected in covariate options.");
                return;
            } else if (adjustedMeta1.equals(blockFac)) {
                sb.addMessage("Error", "Please make sure blocking factor is not also selected in covariate options.");
                return;
            }
        }

        if (tb.getCompDesign().equals("cov")) {
            if (referenceGroupFromAnalysisMeta.equals(contrastFromAnalysisMeta)) {
                sb.addMessage("Error", "Please make sure the reference group is not the same as the contrast group.");
                return;
            }

            res = UniVarTests.performCovariateAnal(sb, imgName, "png", 72, covStyleOpt, analysisMeta, adjustedMeta, referenceGroupFromAnalysisMeta, blockFac, sigThresh, covPvalType, contrastFromAnalysisMeta);

        } else {
            if (analysisMeta2.equals(blockFac) && !analysisMeta2.equals("NA")) {
                sb.addMessage("Error", "Please make sure blocking factor is not the same as second factor.");
                return;
            }
           for (String adjustedMeta1 : getAdjustedMeta()) {
                if (adjustedMeta1.equals(analysisMeta2)) {
                    sb.addMessage("Error", "Please make sure primary data is not also selected in covariate options.");
                    return;
                }  
            }
            
            String designType = tb.getNestCompOpt();
            String selectedContrast;
   
            switch (designType) {
                case "ref" -> {
                    selectedContrast = tb.getSelectedGrp1();

                    res = UniVarTests.performCombineFacAnal(sb, imgName, "png", 72, analysisMeta, analysisMeta2,adjustedMeta, designType, selectedContrast, "NA", "T", blockFac, sigThresh, covPvalType);
                }
                case "custom" -> {
                    if (!tb.getSelectedGrp2().equals("NA")) {
                        selectedContrast = tb.getSelectedGrp1() + " vs. " + tb.getSelectedGrp2();
                    } else {
                        selectedContrast = tb.getSelectedGrp1();
                    }
                    res = UniVarTests.performCombineFacAnal(sb, imgName, "png", 72, analysisMeta, analysisMeta2,adjustedMeta, designType, selectedContrast, "NA", "T", blockFac, sigThresh, covPvalType);
                }
                case "inter" -> {
                    if (analysisMeta2.equals("NA") || analysisMeta2.equals(analysisMeta)) {
                        sb.addMessage("error","Please make sure second factor is selected and not the same as primary factor.");

                        return;
                    }
                    selectedContrast = tb.getSelectedGrp1() + " vs. " + tb.getSelectedGrp2();
                    res = UniVarTests.performCombineFacAnal(sb, imgName, "png", 72, analysisMeta, analysisMeta2,adjustedMeta, designType, selectedContrast, "NA", "T", blockFac, sigThresh, covPvalType);
                }
                case "nested" -> {
                    if (tb.getSelectedContrast1().equals(tb.getSelectedContrast2())) {
                        sb.addMessage("error", "The two nested groups are the same. Please choose two different groups.");
                        return;
                    }
                    res = UniVarTests.performCombineFacAnal(sb, imgName, "png", 72, analysisMeta, analysisMeta2,adjustedMeta, designType, tb.getSelectedContrast1(), tb.getSelectedContrast2(), tb.isInterOnly() ? "T" : "F", blockFac, sigThresh, covPvalType);
                }
                default -> {
                }
            }

        }
  
        if (res == -1) {
            sb.addMessage("Error", RDataUtils.getErrMsg(sb.getRConnection()));
            return;
        }

        if (res == 0) {
            sb.setLmSig(false);
        } else {
            sb.setLmSig(true);
        }

        String msg = RDataUtils.getCurrentMsg(sb.getRConnection());
        sb.addMessage("OK", msg);

        double rawCovThresh = UniVarTests.getRawCovThresh(sb.getRConnection());
        tb.setRawCovThresh(rawCovThresh);
        tb.setCovJsonName(covJsonName);
        tb.setCovJsonName(covJsonName);
        tb.setCovPerformed(true);
    }

    public void doDefaultLm() {
        WorkflowBean fp = (WorkflowBean) DataUtils.findBean("workflowBean");
        if (fp.getFunctionInfos().get("Linear Models") != null) {
            try {
                FunctionInvoker.callSetters(fp.getFunctionInfos().get("Linear Models"));
            } catch (Exception ex) {

            }
        }
    }

    public void compDesignChangeListener() {
        if (tb.getCompDesign().equals("nest")) {
            setSelectedMeta();
        }

    }

    public void setSelectedMeta() {

        if (analysisMeta.equals(analysisMeta2)) {
            sb.addMessage("error","Two metadata are the same!");
        } else {
            if (!analysisMeta2.equals("NA")) {
                String secdType = RDataUtils.GetPrimaryType(sb.getRConnection(), analysisMeta2);
                if (secdType.equals("cont")) {
                    tb.setRadioDisabled(true);
                    tb.setDisableInter(false);
                    tb.setNestCompOpt("inter");

                    return;
                }
            }
            tb.setRadioDisabled(false);
            tb.setDisableInter(false);

            String[] conditionsArr = RDataUtils.setSelectedMetaInfo(sb.getRConnection(), "name", analysisMeta, analysisMeta2, blockFac, false, true);
            if (conditionsArr[0].equals("0") & conditionsArr.length == 1) {
                sb.addMessage("error", "Please select the primary factor!");
                return;
            }
            tb.setGrpContrasts(conditionsArr);

        }

    }
}
