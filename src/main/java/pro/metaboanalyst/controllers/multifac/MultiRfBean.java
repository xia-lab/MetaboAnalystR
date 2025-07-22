/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.ArrayList;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.List;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.Classifying;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.models.MetaDataBean;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@RequestScoped
@Named("mrfBean")
public class MultiRfBean implements Serializable {

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

    private static final Logger LOGGER = LogManager.getLogger(MultiRfBean.class);
    private final String pageID = "RandomForest";
    private int treeNum = 500;
    private int tryNum = 7;
    private int rfRandom = 1;
    private String rfMeta = null;
    private String[] predictedMeta = new String[0];

    //inx indicate which col of sigmat is int, since default all double
    private static String setConfTable(String lbl, double[][] sigmat, String[] rownames, String[] colnames, ArrayList<Integer> inxs, double err) {
        String str = "<b>The OOB error is " + err + "</b></br>";
        str = str + "<table border=\"1\" cellpadding=\"5\">";
        str = str + "<tr><th>" + lbl + "</th>";
        for (String colname : colnames) {
            str = str + "<th>" + colname + "</th>";
        }
        str = str + "</tr>";
        for (int i = 0; i < rownames.length; i++) {
            str = str + "<tr><td>" + rownames[i] + "</td>";
            for (int j = 0; j < colnames.length; j++) {
                if (inxs.contains(j)) {
                    str = str + "<td>" + Math.round(sigmat[i][j]) + "</td>";
                } else {
                    str = str + "<td>" + sigmat[i][j] + "</td>";
                }
            }
            str = str + "</tr>";
        }
        str = str + "</table>";
        return str;
    }

    public int getTreeNum() {
        return treeNum;
    }

    public void setTreeNum(int treeNum) {
        this.treeNum = treeNum;
    }

    public int getTryNum() {
        return tryNum;
    }

    public void setTryNum(int tryNum) {
        this.tryNum = tryNum;
    }

    public int getRfRandom() {
        return rfRandom;
    }

    public void setRfRandom(int rfRandom) {
        this.rfRandom = rfRandom;
    }

    public String getRfMeta() {
        if (rfMeta == null) {
            rfMeta = mfb.getAnalysisMetaOpts()[0].getValue().toString();
        }
        return rfMeta;
    }

    public void setRfMeta(String rfMeta) {
        this.rfMeta = rfMeta;
    }

    public String[] getPredictedMeta() {
        return predictedMeta;
    }

    public void setPredictedMeta(String[] predictedMeta) {
        this.predictedMeta = predictedMeta;
    }

    public void doDefaultRf() {

        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/MultifacRFView.xhtml");
            //do something here
        } else {
            if (wb.getFunctionInfos().get("RandomForest2") != null) {
                try {
                    FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("RandomForest2"));
                } catch (Exception ex) {

                }
            }
        }
    }

    public String getConfText() {
        String[] rownames = Classifying.getRFConfRowNames(sb);
        if ("".equals(rownames[0]) & rownames.length == 1) {
            return ("The analysis has not been performed yet.");
        }
        double[][] confmat = Classifying.getRFConfusionMat(sb);
        String[] colnames = Classifying.getRFConfColNames(sb);
        Double rfOOB = Classifying.getRFOOB(sb);
        ArrayList<Integer> inxVec = new ArrayList();
        inxVec.add(0);
        inxVec.add(1);
        return (setConfTable(" ", confmat, rownames, colnames, inxVec, rfOOB));
    }

    public boolean rfBn_action_time() {
        try {
            if (wb.isEditMode()) {
                sb.addMessage("Info", "Parameters have been updated!");

                jrd.record_rfBn_action_time(this);
                return true;
            }

            if (treeNum < 1 || tryNum > RDataUtils.getNormFeatureNames(sb.getRConnection()).length) {

                sb.addMessage("Error", "Try number is not correct!");
                return false;
            }

            for (String predictedMeta1 : getPredictedMeta()) {
                if (getRfMeta().equals(predictedMeta1)) {
                    sb.addMessage("Error", "Please do not include primary meta-data in predictor meta-data!");
                    return false;
                }
            }
            jrd.record_rfBn_action_time(this);

            int res = Classifying.initRFMeta(sb, treeNum, tryNum, rfRandom, rfMeta, predictedMeta);

            switch (res) {
                case 1 -> {
                    Classifying.plotRFClassicationMeta(sb, sb.getNewImage("rf_cls"), "png", 150);
                    Classifying.plotRFCmpdMeta(sb, sb.getNewImage("rf_imp"), "png", 150);
                    Classifying.plotRFOutlierMf(sb, sb.getNewImage("rf_outlier"), "png", 150);
                }
                case 2 ->
                    sb.addMessage("Error", "The Random Forest module is only set up for classification. Please choose a "
                            + "categorical primary meta-data!");
                default -> {
                    sb.addMessage("Error", "Something wrong occured. " + RDataUtils.getErrMsg(sb.getRConnection()));
                    return false;
                }
            }

        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("rfBn_action_time", e);
        }
        return true;
    }
    @JsonIgnore
    @Inject
    private MultifacBean tb;

    @JsonIgnore
    public boolean isContMeta() {
        List<MetaDataBean> beans = tb.getMetaDataBeans();
        boolean contMeta = false;
        for (int i = 0; i < beans.size(); i++) {
            String type = beans.get(i).getParam();
            String name = beans.get(i).getName();

            if (name.equals(rfMeta)) {
                if (type.equals("cont")) {
                    contMeta = true;
                }
            }
        }
        return contMeta;
    }
    private String mtryMode = "auto";

    public String getMtryMode() {
        return mtryMode;
    }

    public void setMtryMode(String mtryMode) {
        this.mtryMode = mtryMode;
    }

    public Integer getEffectiveMtry() {
        return "auto".equals(mtryMode) ? -1 : tryNum;
    }
}
