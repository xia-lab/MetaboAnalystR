/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.ArrayList;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.Classifying;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("classBean")
public class ClassificationBean implements Serializable {

    @Inject
    SessionBean1 sb;
    
        @JsonIgnore
    @Inject
    private JavaRecord jrd;
        
    private static final Logger LOGGER = LogManager.getLogger(ClassificationBean.class);

    private int treeNum = 500;
    private int tryNum = 7;
    private int rfRandom = 1;

    public int getRfRandom() {
        return rfRandom;
    }

    public void setRfRandom(int rfRandom) {
        this.rfRandom = rfRandom;
    }

    public int getTryNum() {
        return tryNum;
    }

    public void setTryNum(int tryNum) {
        this.tryNum = tryNum;
    }

    public int getTreeNum() {
        return treeNum;
    }

    public void setTreeNum(int treeNum) {
        this.treeNum = treeNum;
    }

    public String rfBn_action() {
        try {

            // index cannot be more than total number of cluster
            if (treeNum < 1 || tryNum > RDataUtils.getNormFeatureNames(sb.getRConnection()).length) {
                return null;
            }
            Classifying.initRF(sb, treeNum, tryNum, rfRandom);
            Classifying.plotRFClassication(sb, sb.getNewImage("rf_cls"), "png", 150);
            Classifying.plotRFCmpd(sb, sb.getNewImage("rf_imp"), "png", 150);
            Classifying.plotRFOutlier(sb, sb.getNewImage("rf_outlier"), "png", 150);
            jrd.record_rfBn_action(this);
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("rfBn_action", e);
        }
        return null;
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

    private String validationOpt = "10";

    public String getValidationOpt() {
        return validationOpt;
    }

    public void setValidationOpt(String validationOpt) {
        this.validationOpt = validationOpt;
    }

    public String svmBn_action() {
        Classifying.initSVMAnal(sb, validationOpt);
        Classifying.plotSVMClassification(sb, sb.getNewImage("svm_cls"), "png", 150);
        Classifying.plotSVMSigCmpds(sb, sb.getNewImage("svm_imp"), "png", 150);
        jrd.record_svmBn_action(this);
        return null;
    }

}
