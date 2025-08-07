/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.annotation.JsonCreator;

import java.beans.ConstructorProperties;
import java.io.Serializable;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author Jeff
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PABean implements Serializable {

    private String integOpt; //for pval combination, need to show result for individual
    private String setName;
    private String keggLink;
    private String smpdbLink = "";
    private int setNum;
    private double expNum;
    private int hitNum;
    private double pVal;
    private double logP;
    private double pgene = 0;
    private double pcmpd = 0;
    private double holmPval;
    private double fdrPval;
    private double impVal; // importance measure

    @JsonCreator
    @ConstructorProperties({"integOpt", "setName", "keggLink", "smpdbLink", "setNum", "expNum", "hitNum", "pVal", "logP", "holmPval", "fdrPval", "impVal", "pcmpd", "pgene"})
    public PABean(String integOpt, String setName, String keggLink, String smpdbLink, int setNum, double expNum, int hitNum,
            double pVal, double logP, double holmPval, double fdrPval, double impVal, double pcmpd, double pgene) {
        this.integOpt = integOpt;
        this.setName = setName;
        this.keggLink = keggLink;
        this.smpdbLink = smpdbLink;
        this.setNum = setNum;
        this.expNum = expNum;
        this.hitNum = hitNum;
        this.pVal = pVal;
        this.logP = logP;
        this.holmPval = holmPval;
        this.fdrPval = fdrPval;
        this.impVal = impVal;
        this.pgene = pgene;
        this.pcmpd = pcmpd;
    }

    public String getIntegOpt() {
        return integOpt;
    }

    public void setIntegOpt(String integOpt) {
        this.integOpt = integOpt;
    }

    public double getpVal() {
        return pVal;
    }

    public void setpVal(double pVal) {
        this.pVal = pVal;
    }

    
    
    public String getMatch() {
        if (integOpt.startsWith("pval")) {
            return "(" + (int) expNum + ", " + hitNum + ")/" + setNum;
        }
        return (hitNum + "/" + setNum);
    }

    public double getLogP() {
        return logP;
    }

    public void setLogP(double logP) {
        this.logP = logP;
    }

    public double getHolmPval() {
        return holmPval;
    }

    public void setHolmPval(double holmPval) {
        this.holmPval = holmPval;
    }

    public double getImpVal() {
        return impVal;
    }

    public void setImpVal(double impVal) {
        this.impVal = impVal;
    }

    public double getFdrPval() {
        return fdrPval;
    }

    public void setFdrPval(double fdrPval) {
        this.fdrPval = fdrPval;
    }

    public double getExpNum() {
        return expNum;
    }

    public void setExpNum(int expNum) {
        this.expNum = expNum;
    }

    public String getKeggLink() {
        return keggLink;
    }

    public void setKeggLink(String keggLink) {
        this.keggLink = keggLink;
    }

    public String getSmpdbLink() {
        return smpdbLink;
    }

    public void setSmpdbLink(String smpdLink) {
        this.smpdbLink = smpdLink;
    }

    public int getHitNum() {
        return hitNum;
    }

    public void setHitNum(int hitNum) {
        this.hitNum = hitNum;
    }

    public int getSetNum() {
        return setNum;
    }

    public void setSetNum(int setNum) {
        this.setNum = setNum;
    }

    public double getPVal() {
        return pVal;
    }

    public String getSetName() {
        return setName;
    }

    public String getDetails() {
        return (keggLink != null ? keggLink : "") + "\n" + (smpdbLink != null ? smpdbLink : "");
    }

    public double getPgene() {
        return pgene;
    }

    public void setPgene(double pgene) {
        this.pgene = pgene;
    }

    public double getPcmpd() {
        return pcmpd;
    }

    public void setPcmpd(double pcmpd) {
        this.pcmpd = pcmpd;
    }

}
