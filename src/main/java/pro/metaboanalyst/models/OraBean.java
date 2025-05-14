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
public class OraBean implements Serializable {

    private String setName;
    private int setNum;
    private double expNum;
    private int hitNum;
    //  private String match;
    private double pVal;
    private double bonPval;
    private double fdrPval;
    private String style;
    
    @JsonCreator
    @ConstructorProperties({"setName", "style", "setNum", "expNum", "hitNum", "pVal", "bonPval", "fdrPval"})
    public OraBean(String setName, String style, int setNum, double expNum, int hitNum, double pVal, double bonPval, double fdrPval) {
        this.setName = setName;
        this.style = style;
        this.setNum = setNum;
        this.expNum = expNum;
        this.hitNum = hitNum;
        //       match = hitNum + "/" + setNum;
        this.pVal = pVal;
        this.bonPval = bonPval;
        this.fdrPval = fdrPval;
    }

    public String getStyle() {
        return style;
    }

    public void setStyle(String style) {
        this.style = style;
    }

    public double getBonPval() {
        return bonPval;
    }

    public void setBonPval(double bonPval) {
        this.bonPval = bonPval;
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

    //   public String getMatch() {
    //       return match;
    //   }
    
    public double getPVal() {
        return pVal;
    }

    public String getSetName() {
        return setName;
    }

}
