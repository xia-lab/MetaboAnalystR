/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

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
public class GseaBean implements Serializable {
    private final String setName;
    private String keggLink;
    private int setNum; //theory total
    private int hitNum;
    private double rawP;
    private double adjP;
    private double nesScore;

    @JsonCreator
    @ConstructorProperties({"setName", "keggLink", "setNum", "hitNum", "rawP", "adjP", "nesScore"})
    public GseaBean(String setName, String keggLink, int setNum, int hitNum,
            double rawP, double adjP, double nesScore) {
        this.setName = setName;
        this.keggLink = keggLink;
        this.setNum = setNum;
        this.hitNum = hitNum;
        this.rawP = rawP;
        this.adjP = adjP;
        this.nesScore = nesScore;
    }
    
    public String getMatch(){
        return (hitNum + "/" + setNum);
    }

    public double getRawP() {
        return rawP;
    }

    public void setRawP(double rawP) {
        this.rawP = rawP;
    }

    public double getAdjP() {
        return adjP;
    }

    public void setAdjP(double adjP) {
        this.adjP = adjP;
    }

    public double getNesScore() {
        return nesScore;
    }

    public void setNesScore(double nesScore) {
        this.nesScore = nesScore;
    }

    public String getKeggLink() {
        return keggLink;
    }

    public void setKeggLink(String keggLink) {
        this.keggLink = keggLink;
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

    public String getSetName() {
        return setName;
    }
}
