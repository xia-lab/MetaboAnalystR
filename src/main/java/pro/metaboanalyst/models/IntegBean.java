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
public class IntegBean implements Serializable {

    private final String setName;
    private String keggLink;
    private int setNum; //theory total
    private int refNum;
    private int hitNum;
    private double mummiP;
    private double gseaP;
    private double comboP;

    @JsonCreator
    @ConstructorProperties({"setName", "keggLink", "setNum", "refNum", "hitNum", "mummiP", "gseaP", "comboP"})
    public IntegBean(String setName, String keggLink, int setNum, int refNum, int hitNum,
                                                                             double mummiP, double gseaP, double comboP) {
        this.setName = setName;
        this.keggLink = keggLink;
        this.setNum = setNum;
        this.refNum = refNum;
        this.hitNum = hitNum;
        this.mummiP = mummiP;
        this.gseaP = gseaP;
        this.comboP = comboP;
    }
    
    public String getMatch(){
        return (hitNum + "/" + setNum);
    }

    public int getRefNum() {
        return refNum;
    }
    
    public void setRefNum(int refNum) {
        this.refNum = refNum;
    }

    public double getMummiP() {
        return mummiP;
    }

    public void setMummiP(double mummiP) {
        this.mummiP = mummiP;
    }

    public double getGseaP() {
        return gseaP;
    }

    public void setGseaP(double gseaP) {
        this.gseaP = gseaP;
    }

    public double getComboP() {
        return comboP;
    }

    public void setComboP(double comboP) {
        this.comboP = comboP;
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
