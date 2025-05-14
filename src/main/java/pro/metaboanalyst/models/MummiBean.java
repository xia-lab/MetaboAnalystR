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
public class MummiBean implements Serializable {

    private String setName;
    private String keggLink;
    private int setNum; //theory total
    private int refNum; //measure total
    private int hitNum;
    private double easeP;
    private double expect;
    private double fisherP;
    private double gammaP;
    private double empiricalP;

    @JsonCreator
    @ConstructorProperties({"setName", "keggLink", "setNum", "refNum", "hitNum", "easeP", "fisherP", "gammaP", "expect", "empiricalP"})
    public MummiBean(String setName, String keggLink, int setNum, int refNum, int hitNum,
            double easeP, double fisherP, double gammaP, double expect, double empiricalP) {
        this.setName = setName;
        this.keggLink = keggLink;
        this.refNum = refNum;
        this.setNum = setNum;
        this.hitNum = hitNum;
        this.easeP = easeP;
        this.fisherP = fisherP;
        this.gammaP = gammaP;
        this.expect = expect;
        this.empiricalP = empiricalP;
    }

    public double getExpect() {
        return expect;
    }

    public void setExpect(double expect) {
        this.expect = expect;
    }

    public String getMatch() {
        return (hitNum + "/" + setNum);
    }

    public int getRefNum() {
        return refNum;
    }

    public void setRefNum(int refNum) {
        this.refNum = refNum;
    }

    public double getEaseP() {
        return easeP;
    }

    public void setEaseP(double easeP) {
        this.easeP = easeP;
    }

    public double getFisherP() {
        return fisherP;
    }

    public void setFisherP(double fisherP) {
        this.fisherP = fisherP;
    }

    public double getGammaP() {
        return gammaP;
    }

    public void setGammaP(double gammaP) {
        this.gammaP = gammaP;
    }

    public String getEmpiricalP() {
        // return empiricalP;
        int count = (int) empiricalP;
        String prefix = "";
        if (count == 0) {
            prefix = "P < 0.01";
        } else {
            prefix = "P = " + empiricalP / 100;
        }
        return prefix + " (" + count + "/" + 100 + ")";
    }

    public void setEmpiricalP(double empiricalP) {
        this.empiricalP = empiricalP;
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

    public void setAlgOpts(String[] alg) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
