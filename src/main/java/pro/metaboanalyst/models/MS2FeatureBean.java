/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author qiang
 */
public class MS2FeatureBean implements Serializable {

    private int FeatureNo;
    private double PrecMz;
    private double similarity_score;
    private double dot_score;
    private String Formulas;
    private String Compounds;
    private String SIMLES;
    private String InchiKeys;
    private double groupPrecMz;
    private String featurelabel;

        
    public MS2FeatureBean(
            int FeatureNo, 
            double PrecMz,
            double similarity_score,
            double dot_score,
            String Formulas,
            String Compounds,
            String SIMLES,
            String InchiKeys,
            double groupPrecMz,
            String featurelabel){
        
        this.Compounds = Compounds;
        this.FeatureNo = FeatureNo;
        this.Formulas = Formulas;
        this.SIMLES = SIMLES;
        this.similarity_score = similarity_score;
        this.dot_score = dot_score;
        this.PrecMz = PrecMz;
        this.InchiKeys = InchiKeys;
        this.groupPrecMz = groupPrecMz;
        this.featurelabel = featurelabel;
    }
    
    public int getFeatureNo() {
        return FeatureNo;
    }

    public void setFeatureNo(int FeatureNo) {
        this.FeatureNo = FeatureNo;
    }

    public double getPrecMz() {
        return PrecMz;
    }

    public void setPrecMz(double PrecMz) {
        this.PrecMz = PrecMz;
    }

    public double getSimilarity_score() {
        return similarity_score;
    }

    public void setSimilarity_score(double similarity_score) {
        this.similarity_score = similarity_score;
    }

    public String getFormulas() {
        return Formulas;
    }

    public void setFormulas(String Formulas) {
        this.Formulas = Formulas;
    }

    public String getCompounds() {
        return Compounds;
    }

    public void setCompounds(String Compounds) {
        this.Compounds = Compounds;
    }

    public String getSIMLES() {
        return SIMLES;
    }

    public void setSIMLES(String SIMLES) {
        this.SIMLES = SIMLES;
    }

    public String getInchiKeys() {
        return InchiKeys;
    }

    public void setInchiKeys(String InchiKeys) {
        this.InchiKeys = InchiKeys;
    }

    public double getDot_score() {
        return dot_score;
    }

    public void setDot_score(double dot_score) {
        this.dot_score = dot_score;
    }

    public double getGroupPrecMz() {
        return groupPrecMz;
    }

    public void setGroupPrecMz(double groupPrecMz) {
        this.groupPrecMz = groupPrecMz;
    }

    public String getFeaturelabel() {
        return featurelabel;
    }

    public void setFeaturelabel(String featurelabel) {
        this.featurelabel = featurelabel;
    }      

}
