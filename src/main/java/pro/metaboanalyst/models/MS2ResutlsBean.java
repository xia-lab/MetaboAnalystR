/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;

/**
 *
 * @author qiang
 */
public class MS2ResutlsBean {

    private int FeatureNo;
    private int sub_idx;
    private double PrecMz;
    private double PrecRT;
    private double similarity_score;
    private double dot_score;
    private String Formulas;
    private String Compounds;
    private String SIMLES;
    private String InchiKeys;
    private double groupPrecMz;
    private String featurelabel;
    private String databases;

    public MS2ResutlsBean(
            int FeatureNo,
            int sub_idx,
            double PrecMz_min,
            double PrecMz_max,
            double PrecRT_min,
            double PrecRT_max,
            double similarity_score,
            double dot_score,
            String Formulas,
            String Compounds,
            String SIMLES,
            String InchiKeys,
            double groupPrecMz,
            String featurelabel,
            String databases) {

        this.Compounds = Compounds;
        this.FeatureNo = FeatureNo;
        this.sub_idx = sub_idx;
        this.Formulas = Formulas;
        this.SIMLES = SIMLES;
        this.similarity_score = similarity_score;
        this.dot_score = dot_score;
        this.PrecMz = Math.round((PrecMz_min + PrecMz_max)*10000)/20000.00;
        this.InchiKeys = InchiKeys;
        this.groupPrecMz = groupPrecMz;
        this.featurelabel = featurelabel;
        this.PrecRT = Math.round((PrecRT_min + PrecRT_max)*100)/200.00;
        this.databases = databases;
    }

    public int getFeatureNo() {
        return FeatureNo;
    }

    public void setFeatureNo(int FeatureNo) {
        this.FeatureNo = FeatureNo;
    }

    public int getSub_idx() {
        return sub_idx;
    }

    public void setSub_idx(int sub_idx) {
        this.sub_idx = sub_idx;
    }

    public double getPrecMz() {
        return PrecMz;
    }

    public void setPrecMz(double PrecMz) {
        this.PrecMz = PrecMz;
    }

    public double getPrecRT() {
        return PrecRT;
    }

    public void setPrecRT(double PrecRT) {
        this.PrecRT = PrecRT;
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

    public String getDatabases() {
        return databases;
    }

    public void setDatabases(String databases) {
        this.databases = databases;
    }

}
