/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.Serializable;

/**
 *
 * @author qiang
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RawFeatureBean implements Serializable{

    private int FeatureNo;
    private double mz;
    private double RT;
    private String Formulas;
    private String Isotopes;
    private String Adducts;
    private String cmpdHits;
    private String Annotations;
    private String HMDBIDs;
    private double pvalues;
    private double pfdrs;
    private double intensity;
    private double cvs;

    public RawFeatureBean(
            int FeatureNo, 
            double mz, 
            double RT, 
            String Annotations, 
            String Formulas, 
            String cmpdHits, 
            String HMDBIDs,
            double intensity,
            double pvalues,
            double pfdrs,
            double cvs
            ) {
        this.Annotations = Annotations;
        this.FeatureNo = FeatureNo;
        this.mz = mz;
        this.RT = RT;
        this.Formulas = Formulas;
        this.cmpdHits = cmpdHits;
        this.HMDBIDs = HMDBIDs;
        this.pfdrs = pfdrs;
        this.pvalues = pvalues;
        this.intensity = intensity;
        this.cvs = cvs;
    }

    public String getAnnotations() {
        return Annotations;
    }

    public void setAnnotations(String Annotations) {
        this.Annotations = Annotations;
    }

    public int getFeatureNo() {
        return FeatureNo;
    }

    public void setFeatureNo(int FeatureNo) {
        this.FeatureNo = FeatureNo;
    }

    public double getMz() {
        return mz;
    }

    public void setMz(double mz) {
        this.mz = mz;
    }

    public double getRT() {
        return RT;
    }

    public void setRT(double RT) {
        this.RT = RT;
    }

    public double getCvs() {
        return cvs;
    }

    public void setCvs(double cvs) {
        this.cvs = cvs;
    }

    public String getFormulas() {
//        if(Formulas.equals("")) {
//            return "";
//        }
//        String[] formus = Formulas.split("; ");        
//
//        String formusNew = "";
//        for (String formu : formus) {
//            formusNew = formusNew + "<a href=https://pubchem.ncbi.nlm.nih.gov/#query=" + formu + " target='_blank'>" + formu + "</a>" + "; ";            
//        }
//        return formusNew;
        return Formulas;
    }

    public void setFormulas(String Formulas) {
        this.Formulas = Formulas;
    }

    public String getIsotopes() {
        return Isotopes;
    }

    public void setIsotopes(String Isotopes) {
        this.Isotopes = Isotopes;
    }

    public String getAdducts() {
        return Adducts;
    }

    public void setAdducts(String Adducts) {
        this.Adducts = Adducts;
    }

    public String getCmpdHits() {
        return cmpdHits;
//        if (cmpdHits.equals("")) {
//            return "";
//        }
//        String[] cmpds = cmpdHits.split("; ");
//        String[] hmdbids = HMDBIDs.split("; ");
//
//        String cmpdsnew = "";
//        for (int i = 0; i < cmpds.length; i++) {
//            cmpdsnew = cmpdsnew + "<a href=http://www.hmdb.ca/metabolites/" + hmdbids[i] + " target='_blank'>" + cmpds[i] + "</a>" + "; ";
//        }
//
//        return cmpdsnew;
    }

    public void setCmpdHits(String cmpdHits) {
        this.cmpdHits = cmpdHits;
    }

    public String getHMDBIDs() {
        return HMDBIDs;
    }

    public void setHMDBIDs(String HMDBIDs) {
        this.HMDBIDs = HMDBIDs;
    }

    public double getPvalues() {
        return pvalues;
    }

    public void setPvalues(double pvalues) {
        this.pvalues = pvalues;
    }

    public double getPfdrs() {
        return pfdrs;
    }

    public void setPfdrs(double pfdrs) {
        this.pfdrs = pfdrs;
    }   

    public double getIntensity() {
        return intensity;
    }

    public void setIntensity(double intensity) {
        this.intensity = intensity;
    }    
    
}
