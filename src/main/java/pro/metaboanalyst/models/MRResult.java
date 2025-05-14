/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

//for Mendelian Randomization results
/**
 *
 * @author jianguox
 */
public class MRResult {
    private String rowID;
    private String numberOfSNPs;
    private String beta;
    private String se;
    private String pvalue;
    private String q;
    private String qdf;
    private String qpval;
    private String eggerIntercept;
    private String method;
    private String se2;
    private String pvalue2;
    
    public String getRowID() {
        return rowID;
    }

    public void setRowID(String rowID) {
        this.rowID = rowID;
    }

    public String getNumberOfSNPs() {
        return numberOfSNPs;
    }

    public void setNumberOfSNPs(String numberOfSNPs) {
        this.numberOfSNPs = numberOfSNPs;
    }

    public String getBeta() {
        return beta;
    }

    public void setBeta(String beta) {
        this.beta = beta;
    }

    public String getSe() {
        return se;
    }

    public void setSe(String se) {
        this.se = se;
    }

    public String getPvalue() {
        return pvalue;
    }

    public void setPvalue(String pvalue) {
        this.pvalue = pvalue;
    }

    public String getQ() {
        return q;
    }

    public void setQ(String q) {
        this.q = q;
    }

    public String getQdf() {
        return qdf;
    }

    public void setQdf(String qdf) {
        this.qdf = qdf;
    }

    public String getQpval() {
        return qpval;
    }

    public void setQpval(String qpval) {
        this.qpval = qpval;
    }

    public String getEggerIntercept() {
        return eggerIntercept;
    }

    public void setEggerIntercept(String eggerIntercept) {
        this.eggerIntercept = eggerIntercept;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public String getSe2() {
        return se2;
    }

    public void setSe2(String se2) {
        this.se2 = se2;
    }

    public String getPvalue2() {
        return pvalue2;
    }

    public void setPvalue2(String pvalue2) {
        this.pvalue2 = pvalue2;
    }

}