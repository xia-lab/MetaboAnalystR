/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.*;

import java.io.Serializable;
import java.text.DecimalFormat;

/**
 *
 * @author Jeff
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class QeaBean implements Serializable {
    @JsonIgnore
    DecimalFormat sci5Formater;
    private String setName;
    private int setNum;
    private int hitNum;
    private double statQ;
    private double expQ;
    private double pVal;
    private double bonPval;
    private double fdrPval;
    private String style;
    @JsonCreator
    public QeaBean(@JsonProperty("setName") String setName,@JsonProperty("style") String style,@JsonProperty("setNum") int setNum,@JsonProperty("hitNum") int hitNum,@JsonProperty("statQ") double statQ,@JsonProperty("expQ") double expQ,@JsonProperty("pVal") double pVal,@JsonProperty("bonPval") double bonP,@JsonProperty("fdrPval") double fdrPval) {
        this.setName = setName;
        this.style=style;
        this.setNum = setNum;
        this.hitNum = hitNum;
        this.statQ = statQ;
        this.expQ = expQ;
        this.pVal = pVal;
        this.bonPval = bonP;
        this.fdrPval = fdrPval;
        sci5Formater = new DecimalFormat("0.####E0");

    }

    public String getStyle() {
        return style;
    }
    
    public void setStyle(String style) {
        this.style = style;
    }

    public int getHitNum() {
        return hitNum;
    }

    public String getSetName() {
        return setName;
    }

    public int getSetNum() {
        return setNum;
    }

    public String getExpQ() {
        if (expQ < 0.00001) {
            return sci5Formater.format(expQ);
        }
        return expQ + "";
    }

    public String getPVal() {
        if (pVal < 0.00001) {
            return sci5Formater.format(pVal);
        }
        return pVal + "";
    }

    public String getStatQ() {
        if (statQ < 0.00001) {
            return sci5Formater.format(statQ);
        }
        return statQ + "";
    }

    public String getBonPval() {
        if (bonPval < 0.00001) {
            return sci5Formater.format(bonPval);
        }
        return bonPval + "";
    }

    public String getFdrPval() {
        if (fdrPval < 0.00001) {
            return sci5Formater.format(fdrPval);
        }
        return fdrPval + "";
    }
}
