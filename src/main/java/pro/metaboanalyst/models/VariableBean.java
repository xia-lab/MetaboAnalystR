/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.beans.ConstructorProperties;
import java.io.Serializable;

/**
 *
 * @author Jeff
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class VariableBean implements Serializable{

    private String name;
    private double min;
    private double median;
    private double max;
    private int missingNo;
    private int zeroNo;
    private double missingPercent;
    private double zeroPercent;
    private Boolean included = true;

    @JsonCreator
    @ConstructorProperties({"name", "min", "median", "min", "max", "missingNo", "zeroNo", "missingPercent", "zeroPercent", "included"})
    public VariableBean(String name, double min, double median, double max, int missingNo, int zeroNo, double missingPercent, double zeroPercent, Boolean included) {
        this.name = name;
        this.min = min;
        this.median = median;
        this.max = max;
        this.missingNo = missingNo;
        this.zeroNo = zeroNo;
        this.missingPercent = missingPercent;
        this.zeroPercent = zeroPercent;
        this.included = included;
    }

    public VariableBean() {
    }

    public VariableBean(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String nm) {
        this.name = nm;
    }

    public void setMin(double min) {
        this.min = min;
    }

    public void setMedian(double median) {
        this.median = median;
    }

    public void setMax(double max) {
        this.max = max;
    }

    public void setMissingNumber(int missingNo) {
        this.missingNo = missingNo;
    }

    public void setZeroNumber(int zeroNo) {
        this.zeroNo = zeroNo;
    }

    public void setMissingPercent(double missingPercent) {
        this.missingPercent = missingPercent;
    }

    public void setZeroPercent(double zeroPercent) {
        this.zeroPercent = zeroPercent;
    }

    public String getRange() {
        return "[ " + min + " .. " + median + " .. " + max + " ]";
    }

    public String getZeroInfo() {
        return zeroNo+ " (" + Math.round(zeroPercent) + "%)";
    }

    public String getMissingInfo() {
        return  missingNo + " (" + Math.round(missingPercent) + "%)";
    }

    public Boolean getIncluded() {
        return included;
    }

    public void setIncluded(Boolean included) {
        this.included = included;
    }
}
