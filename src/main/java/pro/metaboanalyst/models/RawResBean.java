/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author zzggyy
 */
public class RawResBean implements Serializable {
    
    private String name;
    private String group;
    private String rtRange;
    private String mzRange;
    private int peakNum;
    private double missingPeakPercent;
    
    public RawResBean(String name, String group, String rtRange, String mzRange, int peakNum, double missingPeakPercent){
        this.name = name;
        this.group = group;
        this.rtRange = rtRange;
        this.mzRange = mzRange;
        this.peakNum = peakNum;
        this.missingPeakPercent = missingPeakPercent;
    }
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getRtRange() {
        return rtRange;
    }

    public void setRtRange(String rtRange) {
        this.rtRange = rtRange;
    }

    public String getMzRange() {
        return mzRange;
    }

    public void setMzRange(String mzRange) {
        this.mzRange = mzRange;
    }

    public String getGroup() {
        return group;
    }

    public void setGroup(String group) {
        this.group = group;
    }

    public int getPeakNum() {
        return peakNum;
    }

    public void setPeakNum(int peakNum) {
        this.peakNum = peakNum;
    }

    public double getMissingPeakPercent() {
        return missingPeakPercent;
    }

    public void setMissingPeakPercent(double missingPeakPercent) {
        this.missingPeakPercent = missingPeakPercent;
    }
}
