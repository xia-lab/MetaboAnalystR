/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import java.beans.ConstructorProperties;
import java.io.Serializable;

/**
 *
 * @author Jeff
 */
public class FeatureBean implements Serializable {

    private String name;
    private int uniqID; //unique as rowKey
    private double val1;
    private double val2;
    private double val3;
    private double val4;
    private double val5;
    private double val6;
    private double val7;
    private double val8;
    private double val9;
    private double val10;
    private double val11;
    private String extra;
    private String extra2;

    private int count = 0;

    @JsonCreator
    @ConstructorProperties({"name", "uniqID", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9", "val10", "val11", "extra", "extra2", "count"})
    public FeatureBean(String name,
            int uniqID,
            double val1,
            double val2,
            double val3,
            double val4,
            double val5,
            double val6,
            double val7,
            double val8,
            double val9,
            double val10,
            double val11,
            String extra,
            String extra2,
            int count) {
        this.name = name;
        this.uniqID = uniqID;
        this.val1 = val1;
        this.val2 = val2;
        this.val3 = val3;
        this.val4 = val4;
        this.val5 = val5;
        this.val6 = val6;
        this.val7 = val7;
        this.val8 = val8;
        this.val9 = val9;
        this.val10 = val10;
        this.val11 = val11;
        this.extra = extra;
        this.extra2 = extra2;
        this.count = count;
    }

    public String getExtra2() {
        return extra2;
    }

    public void setExtra2(String extra2) {
        this.extra2 = extra2;
    }

    public FeatureBean() {

    }

    public void addValue(double val) {
        switch (count) {
            case 0:
                val1 = val;
                count++;
                break;
            case 1:
                val2 = val;
                count++;
                break;
            case 2:
                val3 = val;
                count++;
                break;
            case 3:
                val4 = val;
                count++;
                break;
            case 4:
                val5 = val;
                count++;
                break;
            case 5:
                val6 = val;
                count++;
                break;
            case 6:
                val7 = val;
                count++;
                break;
            case 7:
                val8 = val;
                count++;
                break;
            case 8:
                val9 = val;
                count++;
                break;
            case 9:
                val10 = val;
                count++;
                break;
            default:
                val11 = val;
                break;
        }
    }

    public int getUniqID() {
        return uniqID;
    }

    public void setUniqID(int uniqID) {
        this.uniqID = uniqID;
    }

    public void addName(String nm) {
        name = nm;
    }

    public void addExtra(String val) {
        extra = val;
    }

    public void addExtra2(String val) {
        extra2 = val;
    }

    public String getExtra() {
        return extra;
    }

    public String getName() {
        return name;
    }

    //the names are editable by users
    public void setName(String nm) {
        this.name = nm;
    }

    public double getVal1() {
        return val1;
    }

    public double getVal2() {
        return val2;
    }

    public double getVal3() {
        return val3;
    }

    public double getVal4() {
        return val4;
    }

    public double getVal5() {
        return val5;
    }

    public double getVal6() {
        return val6;
    }

    public double getVal7() {
        return val7;
    }

    public double getVal8() {
        return val8;
    }

    public double getVal9() {
        return val9;
    }

    public double getVal10() {
        return val10;
    }

    public double getVal11() {
        return val11;
    }
}
