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
 * @author Jeff
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class MetaValueBean implements Serializable {

    private String name;
    private int uniqID; //unique as rowKey
    private String val1;
    private String val2;
    private String val3;
    private String val4;
    private String val5;
    private String val6;
    private String val7;
    private String val8;
    private String extra;
    private int count = 0;

    @JsonCreator
    @ConstructorProperties({"name", "uniqID", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "extra", "count"})
    public MetaValueBean(String name, int uniqID, String val1, String val2, String val3, String val4, String val5, String val6, String val7, String val8, String extra, int count) {
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
        this.extra = extra;
        this.count = count;
    }

    public MetaValueBean() {

    }

    public void addValue(String val) {
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
            default:
                val8 = val;
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

    public String getVal1() {
        return val1;
    }

    public void setVal1(String val1) {
        this.val1 = val1;
    }

    public String getVal2() {
        return val2;
    }

    public void setVal2(String val2) {
        this.val2 = val2;
    }

    public String getVal3() {
        return val3;
    }

    public void setVal3(String val3) {
        this.val3 = val3;
    }

    public String getVal4() {
        return val4;
    }

    public void setVal4(String val4) {
        this.val4 = val4;
    }

    public String getVal5() {
        return val5;
    }

    public void setVal5(String val5) {
        this.val5 = val5;
    }

    public String getVal6() {
        return val6;
    }

    public void setVal6(String val6) {
        this.val6 = val6;
    }

    public String getVal7() {
        return val7;
    }

    public void setVal7(String val7) {
        this.val7 = val7;
    }

    public String getVal8() {
        return val8;
    }

    public void setVal8(String val8) {
        this.val8 = val8;
    }

}
