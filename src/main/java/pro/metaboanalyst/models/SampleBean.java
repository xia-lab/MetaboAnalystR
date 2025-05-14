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
public class SampleBean implements Serializable{

    private String name;
    private String group;
    private double adjust =1.0; //sample specific normalization

    @JsonCreator
    @ConstructorProperties({"name", "group", "adjust"})
    public SampleBean(String name, String group, double adjust) {
        this.name = name;
        this.group = group;
        this.adjust = adjust;
    }

    public SampleBean() {
    }

    public SampleBean(String name, String group) {
        this.name = name;
        this.group = group;
    }

    public SampleBean(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getGroup() {
        return group;
    }
    public void setGroup(String group) {
        this.group = group;
    }
    
    public double getAdjust(){
        return adjust;
    }

    public void setAdjust(double adjust){
        if(adjust==0){
            adjust = 0.0001; //cannot be zero
        }
        this.adjust = adjust;
    }
}
