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
public class NameBean implements Serializable{
    private String name;
    private double prob;
    private String cls;
    @JsonCreator
    @ConstructorProperties({"name", "prob", "cls"})
    public NameBean(String name, double prob, String cls) {
        this.name = name;
        this.prob = prob;
        this.cls = cls;
    }

    public NameBean() {
    }
    
    public NameBean(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public double getProb() {
        return prob;
    }

    public void setProb(double prob) {
        this.prob = prob;
    }

    public String getCls() {
        return cls;
    }

    public void setCls(String cls) {
        this.cls = cls;
    }
}
