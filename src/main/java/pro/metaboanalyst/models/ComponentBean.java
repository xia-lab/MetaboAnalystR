/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import java.io.Serializable;

/**
 * @author Jeff
 */
public class ComponentBean implements Serializable {

    private String name;
    private int varNum;

    public ComponentBean() {
    }

    public ComponentBean(String name, int varNum) {
        this.name = name;
        this.varNum = varNum;
    }

    public String getName() {
        return name;
    }

    public int getVarNum() {
        return varNum;
    }

    public void setVarNum(int varNum) {
        this.varNum = varNum;
    }

}
