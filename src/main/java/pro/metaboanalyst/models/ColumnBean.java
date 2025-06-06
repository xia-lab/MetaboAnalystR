/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.beans.ConstructorProperties;
import java.io.Serializable;

/**
 *
 * @author jianguox
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ColumnBean implements Serializable {

    private String name = "";
    private boolean visible = false;

    @JsonCreator
    @ConstructorProperties({"visible", "name"})
    public ColumnBean(boolean visible, String name) {
        this.visible = visible;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public boolean isVisible() {
        return visible;
    }
}
