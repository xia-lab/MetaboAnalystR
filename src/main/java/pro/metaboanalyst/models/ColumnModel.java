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
 * @author jianguox
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ColumnModel implements Serializable {

    private String header;
    private String property;
    private String type; //int/duble/string; also used as name
    
    @JsonCreator
    @ConstructorProperties({"header", "property", "type"})
    public ColumnModel(String header, String property, String type) {
        this.header = header;
        this.property = property;
        this.type = type;
    }

    public String getHeader() {
        return header;
    }

    public String getProperty() {
        return property;
    }
    
    public String getType(){
        return type;
    }
}