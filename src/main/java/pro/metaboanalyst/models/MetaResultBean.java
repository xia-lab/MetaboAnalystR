/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.HashMap;

/**
 * Results for individual feature Note, maximal 10 data sets, corresponding to
 * ten cols
 *
 * @author Jeff
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class MetaResultBean implements Serializable {

    //this ID is for string only for sorting, not embedded hyperlink
    private String ID; //note, hashmap can also have a ID key => hyperlinked gene
    private String name;
    private boolean significant = false;

    private HashMap<String, String> contentMap;

    @JsonCreator
    @ConstructorProperties({"ID", "name", "significant", "contentMap"})
    public MetaResultBean(String ID, String name,boolean significant, HashMap<String, String> contentMap) {
        this.ID = ID;
        this.name = name;
        this.significant = significant;

        this.contentMap = contentMap;
    }

    public MetaResultBean(String ID) {
        this.ID = ID;
        contentMap = new HashMap<>();
    }

    public String getID() {
        return ID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setValue(String property, String value) {
        contentMap.put(property, value);
    }

    public String getValue(String property) {
        return (contentMap.get(property));
    }

    public boolean isSignificant() {
        return significant;
    }

    public void setSignificant(boolean significant) {
        this.significant = significant;
    }
    
    
}
