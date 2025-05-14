package pro.metaboanalyst.controllers.dose;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;

/**
 * DoseResultBean represents a dose-response analysis result. It stores
 * metadata, significance status, and a content map.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DoseResultBean implements Serializable {

    private final String ID; // Unique identifier for sorting (not a hyperlink)
    private String name;
    private boolean significant = false;
    private List<String> dummy;
    private HashMap<String, String> contentMap;

    @JsonCreator
    @ConstructorProperties({"ID", "name", "significant", "dummy", "contentMap"})
    public DoseResultBean(String ID, String name, boolean significant, List<String> dummy, HashMap<String, String> contentMap) {
        this.ID = ID;
        this.name = name;
        this.significant = significant;
        this.dummy = dummy;
        this.contentMap = (contentMap != null) ? contentMap : new HashMap<>();
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

    public boolean isSignificant() {
        return significant;
    }

    public void setSignificant(boolean significant) {
        this.significant = significant;
    }

    public List<String> getDummy() {
        return dummy;
    }

    public void setDummy(List<String> dummy) {
        this.dummy = dummy;
    }

    public HashMap<String, String> getContentMap() {
        return contentMap;
    }

    public void setContentMap(HashMap<String, String> contentMap) {
        this.contentMap = contentMap;
    }

    public void setValue(String property, String value) {
        contentMap.put(property, value);
    }

    public String getValue(String property) {
        return contentMap.get(property);
    }

    public DoseResultBean(String ID) {
        this.ID = ID;
        contentMap = new HashMap<>();
    }
}
