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
public class CmpdBean implements Serializable {

    private String name;
    private String conc; //double save as String for display purpose
    private String hmdbID;
    private String refConcs;
    private String state; //H, M, L or NA
    private String detail;
    private boolean include = false;

    @JsonCreator
    @ConstructorProperties({"name", "conc", "hmdbID", "refConcs", "state", "detail"})
    public CmpdBean(String name, String conc, String hmdbID, String refConcs, String state, String detail) {
        this.name = name;
        this.conc = conc;
        this.hmdbID = hmdbID;
        this.refConcs = refConcs;
        this.state = state;
        if (state.equalsIgnoreCase("H") || state.equalsIgnoreCase("L")) {
            include = true;
        }
        this.detail = detail;
    }

    public boolean isInclude() {
        return include;
    }

    public void setInclude(boolean include) {
        this.include = include;
    }

    public String getConc() {
        return conc;
    }

    public String getName() {
        if (hmdbID.equals("--")) {
            return name;
        }
        return "<a href=\"http://www.hmdb.ca/metabolites/" + hmdbID + "\" target=\"_blank\">" + name + "</a>";
    }

    public String getNameOnly() {
        return name;
    }

    public String getHmdbID() {
        return "<a href=\"http://www.hmdb.ca/metabolites/" + hmdbID + "\">Go</a>";
    }

    public void setHmdbID(String hmdbID) {
        this.hmdbID = hmdbID;
    }

    public String getRefConcs() {
        return refConcs;
    }

    public String getState() {
        return state;
    }

    public String getDetail() {
        return detail;
    }
}
