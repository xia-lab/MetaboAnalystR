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
public class MetSetBean implements Serializable {

    private String name, members, geneMembers, metaboliteMembers,refs;

    public MetSetBean(String nm, String memb, String rfs) {
        name = nm;
        members = memb;
        refs = rfs;
    }
    @JsonCreator
    @ConstructorProperties({"name", "geneMembers", "metaboliteMembers", "refs"})
    public MetSetBean(String name, String geneMembers, String metaboliteMembers, String refs) {
        this.name = name;
        this.geneMembers = geneMembers;
        this.metaboliteMembers = metaboliteMembers;
        this.refs = refs;
    }

    public String getMembers() {
        return members;
    }

    public String getGeneMembers() {
        return geneMembers;
    }

    public void setGeneMembers(String geneMembers) {
        this.geneMembers = geneMembers;
    }

    public String getMetaboliteMembers() {
        return metaboliteMembers;
    }

    public void setMetaboliteMembers(String metaboliteMembers) {
        this.metaboliteMembers = metaboliteMembers;
    }

    public String getName() {
        return name;
    }

    public String getRefs() {
        return refs;
    }

}

