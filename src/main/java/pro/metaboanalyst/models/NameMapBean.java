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
public class NameMapBean implements Serializable {

    private String query ="";
    private String hit="";
    private String symbol=""; //gene
    private String name=""; //gene
    private String ko=""; //kegg ortholog id
    private String hmdb_id="";
    private String kegg_id="";
    private String pubchem_id="";
    private String chebi_id="";
    private String metlin_id="";
    private String smiles="";
    private String details="";
    private boolean selected = false;

    public NameMapBean() {
    }
    @JsonCreator
    @ConstructorProperties({"query", "hit", "symbol", "name", "ko", "hmdb_id", "kegg_id", "pubchem_id", "chebi_id", "metlin_id", "smiles", "details", "selected"})
    public NameMapBean(String query, String hit, String symbol, String name, String ko, String hmdb_id, String kegg_id, String pubchem_id, String chebi_id, String metlin_id, String smiles, String details, boolean selected) {
        this.query = query;
        this.hit = hit;
        this.symbol = symbol;
        this.name = name;
        this.ko = ko;
        this.hmdb_id = hmdb_id;
        this.kegg_id = kegg_id;
        this.pubchem_id = pubchem_id;
        this.chebi_id = chebi_id;
        this.metlin_id = metlin_id;
        this.smiles = smiles;
        this.details = details;
        this.selected = selected;
    }

    // copy constructor
    public NameMapBean(NameMapBean c) {
        name = c.name;
        chebi_id = c.chebi_id;
        details = c.details;
        hit = c.hit;
        hmdb_id = c.hmdb_id;
        pubchem_id = c.pubchem_id;
        kegg_id = c.kegg_id;
        ko = c.ko;
        metlin_id  = c.metlin_id;
        query = c.query;
        selected = c.selected;
        symbol = c.symbol;
        smiles = c.smiles;
    }

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    public String getKo() {
        return ko;
    }
    
    public void setKo(String ko) {
        this.ko = ko;
    }

    public String getChebi_id() {
        return chebi_id;
    }

    public void setChebi_id(String chebi_id) {
        this.chebi_id = chebi_id;
    }

    public String getHmdb_id() {
        return hmdb_id;
    }

    public void setHmdb_id(String hmdb_id) {
        this.hmdb_id = hmdb_id;
    }

    public String getHit() {
        return hit;
    }

    public void setHit(String hit) {
        this.hit = hit;
    }

    public String getKegg_id() {
        return kegg_id;
    }

    public void setKegg_id(String kegg_id) {
        this.kegg_id = kegg_id;
    }

    public String getMetlin_id() {
        return metlin_id;
    }

    public void setMetlin_id(String metlin_id) {
        this.metlin_id = metlin_id;
    }

    public String getPubchem_id() {
        return pubchem_id;
    }

    public void setPubchem_id(String pubchem_id) {
        this.pubchem_id = pubchem_id;
    }

    public String getQuery() {
        return query;
    }

    public void setQuery(String query) {
        this.query = query;
    }

    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDetails() {
        return details;
    }
    
    public void setDetails(String details) {
        this.details = details;
    }

    public String getSmiles() {
        return smiles;
    }
    
    public void setSmiles(String smiles) {
        this.smiles = smiles;
    }
    
}
