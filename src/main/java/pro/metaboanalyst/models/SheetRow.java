/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;

import java.util.List;

/**
 *
 * @author zgy
 */
// If it's inside a bean class, make it public static.
// Or put it in its own file as a public class.
public class SheetRow implements java.io.Serializable {
    private static final long serialVersionUID = 1L;

    private String id;
    private java.util.List<String> cells;

    // REQUIRED by JSF state saving
    public SheetRow() { 
        this.id = ""; 
        this.cells = new java.util.ArrayList<>(); 
    }

    // Convenience ctor you use when building the model
    public SheetRow(String id, java.util.List<String> cells) {
        this.id = id;
        // ensure it’s modifiable for edits
        this.cells = (cells instanceof java.util.RandomAccess && cells.getClass().getName().contains("ArrayList"))
            ? cells
            : new java.util.ArrayList<>(cells);
    }

    // Getters/Setters required for state restore
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    public java.util.List<String> getCells() { return cells; }
    public void setCells(java.util.List<String> cells) { 
        this.cells = (cells == null) ? new java.util.ArrayList<>() : new java.util.ArrayList<>(cells); 
    }
}

