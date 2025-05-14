/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package pro.metaboanalyst.models;
import java.io.Serializable;

/**
 *
 * @author jianguox
 */
public class ColorBean implements Serializable{
    
    public ColorBean(String nm){
        grpName = nm;
    }
    
    private String grpName;

    public String getGrpName() {
        return grpName;
    }

    public void setGrpName(String grpName) {
        this.grpName = grpName;
    }
    
    //color picker
    private String colorPopup;
 
    public String getColorPopup() {
        return colorPopup;
    }
 
    public void setColorPopup(String colorPopup) {
        this.colorPopup = colorPopup;
    }
    
    private int shapeType = 0; 

    public int getShapeType() {
        return shapeType;
    }

    public void setShapeType(int shapeType) {
        this.shapeType = shapeType;
    }
}
