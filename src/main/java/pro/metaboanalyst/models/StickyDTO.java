/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;
import java.util.UUID;

/**
 *
 * @author zgy
 */
public class StickyDTO implements java.io.Serializable {
    private UUID selectedDatasetId;    

    public UUID getSelectedDatasetId() {
        return selectedDatasetId;
    }

    public void setSelectedDatasetId(UUID selectedDatasetId) {
        this.selectedDatasetId = selectedDatasetId;
    }
    
}