/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.beans.ConstructorProperties;
import java.io.Serializable;

/**
 *
 * @author zgy
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ConnectionState implements Serializable {

    private String sourceAnchor;
    private String targetAnchor;
    private String sourceTitle;
    private String targetTitle;

    @JsonCreator
    @ConstructorProperties({"sourceAnchor", "targetAnchor", "sourceTitle", "targetTitle"})    
    public ConnectionState(String sourceAnchor, String targetAnchor, String sourceTitle, String targetTitle) {

        this.sourceAnchor = sourceAnchor;
        this.targetAnchor = targetAnchor;
        this.sourceTitle = sourceTitle;
        this.targetTitle = targetTitle;
    }

    // Getters and setters
    public String getSourceAnchor() { return sourceAnchor; }
    public void setSourceAnchor(String sourceAnchor) { this.sourceAnchor = sourceAnchor; }
    public String getTargetAnchor() { return targetAnchor; }
    public void setTargetAnchor(String targetAnchor) { this.targetAnchor = targetAnchor; }
    public String getSourceTitle() { return sourceTitle; }
    public void setSourceTitle(String sourceTitle) { this.sourceTitle = sourceTitle; }
    public String getTargetTitle() { return targetTitle; }
    public void setTargetTitle(String targetTitle) { this.targetTitle = targetTitle; }
}
