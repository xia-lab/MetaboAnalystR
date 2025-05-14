/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

/**
 *
 * @author zgy
 */
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.beans.ConstructorProperties;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DiagramState {

    private List<ConnectionState> connectionStates;
    private String input;
    private Map<String, Object> additionalProperties = new HashMap<>();
    private List<String> targetNodes;

    @JsonAnySetter
    public void setAdditionalProperty(String key, Object value) {
        additionalProperties.put(key, value);
    }

    public Map<String, Object> getAdditionalProperties() {
        return additionalProperties;
    }

    // Constructor
    @JsonCreator
    @ConstructorProperties({"connectionStates", "input","targetNodes"})
    public DiagramState(List<ConnectionState> connectionStates, String input, List<String> targetNodes) {
        this.connectionStates = connectionStates;
        this.input = input;
        this.targetNodes = targetNodes;
    }

    // Getters and setters
    public List<ConnectionState> getConnectionStates() {
        return connectionStates;
    }

    public void setConnectionStates(List<ConnectionState> connectionStates) {
        this.connectionStates = connectionStates;
    }

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }

    public List<String> getTargetNodes() {
        return targetNodes;
    }

    public void setTargetNodes(List<String> targetNodes) {
        this.targetNodes = targetNodes;
    }

    @Override
    public String toString() {
        return "DiagramState{" + "connectionStates=" + connectionStates + ", input=" + input + ", additionalProperties=" + additionalProperties + ", targetNodes=" + targetNodes + '}';
    }
    
    
}
