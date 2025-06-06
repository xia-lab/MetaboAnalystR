package pro.metaboanalyst.lts;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.beans.ConstructorProperties;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@JsonIgnoreProperties(ignoreUnknown = true)
public class FunctionInfo {

    private String name;
    private String function;

    private Object description;
    private Map<String, Object> parameters = new HashMap<>();
    private List<String> rCommands = new ArrayList<>();

    @JsonCreator
    @ConstructorProperties({"name", "function", "description"})
    public FunctionInfo(String name, String function, Object description) {
        this.name = name;
        this.function = function;
        this.description = description;
    }

    public void addParameter(String key, Object value) {
        parameters.put(key, value);
    }

    public void addRCommand(String value) {
        rCommands.add(value);
    }

    // Getters for serialization
    public String getName() {
        return name;
    }

    public Object getDescription() {
        return description;
    }

    public String getFunction() {
        return function;
    }

    public void setFunction(String function) {
        this.function = function;
    }

    public Map<String, Object> getParameters() {
        return parameters;
    }

    public List<String> getrCommands() {
        return rCommands;
    }

    public void setrCommands(List<String> rCommands) {
        this.rCommands = rCommands;
    }

}
