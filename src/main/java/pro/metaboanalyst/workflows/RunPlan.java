package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.Serializable;
import java.util.Objects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class RunPlan implements Serializable {
    private static final long serialVersionUID = 1L;

    private WorkflowParameters params; // may be null
    private String moduleName;
    private String origNaviType;

    // No-args for Jackson/JSF
    public RunPlan() {}

    public RunPlan(WorkflowParameters params, String moduleName, String origNaviType) {
        this.params = params;
        this.moduleName = moduleName;
        this.origNaviType = origNaviType;
    }

    public WorkflowParameters getParams() { return params; }
    public String getModuleName() { return moduleName; }
    public String getOrigNaviType() { return origNaviType; }

    public void setParams(WorkflowParameters params) { this.params = params; }
    public void setModuleName(String moduleName) { this.moduleName = moduleName; }
    public void setOrigNaviType(String origNaviType) { this.origNaviType = origNaviType; }

    public String comboKey() {
        return sanitizeName(params != null ? params.getFolderName() : "default")
             + "_" + sanitizeName(moduleName);
    }

    public String folderName() { return comboKey(); }

    private static String sanitizeName(String s) {
        if (s == null) return "NA";
        return s.trim().replaceAll("[^a-zA-Z0-9._-]", "_");
    }

    @Override public String toString() {
        return "RunPlan{params=" + (params!=null?params.getFolderName():"null")
             + ", moduleName='" + moduleName + '\''
             + ", origNaviType='" + origNaviType + '\'' + '}';
    }

    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof RunPlan)) return false;
        RunPlan rp = (RunPlan) o;
        return Objects.equals(moduleName, rp.moduleName)
            && Objects.equals(origNaviType, rp.origNaviType)
            && Objects.equals(params != null ? params.getFolderName() : null,
                              rp.params != null ? rp.params.getFolderName() : null);
    }

    @Override public int hashCode() {
        return Objects.hash(moduleName, origNaviType,
            params != null ? params.getFolderName() : null);
    }
}
