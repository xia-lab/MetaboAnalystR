/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import java.io.Serializable;

/**
 *
 * @author zgy
 */
public class RunSummary implements Serializable {

    private static final long serialVersionUID = 1L;

    private String runId;
    private String normalityDescription;     // e.g., "p-value: 0.01" or "Shapiro-Wilk: W=0.95, p=0.05"
    private int numDEFeatures;              // e.g., 42
    private String pcaSeparationScore;      // e.g., "High separation" or a numeric measure
    private String paramDescription;        // e.g., "Log2 normalization, cutoff=0.05"
    private String pcaImage;        // e.g., "Log2 normalization, cutoff=0.05"
    private int numDEpval;              // e.g., 42
    private int numDEfc;              // e.g., 42

    public RunSummary() {
    }

    public RunSummary(String runId, String normalityDescription, int numDEFeatures,
            String pcaSeparationScore, String paramDescription,
            int numDEpval, int numDEfc) {
        this.runId = runId;
        this.normalityDescription = normalityDescription;
        this.numDEFeatures = numDEFeatures;
        this.pcaSeparationScore = pcaSeparationScore;
        this.paramDescription = paramDescription;
        this.numDEpval = numDEpval;
        this.numDEfc = numDEfc;
    }

    public String getRunId() {
        return runId;
    }

    public void setRunId(String runId) {
        this.runId = runId;
    }

    public String getNormalityDescription() {
        return normalityDescription;
    }

    public void setNormalityDescription(String normalityDescription) {
        this.normalityDescription = normalityDescription;
    }

    public int getNumDEFeatures() {
        return numDEFeatures;
    }

    public void setNumDEFeatures(int numDEFeatures) {
        this.numDEFeatures = numDEFeatures;
    }

    public String getPcaSeparationScore() {
        return pcaSeparationScore;
    }

    public void setPcaSeparationScore(String pcaSeparationScore) {
        this.pcaSeparationScore = pcaSeparationScore;
    }

    public String getParamDescription() {
        return paramDescription;
    }

    public void setParamDescription(String paramDescription) {
        this.paramDescription = paramDescription;
    }

    public String getPcaImage() {
        return pcaImage;
    }

    public void setPcaImage(String pcaImage) {
        this.pcaImage = pcaImage;
    }

    public int getNumDEpval() {
        return numDEpval;
    }

    public void setNumDEpval(int numDEpval) {
        this.numDEpval = numDEpval;
    }

    public int getNumDEfc() {
        return numDEfc;
    }

    public void setNumDEfc(int numDEfc) {
        this.numDEfc = numDEfc;
    }
}
