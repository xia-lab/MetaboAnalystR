/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.beans.ConstructorProperties;
import java.util.Objects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkflowParameters {

    private String rowNormOpt;
    private String transNormOpt;
    private String scaleNormOpt;

    private boolean specNormSpecifed = false;
    private String refSmpl = null;
    private String refGrp = null;
    private String refVar = null;

    // New fields for additional parameters
    private boolean doQCFiltering;
    private int qcCutoff;
    private String varFilterOpt;
    private int filterCutoff;
    private String intFilterOpt;
    private int intFilterCutoff;

    private boolean removeMissing;
    private String missingImputeOpt;
    private String replaceVarOpt;
    private String imputeAlgOpt;

    private String folderName;
    private String detailText;

    @JsonCreator
    @ConstructorProperties({
        "removeMissing", "missingImputeOpt", "replaceVarOpt", "imputeAlgOpt",
        "doQCFiltering", "qcCutoff", "varFilterOpt", "filterCutoff",
        "intFilterOpt", "intFilterCutoff",
        "rowNormOpt", "transNormOpt", "scaleNormOpt",
        "folderName"
    })
    public WorkflowParameters(
            boolean removeMissing, String missingImputeOpt, String replaceVarOpt, String imputeAlgOpt,
            boolean doQCFiltering, int qcCutoff, String varFilterOpt, int filterCutoff,
            String intFilterOpt, int intFilterCutoff,
            String rowNormOpt, String transNormOpt, String scaleNormOpt,
            String folderName) {

        this.removeMissing = removeMissing;
        this.missingImputeOpt = missingImputeOpt;
        this.replaceVarOpt = replaceVarOpt;
        this.imputeAlgOpt = imputeAlgOpt;
        this.doQCFiltering = doQCFiltering;
        this.qcCutoff = qcCutoff;
        this.varFilterOpt = varFilterOpt;
        this.filterCutoff = filterCutoff;
        this.intFilterOpt = intFilterOpt;
        this.intFilterCutoff = intFilterCutoff;
        this.rowNormOpt = rowNormOpt;
        this.transNormOpt = transNormOpt;
        this.scaleNormOpt = scaleNormOpt;
        this.folderName = folderName;

    }

    public String getFolderName() {
        return folderName;
    }

    public void setFolderName(String folderName) {
        this.folderName = folderName;
    }

    // Getters and Setters
    public String getRowNormOpt() {
        return rowNormOpt;
    }

    public void setRowNormOpt(String rowNormOpt) {
        this.rowNormOpt = rowNormOpt;
    }

    public String getTransNormOpt() {
        return transNormOpt;
    }

    public void setTransNormOpt(String transNormOpt) {
        this.transNormOpt = transNormOpt;
    }

    public String getScaleNormOpt() {
        return scaleNormOpt;
    }

    public void setScaleNormOpt(String scaleNormOpt) {
        this.scaleNormOpt = scaleNormOpt;
    }

    public boolean isDoQCFiltering() {
        return doQCFiltering;
    }

    public void setDoQCFiltering(boolean doQCFiltering) {
        this.doQCFiltering = doQCFiltering;
    }

    public int getQcCutoff() {
        return qcCutoff;
    }

    public void setQcCutoff(int qcCutoff) {
        this.qcCutoff = qcCutoff;
    }

    public String getVarFilterOpt() {
        return varFilterOpt;
    }

    public void setVarFilterOpt(String varFilterOpt) {
        this.varFilterOpt = varFilterOpt;
    }

    public int getFilterCutoff() {
        return filterCutoff;
    }

    public void setFilterCutoff(int filterCutoff) {
        this.filterCutoff = filterCutoff;
    }

    public String getIntFilterOpt() {
        return intFilterOpt;
    }

    public void setIntFilterOpt(String intFilterOpt) {
        this.intFilterOpt = intFilterOpt;
    }

    public int getIntFilterCutoff() {
        return intFilterCutoff;
    }

    public void setIntFilterCutoff(int intFilterCutoff) {
        this.intFilterCutoff = intFilterCutoff;
    }

    public boolean isRemoveMissing() {
        return removeMissing;
    }

    public void setRemoveMissing(boolean removeMissing) {
        this.removeMissing = removeMissing;
    }

    public String getMissingImputeOpt() {
        return missingImputeOpt;
    }

    public void setMissingImputeOpt(String missingImputeOpt) {
        this.missingImputeOpt = missingImputeOpt;
    }

    public String getReplaceVarOpt() {
        return replaceVarOpt;
    }

    public void setReplaceVarOpt(String replaceVarOpt) {
        this.replaceVarOpt = replaceVarOpt;
    }

    public String getImputeAlgOpt() {
        return imputeAlgOpt;
    }

    public void setImputeAlgOpt(String imputeAlgOpt) {
        this.imputeAlgOpt = imputeAlgOpt;
    }

    public String getDetailText() {
        StringBuilder details = new StringBuilder();
        details.append("Remove Missing: ").append(removeMissing ? "Yes" : "No");
        details.append(" | Missing Impute: ").append(missingImputeOpt != null ? missingImputeOpt : "None");
        details.append(" | Replace Variance: ").append(replaceVarOpt != null ? replaceVarOpt : "None");
        details.append(" | Imputation Algorithm: ").append(imputeAlgOpt != null ? imputeAlgOpt : "None");
        details.append(" | QC Filtering: ").append(doQCFiltering ? "Enabled" : "Disabled");
        details.append(" | QC Cutoff: ").append(qcCutoff);
        details.append(" | Variance Filter: ").append(varFilterOpt != null ? varFilterOpt : "None");
        details.append(" | Variance Cutoff: ").append(filterCutoff);
        details.append(" | Intensity Filter: ").append(intFilterOpt != null ? intFilterOpt : "None");
        details.append(" | Intensity Cutoff: ").append(intFilterCutoff);
        details.append(" | Row Normalization: ").append(rowNormOpt != null ? rowNormOpt : "None");
        details.append(" | Transformation: ").append(transNormOpt != null ? transNormOpt : "None");
        details.append(" | Scaling: ").append(scaleNormOpt != null ? scaleNormOpt : "None");

        if ("SpecNorm".equals(rowNormOpt)) {
            details.append(" | SpecNorm Specified: ").append(specNormSpecifed ? "Yes" : "No");
        } else if ("CompNorm".equals(rowNormOpt)) {
            details.append(" | Reference Feature: ").append(refVar != null ? refVar : "None");
        } else if ("GroupPQN".equals(rowNormOpt)) {
            details.append(" | Reference Group: ").append(refGrp != null ? refGrp : "None");
        } else if ("SamplePQN".equals(rowNormOpt)) {
            details.append(" | Reference Sample: ").append(refSmpl != null ? refSmpl : "None");
        }

        return details.toString();
    }

    public String getDetailTextHtml() {
        StringBuilder details = new StringBuilder("<ul>");
        details.append("<li><strong>Remove Missing:</strong> ").append(removeMissing ? "Yes" : "No").append("</li>");
        details.append("<li><strong>Missing Impute:</strong> ").append(missingImputeOpt != null ? missingImputeOpt : "None").append("</li>");
        details.append("<li><strong>Replace Variance:</strong> ").append(replaceVarOpt != null ? replaceVarOpt : "None").append("</li>");
        details.append("<li><strong>Imputation Algorithm:</strong> ").append(imputeAlgOpt != null ? imputeAlgOpt : "None").append("</li>");
        details.append("<li><strong>QC Filtering:</strong> ").append(doQCFiltering ? "Enabled" : "Disabled").append("</li>");
        details.append("<li><strong>QC Cutoff:</strong> ").append(qcCutoff).append("</li>");
        details.append("<li><strong>Variance Filter:</strong> ").append(varFilterOpt != null ? varFilterOpt : "None").append("</li>");
        details.append("<li><strong>Variance Cutoff:</strong> ").append(filterCutoff).append("</li>");
        details.append("<li><strong>Intensity Filter:</strong> ").append(intFilterOpt != null ? intFilterOpt : "None").append("</li>");
        details.append("<li><strong>Intensity Cutoff:</strong> ").append(intFilterCutoff).append("</li>");
        details.append("<li><strong>Row Normalization:</strong> ").append(rowNormOpt != null ? rowNormOpt : "None").append("</li>");
        details.append("<li><strong>Transformation:</strong> ").append(transNormOpt != null ? transNormOpt : "None").append("</li>");
        details.append("<li><strong>Scaling:</strong> ").append(scaleNormOpt != null ? scaleNormOpt : "None").append("</li>");

        if ("SpecNorm".equals(rowNormOpt)) {
            details.append("<li><strong>SpecNorm Specified:</strong> ").append(specNormSpecifed ? "Yes" : "No").append("</li>");
        } else if ("CompNorm".equals(rowNormOpt)) {
            details.append("<li><strong>Reference Feature:</strong> ").append(refVar != null ? refVar : "None").append("</li>");
        } else if ("GroupPQN".equals(rowNormOpt)) {
            details.append("<li><strong>Reference Group:</strong> ").append(refGrp != null ? refGrp : "None").append("</li>");
        } else if ("SamplePQN".equals(rowNormOpt)) {
            details.append("<li><strong>Reference Sample:</strong> ").append(refSmpl != null ? refSmpl : "None").append("</li>");
        }

        details.append("</ul>");
        return details.toString();
    }

    public void setDetailText(String detailText) {
        this.detailText = detailText;
    }

    // Override equals and hashCode for uniqueness
    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        WorkflowParameters that = (WorkflowParameters) o;
        return doQCFiltering == that.doQCFiltering
                && qcCutoff == that.qcCutoff
                && filterCutoff == that.filterCutoff
                && intFilterCutoff == that.intFilterCutoff
                && removeMissing == that.removeMissing
                && Objects.equals(rowNormOpt, that.rowNormOpt)
                && Objects.equals(transNormOpt, that.transNormOpt)
                && Objects.equals(scaleNormOpt, that.scaleNormOpt)
                && Objects.equals(specNormSpecifed, that.specNormSpecifed)
                && Objects.equals(refSmpl, that.refSmpl)
                && Objects.equals(refGrp, that.refGrp)
                && Objects.equals(refVar, that.refVar)
                && Objects.equals(varFilterOpt, that.varFilterOpt)
                && Objects.equals(intFilterOpt, that.intFilterOpt)
                && Objects.equals(missingImputeOpt, that.missingImputeOpt)
                && Objects.equals(replaceVarOpt, that.replaceVarOpt)
                && Objects.equals(imputeAlgOpt, that.imputeAlgOpt);
    }

    @Override
    public int hashCode() {
        return Objects.hash(rowNormOpt, transNormOpt, scaleNormOpt, doQCFiltering, qcCutoff,
                varFilterOpt, filterCutoff, intFilterOpt, intFilterCutoff,
                removeMissing, missingImputeOpt, replaceVarOpt, imputeAlgOpt);
    }

    public boolean isSpecNormSpecifed() {
        return specNormSpecifed;
    }

    public void setSpecNormSpecifed(boolean specNormSpecifed) {
        this.specNormSpecifed = specNormSpecifed;
    }

    public String getRefSmpl() {
        return refSmpl;
    }

    public void setRefSmpl(String refSmpl) {
        this.refSmpl = refSmpl;
    }

    public String getRefGrp() {
        return refGrp;
    }

    public void setRefGrp(String refGrp) {
        this.refGrp = refGrp;
    }

    public String getRefVar() {
        return refVar;
    }

    public void setRefVar(String refVar) {
        this.refVar = refVar;
    }
}
