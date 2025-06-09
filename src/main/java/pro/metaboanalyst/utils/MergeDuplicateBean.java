/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.model.StreamedContent;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author qiang
 */
@RequestScoped
@Named("dpEstimator")
public class MergeDuplicateBean {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    private String mergeType = "dup_smpl";

    public String getMergeType() {
        return mergeType;
    }

    public void setMergeType(String mergeType) {
        this.mergeType = mergeType;
    }

    // Class Function
    public String PerformMerging() {
        sb.setJobDone(false);

        if (duplicateTable == null) {
            sb.addMessage("Error", "Please upload your file");
            return null;
        }

        if (duplicateTable.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }

        if (duplicateTable.getSize() > ab.getMAX_UPLOAD_SIZE()) {
            sb.addMessage("Error", "File is too large!");
            return null;
        }

        if (!(duplicateTable.getFileName().endsWith(".csv") || duplicateTable.getFileName().endsWith(".txt"))) {
            sb.addMessage("Error", "Only comma separated format (*.csv) or tab delimited (.txt) will be accepted!");
            return null;
        }

        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
            if (!ok) {
                return null;
            }
        }

        RConnection RC = sb.getRConnection();

        String fileName = DataUtils.uploadFile(sb, duplicateTable, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

        if (RDataUtils.readDPDataTB(RC, fileName, sampleCol)) {
            String msg = "Data <u>" + fileName + "</u> was uploaded successfully!";
            sb.addMessage("info", msg);
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Failed to read in the CSV file." + err);
            return null;
        }

        //resCode = RDataUtils.performDuplicateEstimation(sb.getRConnection(), sampleCol, estiMethod, smoother);
        if (mergeType.equals("dup_smpl")) {
            resCode = RDataUtils.performDuplicateEstimation(sb.getRConnection(), sampleCol, estiMethod, smoother);
        } else {
            resCode = RDataUtils.mergeFeatureDuplicates(sb.getRConnection(), sampleCol, estiMethod);
        }
        
        if (resCode == 1) {
            sb.setJobDone(true);
            sb.addMessage("info", "You can now download the merged data file.");
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Some error occured during analysis: " + err);
        }
        return null;
    }

    public String PerformDuplicateETDemo() {
        sb.setJobDone(false);
        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
            if (!ok) {
                return null;
            }
        }

        RConnection RC = sb.getRConnection();

        String fileName = ab.getResourceByAPI("Malaria_HILIC_duplicates.csv");

        if (RDataUtils.readDPDataTB(RC, fileName, sampleCol)) {
            String msg = "Example Data have been estimated successfully!";
            sb.addMessage("info", msg);
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Failed to read in the CSV file." + err);
            return null;
        }

        resCode = RDataUtils.performDuplicateEstimation(sb.getRConnection(), "col", estiMethod, smoother);
        if (resCode == 1) {
            sb.setJobDone(true);
        }
        return null;
    }

    public StreamedContent getDupMergeFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/MetaboAnalyst_deduplicated_data.csv");
    }

    // Class vairable definiation
    private UploadedFile duplicateTable;
    private boolean smoother = false;
    private String sampleCol = "col";
    private String estiMethod = "mean";
    private boolean exampleDemo = true;
    private int resCode = 0;

    public UploadedFile getDuplicateTable() {
        return duplicateTable;
    }

    public void setDuplicateTable(UploadedFile duplicateTable) {
        this.duplicateTable = duplicateTable;
    }

    public boolean isSmoother() {
        return smoother;
    }

    public void setSmoother(boolean smoother) {
        this.smoother = smoother;
    }

    public String getSampleCol() {
        return sampleCol;
    }

    public void setSampleCol(String sampleCol) {
        this.sampleCol = sampleCol;
    }

    public String getEstiMethod() {
        return estiMethod;
    }

    public void setEstiMethod(String estiMethod) {
        this.estiMethod = estiMethod;
    }

    public boolean isExampleDemo() {
        return exampleDemo;
    }

    public void setExampleDemo(boolean exampleDemo) {
        this.exampleDemo = exampleDemo;
    }

}
