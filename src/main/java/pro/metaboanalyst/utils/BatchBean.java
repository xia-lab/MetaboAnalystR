/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.primefaces.model.StreamedContent;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;

/**
 *
 * @author pzq
 */
@RequestScoped
@Named("batchLoader")
public class BatchBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    private SessionBean1 sb;


    // Set data option
    private String dataOpt = "col";

    public String getDataOpt() {
        return dataOpt;
    }

    public void setDataOpt(String dataOpt) {
        this.dataOpt = dataOpt;
    }

    // Set data option
    private String dataCenter = "NULL";

    public String getDataCenter() {
        return dataCenter;
    }

    public void setDataCenter(String dataCenter) {
        this.dataCenter = dataCenter;
    }

    // Set method option
    private String methodOpt;

    public String getMethodOpt() {
        return methodOpt;
    }

    public void setMethodOpt(String methodOpt) {
        this.methodOpt = methodOpt;
    }

    private String dataOptT = "col";

    public String getDataOptT() {
        return dataOptT;
    }

    public void setDataOptT(String dataOptT) {
        this.dataOptT = dataOptT;
    }

    private String missingEstt = "lods";

    public String getMissingEstt() {
        return missingEstt;
    }

    public void setMissingEstt(String missingEstt) {
        this.missingEstt = missingEstt;
    }

    // Set data option
    private String dataCenterT = "NULL";

    public String getDataCenterT() {
        return dataCenterT;
    }

    public void setDataCenterT(String dataCenterT) {
        this.dataCenterT = dataCenterT;
    }

    // Set method option
    private String methodOptT = "default";

    public String getMethodOptT() {
        return methodOptT;
    }

    public void setMethodOptT(String methodOptT) {
        this.methodOptT = methodOptT;
    }

    // Set example data
    private boolean useBatchDataExample;

    public boolean isUseBatchDataExample() {
        return useBatchDataExample;
    }

    public void setUseBatchDataExample(boolean useBatchDataExample) {
        this.useBatchDataExample = useBatchDataExample;
    }

    private String bctestDataOpt = "bcgenericdata";

    public String getBctestDataOpt() {
        return bctestDataOpt;
    }

    public void setBctestDataOpt(String bctestDataOpt) {
        this.bctestDataOpt = bctestDataOpt;
    }

    // set uploaded file
    private UploadedFile batchFile;

    public UploadedFile getBatchFile() {
        return batchFile;
    }

    public void setBatchFile(UploadedFile batchFile) {
        this.batchFile = batchFile;
    }

    private UploadedFile batchTableFile;

    public UploadedFile getBatchTableFile() {
        return batchTableFile;
    }

    public void setBatchTableFile(UploadedFile batchTableFile) {
        this.batchTableFile = batchTableFile;
    }
    // General Design, After click Proceed, perform data upload and then do the correction

    // Here let me do the data uploading task and correction
    private String resultInfo = "";

    public String getResultInfo() {
        return resultInfo;
    }

    public void setResultInfo(String resultInfo) {
        this.resultInfo = resultInfo;
    }

    private String dataName = "";

    public String getDataName() {
        return dataName;
    }

    public void setDataName(String dataName) {
        this.dataName = dataName;
    }

    private boolean checkUserFile(UploadedFile usrFile, String limitMeth) {
        if (usrFile == null) {
            sb.addMessage("Error", "Please upload your file");
            return false;
        }

        if (usrFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return false;
        }

        if (!(usrFile.getFileName().endsWith(".csv") || usrFile.getFileName().endsWith(".txt"))) {
            sb.addMessage("Error", "Only comma separated format (*.csv) or tab delimited (.txt) will be accepted!");
            return false;
        }

        if ((usrFile.getSize() > ab.getMAX_UPLOAD_SIZE() && "EigenMS".equals(limitMeth))) {
            sb.addMessage("Error", "File is too large for EigenMS!");
            sb.addMessage("Error", "Please use MetaboAnalystR!");
        }

        if (usrFile.getSize() > ab.getMAX_UPLOAD_SIZE()) {
            sb.addMessage("Error", "File is too large!");
            sb.addMessage("Error", "Please use MetaboAnalystR for large data processing!");
            return false;
        }

        return true;
    }

    public void performBatchUpload() {

        if (!checkUserFile(batchFile, methodOpt)) {
            return;
        }

        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
            if (!ok) {
                return;
            }
        }
        RConnection RC = sb.getRConnection();

        String label = dataName;

        String fileName = DataUtils.uploadFile(sb, batchFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
        if (label.equalsIgnoreCase("")) {
            label = fileName.substring(0, fileName.length() - 4);
        }
        dataName = ""; // reset the name
        if (RDataUtils.readBatchDataBC(RC, fileName, dataOpt, label, missingEstt)) {
            String msg = "Data <u>" + fileName + "</u> was uploaded successfully!"
                    + " It is renamed to <u>" + label + "</u>. "
                    + "You can click the <b>Finish</b> button to process your data "
                    + " or click the <b>Upload</b> button to upload more data.";
            sb.addMessage("info", msg);
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Failed to read in the CSV file." + err);
        }
    }

    public void resetBatchData() {
        RDataUtils.resetBatchData(sb.getRConnection());
    }

    public String performBatchCorrection() {
        String[] names = RDataUtils.getBatchNames(sb.getRConnection());
        if (names.length < 2) {
            sb.addMessage("Error", "At least two batches are required!");
            return null;
        }

        RConnection RC = sb.getRConnection();

        if (RDataUtils.performBatchCorrection(sb.getRConnection(), sb.getNewImage("batch"), methodOpt, dataCenter)) {
            /*batchURL = "<b>You can download the corrected data </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                    + "/MetaboAnalyst_batch_data.csv\"><b>" + "here" + "</b></a>";*/
            return ("Batch View");
        } else {
            sb.addMessage("Error", "Failed to perform the procedure!");
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Your data is not eligible for this method!" + err);
            return null;
        }
    }

    public String performBatchtTask() {

        String fileName;

        if (!checkUserFile(batchTableFile, methodOptT)) {
            return null;
        }

        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
            if (!ok) {
                return null;
            }
        }

        RConnection RC = sb.getRConnection();

        // String label = dataName;
        fileName = DataUtils.uploadFile(sb, batchTableFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

        //if (label.equalsIgnoreCase("")) {
        //    label = fileName.substring(0, fileName.length() - 4);
        //}
        // dataName = ""; // reset the name
        if (RDataUtils.readBatchDataTB(RC, fileName, dataOptT, missingEstt)) {

            String msg = "Data <u>" + fileName + "</u> was uploaded successfully!";
            sb.addMessage("info", msg);

        } else {

            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Failed to read in the CSV file." + err);
            return null;
        }

        if (RDataUtils.performBatchCorrection(sb.getRConnection(), sb.getNewImage("batch"), methodOptT, dataCenterT)) {
            //batchURL = "<b>You can download the corrected data </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
            //        + "/MetaboAnalyst_batch_data.csv\"><b>" + "here" + "</b></a>";
            String msg = RDataUtils.getCurrentMsg(RC);
            sb.addMessage("Warn", msg);
            return ("Batch View");
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Your data is not eligible for this method!" + err);
            return null;
        }

    }

    public String performBatchtTestTask() {
        String fileName;
        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
             if (!ok) {
                return null;
            }
        }
        RConnection RC = sb.getRConnection();

        switch (bctestDataOpt) {
            case "bcgenericdata" -> {
                fileName = ab.getResourceByAPI("BC_generic_example.csv");
                sb.addMessage("info", "Use generic example data !");
            }
            case "bcQCrefdata" -> {
                fileName = ab.getResourceByAPI("BC_QC_example.csv");
                sb.addMessage("info", "Use QC example data !");
            }
            case "bcISrefdata" -> {
                fileName = ab.getResourceByAPI("BC_IS_example.csv");
                sb.addMessage("info", "Use IS example data !");
            }
            case "bcperfectrefdata" -> {
                fileName = ab.getResourceByAPI("BC_example.csv");
                sb.addMessage("info", "Use perfect example data !");
            }
            default -> {
                fileName = "";
                sb.addMessage("Error", "Something wrong with your browser!");
            }
        }
                                 
        if (RDataUtils.readBatchDataTB(RC, fileName, "col", missingEstt)) {

            String msg = "Example Data was uploaded successfully!";
            sb.addMessage("info", msg);

        } else {

            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "Failed to read in the CSV file." + err);

        }

        if (RDataUtils.performBatchCorrection(sb.getRConnection(), sb.getNewImage("batch"), methodOptT, dataCenterT)) {
            //batchURL = "<b>You can download the corrected data </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
            //        + "/MetaboAnalyst_batch_data.csv\"><b>" + "here" + "</b></a>";
            return ("Batch View");
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.addMessage("Error", "This Example data is not eligible for " + methodOptT + "!");
            sb.addMessage("Error", "Please choose an appropriate method or use default!" + err);

            return null;
        }

    }

    public String getBatchImg(String name) {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("batch") + name + ".png";
    }

    public StreamedContent getCorrectedFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/MetaboAnalyst_batch_data.csv");
    }
}
