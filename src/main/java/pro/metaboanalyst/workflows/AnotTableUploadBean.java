/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.workflows;

import pro.metaboanalyst.controllers.general.*;

import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.rwrappers.RDataUtils;
import jakarta.inject.Named;
import java.io.*;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 * @author zgy
 */
@ViewScoped
@Named("wfAnotTableUploadBean")
public class AnotTableUploadBean implements Serializable {

    @Inject
    private WorkflowBean wb;
    @Inject
    private SessionBean1 sb;
    @Inject
    private ApplicationBean1 ab;

    @Inject
    private ProcessBean pcb;
    
    private String uploadInfo = "";
    private boolean fileUploaded = false;
    private boolean containMeta;

    private String dataType = "conc";
    private String dataFormat = "colmf";
    private String cmpdIDType;
    private String featType;

    public String getClsOpt() {
        return clsOpt;
    }

    public void setClsOpt(String clsOpt) {
        this.clsOpt = clsOpt;
    }

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    public String getCmpdIDType() {
        return cmpdIDType;
    }

    public void setCmpdIDType(String cmpdIDType) {
        this.cmpdIDType = cmpdIDType;
    }

    public String getFeatType() {
        return featType;
    }

    public void setFeatType(String featType) {
        this.featType = featType;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    public String getUploadInfo() {
        return uploadInfo;
    }

    public void setUploadInfo(String uploadInfo) {
        this.uploadInfo = uploadInfo;
    }

    public boolean isFileUploaded() {
        return fileUploaded;
    }

    public void setFileUploaded(boolean fileUploaded) {
        this.fileUploaded = fileUploaded;
    }

    public boolean isContainMeta() {
        return containMeta;
    }

    public void setContainMeta(boolean containMeta) {
        this.containMeta = containMeta;
    }

    private String clsOpt = "disc";
    private UploadedFile csvFile;

    public String handleAnotTableUpload() {

        if (sb.getCmpdIDType().equalsIgnoreCase("na")) {
            sb.addMessage("Error", "Please specify the ID type for your data input!");
            return null;
        }

        if (!sb.doLogin("conc", "pathqea", clsOpt.equals("cont"), false)) {
            sb.addMessage("Error", "Failed to log in!");
            return null;
        }
        try {

            if (csvFile == null) {
                sb.addMessage("Error", "Please upload your file!");
                return null;
            }

            if (csvFile.getSize() == 0) {
                sb.addMessage("Error", "File is empty!");
                return null;
            }

            String fileName = DataUtils.getJustFileName(csvFile.getFileName());
            DataUtils.uploadFile(sb, csvFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            sb.setDataUploaded();
            //sb.setCmpdIDType(qeaCmpdIDType);
            wb.setDataName(fileName);
            wb.setDataFormat(dataFormat);
            wb.setLblType(clsOpt);
            processAnotData(fileName, dataFormat, clsOpt);
            return null;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    private String processAnotData(String fileName, String dataFormat, String lblType) {
        RConnection RC = sb.getRConnection();
        if (RDataUtils.readTextData(RC, fileName, dataFormat, lblType)) {
            fileUploaded = true;
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            return "null";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Failed to read in the CSV file." + err);
            return null;
        }
    }

    public String anotTableExampleBn_action() {
        if (!sb.doLogin("conc", "pathqea", false, false)) {
            sb.addMessage("Error", "Failed to log in!");
            return null;
        }
        sb.setDataUploaded();
        sb.setCmpdIDType("name");
        wb.setDataName(ab.getResourceByAPI("human_cachexia.csv"));
        wb.setDataFormat("rowu");
        wb.setLblType("disc");
        return processAnotData(ab.getResourceByAPI("human_cachexia.csv"), "rowu", "disc");
    }

}
