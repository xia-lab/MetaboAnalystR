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
import pro.metaboanalyst.rwrappers.SearchUtils;

/**
 * @author zgy
 */
@ViewScoped
@Named("wfListUploadBean")
public class ListUploadBean implements Serializable {

    @Inject
    private SessionBean1 sb;
    @Inject
    WorkflowBean wb;

    private String uploadInfo = "";
    private boolean fileUploaded = false;
    private String fileType;
    private String exampleType = "none";
    private boolean containMeta;

    private String dataType = "conc";
    private String dataFormat = "colmf";
    private String oraList;
    private String featType;

    public String getExampleType() {
        return exampleType;
    }

    public void setExampleType(String exampleType) {
        this.exampleType = exampleType;
    }

    public String getFeatType() {
        return featType;
    }

    public void setFeatType(String featType) {
        this.featType = featType;
    }

    public String getOraList() {
        return oraList;
    }

    public void setOraList(String oraList) {
        this.oraList = oraList;
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

    public String getFileType() {
        return fileType;
    }

    public void setFileType(String fileType) {
        this.fileType = fileType;
    }

    public boolean isContainMeta() {
        return containMeta;
    }

    public void setContainMeta(boolean containMeta) {
        this.containMeta = containMeta;
    }

    public void updateListArea() {
        switch (exampleType) {
            case "met" -> {
                sb.setCmpdIDType("name");
                featType = "met";
                oraList = "Acetoacetic acid\nBeta-Alanine\nCreatine\nDimethylglycine\nFumaric acid\nGlycine\nHomocysteine\nL-Cysteine\n"
                        + "L-Isolucine\nL-Phenylalanine\nL-Serine\nL-Threonine\nL-Tyrosine\nL-Valine\nPhenylpyruvic acid\nPropionic acid\nPyruvic acid\nSarcosine";
            }
            case "lipid" -> {
                sb.setCmpdIDType("name");
                featType = "lipid";
                oraList = "CerP(d18:1/26:1)\nDG(18:0/15:0)\nDG(18:2/19:0)\nLysoPC(10:0)\nLysoPC(17:0)\nLysoPE(22:2)\nPA(18:1/18:0)\nPA(18:1/21:0)\n"
                        + "PA(20:4/20:0)\nPA(22:2/24:0)\nPA(22:6/18:1)\nPC(20:5/18:2)\nPC(P-18:0/18:1)\nPE(18:1/22:1)\nPE(18:2/16:0)\nPE(18:2/21:0)\n"
                        + "PE(18:2/22:1)\nPE(20:2/18:2)\nPE(20:3/20:2)\nPE(20:3/22:0)\nPE(20:4/18:0)\nPE(20:4/20:0)\nPE(P-16:0/18:0)\nPE(P-18:0/13:0)\n"
                        + "PE(P-18:0/17:0)\nPE(P-18:0/20:4)\nPE(P-18:0/20:5)\nPE(P-18:0/22:1)\nPE(P-20:0/22:6)\nPG(18:0/16:0)\nPG(18:1/18:0)\nPG(22:6/20:1)\n"
                        + "PI(18:2/18:1)\nPI(22:2/16:0)\nPS(18:0/21:0)\nPS(18:1/20:3)\nPS(18:1/22:0)\nPS(18:1/24:1)\nPS(18:2/22:1)\nPS(20:1/18:0)\nPS(20:3/21:0)\n"
                        + "PS(22:6/17:2)\nPS(22:6/18:0)\nSQDG(18:0/12:0)";
            }
            default -> {
                featType = "none";
                oraList = "";
            }
        }
        sb.setFeatType(featType);

    }

    public void handleListUpload() {
        if (!sb.doLogin("conc", "msetora", false, false)) {
            sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return;
        }

        if (oraList == null || oraList.trim().length() == 0) {
            sb.addMessage("Error", "Error: the input is empty!");
            return;
        }

        if (featType.equals("none")) {
            sb.addMessage("Error", "Please specify Feature Type!");
            return;
        }

        String[] qVec = DataUtils.getQueryNames(oraList, null);
        RDataUtils.setMapData(sb.getRConnection(), qVec);
        if (featType.equals("lipid")) {
            SearchUtils.crossReferenceExactLipid(sb.getRConnection(), sb.getCmpdIDType());
        } else {
            SearchUtils.crossReferenceExact(sb.getRConnection(), sb.getCmpdIDType());
        }

        sb.setDataUploaded();
        wb.setOraList(oraList);
        sb.setFeatType(featType);
        fileUploaded = true;
        sb.addMessage("info", "List has been uploaded and mapped!");

    }

}
