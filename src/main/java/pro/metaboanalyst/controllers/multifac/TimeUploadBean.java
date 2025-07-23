/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;

import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author jianguox
 */
@RequestScoped
@Named("tsuploader")
public class TimeUploadBean implements Serializable {

    private static final Logger LOGGER = LogManager.getLogger(TimeUploadBean.class);
    @Inject
    WorkflowBean wb;
    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;
    @Inject
    MultifacBean tb;
    private SelectItem[] MetaboloneChemIDOpts;
    private String defaultchemid;
    private UploadedFile csvFile;
    private UploadedFile metaFile;
    private String tsDataType = "conc";
    private String tsFormat = "colmf";
    private String timeDataOpt = "pkcovid";
    /*
     * Handle Metabolone XLSX datasheet
     */
    private UploadedFile metabolonFile;

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    public UploadedFile getMetaFile() {
        return metaFile;
    }

    public void setMetaFile(UploadedFile metaFile) {
        this.metaFile = metaFile;
    }

    public String getTsDataType() {
        return tsDataType;
    }

    public void setTsDataType(String tsDataType) {
        this.tsDataType = tsDataType;
    }

    public String getTsFormat() {
        return tsFormat;
    }

    public void setTsFormat(String tsFormat) {
        this.tsFormat = tsFormat;
    }

    public String getTimeDataOpt() {
        return timeDataOpt;
    }

    public void setTimeDataOpt(String timeDataOpt) {
        this.timeDataOpt = timeDataOpt;
    }

    public String handleTsDataUpload() {

        if (csvFile == null || metaFile == null) {
            sb.addMessage("Error", "You must provide both data and metadata files in order to proceed!");
            return null;
        }
        if (csvFile.getSize() == 0 || metaFile.getSize() == 0) {
            sb.addMessage("Error", "Make sure both files are not empty!");
            return null;
        }
        if (sb.doLogin(tsDataType, "mf", false, false)) {
            try {
                String tsDesign = sb.getTsDesign();
                RConnection RC = sb.getRConnection();
                RDataUtils.setDesignType(RC, tsDesign);
                if (tsDesign.equals("time0") || tsDesign.equals("time")) {
                    tb.setDisableMetaSelection(true);
                }
                String fileName = DataUtils.uploadFile(sb, csvFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                if (fileName == null) {
                    return null;
                }
                if (RDataUtils.readTextDataTs(RC, fileName, tsFormat)) {
                    String metaName = DataUtils.uploadFile(sb, metaFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                    if (metaName == null) {
                        return null;
                    }
                    boolean res = RDataUtils.readMetaData(RC, metaName);
                    if (!res) {
                        String err = RDataUtils.getErrMsg(RC);
                        sb.addMessage("Error", "Failed meta-data integrity check." + err);
                        return null;
                    }
                    sb.setDataUploaded();
                    tb.reinitVariables();
                    return "Data check";
                } else {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    return null;
                }
            } catch (Exception e) {
                //e.printStackTrace();
                LOGGER.error("handleTsDataUpload", e);
            }
        }
        sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }

    //note "mf" here indicate for metadata table module
    //whether it is time or not is based on tsDesign
    public String handleTestDataUpload() {
        String fileName;
        String testMetaFile;
        String tsDesign;
        switch (timeDataOpt) {
            case "pkcovid" -> {
                tsDataType = "pktable";
                tsDesign = "multi";
                tsFormat = "colmf";//colts
                fileName = ab.getResourceByAPI("covid_metabolomics_data.csv");
                testMetaFile = ab.getResourceByAPI("covid_metadata_multiclass.csv");
            }
            case "tce" -> {
                tsDataType = "pktable";
                tsDesign = "multi";
                tsFormat = "colmf";//colts
                fileName = ab.getResourceByAPI("TCE_feature_table.csv");
                testMetaFile = ab.getResourceByAPI("TCE_metadata.csv");
            }
            case "diabetes" -> {
                tsDataType = "pktable";
                tsDesign = "multi";
                tsFormat = "rowmf";
                fileName = ab.getResourceByAPI("diabetes_lipids.txt");
                testMetaFile = ab.getResourceByAPI("diabetes_metadata.csv");
            }
            case "time2" -> {
                tsDataType = "pktable";
                tsDesign = "time";  //indicate whether this is actual time series
                tsFormat = "colu"; // not ts here is for metadata module
                fileName = ab.getResourceByAPI("cress_time.csv");
                testMetaFile = ab.getResourceByAPI("cress_time_meta.csv");
            }
            case "ewaste" -> {
                tsDataType = "pktable";
                tsDesign = "multi";
                tsFormat = "colmf";
                boolean useQC = true;
                if (ab.isOnZgyPc() && !useQC) {
                    fileName = ab.getResourceByAPI("ewaste_data.csv");
                } else {
                    fileName = ab.getResourceByAPI("ewaste_data_QC.csv");
                }
                testMetaFile = ab.getResourceByAPI("ewaste_metadata.csv");
            }
            default -> {
                tsDataType = "pktable";
                tsDesign = "time0";
                tsFormat = "rowmf";
                fileName = ab.getResourceByAPI("cress_time1.csv");
                testMetaFile = ab.getResourceByAPI("cress_time1_meta.csv");
            }
        }
        if (!wb.isReloadingWorkflow()) {

            if (!sb.doLogin(tsDataType, "mf", false, false)) {
                sb.addMessage("Error", "Analysis start failed!");
                return null;
            }
        }
        try {
            RConnection RC = sb.getRConnection();
            sb.setTsDesign(tsDesign);
            RDataUtils.setDesignType(RC, tsDesign);
            if (RDataUtils.readTextDataTs(RC, fileName, tsFormat)) {
                sb.setDataUploaded();
                boolean res = RDataUtils.readMetaData(RC, testMetaFile);
                if (!res) {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    return null;
                }
                //System.out.println("var");
                tb.reinitVariables();
                return "Data check";
            } else {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } catch (Exception e) {
            // e.printStackTrace();
            LOGGER.error("handleTestDataUpload", e);
        }

        sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }

    public UploadedFile getMetabolonFile() {
        return metabolonFile;
    }

    public void setMetabolonFile(UploadedFile metabolonFile) {
        this.metabolonFile = metabolonFile;
    }

    public String handleMetabolonData(String analType) {

        if (metabolonFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty!");
            return null;
        }

        if (sb.doLogin("pktable", analType, false, false)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadXLSXFile(sb, metabolonFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

            if (RDataUtils.validateMetabolon(RC, fileName)) {
                String[] metaFactors, compoundIDs;

                metaFactors = RDataUtils.getMetabolonMetaFactors(RC, fileName);
                compoundIDs = RDataUtils.getMetabolonCMPDIDs(RC, fileName);
                if (metaFactors.length == 0) {
                    sb.addMessage("Error", "'Sample Meta Data' sheet does not contains metadata information more than 2 categories!");
                    return null;
                }

                if (metaFactors.length == 1) {
                    sb.addMessage("Error", "'Sample Meta Data' only contains one metadata factor, please use 'statistical analysis [one-factor]' module!");
                    return null;
                }

                if (compoundIDs.length == 0) {
                    sb.addMessage("warn", "'Chemical Annotation' sheet does not contain supportted ID! Will use default feature label.");
                }

                SelectItem[] CMPDIDsSelect = new SelectItem[compoundIDs.length - 1];

                int ii = 0;
                for (String compoundID : compoundIDs) {
                    if (compoundID.equals("CHEMICAL_NAME")) {
                        continue;
                    }
                    CMPDIDsSelect[ii] = new SelectItem(compoundID, compoundID);
                    ii++;
                }
                setMetaboloneChemIDOpts(CMPDIDsSelect);

                setDefaultchemid("CHEMICAL_NAME");

                PrimeFaces.current().executeScript("PF('metabolondata').show();");
                return null;
            } else {
                sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }

        return null;
    }

    public SelectItem[] getMetaboloneChemIDOpts() {
        return MetaboloneChemIDOpts;
    }

    public void setMetaboloneChemIDOpts(SelectItem[] MetaboloneChemIDOpts) {
        this.MetaboloneChemIDOpts = MetaboloneChemIDOpts;
    }

    public String getDefaultchemid() {

        //System.out.println("======called here!=======");
        if (defaultchemid == null) {
            RConnection RC = sb.getRConnection();
            if (RC == null) {
                return "NA";
            }
            String[] compoundIDs;

            compoundIDs = RDataUtils.getquickMetabolonCMPDIDs(RC);
            if ((compoundIDs.length == 1) & ("NULL".equals(compoundIDs[0]))) {
                return "NULL";
            }
            SelectItem[] CMPDIDsSelect = new SelectItem[compoundIDs.length - 1];

            int ii = 0;
            for (String compoundID : compoundIDs) {
                if (compoundID.equals("CHEMICAL_NAME")) {
                    continue;
                }
                CMPDIDsSelect[ii] = new SelectItem(compoundID, compoundID);
                ii++;
            }
            setMetaboloneChemIDOpts(CMPDIDsSelect);
            defaultchemid = "CHEMICAL_NAME";
        }
        return defaultchemid;
    }

    public void setDefaultchemid(String defaultchemid) {
        this.defaultchemid = defaultchemid;
    }

    public String processMetabolonData() {
        RConnection RC = sb.getRConnection();
        tsDataType = "pktable";
        String tsDesign = "multi";
        tsFormat = "rowmf";
        boolean res = RDataUtils.parseMetabolonData(RC, "all_mf", getDefaultchemid());
        if (!res) {
            sb.addMessage("Error", "Fail to format your data. Please check your data format or post your data to OmicsForum!");
            return null;
        }

        sb.setTsDesign(tsDesign);
        RDataUtils.setDesignType(RC, tsDesign);
        if (RDataUtils.readTextDataTs(RC, "metaboanalyst_input.csv", tsFormat)) {
            sb.setDataUploaded();
            boolean resx = RDataUtils.readMetaData(RC, "metaboanalyst_input_meta.csv");
            if (!resx) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
            //System.out.println("var");
            tb.reinitVariables();
            return "Data check";
        } else {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            return null;
        }
    }

}
