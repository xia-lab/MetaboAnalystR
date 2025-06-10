/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.controllers.stats.RocAnalBean;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("uploader")
public class UploadBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private DoseResponseBean drb;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    private static final Logger LOGGER = LogManager.getLogger(UploadBean.class);
    /*
     * Handle file upoad (.csv or .txt)
     */
    private UploadedFile metaFile;

    public UploadedFile getMetaFile() {
        return metaFile;
    }

    public void setMetaFile(UploadedFile metaFile) {
        this.metaFile = metaFile;
    }

    private String dataType = "conc";

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    private String dataFormat = "rowu";

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    private UploadedFile dataFile;

    public UploadedFile getDataFile() {
        return dataFile;
    }

    public void setDataFile(UploadedFile dataFile) {
        this.dataFile = dataFile;
    }

    /*
    Data upload for statistics module
     */
    public String handleFileUpload() {

        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }

        if (sb.doLogin(dataType, "stat", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                if (fileName == null) {
                    sb.addMessage("Error", "Failed to read in the CSV file.");
                    return null;
                }

                if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                    sb.setDataUploaded();
                    return "Data check";
                } else {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.addMessage("Error", "Failed to read in the CSV file." + err);
                    return null;
                }
            } catch (Exception e) {
                //e.printStackTrace();
                sb.addMessage("Error", "Exception occured: " + e);
                LOGGER.error("handleFileUpload", e);
            }
        }
        sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }

    /*
     * Handle zip file examples (containing csv or txt files)
     */
    private UploadedFile zipFile;

    public UploadedFile getZipFile() {
        return zipFile;
    }

    public void setZipFile(UploadedFile zipFile) {
        this.zipFile = zipFile;
    }

    private String zipDataType = "nmrpeak";

    public String getZipDataType() {
        return zipDataType;
    }

    public void setZipDataType(String zipDataType) {
        this.zipDataType = zipDataType;
    }

    private String zipFormat;

    public String getZipFormat() {
        return zipFormat;
    }

    public void setZipFormat(String zipFormat) {
        this.zipFormat = zipFormat;
    }

    private UploadedFile pairFile;

    public UploadedFile getPairFile() {
        return pairFile;
    }

    public void setPairFile(UploadedFile file) {
        this.pairFile = file;
    }

    public String handleZipFileUpload() {

        boolean paired = false;
        if (pairFile != null && pairFile.getSize() > 0) {
            paired = true;
        }

        if (sb.doLogin(zipDataType, "stat", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                String homDir = sb.getCurrentUser().getHomeDir();
                DataUtils.uploadFile(sb, zipFile, homDir, null, ab.isOnProServer());
                if (paired) {
                    DataUtils.uploadFile(sb, pairFile, homDir, "pairs.txt", ab.isOnProServer());
                }
                String homeDir = sb.getCurrentUser().getHomeDir();
                String inPath = zipFile.getFileName();
                String outPath = "upload";

                if (RDataUtils.readZipData(sb, inPath, outPath, zipDataType, homeDir)) {

                    sb.setDataUploaded();
                    sb.initNaviTree("stat-peak");
                    return zipDataType;
                } else {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    return null;
                }
            } catch (Exception e) {
                //e.printStackTrace();
                sb.addMessage("Error", "Exception occurred: " + e);
                LOGGER.error("handleZipFileUpload", e);
                return null;
            }
        }

        sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }

    /*
     * Handle test examples for statistics mode
     */
    private String testDataOpt = "conccancer";

    public String getTestDataOpt() {
        return testDataOpt;
    }

    public void setTestDataOpt(String testDataOpt) {
        this.testDataOpt = testDataOpt;
    }

    public String handleStatTestFileUpload() {
        String format = "";
        boolean paired = false;
        boolean isZip = false;
        boolean isMzTab = false;
        String testFile;
        //String testMetaFile = null;        
        switch (testDataOpt) {
            case "conccancer" -> {
                dataType = "conc";
                testFile = ab.getResourceByAPI("human_cachexia.csv");
                format = "rowu";
            }
            case "conccow" -> {
                dataType = "conc";
                testFile = ab.getResourceByAPI("cow_diet.csv");
                format = "rowu";
            }
            case "nmrspecbin" -> {
                dataType = "specbin";
                testFile = ab.getResourceByAPI("nmr_bins.csv");
                format = "rowu";
            }
            case "concpair" -> {
                dataType = "conc";
                paired = true;
                format = "colp";
                testFile = ab.getResourceByAPI("time_series.csv");
            }
            case "mspkint" -> {
                dataType = "pktable";
                testFile = ab.getResourceByAPI("lcms_table.csv");
                format = "colu";
            }
            case "nmrpeaklist" -> {
                dataType = "nmrpeak";
                sb.initNaviTree("stat-peak");
                isZip = true;
                testFile = ab.getResourceByAPI("nmr_peaks.zip");
            }
            case "mspklist" -> {
                dataType = "mspeak";
                sb.initNaviTree("stat-peak");
                isZip = true;
                testFile = ab.getResourceByAPI("lcms_3col_peaks.zip");
            }
            case "mztabmouse" -> {
                dataType = "mztab";
                isMzTab = true;
                testFile = ab.getResourceByAPI("MouseLiver_negative.mzTab");
            }
            case "mztabgc" -> {
                dataType = "mztab";
                isMzTab = true;
                testFile = ab.getResourceByAPI("gcms_tms_height.mzTab");
            }
            default -> {
                sb.addMessage("Error", "Unknown data selected?");
                return null;
            }
        }

        if (!sb.doLogin(dataType, "stat", false, paired)) {
            sb.addMessage("Error", "Log in failed.");
            return null;
        }

        RConnection RC = sb.getRConnection();

        if (isZip) {

            String homeDir = sb.getCurrentUser().getHomeDir();
            //need to fetch here first from URL
            String name = DataUtils.getJustFileName(testFile);
            String outpath = homeDir + File.separator + name;
            DataUtils.fetchFile(testFile, new File(outpath));
            if (!RDataUtils.readZipData(sb, outpath, "upload", dataType, homeDir)) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } else if (isMzTab) {
            if (!RDataUtils.readMzTabData(RC, testFile, identifier)) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } else {
            if (!RDataUtils.readTextData(RC, testFile, format, "disc")) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        sb.setDataUploaded();
        if (dataType.equals("conc") || dataType.equals("pktable") || dataType.equals("specbin") || dataType.equals("mztab")) {
            return "Data check";
        }
        return dataType;
    }

    private boolean useExample = false;

    public boolean isUseExample() {
        return useExample;
    }

    public void setUseExample(boolean useExample) {
        this.useExample = useExample;
    }

    /*
    Handle data for power analysis
     */
    public String uploadPilotData() {

        if (dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }
        if (sb.doLogin(dataType, "power", false, paired)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                sb.setDataUploaded();
                return "Data check";
            } else {
                sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        return null;
    }

    public String handlePowerTestFileUpload() {
        if (!sb.doLogin("conc", "power", false, false)) {
            return null;
        }
        RConnection RC = sb.getRConnection();
        RDataUtils.readTextData(RC, ab.getResourceByAPI("human_cachexia.csv"), "rowu", "disc");
        sb.setDataUploaded();
        return "Data check";
    }

    public String uploadDoseData() {
        //check if data is uploaded
//        if (useExample) {
//            return handleDoseTestFileUpload();
//        }

        if (dataFile == null) {
            sb.addMessage("Error", "No data file is uploaded!");
            return null;
        }

        if (dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty!");
            return null;
        }

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }
        if (sb.doLogin(dataType, "dose", false, paired)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            boolean read_res;

            if (metaFile == null) {
                if (dataClsOpt.equals("disc")) {
                    read_res = RDataUtils.readTextDataDose(RC, fileName, dataFormat, "disc");
                    drb.setContineousDoes(false);
                } else {
                    read_res = RDataUtils.readTextDataDose(RC, fileName, dataFormat, "cont");
                    drb.setContineousDoes(true);
                }
                if (!read_res) {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.addMessage("Error", err);
                    return null;
                }
            } else {
                String metaName = DataUtils.uploadFile(sb, metaFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                if (metaName == null) {
                    return null;
                }
                if (dataClsOpt.equals("disc")) {
                    read_res = RDataUtils.readTextDataDoseWithMeta(RC, fileName, metaName, dataFormat, "disc");
                    drb.setContineousDoes(false);
                } else {
                    read_res = RDataUtils.readTextDataDoseWithMeta(RC, fileName, metaName, dataFormat, "cont");
                    drb.setContineousDoes(true);
                }
                if (!read_res) {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.addMessage("Error", "Failed meta-data integrity check." + err);
                    return null;
                }
            }

            sb.setDataUploaded();
            return "Data check";

        }
        return null;
    }

    public String handleDoseTestFileUpload() {
        if (!sb.doLogin("pktable", "dose", false, false)) {
            return null;
        }
        RConnection RC = sb.getRConnection();
        RDataUtils.readTextDataDose(RC, ab.getInternalData("dose_example.csv"), "colu", "disc");
        sb.setDataUploaded();
        return "Data check";
    }

    /*
    ROC data upload
     */
    private String dataOpt = "data1";

    public String getDataOpt() {
        return dataOpt;
    }

    public void setDataOpt(String dataOpt) {
        this.dataOpt = dataOpt;
    }

    public String uploadRocData() {

        if (dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }
        if (sb.doLogin(dataType, "roc", false, false)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                if (metaFile == null) {
                    sb.setDataUploaded();
                    return "Data check";

                } else {
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
                    mfb.setUniqueMetaList(null);
                    return "Data check";
                }
            } else {
                sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        return null;
    }

    public String handleRocTestFileUpload() {
        if (!sb.doLogin("conc", "roc", false, false)) {
            return null;
        }

        RConnection RC = sb.getRConnection();
        switch (dataOpt) {
            case "data1" ->
                RDataUtils.readTextData(RC, ab.getResourceByAPI("plasma_nmr.csv"), "rowu", "disc");
            case "data2" ->
                RDataUtils.readTextData(RC, ab.getResourceByAPI("plasma_nmr_new.csv"), "rowu", "disc");
            case "diabetes" -> {
                sb.setDataType("pktable");
                String fileName = ab.getResourceByAPI("diabetes_lipids.txt");
                String testMetaFile = ab.getResourceByAPI("diabetes_metadata.csv");
                RDataUtils.readTextData(RC, fileName, "rowmf", "disc");
                boolean res = RDataUtils.readMetaData(RC, testMetaFile);
            }
            default -> {
                sb.setDataType("pktable");
                String fileName = ab.getResourceByAPI("TCE_feature_table_small.csv");
                String testMetaFile = ab.getResourceByAPI("TCE_metadata.csv");
                RDataUtils.readTextData(RC, fileName, "colmf", "disc");
                boolean res = RDataUtils.readMetaData(RC, testMetaFile);
            }
        }
        sb.setDataUploaded();
        mfb.setUniqueMetaList(null);
        return "Data check";
    }


    /*
     * Handle mzTab file examples (containing csv or txt files)
     */
    private UploadedFile mzTabFile;

    public UploadedFile getMzTabFile() {
        return mzTabFile;
    }

    public void setMzTabFile(UploadedFile mzTabFile) {
        this.mzTabFile = mzTabFile;
    }

    private String identifier = "name";

    public String getIdentifier() {
        return identifier;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public String handleMzTabUpload() {

        if (mzTabFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty!");
            return null;
        }

        dataType = "mztab";
        if (sb.doLogin(dataType, "stat", false, false)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadFile(sb, mzTabFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

            if (RDataUtils.readMzTabData(RC, fileName, identifier)) {
                sb.setDataUploaded();
                return "Data check";
            } else {
                sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        return null;
    }

    /*
     * Handle Metabolomics Workbench datasets
     */
    private String nmdrStudyId = "ST001301";

    public String getNmdrStudyId() {
        return nmdrStudyId;
    }

    public void setNmdrStudyId(String nmdrStudyId) {
        this.nmdrStudyId = nmdrStudyId;
    }

    public String handleMetWorkbenchData(String module) {

        if (!nmdrStudyId.startsWith("ST")) {
            sb.addMessage("Error", "Invalid Study ID!");
            return null;
        }

        if (module.equals("stat")) {
            if (sb.doLogin("conc", "stat", false, false)) {
                try {
                    RConnection RC = sb.getRConnection();

                    if (RDataUtils.getMetabolomicsWorkbenchData(RC, nmdrStudyId)) {
                        if (RDataUtils.readMetabolomicsWorkbenchData(RC, nmdrStudyId, "rowu", "disc")) {
                            sb.setDataUploaded();
                            return "Data check";
                        } else {
                            String err = RDataUtils.getErrMsg(RC);
                            sb.addMessage("Error", "Failed to read in the txt file." + err);
                            return null;
                        }
                    } else {
                        String err = RDataUtils.getErrMsg(RC);
                        sb.addMessage("Error", "Failed to retrieve study from Metabolomics Workbench!" + err);
                        return null;
                    }
                } catch (Exception e) {
                    //e.printStackTrace();
                    LOGGER.error("handleMetWorkbenchData-stat", e);
                    return null;
                }
            }
        } else if (module.equals("roc")) {
            if (sb.doLogin("conc", "roc", false, false)) {
                try {
                    RConnection RC = sb.getRConnection();

                    if (RDataUtils.getMetabolomicsWorkbenchData(RC, nmdrStudyId)) {
                        if (RDataUtils.readMetabolomicsWorkbenchData(RC, nmdrStudyId, "rowu", "disc")) {
                            sb.setDataUploaded();
                            return "Data check";
                        } else {
                            String err = RDataUtils.getErrMsg(RC);
                            sb.addMessage("Error", "Failed to read in the txt file." + err);
                            return null;
                        }
                    } else {
                        String err = RDataUtils.getErrMsg(RC);
                        sb.addMessage("Error", "Failed to retrieve study from Metabolomics Workbench!" + err);
                        return null;
                    }
                } catch (Exception e) {
                    //e.printStackTrace();
                    LOGGER.error("handleMetWorkbenchData-roc", e);
                    return null;
                }
            }
        }

        sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");

        return null;
    }

    /*
     * Handle Metabolone XLSX datasheet
     */
    private UploadedFile metabolonFile;

    public UploadedFile getMetabolonFile() {
        return metabolonFile;
    }

    public void setMetabolonFile(UploadedFile metabolonFile) {
        this.metabolonFile = metabolonFile;
    }

    private SelectItem[] MetaboloneMetaOpts, MetaboloneChemIDOpts;
    private String defaultMetafactor, defaultchemid;

    public String handleMetabolonData(String analType) {

        if (metabolonFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty!");
            return null;
        }

        dataType = "conc";
        if (sb.doLogin(dataType, analType, false, false)) {
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

                if (compoundIDs.length == 0) {
                    sb.addMessage("warn", "'Chemical Annotation' sheet does not contain supportted ID! Will use default feature label.");
                }

                SelectItem[] metaFactorsSelect = new SelectItem[metaFactors.length];
                SelectItem[] CMPDIDsSelect = new SelectItem[compoundIDs.length - 1];
                for (int i = 0; i < metaFactors.length; i++) {
                    metaFactorsSelect[i] = new SelectItem(metaFactors[i], metaFactors[i]);
                }
                int ii = 0;
                for (String compoundID : compoundIDs) {
                    if (compoundID.equals("CHEMICAL_NAME")) {
                        continue;
                    }
                    CMPDIDsSelect[ii] = new SelectItem(compoundID, compoundID);
                    ii++;
                }
                setMetaboloneMetaOpts(metaFactorsSelect);
                setMetaboloneChemIDOpts(CMPDIDsSelect);
                setDefaultMetafactor(metaFactors[0]);
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

    public SelectItem[] getMetaboloneMetaOpts() {
        return MetaboloneMetaOpts;
    }

    public void setMetaboloneMetaOpts(SelectItem[] MetaboloneMetaOpts) {
        this.MetaboloneMetaOpts = MetaboloneMetaOpts;
    }

    public SelectItem[] getMetaboloneChemIDOpts() {
        return MetaboloneChemIDOpts;
    }

    public void setMetaboloneChemIDOpts(SelectItem[] MetaboloneChemIDOpts) {
        this.MetaboloneChemIDOpts = MetaboloneChemIDOpts;
    }

    public String getDefaultMetafactor() {
        if (defaultMetafactor == null) {
            RConnection RC = sb.getRConnection();
            if (RC == null) {
                return "NA";
            }
            String[] metaFactors, compoundIDs;

            metaFactors = RDataUtils.getquickMetabolonMetaFactors(RC);
            compoundIDs = RDataUtils.getquickMetabolonCMPDIDs(RC);

            if (metaFactors.length == 0) {
                sb.addMessage("Error", "'Sample Meta Data' sheet does not contains metadata information more than 2 categories!");
                return null;
            }

            if (compoundIDs.length == 0) {
                sb.addMessage("warn", "'Chemical Annotation' sheet does not contain supportted ID! Will use default feature label.");
            }

            SelectItem[] metaFactorsSelect = new SelectItem[metaFactors.length];
            SelectItem[] CMPDIDsSelect = new SelectItem[compoundIDs.length - 1];
            for (int i = 0; i < metaFactors.length; i++) {
                metaFactorsSelect[i] = new SelectItem(metaFactors[i], metaFactors[i]);
            }
            int ii = 0;
            for (String compoundID : compoundIDs) {
                if (compoundID.equals("CHEMICAL_NAME")) {
                    continue;
                }
                CMPDIDsSelect[ii] = new SelectItem(compoundID, compoundID);
                ii++;
            }
            setMetaboloneMetaOpts(metaFactorsSelect);
            setMetaboloneChemIDOpts(CMPDIDsSelect);
            setDefaultMetafactor(metaFactors[0]);
            setDefaultchemid("CHEMICAL_NAME");
        }
        return defaultMetafactor;
    }

    public void setDefaultMetafactor(String defaultMetafactor) {
        this.defaultMetafactor = defaultMetafactor;
    }

    public String getDefaultchemid() {
        if (defaultchemid == null) {
            defaultchemid = "CHEMICAL_NAME";
        }
        return defaultchemid;
    }

    public void setDefaultchemid(String defaultchemid) {
        this.defaultchemid = defaultchemid;
    }

    public String processMetabolonData() {
        RConnection RC = sb.getRConnection();
        boolean res = RDataUtils.parseMetabolonData(RC, getDefaultMetafactor(), getDefaultchemid());
        if (!res) {
            sb.addMessage("Error", "Fail to format your data. Please check your data format or post your data to OmicsForum!");
            return null;
        }
        if (RDataUtils.readTextData(RC, "metaboanalyst_input.csv", "rowu", "disc")) {
            sb.setDataUploaded();
            return "Data check";
        } else {
            sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
            return null;
        }
    }

    private String testDoseOpt = "dist";

    public String getTestDoseOpt() {
        return testDoseOpt;
    }

    public void setTestDoseOpt(String testDataOpt) {
        this.testDoseOpt = testDataOpt;
    }

    private String dataClsOpt = "disc";

    public String getDataClsOpt() {
        return dataClsOpt;
    }

    public void setDataClsOpt(String dataClsOpt) {
        this.dataClsOpt = dataClsOpt;
    }

    public String uploadDoseDataExample() {
        int option_example = 0;
        if (testDoseOpt.equals("cont")) {
            option_example = 1;
        } else if (testDoseOpt.equals("tce")) {
            option_example = 2;
        }
        return handleDoseTestFileUpload(option_example);
    }

    public String handleDoseTestFileUpload(int option) {
        if (!sb.doLogin("pktable", "dose", false, false)) {
            return null;
        }
        RConnection RC = sb.getRConnection();
        switch (option) {
            case 1 -> {
                drb.setContineousDoes(true);
                RDataUtils.readTextDataDoseWithMeta(RC, ab.getInternalData("ewaste_data.csv"), ab.getInternalData("ewaste_metadata_dose.csv"), "colu", "cont");
            }
            case 2 -> {
                drb.setContineousDoes(true);
                RDataUtils.readTextDataDose(RC, ab.getResourceByAPI("TCE_subset_with_conc.csv"), "colu", "cont");
            }
            default -> {
                drb.setContineousDoes(false);
                RDataUtils.readTextDataDose(RC, ab.getInternalData("dose_example.csv"), "colu", "disc");
            }
        }
        sb.setDataUploaded();
        return "Data check";
    }
}
