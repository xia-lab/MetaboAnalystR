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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import org.primefaces.event.FileUploadEvent;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.datalts.DatasetController;
import pro.metaboanalyst.datalts.DatasetFile;

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

    @JsonIgnore
    @Inject
    private DatasetController dc;

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

        boolean paired = dataFormat != null && dataFormat.endsWith("p");

        if (!sb.doLogin(dataType, "stat", false, paired)) {
            sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }

        try {
            RConnection RC = sb.getRConnection();

            // 1) Save to user's home dir for R (existing behavior)
            String fileName = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (fileName == null) {
                sb.addMessage("Error", "Failed to read in the CSV file.");
                return null;
            }

            // 2) Let R read/validate (existing behavior)
            if (!RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                String err = RDataUtils.getErrMsg(RC);
                sb.addMessage("Error", "Failed to read in the CSV file. " + err);
                return null;
            }
            sb.setDataUploaded();

            // 3) STAGE ONLY (no DB insert, no dataset-folder save yet)
            String niceTitle = stripExt(fileName);
            int samples = 10; // or inferSampleNumFromR(RC)

            List<UploadedFile> files = List.of(dataFile);
            List<String> roles = List.of("data");

            // Assumes you have @Inject DatasetController datasetController; or otherwise get the bean
            dc.stageDataset(niceTitle, samples, files, roles);
            sb.addMessage("info", "Dataset staged in memory.");

            return "Data check"; // continue to your next view as before

        } catch (Exception e) {
            sb.addMessage("Error", "Exception occured: " + e.getMessage());
            LOGGER.error("handleFileUpload", e);
            return null;
        }
    }

// Small helper if not already present
    private static String stripExt(String pathOrName) {
        if (pathOrName == null) {
            return "";
        }
        String name = pathOrName.replace('\\', '/'); // normalize
        int slash = name.lastIndexOf('/');
        if (slash >= 0) {
            name = name.substring(slash + 1);
        }
        int dot = name.lastIndexOf('.');
        return (dot > 0) ? name.substring(0, dot) : name;
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

        if (zipFile == null || zipFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty!");
            return null;
        }

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

                    // 3) STAGE ONLY (no DB insert, no dataset-folder save yet)
                    String niceTitle = stripExt(inPath);
                    int samples = 10; // or inferSampleNumFromR(RC)

                    List<UploadedFile> files = List.of(zipFile);
                    List<String> roles = List.of("data");

                    dc.stageDataset(niceTitle, samples, files, roles);
                    sb.addMessage("info", "Dataset staged in memory.");

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
        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }

        boolean paired = dataFormat != null && dataFormat.endsWith("p");

        if (!sb.doLogin(dataType, "power", false, paired)) {
            return null;
        }

        try {
            RConnection RC = sb.getRConnection();

            // 1) Save upload to user's home dir (for R)
            String fileName = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (fileName == null) {
                sb.addMessage("Error", "Failed to save upload.");
                return null;
            }

            // 2) Let R read/validate
            if (!RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
            sb.setDataUploaded();

            // 3) STAGE the dataset (no DB insert, no dataset-folder copy yet)
            String niceTitle = stripExt(fileName);                    // e.g., "pilot_data"
            int samples = 0; // or infer from R: inferSampleNumFromR(RC)

            List<UploadedFile> files = List.of(dataFile);
            List<String> roles = List.of("data");

            dc.stageDataset(niceTitle, samples, files, roles);

            sb.addMessage("info", "Dataset staged in memory. You can review and commit later.");
            return "Data check";

        } catch (Exception e) {
            sb.addMessage("Error", "Exception occurred: " + e.getMessage());
            LOGGER.error("uploadPilotData", e);
            return null;
        }
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
        // Basic checks
        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("Error", "No data file is uploaded or file is empty!");
            return null;
        }

        boolean paired = (dataFormat != null && dataFormat.endsWith("p"));

        if (!sb.doLogin(dataType, "dose", false, paired)) {
            return null;
        }

        RConnection RC = sb.getRConnection();
        try {
            // 1) Save uploads to user's home dir (to have local paths)
            String dataPath = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (dataPath == null) {
                sb.addMessage("Error", "Failed to save data file.");
                return null;
            }

            String metaPath = null;
            boolean hasMeta = metaFile != null && metaFile.getSize() > 0;
            if (hasMeta) {
                metaPath = DataUtils.uploadFile(sb, metaFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                if (metaPath == null) {
                    sb.addMessage("Error", "Failed to save metadata file.");
                    return null;
                }
            }

            // 2) Let R read/validate
            boolean ok;
            if (!hasMeta) {
                if ("disc".equalsIgnoreCase(dataClsOpt)) {
                    ok = RDataUtils.readTextDataDose(RC, dataPath, dataFormat, "disc");
                    drb.setContineousDoes(false);
                } else {
                    ok = RDataUtils.readTextDataDose(RC, dataPath, dataFormat, "cont");
                    drb.setContineousDoes(true);
                }
            } else {
                if ("disc".equalsIgnoreCase(dataClsOpt)) {
                    ok = RDataUtils.readTextDataDoseWithMeta(RC, dataPath, metaPath, dataFormat, "disc");
                    drb.setContineousDoes(false);
                } else {
                    ok = RDataUtils.readTextDataDoseWithMeta(RC, dataPath, metaPath, dataFormat, "cont");
                    drb.setContineousDoes(true);
                }
            }
            if (!ok) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }

            sb.setDataUploaded();

            // 3) STAGE (no DB insert, no dataset-folder copy)
            String dataName = DataUtils.getJustFileName(dataPath);
            String niceTitle = stripExt(dataName);
            int samples = 0; // TODO: infer from R if desired

            java.util.List<DatasetFile> files = new java.util.ArrayList<>();
            java.util.List<java.nio.file.Path> srcPaths = new java.util.ArrayList<>();

            // data file
            java.nio.file.Path dp = java.nio.file.Paths.get(dataPath);
            DatasetFile df = new DatasetFile();
            df.setRole("data");
            df.setFilename(dataName);
            df.setType(extOf(dataName).isEmpty() ? "bin" : extOf(dataName));
            df.setSizeBytes(java.nio.file.Files.size(dp));
            df.setUploadedAt(java.time.OffsetDateTime.now());
            files.add(df);
            srcPaths.add(dp);

            // metadata file (optional)
            if (hasMeta) {
                String metaName = DataUtils.getJustFileName(metaPath);
                java.nio.file.Path mp = java.nio.file.Paths.get(metaPath);

                DatasetFile mf = new DatasetFile();
                mf.setRole("metadata");
                mf.setFilename(metaName);
                mf.setType(extOf(metaName).isEmpty() ? "bin" : extOf(metaName));
                mf.setSizeBytes(java.nio.file.Files.size(mp));
                mf.setUploadedAt(java.time.OffsetDateTime.now());
                files.add(mf);
                srcPaths.add(mp);
            }

            // stage from local paths
            dc.stageDatasetFromPaths(niceTitle, samples, files, srcPaths);

            sb.addMessage("info", "Dataset staged in memory. You can review and commit later.");
            return "Data check";

        } catch (Exception e) {
            sb.addMessage("Error", "Exception: " + e.getMessage());
            LOGGER.error("uploadDoseData", e);
            return null;
        }
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

        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }

        if (!sb.doLogin(dataType, "roc", false, false)) {
            return null;
        }

        RConnection RC = sb.getRConnection();

        try {
            // 1) Save primary data file to user's home (local path for R + later commit)
            String dataPath = DataUtils.uploadFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (dataPath == null) {
                sb.addMessage("Error", "Failed to save data file.");
                return null;
            }

            // 2) R: read data
            if (!RDataUtils.readTextData(RC, dataPath, dataFormat, "disc")) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }

            // 3) Optional metadata
            String metaPath = null;
            if (metaFile != null && metaFile.getSize() > 0) {
                metaPath = DataUtils.uploadFile(sb, metaFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                if (metaPath == null) {
                    return null;
                }
                boolean ok = RDataUtils.readMetaData(RC, metaPath);
                if (!ok) {
                    sb.addMessage("Error", "Failed meta-data integrity check. " + RDataUtils.getErrMsg(RC));
                    return null;
                }
                // your original side-effect
                mfb.setUniqueMetaList(null);
            }

            sb.setDataUploaded();

            // 4) STAGE (no DB insert, no dataset-folder copy)
            String dataName = DataUtils.getJustFileName(dataPath);
            String niceTitle = stripExt(dataName);
            int samples = 0; // set if you want to infer

            java.util.List<DatasetFile> files = new java.util.ArrayList<>();
            java.util.List<java.nio.file.Path> srcPaths = new java.util.ArrayList<>();

            // data file entry
            java.nio.file.Path dp = java.nio.file.Paths.get(dataPath);
            DatasetFile df = new DatasetFile();
            df.setRole("data");
            df.setFilename(dataName);
            df.setType(extOf(dataName).isEmpty() ? "bin" : extOf(dataName));
            df.setSizeBytes(java.nio.file.Files.size(dp));
            df.setUploadedAt(java.time.OffsetDateTime.now());
            files.add(df);
            srcPaths.add(dp);

            // metadata file entry (optional)
            if (metaPath != null) {
                String metaName = DataUtils.getJustFileName(metaPath);
                java.nio.file.Path mp = java.nio.file.Paths.get(metaPath);

                DatasetFile mf = new DatasetFile();
                mf.setRole("metadata");
                mf.setFilename(metaName);
                mf.setType(extOf(metaName).isEmpty() ? "bin" : extOf(metaName));
                mf.setSizeBytes(java.nio.file.Files.size(mp));
                mf.setUploadedAt(java.time.OffsetDateTime.now());
                files.add(mf);
                srcPaths.add(mp);
            }

            // stage using local paths (since no UploadedFile needed here)
            dc.stageDatasetFromPaths(niceTitle, samples, files, srcPaths);

            sb.addMessage("info", "Dataset staged in memory. You can review and commit later.");
            return "Data check";

        } catch (Exception e) {
            sb.addMessage("Error", "Exception: " + e.getMessage());
            LOGGER.error("uploadRocData", e);
            return null;
        }
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

        if (mzTabFile == null || mzTabFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty!");
            return null;
        }

        dataType = "mztab";
        if (!sb.doLogin(dataType, "stat", false, false)) {
            return null;
        }

        RConnection RC = sb.getRConnection();
        try {
            // 1) Save to user's home dir (local path for R + later commit)
            String filePath = DataUtils.uploadFile(sb, mzTabFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            if (filePath == null) {
                sb.addMessage("Error", "Failed to save mzTab file.");
                return null;
            }

            // 2) R validation (use local path)
            if (!RDataUtils.readMzTabData(RC, filePath, identifier)) {
                sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
            sb.setDataUploaded();

            // 3) STAGE (no DB insert / no file move yet)
            String name = DataUtils.getJustFileName(filePath);
            String niceTitle = stripExt(name);
            java.nio.file.Path p = java.nio.file.Paths.get(filePath);

            DatasetFile df = new DatasetFile();
            df.setRole("data");
            df.setFilename(name);
            String ext = extOf(name);
            df.setType(ext.isEmpty() ? "mztab" : ext);               // typically "mztab"
            df.setSizeBytes(java.nio.file.Files.size(p));
            df.setUploadedAt(java.time.OffsetDateTime.now());

            // Stage using local path (no UploadedFile needed for commit)
            dc.stageDatasetFromPaths(
                    niceTitle,
                    /*sampleNum*/ 0,
                    java.util.Arrays.asList(df),
                    java.util.Arrays.asList(p)
            );

            sb.addMessage("info", "mzTab dataset staged in memory. You can review and commit later.");
            return "Data check";

        } catch (Exception e) {
            sb.addMessage("Error", "Exception: " + e.getMessage());
            LOGGER.error("handleMzTabUpload", e);
            return null;
        }
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

        // Basic sanity for MW study id (e.g., "ST001234")
        if (nmdrStudyId == null || !nmdrStudyId.startsWith("ST")) {
            sb.addMessage("Error", "Invalid Study ID!");
            return null;
        }

        // Decide login target by module
        final String loginModule;
        if ("stat".equalsIgnoreCase(module)) {
            loginModule = "stat";
        } else if ("roc".equalsIgnoreCase(module)) {
            loginModule = "roc";
        } else {
            sb.addMessage("Error", "Unknown module: " + module);
            return null;
        }

        // For MW examples we treat as concentration data
        if (!sb.doLogin("conc", loginModule, false, false)) {
            sb.addMessage("Error", "Log in failed. Please check R/Rserve settings.");
            return null;
        }

        try {
            RConnection RC = sb.getRConnection();

            // 1) Ask R to fetch the study from Metabolomics Workbench
            if (!RDataUtils.getMetabolomicsWorkbenchData(RC, nmdrStudyId)) {
                sb.addMessage("Error", "Failed to retrieve study from Metabolomics Workbench! " + RDataUtils.getErrMsg(RC));
                return null;
            }

            // 2) Ask R to read the study into the current session
            if (!RDataUtils.readMetabolomicsWorkbenchData(RC, nmdrStudyId, "rowu", "disc")) {
                sb.addMessage("Error", "Failed to read the study file. " + RDataUtils.getErrMsg(RC));
                return null;
            }

            sb.setDataUploaded();

            // ---------- STAGE (no DB insert, no dataset-folder copy) ----------
            // The MW helper uses a TXT export. We assume it’s saved under user's home with <STUDY>.txt
            final String homeDir = sb.getCurrentUser().getHomeDir();
            final String guessedName = nmdrStudyId + ".txt"; // matches your original error message wording
            final java.nio.file.Path guessedPath = java.nio.file.Paths.get(homeDir, guessedName);

            long size = 0L;
            try {
                if (java.nio.file.Files.exists(guessedPath)) {
                    size = java.nio.file.Files.size(guessedPath);
                }
            } catch (Exception ignore) {
            }

            DatasetFile df = new DatasetFile();
            df.setRole("data");
            df.setFilename(guessedName);
            df.setType("txt");
            df.setSizeBytes(Math.max(0L, size));
            df.setUploadedAt(java.time.OffsetDateTime.now());

            if (java.nio.file.Files.exists(guessedPath)) {
                dc.stageDatasetFromPaths(
                        "MW " + nmdrStudyId, // title
                        /*sampleNum*/ 0,
                        java.util.List.of(df),
                        java.util.List.of(guessedPath)
                );
            } else {
                // Fallback: stage metadata, remember intended path for commit
                dc.stageDatasetFromPaths(
                        "MW " + nmdrStudyId,
                        /*sampleNum*/ 0,
                        java.util.List.of(df),
                        java.util.List.of(guessedPath) // commit will verify and error if missing
                );
            }

            sb.addMessage("info", "Metabolomics Workbench study staged in memory. You can review and commit later.");
            return "Data check";

        } catch (Exception e) {
            LOGGER.error("handleMetWorkbenchData-" + module, e);
            sb.addMessage("Error", "Exception: " + e.getMessage());
            return null;
        }
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

        if (metabolonFile == null || metabolonFile.getSize() == 0) {
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
                RDataUtils.readTextDataDoseWithMeta(RC, ab.getResourceByAPI("ewaste_data.csv"), ab.getResourceByAPI("ewaste_metadata_dose.csv"), "colu", "cont");
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

    public void handleFileUploadMulti(FileUploadEvent event) {

        UploadedFile dataFile = event.getFile();

        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("error", "Empty data file?");
            return;
        }

        if (ab.isOnProServer()) { // size limit will apply only on public server
            if (dataFile.getSize() > ab.getMAX_UPLOAD_SIZE()) {
                sb.addMessage("error", "The file size exceeds limit:" + ab.getMAX_UPLOAD_SIZE());
                dataFile = null;
                return;
            }
        }

        RConnection RC = sb.getRConnection();
        String homeDir = sb.getCurrentUser().getHomeDir();

        //do actual upload 
        String fileName = dataFile.getFileName();
        try {
            InputStream in = dataFile.getInputStream();
            OutputStream out = new FileOutputStream(new File(homeDir + File.separator + fileName));
            byte[] buffer = new byte[1024];
            int len;
            while ((len = in.read(buffer)) >= 0) {
                out.write(buffer, 0, len);
            }
            in.close();
            out.close();
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }

    }

    private static String sanitizeFilename(String name) {
        // strip any path, keep only the last segment
        name = name.replace('\\', '/');
        int idx = name.lastIndexOf('/');
        if (idx >= 0) {
            name = name.substring(idx + 1);
        }

        // remove illegal chars; allow letters, digits, dot, dash, underscore, and space
        name = name.replaceAll("[^A-Za-z0-9._\\- ]", "_").trim();
        if (name.isEmpty()) {
            name = "file.bin";
        }
        // limit length
        if (name.length() > 180) {
            String ext = extOf(name);
            String base = ext.isEmpty() ? name : name.substring(0, name.length() - ext.length() - 1);
            base = base.substring(0, Math.min(base.length(), 170));
            name = ext.isEmpty() ? base : base + "." + ext;
        }
        return name;
    }

    private static String extOf(String filename) {
        int dot = (filename == null) ? -1 : filename.lastIndexOf('.');
        return (dot > 0 && dot < filename.length() - 1)
                ? filename.substring(dot + 1).toLowerCase(Locale.ROOT)
                : "";
    }
}
