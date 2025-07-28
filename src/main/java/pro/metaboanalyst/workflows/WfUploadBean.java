/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.workflows;

import pro.metaboanalyst.controllers.general.*;

import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Named;
import java.io.*;
import java.util.ArrayList;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rosuda.REngine.REXPMismatchException;
import pro.metaboanalyst.controllers.enrich.EnrichUploadBean;
import pro.metaboanalyst.controllers.enrich.MappingBean;
import pro.metaboanalyst.controllers.enrich.PathUploadBean;
import pro.metaboanalyst.controllers.meta.MetaLoadBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.mnet.MnetLoadBean;
import pro.metaboanalyst.controllers.mnet.MnetMapBean;
import pro.metaboanalyst.controllers.multifac.TimeUploadBean;
import pro.metaboanalyst.controllers.mummichog.PeakUploadBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.spectra.SpectraUploadBean;

/**
 * @author zgy
 */
@ViewScoped
@Named("wfUploadBean")
public class WfUploadBean implements Serializable {

    @Inject
    private SessionBean1 sb;
    @Inject
    private ApplicationBean1 ab;

    @Inject
    private WorkflowBean wb;

    @Inject
    private WorkflowView wf;

    @Inject
    private TimeUploadBean tub;

    @Inject
    private UploadBean upb;

    @Inject
    private ProcessBean pcb;

    @Inject
    private EnrichUploadBean eub;

    @Inject
    private PathUploadBean pub;

    @Inject
    private MappingBean mapb;

    @Inject
    private MnetLoadBean mnlb;

    @Inject
    private MnetMapBean mntb;

    @Inject
    private PeakUploadBean pkub;

    @Inject
    private SpectraUploadBean spub;

    @Inject
    private SpectraProcessBean sppb;
    @Inject
    private SpectraControlBean spcb;

    @Inject
    private DiagramView dv;

    @Inject
    private MetaLoadBean mlb;

    @Inject
    private MetaPathLoadBean mplb;

    @Inject
    private NormBean normBean;

    private UploadedFile libFile;

    private UploadedFile file;
    private UploadedFile fileMeta;
    private UploadedFile workflowFile;
    private String metaName = "";

    private String uploadInfo = "";
    private boolean fileUploaded = false;
    private String fileType;
    private String graphExample = "json";
    private boolean containMeta;
    private ArrayList<String> multiFileNames = new ArrayList();
    private String enrichOpt = "ora";
    private String pathOpt = "list";
    private String networkOpt = "list";
    private String mumOpt = "list";
    private String specUpOpt = "googledrive";

    private String dataType = "conc";
    private String dataFormat = "colu";
    private String statOpt = "table";
    private UploadedFile csvFile;
    private UploadedFile metaFile;
    private String tableOpt = "generic";
    private String listOpt = "compound";
    private String multiTableOpt = "generic";

    public String getMultiTableOpt() {
        return multiTableOpt;
    }

    public void setMultiTableOpt(String multiTableOpt) {
        this.multiTableOpt = multiTableOpt;
    }

    public String getListOpt() {
        return listOpt;
    }

    public void setListOpt(String listOpt) {
        this.listOpt = listOpt;
    }

    public String getTableOpt() {
        return tableOpt;
    }

    public void setTableOpt(String tableOpt) {
        this.tableOpt = tableOpt;
    }

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

    public String getStatOpt() {
        return statOpt;
    }

    public void setStatOpt(String statOpt) {
        this.statOpt = statOpt;
    }

    public String getMumOpt() {
        return mumOpt;
    }

    public void setMumOpt(String mumOpt) {
        this.mumOpt = mumOpt;
    }

    public String getNetworkOpt() {
        return networkOpt;
    }

    public void setNetworkOpt(String networkOpt) {
        this.networkOpt = networkOpt;
    }

    public String getPathOpt() {
        return pathOpt;
    }

    public void setPathOpt(String pathOpt) {
        this.pathOpt = pathOpt;
    }

    public String getEnrichOpt() {
        return enrichOpt;
    }

    public void setEnrichOpt(String enrichOpt) {
        this.enrichOpt = enrichOpt;
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

    public UploadedFile getWorkflowFile() {
        return workflowFile;
    }

    public void setWorkflowFile(UploadedFile workflowFile) {
        this.workflowFile = workflowFile;
    }

    public UploadedFile getLibFile() {
        return libFile;
    }

    public void setLibFile(UploadedFile libFile) {
        this.libFile = libFile;
    }

    public String getUploadInfo() {
        return uploadInfo;
    }

    public void setUploadInfo(String uploadInfo) {
        this.uploadInfo = uploadInfo;
    }

    public UploadedFile getFile() {
        return file;
    }

    public void setFile(UploadedFile file) {
        this.file = file;
    }

    public UploadedFile getFileMeta() {
        return fileMeta;
    }

    public void setFileMeta(UploadedFile fileMeta) {
        this.fileMeta = fileMeta;
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

    public String getGraphExample() {
        return graphExample;
    }

    public void setGraphExample(String graphExample) {
        this.graphExample = graphExample;
    }

    public boolean isContainMeta() {
        return containMeta;
    }

    public void setContainMeta(boolean containMeta) {
        this.containMeta = containMeta;
    }

    public String getSpecUpOpt() {
        return specUpOpt;
    }

    public void setSpecUpOpt(String specUpOpt) {
        this.specUpOpt = specUpOpt;
    }

    public boolean handleFileUploadWorkflow(FileUploadEvent event) throws IOException {
        UploadedFile dataFile = event.getFile();
        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("error", "Empty file?");
            uploadInfo = "Upload error! Empty data file?";
            return false;
        }

        fileUploaded = false;

        String fileName = dataFile.getFileName();
        if (!fileName.endsWith(".zip") && !fileName.endsWith(".txt") && !fileName.endsWith(".csv")) {
            sb.addMessage("error", "Only .txt or .zip file is acceptable!");
            uploadInfo = "Upload error! Only .txt or .zip file forat is acceptable.";
            file = null;
            return false;
        }
        sb.setSaveEnabled(false);

        if (!sb.isLoggedIn()) {
            boolean ok = sb.doLogin(dataType, "stat", false, false);
            if (!ok) {
                return false;
            }
        }

        String homeDir = sb.getCurrentUser().getHomeDir();

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

        if (!multiFileNames.contains(fileName)) {
            multiFileNames.add(fileName);
        }
        if (fileName.contains("metadata")) {
            metaName = fileName;
            wb.setMetaName(fileName);
        } else {
            wb.setDataName(fileName);
        }

        return fileUploaded;
    }

    public boolean processFiles() throws IOException {

        if (csvFile == null) {
            sb.addMessage("Error", "You must provide data file in order to proceed!");
            return false;
        }

        if (csvFile.getSize() == 0) {
            sb.addMessage("Error", "Make sure the data file is not empty!");
            return false;
        }

        String module = "stat";
        if (metaFile != null && metaFile.getSize() != 0) {
            module = "mf";
        }

        if (!sb.doLogin("conc", module, false, false)) {
            return false;
        }
        RConnection RC = sb.getRConnection();

        String fileName = DataUtils.uploadFile(sb, csvFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
        boolean resBool = false;
        if (metaFile != null) {
            resBool = RDataUtils.readTextDataTs(RC, fileName, dataFormat);
        } else {
            resBool = RDataUtils.readTextData(RC, fileName, dataFormat, "disc");
        }

        if (!resBool) {
            String errMsg = RDataUtils.getCurrentMsg(RC);
            sb.addMessage("error", errMsg);
            uploadInfo = "Upload error: " + errMsg;
            return false;
        } else {
            if (wb.getTableAnalType().equals("dose")) {
                resBool = RDataUtils.readTextDataDose(RC, fileName, dataFormat, "disc");
                if (!resBool) {
                    String errMsg = RDataUtils.getCurrentMsg(RC);
                    sb.addMessage("error", errMsg);
                    uploadInfo = "Upload error: " + errMsg;
                    return false;
                }
            }
            String infomsg = RDataUtils.getCurrentMsg(RC);
            sb.addMessage("Info", infomsg);
            sb.setDataUploaded();

            String metaName = "";
            if (metaFile != null) {
                metaName = DataUtils.uploadFile(sb, metaFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
                boolean res = RDataUtils.readMetaData(RC, metaName);
                if (!res) {
                    sb.addMessage("Error", RDataUtils.getErrMsg(RC));
                    return false;
                }
            }
        }

        if (resBool) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next page.";
            wb.setDataName(fileName);
            wb.setDataFormat(dataFormat);
            wb.setDataType(dataType);
            fileUploaded = true;

        } else {
            uploadInfo = uploadInfo + "<br/>Upload failed! Pay attention to the error messages. You can try to address yourself or use <b>OmicsForum</b> for support.";
        }
        return true;
    }

    public boolean processMultiFiles() throws IOException {
        RConnection RC = sb.getRConnection();

        int res = 0;

        String fileName = "";
        if (multiFileNames.size() == 1) {
            fileName = multiFileNames.get(0);
        }
        boolean resBool = RDataUtils.readTextData(RC, fileName, dataFormat, "disc");

        if (!resBool) {
            String errMsg = RDataUtils.getCurrentMsg(RC);
            sb.addMessage("error", errMsg);
            uploadInfo = "Upload error: " + errMsg;
            return false;
        } else {
            String infomsg = RDataUtils.getCurrentMsg(RC);
            sb.addMessage("Info", infomsg);
            sb.setDataUploaded();
        }

        if (fileUploaded) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next page.";
            wb.setDataName(fileName);
            wb.setDataFormat(dataFormat);
            wb.setDataType(dataType);
            fileUploaded = true;

        } else {
            uploadInfo = uploadInfo + "<br/>Upload failed! Pay attention to the error messages. You can try to address yourself or use <b>OmicsForum</b> for support.";
        }
        return true;
    }

    private String testDataOpt = "conccancer";

    public String getTestDataOpt() {
        return testDataOpt;
    }

    public void setTestDataOpt(String testDataOpt) {
        this.testDataOpt = testDataOpt;
    }

    public String handleTestDataUpload_mf() {

        String res = tub.handleTestDataUpload();
        if (res != null) {
            uploadInfo = "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String handleTestDataUpload_table() {
        if (upb.getDataOpt().equals("pkcovid")) {
            wb.setDataName("https://api2.xialab.ca/api/download/metaboanalyst/cress_time1.csv");
            wb.setMetaName("https://api2.xialab.ca/api/download/metaboanalyst/cress_time1_meta.csv");

            tub.setTimeDataOpt(testDataOpt);
            handleTestDataUpload_mf();
        } else {
            handleTestDataUpload_roc();
        }
        return null;
    }

    public String handleTestDataUpload_roc() {
        String res = handleRocTestFileUpload();
        if (res != null) {
            uploadInfo = "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String handleRocTestFileUpload() {
        if (!wb.isReloadingWorkflow()) {
            if (!sb.doLogin("conc", "stat", false, false)) {
                return null;
            }
        }

        RConnection RC = sb.getRConnection();
        if (upb.getDataOpt().equals("data1")) {
            RDataUtils.readTextData(RC, ab.getResourceByAPI("plasma_nmr.csv"), "rowu", "disc");
            wb.setDataName(ab.getResourceByAPI("plasma_nmr.csv"));

        } else {
            RDataUtils.readTextData(RC, ab.getResourceByAPI("plasma_nmr_new.csv"), "rowu", "disc");
            wb.setDataName(ab.getResourceByAPI("plasma_nmr_new.csv"));
        }

        wb.setDataFormat("rowu");
        wb.setDataType("conc");
        wb.setLblType("disc");

        sb.setDataUploaded();
        return "Data check";
    }

    public String handleDoseTestFileUpload() {
        String res = upb.handleDoseTestFileUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public void handleOraListUpload() {
        String res = eub.handleOraListUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
    }

    public String handleSspDataUpload() {
        String res = eub.handleSspDataUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String handleQeaDataUpload() {
        String res = eub.handleQeaDataUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String msetQeaTestBn_action() {
        String res = eub.msetQeaTestBn_action();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            pcb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String navigateToEnrichUpload() {
        wb.setReturnType("individual");
        String url = switch (enrichOpt) {
            case "ora" ->
                "WfEnrichUploadView";
            case "ssp" ->
                "WfSspUploadView";
            default ->
                "WfQeaUploadView";
        };
        return (url);
    }

    public void loadEnrichExample() {

        String res;
        switch (enrichOpt) {
            case "ora" -> {
                eub.setExampleType("met");
                eub.updateOraArea();
                res = eub.handleOraListUpload();
            }
            case "ssp" -> {
                eub.setUseMsetSspExample(true);
                res = eub.handleSspDataUpload();
            }
            default -> {
                res = msetQeaTestBn_action();
            }
        };

    }

    public String navigateToPathUpload() {
        String url = switch (enrichOpt) {
            case "list" ->
                "WfPathUploadView";
            default ->
                "WfPathTableUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadPathExample() {
        String res = "";
        switch (pathOpt) {
            case "list" -> {
                pub.setUsePathListExample(true);
                pub.handlePathListUpload();
            }
            default -> {
                pub.setUsePathListExample(true);
                pub.pathQeaExampleBn_action();
            }
        };

    }

    public String handlePathListUpload() {
        String res = pub.handlePathListUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String pathQeaExampleBn_action() {
        String res = pub.pathQeaExampleBn_action();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String pathQeaBn_action() {
        String res = pub.pathQeaBn_action();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public void sspNextBn_action() {
        mapb.sspNextBn_action();
        wb.setActiveIndex(3);
    }

    public void skipButton_action_default(int index) {
        String res = pcb.skipButton_action_default();
        if (res != null) {
            wb.settingActiveIndex(index);
        } else {
            sb.addMessage("error", "Name mapping failed!");
        }
    }

    public void prepareNetworkData() {
        String res = mntb.prepareNetworkData();
        if (res != null) {
            //wb.setActiveIndex(2);
            wb.finishMultiPreparation("Gene List");
        } else {
            //sb.addMessage("error", "Name mapping failed!");
        }
    }

    public String navigateToNetUpload() {
        String url = switch (networkOpt) {
            case "list" ->
                "WfNetUploadView";
            default ->
                "WfNetTableUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadNetExample() {

        String res = "";
        switch (networkOpt) {
            case "list" -> {
                mnlb.setExampleInputList("metabogene");
                mnlb.updateListArea();
                mnlb.integrityCheck();
            }
            default -> {
                mnlb.performExampleDspc();
            }
        };

    }

    public String handleMnetDataUpload() {
        String res = mnlb.handleMnetDataUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String performExampleDspc() {
        String res = mnlb.performExampleDspc();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String integrityCheck() {
        String res = mnlb.integrityCheck();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String navigateToMumUpload() {
        String url = switch (mumOpt) {
            case "list" ->
                "WfMumUploadView";
            default ->
                "WfMumTableUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadMumExample() {
        String res = "";
        switch (mumOpt) {
            case "list" -> {
                pkub.processListExampleUpload();
            }
            default -> {
                pkub.setTableDataOpt("table_ibd");
                pkub.processTableExampleUpload();
            }
        };

    }

    public String processListExampleUpload() {
        String res = pkub.processListExampleUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String handleMassAllUpload() {
        String res = pkub.processListExampleUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String handleMassAllUploadTable() {
        String res = pkub.handleMassAllUploadTable();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String processTableExampleUpload() {
        String res = pkub.processTableExampleUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String navigateToStatUpload() {
        String url;

        switch (statOpt) {
            case "table" ->
                url = "WfStatUploadView";
            case "mztab" -> {
                url = "WfStatMztabUploadView";
                upb.setTestDataOpt("mztabmouse");
            }
            default -> {
                url = "WfStatZipUploadView";
                upb.setTestDataOpt("nmrpeaklist");
            }
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadStatExample() {
        String res = "";
        switch (statOpt) {
            case "table" -> {
                upb.setTestDataOpt("conccancer");
                res = upb.handleStatTestFileUpload();
            }
            case "mztabmouse" -> {
                upb.setTestDataOpt("mztabmouse");
                res = upb.handleStatTestFileUpload();
            }
            default -> {
                upb.setTestDataOpt("nmrpeaklist");
                res = upb.handleStatTestFileUpload();
            }
        };

    }

    public void msPeakNextBn_action(int index) {
        pcb.performSanityCheck();
        sb.setIntegChecked();

        wb.setActiveIndex(index);

    }

    public void nmrNextBn_action(int index) {
        pcb.performSanityCheck();
        sb.setIntegChecked();

        wb.setActiveIndex(index);
    }

    public void handleZipFileUpload() {
        String res = upb.handleZipFileUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
    }

    public void handleStatTestFileUpload() {
        String res = upb.handleStatTestFileUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
    }

    public String navigateToMfUpload() {
        String url = "WfUploadView";
        wb.setReturnType("individual");

        return (url);
    }

    public void loadMfExample() {
        tub.handleTestDataUpload();
    }

    public String navigateToSpecUpload() {
        String url = switch (specUpOpt) {
            case "googledrive" ->
                "WfSpecGoogleUploadView";
            default ->
                "WfSpecUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public String handleRawSpecAllUploadTable() {
        String res = null;
        try {
            res = spub.goToProcessing();
        } catch (REXPMismatchException ex) {
            Logger.getLogger(WfUploadBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        if (!"".equals(res)) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String processSpecExampleUpload() {
        String res = null;
        try {
            //sub.setSelectedExample("malaria");
            res = spub.uploadExampleSpectra();
        } catch (Exception ex) {
            Logger.getLogger(WfUploadBean.class.getName()).log(Level.SEVERE, null, ex);
        }

        if (sppb.isIsms2DIA()) {
            sppb.prepareDIASpec();
        } else {
            sppb.prepareSpecProc();
        }
        spcb.goToJobStatus(false);
        //System.out.println("===== processSpecExampleUpload res ===== >" + res);
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String processSpecDataUpload() {
        String res = spub.processSaintyCheck();
        if (res.equals("Spectra check")) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
            spub.setAllowGoogleDriveContinue(true);
        }
        return null;
    }

    public void skipButton_action_spec(int index) {
        //ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
        //String res = pb.skipButton_action_default();
        String res = sppb.prepareSpecProc();
        if (index == 2) {
            if (sppb.isIsms2DIA()) {
                sppb.prepareDIASpec();
            }
        }
        if (index == 3) {
            if (sppb.isIsms2DIA()) {
                sppb.prepareDIASpecProc();
            }
        }
        wb.settingActiveIndex(index);
        if (res != null) {
            if (index == -1) {
                wb.finishMultiPreparation("LC-MS Spectra");
            } else {
                wb.settingActiveIndex(index);
            }
        } else {
            sb.addMessage("error", "Spectra processing failed!");
        }
    }

    public void navigateToTableUpload() {
        dv.resetDiagram();
        wb.setActiveIndex(0);

        switch (tableOpt) {
            case "compound" ->
                dv.navToPage("Compound Table");
            case "peak" ->
                dv.navToPage("Peak Table");
            default ->
                dv.navToPage("Generic Table");
        }
    }

    public void navigateToListUpload() {

        dv.resetDiagram();
        wb.setActiveIndex(0);

        switch (listOpt) {
            case "compound" ->
                dv.navToPage("Metabolite List");
            case "peak" ->
                dv.navToPage("Peak List");
            case "gene" ->
                dv.navToPage("Gene List");
            default -> {
            }
        }
    }

    public void navigateToMultiTableUpload() {
        dv.resetDiagram();
        wb.setActiveIndex(0);

        if (multiTableOpt.equals("peak")) {
            dv.navToPage("Peak Tables");
        } else {
            dv.navToPage("Generic Tables");
        }
    }

    public String doDefaultMetaAnalysis() {
        mlb.doDefaultMetaAnalysis();
        //if (res != null) {
        uploadInfo = uploadInfo + "<br/>Upload and processing successful! Please click the <b>Proceed</b> button to the next step.";
        sb.addMessage("Info", "Data has been uploaded successfully");
        fileUploaded = true;
        //}
        return null;
    }

    public void performMetaIntegrityCheck() {
        mlb.performMetaIntegrityCheck();
        ArrayList<String> msgVec = new ArrayList();

        if (mlb.isAllDataConsistent()) {
            RConnection RC = sb.getRConnection();
            String[] msgArray = RDataUtils.getSanityCheckMessage(RC);
            RCenter.recordMessage(RC, "Data integrity check - <b>passed</b>");
            msgVec.addAll(Arrays.asList(msgArray));
            String msg;
            msg = "<table face=\"times\" size = \"3\">";
            if (!sb.getAnalType().equals("pathinteg")) {
                msg = msg + "<tr><th> Data processing information: " + "</th></tr>";
            }
            for (int i = 0; i < msgVec.size(); i++) {
                msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
            }
            msg = msg + "</table>";

            pcb.setMsgText(msg);
            //wb.setActiveIndex(1);
        }
    }

    public void doDefaultMetaPathAnalysis() {
        mplb.doDefaultMetaAnalysis();
        //System.out.println("metaDefault==1");
        //if (res != null) {
        uploadInfo = uploadInfo + "<br/>Upload and processing successful! Please click the <b>Proceed</b> button to the next step.";
        sb.addMessage("Info", "Data has been uploaded successfully");
        fileUploaded = true;

    }

    public void performMetaPathIntegrityCheck() {
        mplb.performMetaIntegrityCheck();
        ArrayList<String> msgVec = new ArrayList();

        if (mplb.isAllDataConsistent()) {
            RConnection RC = sb.getRConnection();
            String[] msgArray = RDataUtils.getSanityCheckMessage(RC);
            RCenter.recordMessage(RC, "Data integrity check - <b>passed</b>");
            msgVec.addAll(Arrays.asList(msgArray));
            String msg;
            msg = "<table face=\"times\" size = \"3\">";
            if (!sb.getAnalType().equals("pathinteg")) {
                msg = msg + "<tr><th> Data processing information: " + "</th></tr>";
            }
            for (int i = 0; i < msgVec.size(); i++) {
                msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
            }
            msg = msg + "</table>";

            pcb.setMsgText(msg);
            //wb.setActiveIndex(1);
        }
    }

    public void confirmAllData() {
        mplb.confirmAllData();
        wb.settingActiveIndex(1);
    }

    public void proceedToNorm(int tabInx) {

        pcb.skipButton_action_default();
        wb.settingActiveIndex(tabInx);
    }
}
