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
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
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

        String fileName = DataUtils.uploadFile(csvFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
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
                metaName = DataUtils.uploadFile(metaFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
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
        TimeUploadBean tb = (TimeUploadBean) DataUtils.findBean("tsuploader");
        String res = tb.handleTestDataUpload();
        if (res != null) {
            uploadInfo = "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.performSanityCheck();
            fileUploaded = true;

        }
        return null;
    }

    public String handleTestDataUpload_table() {
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");
        if (ub.getDataOpt().equals("pkcovid")) {
            TimeUploadBean tb = (TimeUploadBean) DataUtils.findBean("tsuploader");
            wb.setDataName("https://api2.xialab.ca/api/download/metaboanalyst/cress_time1.csv");
            wb.setMetaName("https://api2.xialab.ca/api/download/metaboanalyst/cress_time1_meta.csv");

            tb.setTimeDataOpt(testDataOpt);
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
            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.performSanityCheck();
            fileUploaded = true;

        }
        return null;
    }

    public String handleRocTestFileUpload() {
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");
        if (!wb.isReloadingWorkflow()) {
            if (!sb.doLogin("conc", "stat", false, false)) {
                return null;
            }
        }

        RConnection RC = sb.getRConnection();
        if (ub.getDataOpt().equals("data1")) {
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
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");
        String res = ub.handleDoseTestFileUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public void handleOraListUpload() {
        System.out.println("====================handleOraListUpload");
        EnrichUploadBean ub = (EnrichUploadBean) DataUtils.findBean("enrichLoader");
        String res = ub.handleOraListUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
    }

    public String handleSspDataUpload() {
        EnrichUploadBean ub = (EnrichUploadBean) DataUtils.findBean("enrichLoader");
        String res = ub.handleSspDataUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String handleQeaDataUpload() {
        EnrichUploadBean ub = (EnrichUploadBean) DataUtils.findBean("enrichLoader");
        String res = ub.handleQeaDataUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String msetQeaTestBn_action() {
        EnrichUploadBean ub = (EnrichUploadBean) DataUtils.findBean("enrichLoader");
        String res = ub.msetQeaTestBn_action();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.performSanityCheck();
            fileUploaded = true;
        }
        return null;
    }

    public String navigateToEnrichUpload() {
        System.out.println("enrichnav===========");
        String url = "";
        url = switch (enrichOpt) {
            case "ora" ->
                "WfEnrichUploadView";
            case "ssp" ->
                "WfSspUploadView";
            default ->
                "WfQeaUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadEnrichExample() {
        EnrichUploadBean ub = (EnrichUploadBean) DataUtils.findBean("enrichLoader");
        String res = "";
        switch (enrichOpt) {
            case "ora" -> {
                ub.setExampleType("met");
                ub.updateOraArea();
                res = ub.handleOraListUpload();
            }
            case "ssp" -> {
                ub.setUseMsetSspExample(true);
                res = ub.handleSspDataUpload();
            }
            default -> {
                res = msetQeaTestBn_action();
            }
        };

    }

    public String navigateToPathUpload() {
        String url = "";
        url = switch (enrichOpt) {
            case "list" ->
                "WfPathUploadView";
            default ->
                "WfPathTableUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadPathExample() {
        PathUploadBean pb = (PathUploadBean) DataUtils.findBean("pathLoader");
        String res = "";
        switch (pathOpt) {
            case "list" -> {
                pb.setUsePathListExample(true);
                pb.handlePathListUpload();
            }
            default -> {
                pb.setUsePathListExample(true);
                pb.pathQeaExampleBn_action();
            }
        };

    }

    public String handlePathListUpload() {
        PathUploadBean pb = (PathUploadBean) DataUtils.findBean("pathLoader");
        String res = pb.handlePathListUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String pathQeaExampleBn_action() {
        PathUploadBean pb = (PathUploadBean) DataUtils.findBean("pathLoader");
        String res = pb.pathQeaExampleBn_action();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String pathQeaBn_action() {
        PathUploadBean pb = (PathUploadBean) DataUtils.findBean("pathLoader");
        String res = pb.pathQeaBn_action();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public void sspNextBn_action() {
        MappingBean mapb = (MappingBean) DataUtils.findBean("mapBean");
        mapb.sspNextBn_action();
        wb.setActiveIndex(3);
    }

    public void skipButton_action_default(int index) {
        ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
        String res = pb.skipButton_action_default();
        if (res != null) {
            wb.settingActiveIndex(index);
        } else {
            sb.addMessage("error", "Name mapping failed!");
        }
    }

    public void prepareNetworkData() {
        MnetMapBean net = (MnetMapBean) DataUtils.findBean("mnetMapBean");
        String res = net.prepareNetworkData();
        if (res != null) {
            //wb.setActiveIndex(2);
            wb.finishMultiPreparation("Gene List");
        } else {
            //sb.addMessage("error", "Name mapping failed!");
        }
    }

    public String navigateToNetUpload() {
        String url = "";
        url = switch (networkOpt) {
            case "list" ->
                "WfNetUploadView";
            default ->
                "WfNetTableUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadNetExample() {
        MnetLoadBean net = (MnetLoadBean) DataUtils.findBean("mnetLoader");
        String res = "";
        switch (networkOpt) {
            case "list" -> {
                net.setExampleInputList("metabogene");
                net.updateListArea();
                net.integrityCheck();
            }
            default -> {
                net.performExampleDspc();
            }
        };

    }

    public String handleMnetDataUpload() {
        MnetLoadBean net = (MnetLoadBean) DataUtils.findBean("mnetLoader");
        String res = net.handleMnetDataUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String performExampleDspc() {
        MnetLoadBean net = (MnetLoadBean) DataUtils.findBean("mnetLoader");
        String res = net.performExampleDspc();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String integrityCheck() {
        MnetLoadBean net = (MnetLoadBean) DataUtils.findBean("mnetLoader");
        String res = net.integrityCheck();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String navigateToMumUpload() {
        String url = "";
        url = switch (mumOpt) {
            case "list" ->
                "WfMumUploadView";
            default ->
                "WfMumTableUploadView";
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadMumExample() {
        PeakUploadBean pk = (PeakUploadBean) DataUtils.findBean("peakLoader");
        String res = "";
        switch (mumOpt) {
            case "list" -> {
                pk.processListExampleUpload();
            }
            default -> {
                pk.setTableDataOpt("table_ibd");
                pk.processTableExampleUpload();
            }
        };

    }

    public String processListExampleUpload() {
        PeakUploadBean pk = (PeakUploadBean) DataUtils.findBean("peakLoader");
        String res = pk.processListExampleUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String handleMassAllUpload() {
        PeakUploadBean pk = (PeakUploadBean) DataUtils.findBean("peakLoader");
        String res = pk.processListExampleUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String handleMassAllUploadTable() {
        PeakUploadBean pk = (PeakUploadBean) DataUtils.findBean("peakLoader");
        String res = pk.handleMassAllUploadTable();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String processTableExampleUpload() {
        PeakUploadBean pk = (PeakUploadBean) DataUtils.findBean("peakLoader");
        String res = pk.processTableExampleUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String navigateToStatUpload() {
        String url = "";
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");

        switch (statOpt) {
            case "table" ->
                url = "WfStatUploadView";
            case "mztab" -> {
                url = "WfStatMztabUploadView";
                ub.setTestDataOpt("mztabmouse");
            }
            default -> {
                url = "WfStatZipUploadView";
                ub.setTestDataOpt("nmrpeaklist");
            }
        };
        wb.setReturnType("individual");

        return (url);
    }

    public void loadStatExample() {
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");
        String res = "";
        switch (statOpt) {
            case "table" -> {
                ub.setTestDataOpt("conccancer");
                res = ub.handleStatTestFileUpload();
            }
            case "mztabmouse" -> {
                ub.setTestDataOpt("mztabmouse");
                res = ub.handleStatTestFileUpload();
            }
            default -> {
                ub.setTestDataOpt("nmrpeaklist");
                res = ub.handleStatTestFileUpload();
            }
        };

    }

    public void msPeakNextBn_action(int index) {
        ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
        sb.setDataProcessed(true);
        pb.performSanityCheck();

        wb.setActiveIndex(index);

    }

    public void nmrNextBn_action(int index) {
        ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
        sb.setDataProcessed(true);
        pb.performSanityCheck();

        wb.setActiveIndex(index);
    }

    public void handleZipFileUpload() {
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");
        String res = ub.handleZipFileUpload();
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
    }

    public void handleStatTestFileUpload() {
        UploadBean ub = (UploadBean) DataUtils.findBean("uploader");
        String res = ub.handleStatTestFileUpload();
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
        TimeUploadBean tb = (TimeUploadBean) DataUtils.findBean("tsuploader");
        tb.handleTestDataUpload();
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
        SpectraUploadBean sub = (SpectraUploadBean) DataUtils.findBean("specLoader");
        String res = null;
        try {
            res = sub.goToProcessing();
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
        SpectraUploadBean sub = (SpectraUploadBean) DataUtils.findBean("specLoader");
        String res = null;
        try {
            //sub.setSelectedExample("malaria");
            res = sub.uploadExampleSpectra();
        } catch (Exception ex) {
            Logger.getLogger(WfUploadBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        SpectraProcessBean sp = (SpectraProcessBean) DataUtils.findBean("spectraProcessor");
        SpectraControlBean sc = (SpectraControlBean) DataUtils.findBean("spectraController");
        if (sp.isIsms2DIA()) {
            sp.prepareDIASpec();
        } else {
            sp.prepareSpecProc();
        }
        sc.goToJobStatus(false);
        System.out.println("===== processSpecExampleUpload res ===== >" + res);
        if (res != null) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
        }
        return null;
    }

    public String processSpecDataUpload() {
        SpectraUploadBean slb = (SpectraUploadBean) DataUtils.findBean("specLoader");
        String res = slb.processSaintyCheck();
        if (res.equals("Spectra check")) {
            uploadInfo = uploadInfo + "<br/>Upload successful! Please click the <b>Proceed</b> button to the next step.";
            sb.addMessage("Info", "Data has been uploaded successfully");
            fileUploaded = true;
            slb.setAllowGoogleDriveContinue(true);
        }
        return null;
    }

    public void skipButton_action_spec(int index) {
        //ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
        //String res = pb.skipButton_action_default();
        SpectraProcessBean spb = (SpectraProcessBean) DataUtils.findBean("spectraProcessor");
        String res = spb.prepareSpecProc();
        if (index == 2) {
            if (spb.isIsms2DIA()) {
                spb.prepareDIASpec();
            }
        }
        if (index == 3) {
            if (spb.isIsms2DIA()) {
                spb.prepareDIASpecProc();
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
        DiagramView dv = (DiagramView) DataUtils.findBean("diagramView");
        dv.resetDiagram();
        wb.setActiveIndex(0);

        if (tableOpt.equals("compound")) {
            dv.navToPage("Compound Table");
        } else if (tableOpt.equals("peak")) {
            dv.navToPage("Peak Table");
        } else {
            dv.navToPage("Generic Table");
        }
    }

    public void navigateToListUpload() {
        DiagramView dv = (DiagramView) DataUtils.findBean("diagramView");
        dv.resetDiagram();
        wb.setActiveIndex(0);

        if (listOpt.equals("compound")) {
            dv.navToPage("Metabolite List");
        } else if (listOpt.equals("peak")) {
            dv.navToPage("Peak List");
        } else if (listOpt.equals("gene")) {
            dv.navToPage("Gene List");
        }
    }

    public void navigateToMultiTableUpload() {
        DiagramView dv = (DiagramView) DataUtils.findBean("diagramView");
        dv.resetDiagram();
        wb.setActiveIndex(0);

        if (multiTableOpt.equals("peak")) {
            dv.navToPage("Peak Tables");
        } else {
            dv.navToPage("Generic Tables");
        }
    }

    public String doDefaultMetaAnalysis() {
        MetaLoadBean lb = (MetaLoadBean) DataUtils.findBean("loadBean");
        lb.doDefaultMetaAnalysis();
        //if (res != null) {
        uploadInfo = uploadInfo + "<br/>Upload and processing successful! Please click the <b>Proceed</b> button to the next step.";
        sb.addMessage("Info", "Data has been uploaded successfully");
        fileUploaded = true;
        //}
        return null;
    }

    public void performMetaIntegrityCheck() {
        MetaLoadBean lb = (MetaLoadBean) DataUtils.findBean("loadBean");
        lb.performMetaIntegrityCheck();
        ArrayList<String> msgVec = new ArrayList();

        if (lb.isAllDataConsistent()) {
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

            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.setMsgText(msg);
            //wb.setActiveIndex(1);
        }
    }

    public String doDefaultMetaPathAnalysis() {
        System.out.println("metaDefault==0");
        MetaPathLoadBean lb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");
        lb.doDefaultMetaAnalysis();
        System.out.println("metaDefault==1");
        //if (res != null) {
        uploadInfo = uploadInfo + "<br/>Upload and processing successful! Please click the <b>Proceed</b> button to the next step.";
        sb.addMessage("Info", "Data has been uploaded successfully");
        fileUploaded = true;
        //}
        return null;
    }

    public void performMetaPathIntegrityCheck() {
        MetaPathLoadBean lb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");
        lb.performMetaIntegrityCheck();
        ArrayList<String> msgVec = new ArrayList();

        if (lb.isAllDataConsistent()) {
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

            ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
            pb.setMsgText(msg);
            //wb.setActiveIndex(1);
        }
    }

    public void confirmAllData() {
        MetaPathLoadBean lb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");
        lb.confirmAllData();
        wb.settingActiveIndex(1);
    }

    public void proceedToNorm(int tabInx) {
        NormBean normBean = (NormBean) DataUtils.findBean("normBean");
        ProcessBean pb = (ProcessBean) DataUtils.findBean("procBean");
        pb.skipButton_action_default();
        normBean.preparePrenormData();

        wb.settingActiveIndex(tabInx);
    }
}
