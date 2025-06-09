/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.meta;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.ObjectMapper;
import pro.metaboanalyst.models.DataModel;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import pro.metaboanalyst.models.User;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RMetaUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.file.UploadedFile;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;
import jakarta.inject.Inject;
import java.util.HashMap;
import java.util.Map;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ProcessBean;

/**
 *
 * @author jianguox
 */
@SessionScoped
@Named("loadBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class MetaLoadBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private ProcessBean pcb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    /**
     * Record the currently selected data
     */
    private int dataNum = 1;
    private String procMsg = "";

    public String getProcMsg() {
        return procMsg;
    }

    public void setProcMsg(String procMsg) {
        this.procMsg = procMsg;
    }

    public int getDataNum() {
        return dataNum;
    }

    public void setDataNum(int dataNum) {
        this.dataNum = dataNum;
    }

    private String dataFormat = "colu";

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    private String selectedTestData = "Test1";

    public String getSelectedTestData() {
        return selectedTestData;
    }

    public void setSelectedTestData(String selectedTestData) {
        this.selectedTestData = selectedTestData;
    }

    private DataModel selectedData;

    public DataModel getSelectedData() {
        return selectedData;
    }

    public void setSelectedData(DataModel selectedData) {
        this.selectedData = selectedData;
    }

    public DataModel getData4Vis() {
        return selectedData;
    }

    public void setData4Vis(DataModel selectedData) {
        this.selectedData = selectedData;
        selectedData.plotDataProfile();
    }

    private boolean allDataConsistent = false;

    public boolean isAllDataConsistent() {
        return allDataConsistent;
    }

    public void setAllDataConsistent(boolean allDataConsistent) {
        this.allDataConsistent = allDataConsistent;
    }
    private String integCheckMsg;

    public String getIntegCheckMsg() {
        return integCheckMsg;
    }

    private String analMethod = "metap";

    public String getAnalMethod() {
        return analMethod;
    }

    public void setAnalMethod(String analMethod) {
        this.analMethod = analMethod;
    }

    private DataModel selectedDataSet;

    public DataModel getSelectedDataSet() {
        return selectedDataSet;
    }

    public void setSelectedDataSet(DataModel dm) {
        if (!selectedDataSet.getName().equals(dm.getName())) {
            this.selectedDataSet = dm;
            RMetaUtils.setCurrentData(sb.getRConnection(), selectedDataSet.getName());
            sb.addMessage("info", "Current data is: " + selectedDataSet.getName() + ", ready for analysis.");
        }
    }

    private final Map<String, Integer> currentDeNumMap = new HashMap<>();

    /**
     * Returns the current DE number for a given key (e.g. sample ID), or 0 if
     * none set yet.
     */
    public int getCurrentDeNum(String key) {
        return currentDeNumMap.getOrDefault(key, 0);
    }

    /**
     * Sets the current DE number for a given key.
     */
    public void setCurrentDeNum(String key, int value) {
        currentDeNumMap.put(key, value);
    }

    private boolean statsOnlyMode = false;

    public boolean isStatsOnlyMode() {
        return statsOnlyMode;
    }

    public void setStatsOnlyMode(boolean statsOnlyMode) {
        this.statsOnlyMode = statsOnlyMode;
    }

    public void addData() {
        addNewData("Upload");
    }

    public void deleteData() {
        deleteData(selectedData);
    }

    /**
     * Handle file upload Use simple mode, as other fancy mode does not work in
     * all browsers (IE)
     */
    private UploadedFile file;

    public UploadedFile getFile() {
        return file;
    }
    private int count;

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public void increment() {
        count++;
    }

    public void setFile(UploadedFile file) {
        this.file = file;
    }

    private void checkLogIn() {
        if (!loggedIn) {
            sb.doLogin("conc", "metadata", false, false);
            RConnection RC = sb.getRConnection();

            //need to update all dataSets created for meta and multi
            dataSets = getDataSets();
            for (int i = 0; i < dataSets.size(); i++) {
                dataSets.get(i).setRC(RC);
            }
            loggedIn = true;
        }

    }
    private boolean loggedIn = false;

    public boolean isLoggedIn() {
        return loggedIn;
    }

    public void setLoggedIn(boolean loggedIn) {
        this.loggedIn = loggedIn;
    }

    public boolean handleFileUpload() {

        if (file == null || file.getSize() == 0) {
            sb.addMessage("error", "Empty file?");
            return false;
        }

        if (ab.isOnProServer()) { // size limit will apply only on public server
            if (file.getSize() > ab.getMAX_UPLOAD_SIZE()) {
                sb.addMessage("error", "The file size exceeds limit:" + ab.getMAX_UPLOAD_SIZE());
                file = null;
                return false;
            }
        }

        String fileName = file.getFileName();
        if (!fileName.endsWith(".csv") && !fileName.endsWith(".txt")) {
            sb.addMessage("error", "Only .txt or .csv file is acceptable!");
            file = null;
            return false;
        }

        checkLogIn();

        fileName = DataUtils.uploadFile(sb, file, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
        int res = RMetaUtils.readIndExpressTable(sb.getRConnection(), fileName, dataFormat);
        if (res == 0) {
            String errMsg = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("error", errMsg);
            //remove the file
            file = null;
            return false;
        } else {
            sb.addMessage("info", fileName + " is uploaded and parsed out." + RDataUtils.getCurrentMsg(sb.getRConnection()));
            selectedData.setUploaded(true);
            selectedData.setName(fileName);
            sb.setDataUploaded(true);
            selectedData.processMetaData();
            file = null;
            return true;
        }

    }

    private boolean adjustBatch = false;

    public boolean isAdjustBatch() {
        return adjustBatch;
    }

    public void setAdjustBatch(boolean adjustBatch) {
        this.adjustBatch = adjustBatch;
    }

    public void performMetaIntegrityCheck() {
        allDataConsistent = false;
        //first get datasets
        ArrayList<String> selectedDataSets = getCurrentDataSets();
        integCheckMsg = "";

        if (selectedDataSets.isEmpty()) {
            integCheckMsg = "Integrity Check Failed. No data was found.";
            return;
        }

        //check if the summary view is checked
        for (String var : selectedDataSets) {
            DataModel dm = getData(var);
            setStatsOnlyMode(true);

            if (!dm.isAllDone()) {
                integCheckMsg = "Integrity Check Failed. Please specify or confirm group comparison for data: " + dm.getFullName()
                        + " using the <b>View</b> option under <u>Data Summary</u>";
                return;
            }

            // dataSets.add(dm);
        }

        if (RDataUtils.setSelectedDataNames(sb.getRConnection(), selectedDataSets.toArray(new String[0])) == 0) {
            integCheckMsg = "Integrity Check Failed. Cannot locate the selected data!";
            return;
        }
        int res = RMetaUtils.checkMetaDataConsistency(sb.getRConnection(), "F");
        switch (res) {
            case 0 ->
                integCheckMsg = "Integrity Check Failed. " + RDataUtils.getErrMsg(sb.getRConnection());
            case -1 ->
                integCheckMsg = "Integrity Check Failed. " + RDataUtils.getErrMsg(sb.getRConnection()) + ". You can use the Annotation "
                        + "cell to edit group labels.";
            default -> {
                allDataConsistent = true;
                if (res == 1) {
                    setSingleMode(false);
                    integCheckMsg = "OK, all datasets passed intergrity check. "
                            + "Click <b>Next</b> button to next page.";
                } else {
                    setSingleMode(true);
                    integCheckMsg = "OK, the dataset passed intergrity check. "
                            + "Click <b>Next</b> button to next page. "
                            + "Note, only a single dataset is found. "
                            + "You can only perform regular differential expression analysis "
                            + "using <b><u>Direct Merging</u></b> method.";
                }
                RDataUtils.plotMetaPCA(sb, sb.getRConnection(), sb.getNewImage("qc_meta_pca"), 150, "png");
                //RDataUtils.plotMetaDensity(sb, sb.getRConnection(), sb.getNewImage("qc_meta_pca"), 150, "png");
            }
        }
        procMsg = RDataUtils.getMetaProcMsg(sb.getRConnection());
        sb.setDataUploaded();
        sb.setDataNormed();
        //meta analysis default all selected        
        RDataUtils.setSelectedDataNames(sb.getRConnection(), selectedDataSets.toArray(String[]::new));
        setDataNum(selectedDataSets.size());
    }

    //do default analysis for testing purpose
    public void doDefaultMetaAnalysis() {

        checkLogIn();

        User currentUser = sb.getCurrentUser();
        List<DataModel> metaDataSets = getDataSets();

        //reset data 
        metaDataSets.clear();
        String format = "colu";

        //first data
        String inpath = ab.getResourceByAPI("data1.csv");
        String name = DataUtils.getJustFileName(inpath);
        String outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.fetchFile(inpath, new File(outpath));
        addNewData("Upload");
        RMetaUtils.readIndExpressTable(sb.getRConnection(), name, format);
        selectedData = metaDataSets.get(0);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        //2nd data
        inpath = ab.getResourceByAPI("data2.csv");
        name = DataUtils.getJustFileName(inpath);
        outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.fetchFile(inpath, new File(outpath));
        addNewData("Upload");
        RMetaUtils.readIndExpressTable(sb.getRConnection(), name, format);

        selectedData = metaDataSets.get(1);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        //3rd data
        inpath = ab.getResourceByAPI("data3.csv");
        name = DataUtils.getJustFileName(inpath);
        outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.fetchFile(inpath, new File(outpath));
        addNewData("Upload");
        RMetaUtils.readIndExpressTable(sb.getRConnection(), name, format);

        selectedData = metaDataSets.get(2);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        //
        inpath = ab.getResourceByAPI("data4.csv");
        name = DataUtils.getJustFileName(inpath);
        outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.fetchFile(inpath, new File(outpath));
        addNewData("Upload");
        RMetaUtils.readIndExpressTable(sb.getRConnection(), name, format);

        selectedData = metaDataSets.get(3);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        sb.addMessage("info", "Three datasets were uploaded and processed on the server.");

        sb.setDataNormed();
    }

    public void perfromDefaultMetaProcess(DataModel dm) {
        dm.setUploaded(true);
        dm.processMetaData();
        dm.setProcessed(true);
        dm.setNormOpt("log");
        dm.setAutoscaleOpt(true);
        dm.performNormalization();
        dm.setNormed(true);

        //DE
        dm.setSigLevel(0.05);
        dm.setFcLevel(0.0);
        dm.performDEAnalysis();
        dm.setAnalDone(true);
        dm.setAllDone();
        int featureNum = dm.getGeneNum();
        int sampleNum = dm.getSmplNum();
        String groupinfo = dm.getGroupInfo();

        if (pcb.getDataSets() == null || pcb.getDataSets().isEmpty()) {
            pcb.setDataSets(new ArrayList());
        }
        DataModel ds = new DataModel(sb, dm.getName());
        ds.setSmplNum(sampleNum);
        ds.setGeneNum(featureNum);
        ds.setGroupInfo(groupinfo);
        ds.setName(dm.getName());
        ds.setDataName(dm.getFullName());
        pcb.getDataSets().add(ds);

    }

    //record data in each mode
    private List<DataModel> dataSets;
    private List<DataModel> mDataSets; //the contains meta

    public List<DataModel> getDataSets() {
        if (dataSets == null) {
            dataSets = new ArrayList();
            dataSets.add(new DataModel(sb, "Upload"));
        }
        return dataSets;
    }

    public List<DataModel> getMetaDataSets() {
        if (mDataSets == null) {
            mDataSets = new ArrayList();
            for (int i = 0; i < dataSets.size(); i++) {
                mDataSets.add(dataSets.get(i));
            }
            //now add the meta-dataSet
            DataModel dm = new DataModel(sb, "meta_dat");
            dm.setDeNum(getCurrentDeNum(analMethod));
            mDataSets.add(dm);
        }
        return mDataSets;
    }

    private List<DataModel> dataSetsInfo;

    public List<DataModel> getDataSetsInfo() {
        if (dataSetsInfo == null || dataSetsInfo.isEmpty()) {
            dataSetsInfo = new ArrayList();
        }
        return dataSetsInfo;
    }

    public void setDataSetsInfo(List<DataModel> dataSetsInfo) {
        this.dataSetsInfo = dataSetsInfo;
    }

    public DefaultStreamedContent getMergedFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/MetaboAnalyst_merged_data.csv");
    }

    public DefaultStreamedContent getMetaResFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/meta_sig_features_" + analMethod + ".csv");
    }

    @SuppressWarnings("unchecked")
    public void setDataSets(List<?> dataSets) {

        if (dataSets == null) {
            this.dataSets = null;
            return;
        }

        List<DataModel> converted = new ArrayList<>(dataSets.size());

        for (Object obj : dataSets) {
            if (obj instanceof DataModel) {
                converted.add((DataModel) obj);                 // already OK
            } else if (obj instanceof Map) {
                converted.add(MAPPER.convertValue(obj, DataModel.class)); // LinkedHashMap → POJO
            } else {
                throw new IllegalArgumentException(
                        "Unsupported element type: " + obj.getClass());
            }
        }
        this.dataSets = converted;
    }
    private static final ObjectMapper MAPPER = new ObjectMapper();

    //only meta-analysis
    public void addNewData(String dataName) {
        dataSets.add(new DataModel(sb, dataName));
    }

    public void deleteData(DataModel selectedData) {
        dataSets.remove(selectedData);
        //remove data from R inmex.vec 
        RMetaUtils.removeData(sb.getRConnection(), selectedData.getFullName());
        String homeDir = sb.getCurrentUser().getHomeDir();
        String fileName = selectedData.getFullName();
        File rmFile = new File(homeDir + File.separator + fileName);
        rmFile.delete();

        sb.addMessage("info", "The selected data entry is deleted");
    }

    public ArrayList<String> getCurrentDataSets() {
        ArrayList<String> currentDataNms = new ArrayList();
        for (int i = 0; i < dataSets.size(); i++) {
            if (dataSets.get(i).isInclude()) {
                currentDataNms.add(dataSets.get(i).getFullName());
            }
        }
        return currentDataNms;
    }

    //see if only single dataset to be analyzed
    private boolean singleMode = false;

    public boolean isSingleMode() {
        return singleMode;
    }

    public void setSingleMode(boolean singleMode) {
        this.singleMode = singleMode;
    }

    public DataModel getData(String dataName) {
        for (DataModel dm : dataSets) {
            if (dm.getFullName().equals(dataName)) {
                return dm;
            }
        }
        return null;
    }

    public String prepareUpsetView() {

        jrd.record_prepareUpsetView(this);

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            return null;
        }

        String fileName = sb.getNewImage("upset_stat");
        int res = RDataUtils.prepareUpsetData(sb, fileName);
        if (res == 1) {
            return "Upset diagram";
        } else {
            PrimeFaces.current().executeScript("PF('statusDialog').hide()");
            sb.addMessage("error", RDataUtils.getErrMsg(sb.getRConnection()));
            wb.getCalledWorkflowsError().add("Upset Diagram");

            return null;
        }
    }

    public void performBatchCorrection() {

        int res = 0;
        if (adjustBatch) {
            res = RMetaUtils.checkMetaDataConsistency(sb.getRConnection(), adjustBatch ? "T" : "F");
        }
        System.out.println(res + "==================RESSS");
        switch (res) {
            case 0 -> {
                if (adjustBatch) {
                    allDataConsistent = false;
                    //integCheckMsg = "ComBat Normalization failed.";
                    sb.addMessage("error", "OK, ComBat Normalization failed!");
                    adjustBatch = false;
                } else {
                    //integCheckMsg = "Reverted back to raw expression data.";
                    sb.addMessage("info", "OK, reverted back to non-adjusted state!");
                }
            }
            default -> {
                allDataConsistent = true;
                /*
                integCheckMsg = "OK, ComBat normalization succeeded. "
                + "Click <b>Next</b> button to next page.";
                 */
                RDataUtils.plotMetaPCA(sb, sb.getRConnection(), sb.getNewImage("qc_meta_pca"), 150, "png");
                if (adjustBatch) {
                    sb.addMessage("info", "OK, ComBat batch correction succeeded!");
                }
            }
        }
        jrd.record_performBatchCorrection(this);
    }
}
