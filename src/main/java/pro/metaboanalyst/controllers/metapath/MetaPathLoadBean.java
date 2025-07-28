/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.metapath;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import java.util.Arrays;
import pro.metaboanalyst.models.User;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetaPathModel;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RMetaPathUtils;
import pro.metaboanalyst.rwrappers.RMetaUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.DualListModel;
import org.primefaces.model.StreamedContent;
import org.primefaces.model.file.UploadedFile;
import pro.metaboanalyst.controllers.general.ProcessBean;
import pro.metaboanalyst.models.DataModel;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author jianguox
 */
@SessionScoped
@Named("pLoadBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class MetaPathLoadBean implements Serializable {

    @Inject
    @JsonIgnore
    private ApplicationBean1 ab;
    @Inject
    @JsonIgnore
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    @Inject
    @JsonIgnore
    private ProcessBean pcb;
    /**
     * Record the currently selected data
     */

    // Section I --- variables, setter and getter <--------------
    private int dataNum = 1;

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

    private String dataType = "massPeaks";

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    private String selectedTestData = "Test1";

    public String getSelectedTestData() {
        return selectedTestData;
    }

    public void setSelectedTestData(String selectedTestData) {
        this.selectedTestData = selectedTestData;
    }

    private MetaPathModel selectedData;

    public MetaPathModel getSelectedData() {
        return selectedData;
    }

    public void setSelectedData(MetaPathModel selectedData) {
        this.selectedData = selectedData;
        RMetaUtils.setCurrentData(sb.getRConnection(), selectedData.getName());
        sb.addMessage("info", "Current data is: " + selectedData.getName() + ", ready for analysis.");
    }

    public MetaPathModel getData4Vis() {
        return selectedData;
    }

    public void setData4Vis(MetaPathModel selectedData) {
        this.selectedData = selectedData;
        selectedData.PlotPathDataProfile();
    }

    private boolean allDataConsistent = true;

    public boolean isAllDataConsistent() {
        return allDataConsistent;
    }

    public void setAllDataConsistent(boolean allDataConsistent) {
        this.allDataConsistent = allDataConsistent;
    }

    private String integCheckMsg = "No data set uploaded yet!";

    public String getIntegCheckMsg() {
        return integCheckMsg;
    }

    private String integCheckMsg2 = "";

    public String getIntegCheckMsg2() {
        return integCheckMsg2;
    }

    private String width = "400px";

    public String getWidth() {
        if (selectedData.getName2() != null) {
            width = "575px";
        } else {
            width = "400px";
        }
        return width;
    }

    public void setWidth(String width) {
        this.width = width;
    }

    // Section II --- Handle File uploading  <--------------
    private UploadedFile fileNeg;

    private UploadedFile fileReg;

    private UploadedFile filePos;

    public UploadedFile getFileNeg() {
        return fileNeg;
    }

    public void setFileNeg(UploadedFile fileNeg) {
        this.fileNeg = fileNeg;
    }

    public UploadedFile getFileReg() {
        return fileReg;
    }

    public void setFileReg(UploadedFile fileReg) {
        this.fileReg = fileReg;
    }

    public UploadedFile getFilePos() {
        return filePos;
    }

    public void setFilePos(UploadedFile filePos) {
        this.filePos = filePos;
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

    private void checkLogIn() {
        if (!loggedIn) {
            sb.doLogin("conc", "metapaths", false, false);
            //need to update all dataSets created for meta and multi
            dataSets = getDataSets();
            for (int i = 0; i < dataSets.size(); i++) {
                dataSets.get(i).setRC(sb.getRConnection());
            }
            loggedIn = true;
        }

    }
    private boolean loggedIn = false;

    public boolean handleFileUpload() {

        UploadedFile file = null;
        UploadedFile file2 = null;
        String dataIon = selectedData.getIonMode();

        if ("mixed".equals(dataIon)) {
            file = filePos;
            file2 = fileNeg;
        } else {
            file = fileReg;
        }

        if (file == null || file.getSize() == 0) {
            sb.addMessage("error", "Empty file?");
            return false;
        }

        if ("mixed".equals(dataIon) && (file2 == null || file2.getSize() == 0)) {
            sb.addMessage("error", "Empty file?");
            return false;
        }

        if (ab.isOnProServer()) { // size limit will apply only on public server

            if (file.getSize() > ab.getMAX_UPLOAD_SIZE()) {
                sb.addMessage("error", "The file size exceeds limit: " + ab.getMAX_UPLOAD_SIZE());
                return false;
            }

            if ("mixed".equals(dataIon) && (file2.getSize() > ab.getMAX_UPLOAD_SIZE())) {
                sb.addMessage("error", "The file size exceeds limit: " + ab.getMAX_UPLOAD_SIZE());
                return false;
            }
        }

        String fileName = file.getFileName();
        if (!fileName.endsWith(".csv") && !fileName.endsWith(".txt")) {
            sb.addMessage("error", "Only .txt or .csv file is acceptable!");
            file = null;
            return false;
        }

        if ("mixed".equals(dataIon)) {
            String fileName2 = file2.getFileName();

            if (!fileName2.endsWith(".csv") && !fileName2.endsWith(".txt")) {
                sb.addMessage("error", "Only .txt or .csv file is acceptable!");
                return false;
            }
        }

        checkLogIn();

        fileName = DataUtils.uploadFile(sb, file, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

        int res = 0;
        int res2 = 0;

        if ("mixed".equals(dataIon)) {
            String fileName2 = file2.getFileName();
            fileName2 = DataUtils.uploadFile(sb, file2, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            res2 = RMetaPathUtils.readMetaPathTableMix(sb.getRConnection(), fileName, fileName2, dataFormat, dataType);
        } else {
            res = RMetaPathUtils.readMetaPathTable(sb.getRConnection(), fileName, dataFormat, dataType);
        }

        // 2. Process the files
        if ((!"mixed".equals(dataIon) && res == 0) || ("mixed".equals(dataIon) && res2 == 0)) {
            sb.addMessage("error", RDataUtils.getErrMsg(sb.getRConnection()));
            //remove the file
            file = null;
            return false;
        } else {
            sb.addMessage("info", fileName + " is uploaded and parsed out." + RDataUtils.getCurrentMsg(sb.getRConnection()));

            if ("mixed".equals(dataIon)) {
                String fileName2 = file2.getFileName();
                sb.addMessage("info", fileName2 + " is uploaded and parsed out." + RDataUtils.getCurrentMsg(sb.getRConnection()));
                selectedData.setName2(fileName2);
            }

            selectedData.setUploaded(true);
            selectedData.setName(fileName);
            selectedData.setDataformat(dataFormat);
            sb.setDataUploaded(true);

            selectedData.processMetaPathData();
            file = null;
            return true;
        }
    }

    private int currentDeNum = 0;

    public int getCurrentDeNum() {
        return currentDeNum;
    }

    public void setCurrentDeNum(int currentDeNum) {
        this.currentDeNum = currentDeNum;
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

    // Section V ---- Functional Utilities  <--------------
    public void addNewData(String dataName) {
        dataSets.add(new MetaPathModel(sb, dataName));
    }

    public void deleteData(MetaPathModel selectedData) {
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

    public MetaPathModel getData(String dataName) {
        for (MetaPathModel dm : dataSets) {
            if (dm.getFullName().equals(dataName)) {
                //System.out.println("----------------- Getting this data dataName ---> " + dataName);
                return dm;
            }
        }
        return null;
    }

    private List<MetaPathModel> dataSets;

    private List<MetaPathModel> mDataSets; //the contains meta

    public void setmDataSets(List<MetaPathModel> mDataSets) {
        this.mDataSets = mDataSets;
    }

    public List<MetaPathModel> getmDataSets() {
        return mDataSets;
    }

    public List<MetaPathModel> getDataSets() {
        if (dataSets == null) {
            dataSets = new ArrayList();
            dataSets.add(new MetaPathModel(sb, "Upload"));
        }
        return dataSets;
    }

    private List<MetaPathModel> dataSetsInfo;

    public List<MetaPathModel> getDataSetsInfo() {
        if (dataSetsInfo == null || dataSetsInfo.isEmpty()) {
            dataSetsInfo = new ArrayList();
        }
        return dataSetsInfo;
    }

    public void setDataSetsInfo(List<MetaPathModel> dataSetsInfo) {
        this.dataSetsInfo = dataSetsInfo;
    }

    private double pvalCutoff = 0.05;

    public double getPvalCutoff() {
        return pvalCutoff;
    }

    public void setPvalCutoff(double pvalCutoff) {
        this.pvalCutoff = pvalCutoff;
    }

    public List<MetaPathModel> getMetaDataSets() {
        if (mDataSets == null) {
            updateMetaDataSets();
        }
        return mDataSets;
    }

    public void updateMetaDataSets() {
        int[] res = RMetaPathUtils.performPathSum(sb, pvalCutoff);
        currentDeNum = res.length;
        mDataSets = new ArrayList();
        int num = 0;

        for (int i = 0; i < dataSets.size(); i++) {
            if (dataSets.get(i).isInclude()) {
                dataSets.get(i).setDeNum(res[num]);
                num++;
                mDataSets.add(dataSets.get(i));
            }
        }
        MetaPathModel dm = new MetaPathModel(sb, "meta_dat");
        dm.setDeNum(res[res.length - 1]);
        //dm.setDe
        mDataSets.add(dm);
    }

    @JsonIgnore
    public DefaultStreamedContent getMergedFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/MetaboAnalyst_merged_data.csv");
    }

    public void setDataSets(List<MetaPathModel> dataSets) {
        this.dataSets = dataSets;
    }

    // Section IV --- Example Utilities  <--------------
    private int example = 1;

    public int getExample() {
        return example;
    }

    public void setExample(int example) {
        this.example = example;
    }

    private boolean allowContinue = false;

    public boolean isAllowContinue() {
        return allowContinue;
    }

    public void setAllowContinue(boolean allowContinue) {
        this.allowContinue = allowContinue;
    }

    public void performMetaIntegrityCheck() {
        // 1. clean the msgset
        integCheckMsg = "";

        // 2. get datasets
        ArrayList<String> selectedDataSets = getCurrentDataSets();
        int datasizenum = dataSets.size();
        if (!dataSets.get(0).isUploaded()) {
            datasizenum = datasizenum - 1;
        }

        integCheckMsg2 = "<b>" + selectedDataSets.size() + "</b> datasets (total: <b>" + datasizenum + "</b>) have been selected.";

        if (selectedDataSets.isEmpty()) {
            integCheckMsg = "No data was found. Please upload/select first !";
            allowContinue = false;
            return;
        } else if (selectedDataSets.size() == 1) {
            integCheckMsg = "At least two datasets are required for meta-analysis.";
            allowContinue = false;
        } else {
            allowContinue = true;
        }

        // 3. Define the global select datasets
        String datas = DataUtils.createStringVector(selectedDataSets.toArray(String[]::new));
        RMetaPathUtils.setInclusionDataSets(sb.getRConnection(), datas);
        //RDataUtils.setSelectedDataNames(sb.getRConnection(), selectedDataSets.toArray(new String[0]));
        setDataNum(selectedDataSets.size());
    }

    //do default analysis for testing purpose
    public void doDefaultMetaAnalysis() {

        checkLogIn();

        User currentUser = sb.getCurrentUser();
        List<MetaPathModel> metaDataSets = getDataSets();

        //reset data 
        metaDataSets.clear();

        switch (example) {
            case 1 -> {
                System.out.println("Running Meta Path analysis example 1 @_@");
                //first data
                String inpath = ab.getResourceByAPI("A1_pos.csv");
                String name = DataUtils.getJustFileName(inpath);
                String outpath = currentUser.getHomeDir() + File.separator + name;
                DataUtils.fetchFile(inpath, new File(outpath));
                addNewData("Upload");
                RMetaPathUtils.readMetaPathTable(sb.getRConnection(), name, "colu", "massPeaks");
                selectedData = metaDataSets.get(0);
                selectedData.setUploaded(true);
                selectedData.setName(name);
                selectedData.processMetaPathData();
                perfromDefaultMetaProcess(selectedData);
                selectedData.setInclude(true);
                selectedData.setDisabledModify(true);
                //2nd data
                inpath = ab.getResourceByAPI("B1_pos.csv");
                name = DataUtils.getJustFileName(inpath);
                outpath = currentUser.getHomeDir() + File.separator + name;
                DataUtils.fetchFile(inpath, new File(outpath));
                addNewData("Upload");
                RMetaPathUtils.readMetaPathTable(sb.getRConnection(), name, "colu", "massPeaks");
                selectedData = metaDataSets.get(1);
                selectedData.setUploaded(true);
                selectedData.setName(name);
                selectedData.processMetaPathData();
                perfromDefaultMetaProcess(selectedData);
                selectedData.setInclude(true);
                selectedData.setDisabledModify(true);
                //3rd data
                inpath = ab.getResourceByAPI("C1_pos.csv");
                name = DataUtils.getJustFileName(inpath);
                outpath = currentUser.getHomeDir() + File.separator + name;
                DataUtils.fetchFile(inpath, new File(outpath));
                addNewData("Upload");
                RMetaPathUtils.readMetaPathTable(sb.getRConnection(), name, "colu", "massPeaks");
                selectedData = metaDataSets.get(2);
                selectedData.setUploaded(true);
                selectedData.setName(name);
                selectedData.processMetaPathData();
                perfromDefaultMetaProcess(selectedData);
                selectedData.setInclude(true);
                selectedData.setDisabledModify(true);
            }
            case 2 -> {
                System.out.println("Running Meta Path analysis example 2 T_T");
                //first data
                String inpath = ab.getResourceByAPI("A1_pos.csv");
                String inpath2 = ab.getResourceByAPI("A1_neg.csv");
                String name = DataUtils.getJustFileName(inpath);
                String name2 = DataUtils.getJustFileName(inpath2);
                String outpath = currentUser.getHomeDir() + File.separator + name;
                String outpath2 = currentUser.getHomeDir() + File.separator + name2;
                DataUtils.fetchFile(inpath, new File(outpath));
                DataUtils.fetchFile(inpath2, new File(outpath2));
                addNewData("Upload");
                RMetaPathUtils.readMetaPathTableMix(sb.getRConnection(), name, name2, "colu", "massPeaks");
                selectedData = metaDataSets.get(0);
                selectedData.setUploaded(true);
                selectedData.setName(name);
                selectedData.setName2(name2);
                selectedData.processMetaPathData();
                perfromDefaultMetaProcess(selectedData);
                selectedData.setInclude(true);
                selectedData.setDisabledModify(true);
                //2nd data
                inpath = ab.getResourceByAPI("B1_pos.csv");
                inpath2 = ab.getResourceByAPI("B1_neg.csv");
                name = DataUtils.getJustFileName(inpath);
                name2 = DataUtils.getJustFileName(inpath2);
                outpath = currentUser.getHomeDir() + File.separator + name;
                outpath2 = currentUser.getHomeDir() + File.separator + name2;
                DataUtils.fetchFile(inpath, new File(outpath));
                DataUtils.fetchFile(inpath2, new File(outpath2));
                addNewData("Upload");
                RMetaPathUtils.readMetaPathTableMix(sb.getRConnection(), name, name2, "colu", "massPeaks");
                selectedData = metaDataSets.get(1);
                selectedData.setUploaded(true);
                selectedData.setName(name);
                selectedData.setName2(name2);
                selectedData.processMetaPathData();
                perfromDefaultMetaProcess(selectedData);
                selectedData.setInclude(true);
                selectedData.setDisabledModify(true);
                //3rd data
                inpath = ab.getResourceByAPI("C1_pos.csv");
                inpath2 = ab.getResourceByAPI("C1_neg.csv");
                name = DataUtils.getJustFileName(inpath);
                name2 = DataUtils.getJustFileName(inpath2);
                outpath = currentUser.getHomeDir() + File.separator + name;
                outpath2 = currentUser.getHomeDir() + File.separator + name2;
                DataUtils.fetchFile(inpath, new File(outpath));
                DataUtils.fetchFile(inpath2, new File(outpath2));
                addNewData("Upload");
                RMetaPathUtils.readMetaPathTableMix(sb.getRConnection(), name, name2, "colu", "massPeaks");
                selectedData = metaDataSets.get(2);
                selectedData.setUploaded(true);
                selectedData.setName(name);
                selectedData.setName2(name2);
                selectedData.processMetaPathData();
                perfromDefaultMetaProcess(selectedData);
                selectedData.setInclude(true);
                selectedData.setDisabledModify(true);
            }
            default -> {
                //No more examples for now! Do nothing but report an error
                sb.addMessage("error", "Something wrong with your example selection !");
                return;
            }
        }

        sb.addMessage("info", "Three datasets were uploaded and processed on the server.");

        sb.setDataUploaded(true);
    }

    private void perfromDefaultMetaProcess(MetaPathModel dm) {
        dm.setUploaded(true);
        dm.processMetaPathData();
        dm.setProcessed(true);
        dm.setNormOpt("log");
        dm.setAutoscaleOpt(false);
        dm.performNormalization();
        dm.setNormed(true);

        String dataIon;
        if (example == 1) {
            dataIon = "positive";
        } else {
            dataIon = "mixed";
        }
        dm.setIonMode(dataIon);
        dm.setAdductItems(getAdductItem(dataIon));

        dm.setSigLevel(0.05);
        dm.performPathAnalysis();
        dm.setAnalDone(true);
        dm.setAllDone();
        dm.performmSetQSclean();

        int featureNum = dm.getGeneNum();
        int sampleNum = dm.getSmplNum();
        String groupinfo = dm.getGroupInfo();

        if (pcb.getDataSets() == null || pcb.getDataSets().isEmpty()) {
            pcb.setDataSets(new ArrayList());
        }
        // System.out.println("pro.metaboanalyst.controllers.metapath.MetaPathLoadBean.perfromDefaultMetaProcess()"+dm.getName0()+"========="+dm.getName2());
        DataModel ds = new DataModel(sb, dm.getName0());
        ds.setSmplNum(sampleNum);
        ds.setGeneNum(featureNum);
        ds.setGroupInfo(groupinfo);
        ds.setName(dm.getName());
        ds.setDataName(dm.getName0());
        pcb.getDataSets().add(ds);

    }

    private ArrayList<String> selectedDataNms = new ArrayList();

    public ArrayList<String> getSelectedDataNms() {
        return selectedDataNms;
    }

    public void setSelectedDataNms(ArrayList<String> selectedDataNms) {
        this.selectedDataNms = selectedDataNms;
    }

    public String prepareMetaPathUpsetView() {

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_prepareMetaPathUpsetView(this);
            return null;
        }
        selectedDataNms.clear();
        //System.out.println("--------mDataSets.size is ---->" + mDataSets.size());
        for (int i = 0; i < getMetaDataSets().size(); i++) {
            MetaPathModel dm = mDataSets.get(i);

            if (dm.isVennInclude()) {
                if (dm.getName2() != null) {
                    String mixNM = dm.getName0().replaceAll(".csv", "") + "mixed";
                    //System.out.println("--------mixNM is ---->" + mixNM);
                    selectedDataNms.add(mixNM);
                } else {
                    String cleanNM = dm.getName().replaceAll(".csv", "");
                    //System.out.println("--------cleanNM is ---->" + cleanNM);
                    selectedDataNms.add(cleanNM);
                }
            }
        }
        RMetaPathUtils.setSelectedMetaPathNames(sb, selectedDataNms.toArray(String[]::new));

        String fileName = sb.getNewImage("upset_path");
        int res = RMetaPathUtils.prepareMetaPathData(sb, fileName);

        if (res == 1) {
            jrd.record_prepareMetaPathUpsetView(this);
            return "Upset diagram";
        } else {
            updateMetaDataSets();
            return prepareMetaPathUpsetView();
        }

    }

    public String naiveDisPlot() {
        String fileRoot = ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator;
        String imageNM = "";
        imageNM = selectedData.getName().split(" \\+ ")[0] + "_qc_boxdpi150.png";
        if (selectedData.getName2() != null) {
            imageNM = selectedData.getName().split(" \\+ ")[0] + "_"
                    + selectedData.getName2().substring(0, selectedData.getName2().length() - 4)
                    + "_dpi150_qc_box.png";
        }
        return fileRoot + imageNM;
    }

    public StreamedContent getImage() throws IOException {
        String filename = selectedData.getName().split(" \\+ ")[0];
        String filename2 = "";
        String filepath = sb.getCurrentUser().getHomeDir() + "/";

        if (selectedData.getName2() != null) {
            filename2 = selectedData.getName2().substring(0, selectedData.getName2().length() - 4);
            filename = filename + "_" + filename2 + "_dpi150_norm_box.png";
        } else {
            filename = filename + "_norm_boxdpi150.png";
        }

        StreamedContent FileImage = null;
        try {
            FileImage = DataUtils.getStreamedImage(filepath, filename);
        } catch (Exception e) {

        }
        return FileImage;
    }

    public void finishDataSet() {
        selectedData.setDisabledModify(true);
        selectedData.setAllDone();

    }

    public String confirmAllData() {
        RMetaPathUtils.restoreCmdHistory(sb.getRConnection());
        return "Meta-Analysis Params";
    }

    public DualListModel<String> getAdductItem(String mode) {
        //MetaPathLoadBean mplb = (MetaPathLoadBean) DataUtils.findBean("pLoadBean");

        String adductSPath;
        String adductTPath;

        switch (mode) {
            case "positive" -> {
                adductSPath = ab.getInternalData("source_pos_add_list.txt");
                adductTPath = ab.getInternalData("target_pos_add_list.txt");
            }
            case "negative" -> {
                adductSPath = ab.getInternalData("source_neg_add_list.txt");
                adductTPath = ab.getInternalData("target_neg_add_list.txt");
            }
            default -> {
                // need to deal w. mixed mode
                adductSPath = ab.getInternalData("source_mixed_add_list.txt");
                adductTPath = ab.getInternalData("target_mixed_add_list.txt");
            }
        }

        String allSAdducts = DataUtils.readTextFile(adductSPath);
        String[] newsadducts = allSAdducts.split("\n");

        String allTAdducts = DataUtils.readTextFile(adductTPath);
        String[] newtadducts = allTAdducts.split("\n");
        return new DualListModel(Arrays.asList(newsadducts), Arrays.asList(newtadducts));
    }
}
