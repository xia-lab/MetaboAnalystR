/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.spectra;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.Semaphore;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.ListDataModel;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import java.util.logging.Level;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;
import org.primefaces.model.StreamedContent;
import org.primefaces.model.Visibility;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.datacenter.DatasetController;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.models.MS2FeatureBean;
import pro.metaboanalyst.project.SchedulerUtils;
import pro.metaboanalyst.rwrappers.RSpectraUtils;
import pro.metaboanalyst.utils.DataUtils;
import static pro.metaboanalyst.utils.DataUtils.processExec;
import software.xdev.chartjs.model.charts.PieChart;
import software.xdev.chartjs.model.data.PieData;
import software.xdev.chartjs.model.dataset.PieDataset;

/**
 * @author qiang
 */
@SessionScoped
@Named("tandemMS")
@JsonIgnoreProperties(ignoreUnknown = true)
public class TandemMSBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private FireBaseController fbc;

    @JsonIgnore
    @Inject
    private DatasetController datasetController;
            
    private String tandemMSList;
    private double precMZ = 0.0000, ppm_val1 = 5.0, ppm_val2 = 10.0;
    private String ionMode = "postive"; // can be "positive" or "negative"
    private String database = "all", energy = "all", unit1 = "ppm", unit2 = "ppm";
    private int simlarity_meth = 0; // 0, dot-product; 1, spectral entropy
    private double simi_cutoff = 20.0;
    private String msmsDBOpt = "all";
    private String[] msmsDBOpt_multi = {"hmdb_exp"};
    private String[] msmsDBOpt_multi2 = {"hmdb_exp"};
    private String exampleType = "none";
    private boolean isSingle = true;
    private boolean useNeutralLoss = false;
    @JsonIgnore
    private ListDataModel<MS2FeatureBean> FeatureModel = null;
    private int resNum = 0;
    private String mirrorplot_imgNM;
    private String mirrorplot_jsonNM;
    private HashMap<String, String> jsonHashMap, refspecHMap, cmpdinfHMap, imgsHashMap;
    private String jsonDir;
    private String EleID = "myid0";
    private String mirrorwidget = "mirrorwidget0";
    private String summarize_res_plot = "";
    @JsonIgnore
    private StreamedContent image;
    @JsonIgnore
    private StreamedContent singleMirrorImage, refSpecTxt, cmpdInfoTxt;
    private String mspFotmat = "mgf";
    private SelectItem[] precmzs;
    private int prec_idx = 0;
    @JsonIgnore
    private UploadedFile dataFile;
    private String sanityMsg = "";
    @JsonIgnore
    private ListDataModel<MS2FeatureBean> FeatureModelbatch = null;
    private double firstPrecMZ = 999999.0;
    private String current_precgrp = "";
    private String testOpt = "msp_wb";
    @JsonIgnore
    private DualListModel<String> precIncluItems;
    @JsonIgnore
    private String pieModel;
    private boolean resultinited = false;
    private int[] expandedRows = {};
    private boolean currentcmdstatus = false;
    private double low_cutoff = 20;
    private double high_cutoff = 60;
    private int total_num_spectra = 0;
    private boolean useScheduler = false;
    private int jobID = 0;
    private String jobStatus = "Pending";
    private double jobProgress = 0.0;
    private boolean jobSubmitted = false;
    private boolean stopStatusCheck = false;
    private int checkcount = 0;
    private boolean jobCompleted = false;

    public TandemMSBean() {
        this.jsonHashMap = new HashMap<>();
        this.imgsHashMap = new HashMap<>();
        this.refspecHMap = new HashMap<>();
        this.cmpdinfHMap = new HashMap<>();
    }

    public String getTandemMSList() {
        return tandemMSList;
    }

    public void setTandemMSList(String tandemMSList) {
        this.tandemMSList = tandemMSList;
    }

    public double getPrecMZ() {
        return precMZ;
    }

    public void setPrecMZ(double precMZ) {
        this.precMZ = precMZ;
    }

    public double getPpm_val1() {
        return ppm_val1;
    }

    public void setPpm_val1(double ppm_val1) {
        this.ppm_val1 = ppm_val1;
    }

    public double getPpm_val2() {
        return ppm_val2;
    }

    public void setPpm_val2(double ppm_val2) {
        this.ppm_val2 = ppm_val2;
    }

    public String getIonMode() {
        return ionMode;
    }

    public void setIonMode(String ionMode) {
        this.ionMode = ionMode;
    }

    public String getDatabase() {
        return database;
    }

    public void setDatabase(String database) {
        this.database = database;
    }

    public String getEnergy() {
        return energy;
    }

    public void setEnergy(String energy) {
        this.energy = energy;
    }

    public String getUnit1() {
        return unit1;
    }

    public void setUnit1(String unit1) {
        this.unit1 = unit1;
    }

    public String getUnit2() {
        return unit2;
    }

    public void setUnit2(String unit2) {
        this.unit2 = unit2;
    }

    public int getSimlarity_meth() {
        return simlarity_meth;
    }

    public void setSimlarity_meth(int simlarity_meth) {
        this.simlarity_meth = simlarity_meth;
    }

    public double getSimi_cutoff() {
        return simi_cutoff;
    }

    public void setSimi_cutoff(double simi_cutoff) {
        this.simi_cutoff = simi_cutoff;
    }

    public String getMsmsDBOpt() {
        return msmsDBOpt;
    }

    public void setMsmsDBOpt(String msmsDBOpt) {
        this.msmsDBOpt = msmsDBOpt;
    }

    public String[] getMsmsDBOpt_multi() {
        return msmsDBOpt_multi;
    }

    public void setMsmsDBOpt_multi(String[] msmsDBOpt_multi) {
        if (Arrays.toString(msmsDBOpt_multi).contains("all")) {
            msmsDBOpt = "all";
        } else if (msmsDBOpt.equals("all")) {
            msmsDBOpt = "all";
        } else {
            msmsDBOpt = Arrays.toString(msmsDBOpt_multi);
        }
        this.msmsDBOpt_multi = msmsDBOpt_multi;
    }

    public void updateMSmsDBSelection() {
        if (Arrays.toString(msmsDBOpt_multi).contains("all")) {
            msmsDBOpt_multi = new String[]{"all"};
            msmsDBOpt = "all";
        } else if (msmsDBOpt_multi.length == 0) {
            msmsDBOpt_multi = new String[]{"hmdb_exp"};
        }
    }

    public String[] getMsmsDBOpt_multi2() {
        return msmsDBOpt_multi2;
    }

    public void setMsmsDBOpt_multi2(String[] msmsDBOpt_multi2) {
        if (Arrays.toString(msmsDBOpt_multi2).contains("all")) {
            msmsDBOpt = "all";
        } else if (msmsDBOpt.equals("all")) {
            msmsDBOpt = "all";
        } else {
            msmsDBOpt = Arrays.toString(msmsDBOpt_multi2);
        }
        this.msmsDBOpt_multi2 = msmsDBOpt_multi2;
    }

    public void updateMSmsDBSelection2() {
        if (Arrays.toString(msmsDBOpt_multi2).contains("all")) {
            msmsDBOpt_multi2 = new String[]{"all"};
            msmsDBOpt = "all";
        } else if (msmsDBOpt_multi.length == 0) {
            msmsDBOpt_multi = new String[]{"hmdb_exp"};
        } else {
            msmsDBOpt = Arrays.toString(msmsDBOpt_multi2);
        }
    }

    public String getExampleType() {
        return exampleType;
    }

    public void setExampleType(String exampleType) {
        this.exampleType = exampleType;
    }

    public void updateExampleArea() {
        switch (exampleType) {
            case "true" -> {
                tandemMSList = "135.0802\t9.23\n147.0807\t27.55\n149.0965\t8.74\n153.091\t22.39\n159.0806\t9.47\n161.0966\t8.84\n"
                        + "171.0805\t15.77\n215.1071\t13.62\n235.1112\t12.59\n237.1279\t23.62\n267.138\t11.0\n277.1586\t27.9\n"
                        + "279.1744\t77.14\n309.1851\t30.04\n325.1792\t20.22\n337.1802\t100.0\n393.21\t44.44";
                precMZ = 393.2072;
                simi_cutoff = 35.0;
                ppm_val1 = 10;
                ppm_val2 = 30;
                ionMode = "positive";
                msmsDBOpt = "hmdb_exp";
                unit1 = unit2 = "ppm";
            }
            default -> {
                tandemMSList = "";
                simi_cutoff = 20.0;
                ppm_val1 = 5;
                ppm_val2 = 10;
            }
        }
    }

    public boolean isIsSingle() {
        return isSingle;
    }

    public void setIsSingle(boolean isSingle) {
        this.isSingle = isSingle;
    }

    public boolean isUseNeutralLoss() {
        return useNeutralLoss;
    }

    public void setUseNeutralLoss(boolean useNeutralLoss) {
        this.useNeutralLoss = useNeutralLoss;
    }

    public String processMSMSupload() {

        // need to detect if the ms2 database exists for docker only
        if (ab.isInDocker()) {
            String database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            String fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            String NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            String NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
            File db_sqlite = new File(database_path);
            if (!db_sqlite.exists()) {
                sb.addMessage("Error", "Please make sure MS2ID_Complete_v09102023.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite2 = new File(fragmentDB_pth);
            if (!db_sqlite2.exists()) {
                sb.addMessage("Error", "Please make sure FragsAnnotateDB_v02042024.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite3 = new File(NLdb_path);
            if (!db_sqlite3.exists()) {
                sb.addMessage("Error", "Please make sure MS2ID_CompleteNL_v09102023.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite4 = new File(NLfragDB_path);
            if (!db_sqlite4.exists()) {
                sb.addMessage("Error", "Please make sure FragsAnnotateDBNL_v02042024.sqlite has been successfully attached into your docker!");
                return null;
            }
            //System.out.println("All required database exits!");
        }

        if (precMZ < 40) {
            sb.addMessage("Error", "mz of precursor cannot be lower than 40!");
            return null;
        }

        if ("".equals(tandemMSList) || tandemMSList.length() < 3) {
            sb.addMessage("Error", "the input is empty!");
            return null;
        }
        setIsSingle(true);

        // do login
        if (!sb.doLogin("ms2", "tandemMS", false, false)) {
            sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }

        // here, we need set useNL or NOT into mSet for further analysis
        RSpectraUtils.setMS2DBOpt(sb.getRConnection(), useNeutralLoss ? "nl" : "regular");

        // perform MSMS spectrum upload
        boolean res1 = RSpectraUtils.processMSMSuploadSingleSpec(sb.getRConnection(), tandemMSList);
        if (!res1) {
            sb.addMessage("Error", "MS/MS spectrum uploading failed!");
        }

        // perform MSMS search
        String database_path = "", fragmentDB_pth = "", NLdb_path = "", NLfragDB_path = "";
        if (ab.isOnQiangPc()) {
            database_path = "/data/glassfish/ms2databases/MS2ID_Complete.sqlite";
            fragmentDB_pth = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/data/COMPOUND_DBs/100_NL_DB/v09102023/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDBNL_v02042024.sqlite";
        } else if (ab.isOnVipServer() || ab.isInDocker() || ab.isOnProServer()) {
            database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
        } else if (ab.isOnZgyPc()) {
            database_path = "/home/zgy/sqlite/MS2ID_Complete_v09102023.sqlite";
        } else if (ab.isOnYaoPc()) {
            database_path = "/Users/lzy/sqlite/MS2ID_Complete_v09102023.sqlite";
            fragmentDB_pth = "/Users/lzy/sqlite/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/Users/lzy/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/Users/lzy/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
        } else {
            System.out.println("Now you must have the sqlite database at your local and specify the path here!!!");
        }

        // here, we are going to call an R function from OptiLCMS to search the database and return results
        if (useNeutralLoss) {
            database_path = NLdb_path;
            fragmentDB_pth = NLfragDB_path;
        }
        boolean res2 = RSpectraUtils.performMS2searchSingle(sb.getRConnection(),
                ppm_val1, ppm_val2,
                fragmentDB_pth,
                database_path, msmsDBOpt,
                simlarity_meth, precMZ, simi_cutoff, ionMode, unit1, unit2);
        if (!res2) {
            sb.addMessage("Error", "Failed to perform MS/MS database searching!");
        } else {
            // let plot a summary plot at first
            int res3 = RSpectraUtils.PlotMS2SummarySingle(sb.getRConnection(), "0L", "MS2Summary_res", 150, "png", "10", "6");
            summarize_res_plot = "MS2Summary_res_150.png";
            if (res3 == 0) {
                sb.addMessage("Error", "Failed to summarize the MS/MS analysis results!");
            }

            // now we need to populate the results table
            String[] allcmpds = RSpectraUtils.GetMSMSCompoundNames_single(sb.getRConnection(), 1);
            String[] allformulas = RSpectraUtils.GetMSMSFormulas_single(sb.getRConnection(), 1);
            String[] allsmiles = RSpectraUtils.GetMSMSSmiles_single(sb.getRConnection(), 1);
            String[] allinchikeys = RSpectraUtils.GetMSMSInchiKeys_single(sb.getRConnection(), 1);
            double[] allscores = RSpectraUtils.GetMSMSSimScores_single(sb.getRConnection(), 1);
            double[] alldotscores = RSpectraUtils.GetMSMSDot_single(sb.getRConnection(), 1);
            double[] allprecurs = RSpectraUtils.GetMSMSPrecs_single(sb.getRConnection(), 1);

            ArrayList<MS2FeatureBean> MS2FeatureBeans = new ArrayList<>();

            MS2FeatureBean mfb;
            for (int i = 0; i < allcmpds.length; i++) {
                mfb = new MS2FeatureBean(i,
                        allprecurs[i], allscores[i], alldotscores[i],
                        allformulas[i], allcmpds[i],
                        allsmiles[i], allinchikeys[i], precMZ, "");
                MS2FeatureBeans.add(mfb);
            }
            FeatureModel = new ListDataModel(MS2FeatureBeans);
            precmzs = new SelectItem[1];
            precmzs[0] = new SelectItem(0, precMZ + "");
            plotMirrorMatching();
            return "MSMSResult";
        }

        return null;
    }

    @JsonIgnore
    public ListDataModel<MS2FeatureBean> getMS2FeatureBean() {
        return FeatureModel;
    }

    public int plotMirrorMatching() {
        String imageNM = "mirror_plotting_" + resNum + "_" + precMZ + "_150.png";
        mirrorplot_imgNM = imageNM;
        mirrorplot_jsonNM = "mirror_plotting_" + resNum + "_" + precMZ + "_150.json";
        int res = RSpectraUtils.plotMirror(sb.getRConnection(),
                resNum + 1, precMZ, ppm_val1, imageNM, 150,
                "png", 10, 6);
        jsonHashMap.put(resNum + "", mirrorplot_jsonNM);
        imgsHashMap.put(resNum + "", mirrorplot_imgNM);
        refspecHMap.put(resNum + "", "reference_spectrum_" + resNum + "_" + precMZ + ".txt");
        cmpdinfHMap.put(resNum + "", "compound_info_" + resNum + "_" + precMZ + ".txt");
        System.out.println("mirror=====" + resNum);
        return res;
    }

    public int getResNum() {
        return resNum;
    }

    public void setResNum(int resNum) {
        setEleID("myid" + resNum);
        this.resNum = resNum;
    }

    public String getMirrorplot_imgNM() {
        return mirrorplot_imgNM;
    }

    public void setMirrorplot_imgNM(String mirrorplot_imgNM) {
        this.mirrorplot_imgNM = mirrorplot_imgNM;
    }

    public String getMirrorplot_jsonNM() {
        return mirrorplot_jsonNM;
    }

    public void setMirrorplot_jsonNM(String mirrorplot_jsonNM) {
        this.mirrorplot_jsonNM = mirrorplot_jsonNM;
    }

    public String getJsonDir() {
        String image_json_name = mirrorplot_jsonNM;
        if (!jsonHashMap.isEmpty()) {
            if (jsonHashMap.containsKey(resNum + "")) {
                image_json_name = jsonHashMap.get(resNum + "");
            }
        }

        jsonDir = sb.getCurrentUser().getRelativeDir() + "/" + image_json_name;
        return jsonDir;
    }

    public void setJsonDir(String jsonDir) {
        this.jsonDir = jsonDir;
    }

    public String getEleID() {
        return EleID;
    }

    public void setEleID(String EleID) {
        this.EleID = EleID;
    }

    public String getMirrorwidget() {
        return mirrorwidget;
    }

    public void setMirrorwidget(String mirrorwidget) {
        this.mirrorwidget = mirrorwidget;
    }

    public String getSummarize_res_plot() {
        return summarize_res_plot;
    }

    public void setSummarize_res_plot(String summarize_res_plot) {
        this.summarize_res_plot = summarize_res_plot;
    }

    @JsonIgnore
    public StreamedContent getImage() throws IOException {
        FacesContext context = FacesContext.getCurrentInstance();
        String filename = context.getExternalContext().getRequestParameterMap().get("filename");
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        StreamedContent FileImage = null;
        try {
            FileImage = DataUtils.getStreamedImage(filepath, filename);
        } catch (Exception e) {
        }
        return FileImage;
    }

    public void setImage(StreamedContent image) {
        this.image = image;
    }

    @JsonIgnore
    public StreamedContent getSingleMirrorImage() {
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        try {
            singleMirrorImage = DataUtils.getStreamedImage(filepath, mirrorplot_imgNM);
        } catch (Exception e) {
        }
        return singleMirrorImage;
    }

    @JsonIgnore
    public StreamedContent getRefSpecTxt() {
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        try {
            refSpecTxt = DataUtils.getStreamedImage(filepath, "reference_spectrum_" + resNum + "_" + precMZ + ".txt");
        } catch (Exception e) {
        }
        return refSpecTxt;
    }

    @JsonIgnore
    public StreamedContent getCmpdInfoTxt() {
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        try {
            cmpdInfoTxt = DataUtils.getStreamedImage(filepath, "compound_info_" + resNum + "_" + precMZ + ".txt");
        } catch (Exception e) {
        }
        return cmpdInfoTxt;
    }

    public void updatemirrorplot_imageNM() {
        if (imgsHashMap.containsKey(resNum + "")) {
            mirrorplot_imgNM = imgsHashMap.get(resNum + "");
        } else {
            plotMirrorMatching();
        }
    }

    public String getMspFotmat() {
        return mspFotmat;
    }

    public void setMspFotmat(String mspFotmat) {
        this.mspFotmat = mspFotmat;
    }

    public SelectItem[] getPrecmzs() {
        return precmzs;
    }

    public void setPrecmzs(SelectItem[] precmzs) {
        this.precmzs = precmzs;
    }

    public int getPrec_idx() {
        return prec_idx;
    }

    public void setPrec_idx(int prec_idx) {
        this.prec_idx = prec_idx;
    }

    @JsonIgnore
    public UploadedFile getDataFile() {
        return dataFile;
    }

    public void setDataFile(UploadedFile dataFile) {
        this.dataFile = dataFile;
    }

    public String processMSPFileupload() {

        // need to detect if the ms2 database exists for docker only
        if (ab.isInDocker()) {
            String database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            String fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            String NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            String NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
            File db_sqlite = new File(database_path);
            if (!db_sqlite.exists()) {
                sb.addMessage("Error", "Please make sure MS2ID_Complete_v09102023.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite2 = new File(fragmentDB_pth);
            if (!db_sqlite2.exists()) {
                sb.addMessage("Error", "Please make sure FragsAnnotateDB_v02042024.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite3 = new File(NLdb_path);
            if (!db_sqlite3.exists()) {
                sb.addMessage("Error", "Please make sure MS2ID_CompleteNL_v09102023.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite4 = new File(NLfragDB_path);
            if (!db_sqlite4.exists()) {
                sb.addMessage("Error", "Please make sure FragsAnnotateDBNL_v02042024.sqlite has been successfully attached into your docker!");
                return null;
            }
            System.out.println("All required database exits!");
        }

        // make sure file is not empty
        if (dataFile == null || dataFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return null;
        }
        if (dataFile.getSize() > 5347738) {
            sb.addMessage("Error", "File is too big! We only allow 5.0 MB at most!");
            return null;
        }
        if ((!dataFile.getFileName().endsWith("msp")) & (mspFotmat.equals("msp"))) {
            sb.addMessage("Error", "Please make sure you are uploading a *.msp file!");
            return null;
        }

        if ((!dataFile.getFileName().endsWith("mgf")) & (mspFotmat.equals("mgf"))) {
            sb.addMessage("Error", "Please make sure you are uploading a *.mgf file!");
            return null;
        }

        setIsSingle(false);

        // do login
        if (!sb.doLogin("ms2", "tandemMS", false, false)) {
            sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }

        // upload the msp file now
        RConnection RC = sb.getRConnection();
        boolean onProServer = ab.isOnVipServer() || ab.isInDocker() || ab.isOnProServer();
        String fileName = DataUtils.uploadMSPFile(sb, dataFile, sb.getCurrentUser().getHomeDir(), null, onProServer);
        if (fileName == null) {
            return null;
        }

        // here, we need set useNL or NOT into mSet for further analysis
        RSpectraUtils.setMS2DBOpt(sb.getRConnection(), useNeutralLoss ? "nl" : "regular");

        // do sainty/format check on the msp file
        int res_sainty = RSpectraUtils.SaintyCheckMSPfile(RC, dataFile.getFileName(), mspFotmat);
        if (res_sainty == 0) {
            sb.addMessage("Error", "Sainty check failed! Please make sure you are using standard msp/mgf files!");
            // TODO: add some error complain function
        } else {
            //GetCountsOfIncludedIons
            int count_ion = RSpectraUtils.GetCountsOfIncludedIons(RC);
            total_num_spectra = count_ion;
            useScheduler = count_ion > 20;
            return "MSMSSanity";
        }

        return null;
    }

    public String getSanityMsg() {

        RConnection RC = sb.getRConnection();
        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;
        msgVec.add("Checking data content ...passed.");
        msgArray = RSpectraUtils.GetMSPSanityCheckMsg(RC);
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
        sanityMsg = msg;
        return sanityMsg;
    }

    public void setSanityMsg(String sanityMsg) {
        this.sanityMsg = sanityMsg;
    }

    @JsonIgnore
    public ListDataModel<MS2FeatureBean> getMS2FeatureBatchBean() {
        return FeatureModelbatch;
    }

    public double getFirstPrecMZ() {
        return firstPrecMZ;
    }

    public String performMSPdataSearch() {
        // perform MSMS search
        String database_path = "", fragmentDB_pth = "", NLdb_path = "", NLfragDB_path = "";
        if (ab.isOnQiangPc()) {
            database_path = "/data/glassfish/ms2databases/MS2ID_Complete.sqlite";
            fragmentDB_pth = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/data/COMPOUND_DBs/100_NL_DB/v09102023/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite";//to update later
        } else if (ab.isOnVipServer() || ab.isInDocker() || ab.isOnProServer()) {
            database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            ;//to update later
        } else if (ab.isOnYaoPc()) {
            database_path = "/Users/lzy/sqlite/MS2ID_Complete_v09102023.sqlite";
            fragmentDB_pth = "/Users/lzy/sqlite/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/Users/lzy/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/Users/lzy/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
        } else {
            System.out.println("Now you must have the sqlite database at your local and specify the path here!!!");
        }

        //throttling
        Semaphore semphore = sb.getPermissionToStart();
        if (semphore == null) {
            return null;
        }
        if (useNeutralLoss) {
            database_path = NLdb_path;
            fragmentDB_pth = NLfragDB_path;
        }

        // run the search: here, we are going to call an R function from OptiLCMS to search the database and return results
        boolean res2 = RSpectraUtils.performMS2searchBatch(sb.getRConnection(), ppm_val1, ppm_val2,
                database_path, fragmentDB_pth,
                msmsDBOpt, simlarity_meth, precMZ, simi_cutoff, ionMode, unit1, unit2, 4);

        //release
        semphore.release();

        if (!res2) {
            sb.addMessage("Error", "Failed to perform MS/MS database searching!");
        } else {
            // fetch results - default 1st
            // now we need to populate the results table
            double[] allprecurs_ori = RSpectraUtils.GetMSMSPrecMZvec_msp(sb.getRConnection());
            precmzs = new SelectItem[allprecurs_ori.length];
            ArrayList<MS2FeatureBean> MS2FeatureBeans = new ArrayList<>();

            MS2FeatureBean mfb;
            int d_idx = 0;
            for (int d = 0; d < allprecurs_ori.length; d++) {
                precmzs[d] = new SelectItem(d, allprecurs_ori[d] + "");

                String[] allcmpds = RSpectraUtils.GetMSMSCompoundNames_single(sb.getRConnection(), d + 1);
                String[] allformulas = RSpectraUtils.GetMSMSFormulas_single(sb.getRConnection(), d + 1);
                String[] allsmiles = RSpectraUtils.GetMSMSSmiles_single(sb.getRConnection(), d + 1);
                String[] allinchikeys = RSpectraUtils.GetMSMSInchiKeys_single(sb.getRConnection(), d + 1);
                double[] allscores = RSpectraUtils.GetMSMSSimScores_single(sb.getRConnection(), d + 1);
                double[] alldotscores = RSpectraUtils.GetMSMSDot_single(sb.getRConnection(), d + 1);
                double[] allprecurs = RSpectraUtils.GetMSMSPrecs_single(sb.getRConnection(), d + 1);
                String featurelabel = RSpectraUtils.GetMSMSFeatureLabel(sb.getRConnection(), d + 1);

                for (int i = 0; i < allcmpds.length; i++) {
                    mfb = new MS2FeatureBean(i,
                            allprecurs[i], allscores[i], alldotscores[i],
                            allformulas[i], allcmpds[i],
                            allsmiles[i], allinchikeys[i], allprecurs_ori[d], featurelabel);
                    MS2FeatureBeans.add(mfb);
                }
                if (allcmpds.length > 0) {
                    if (firstPrecMZ > allprecurs_ori[d]) {
                        precMZ = firstPrecMZ = allprecurs_ori[d];
                        d_idx = d;
                    }
                }
            }

            RSpectraUtils.Setcurrentmsmsidx(sb.getRConnection(), d_idx + 1);
            FeatureModel = new ListDataModel(MS2FeatureBeans);
            plotMirrorMatching();
            return "MSMSResult";
        }
        return null;
    }

    public String getCurrent_precgrp() {
        return current_precgrp;
    }

    public void setCurrent_precgrp(String current_precgrp) {
        //Updatecurrentmsmsidx
        RSpectraUtils.Updatecurrentmsmsidx(sb.getRConnection(), current_precgrp);
        this.current_precgrp = current_precgrp;
    }

    public String getTestOpt() {
        return testOpt;
    }

    public void setTestOpt(String testOpt) {
        this.testOpt = testOpt;
    }

    public int updateSearchResult() {

        // now we need to populate the results table again
        String[] allcmpds = RSpectraUtils.GetMSMSCompoundNames_single(sb.getRConnection(), prec_idx + 1);
        String[] allformulas = RSpectraUtils.GetMSMSFormulas_single(sb.getRConnection(), prec_idx + 1);
        String[] allsmiles = RSpectraUtils.GetMSMSSmiles_single(sb.getRConnection(), prec_idx + 1);
        String[] allinchikeys = RSpectraUtils.GetMSMSInchiKeys_single(sb.getRConnection(), prec_idx + 1);
        double[] allscores = RSpectraUtils.GetMSMSSimScores_single(sb.getRConnection(), prec_idx + 1);
        double[] alldotscores = RSpectraUtils.GetMSMSDot_single(sb.getRConnection(), prec_idx + 1);
        double[] allprecurs = RSpectraUtils.GetMSMSPrecs_single(sb.getRConnection(), prec_idx + 1);
        double[] allprecurs_ori = RSpectraUtils.GetMSMSPrecMZvec_msp(sb.getRConnection());

        precMZ = allprecurs_ori[prec_idx];
        ArrayList<MS2FeatureBean> MS2FeatureBeans = new ArrayList<>();

        MS2FeatureBean mfb;
        for (int i = 0; i < allcmpds.length; i++) {
            mfb = new MS2FeatureBean(i,
                    allprecurs[i], allscores[i], alldotscores[i],
                    allformulas[i], allcmpds[i],
                    allsmiles[i], allinchikeys[i], precMZ, "");
            MS2FeatureBeans.add(mfb);
        }
        FeatureModel = new ListDataModel(MS2FeatureBeans);

        return 0;
    }

    public String processMSPtest() {

        // need to detect if the ms2 database exists for docker only
        if (ab.isInDocker()) {
            String database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            String fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            String NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            String NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
            File db_sqlite = new File(database_path);
            if (!db_sqlite.exists()) {
                sb.addMessage("Error", "Please make sure MS2ID_Complete_v09102023.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite2 = new File(fragmentDB_pth);
            if (!db_sqlite2.exists()) {
                sb.addMessage("Error", "Please make sure FragsAnnotateDB_v02042024.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite3 = new File(NLdb_path);
            if (!db_sqlite3.exists()) {
                sb.addMessage("Error", "Please make sure MS2ID_CompleteNL_v09102023.sqlite has been successfully attached into your docker!");
                return null;
            }
            File db_sqlite4 = new File(NLfragDB_path);
            if (!db_sqlite4.exists()) {
                sb.addMessage("Error", "Please make sure FragsAnnotateDBNL_v02042024.sqlite has been successfully attached into your docker!");
                return null;
            }
            System.out.println("All required database exits!");
        }

        // do login
        if (!sb.doLogin("ms2", "tandemMS", false, false)) {
            sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }

        // fetch and upload the msp file now
        RConnection RC = sb.getRConnection();
        String testFile = "";
        //String testMetaFile = null;
        switch (testOpt) {
            case "msp_wb" -> {
                mspFotmat = "msp";
                //msmsDBOpt = "hmdb_exp";
                testFile = ab.getApiResourcePath() + "QCDDA_SCAN1_02.msp";
            }
            case "mgf_wb" -> {
                mspFotmat = "mgf";
                //msmsDBOpt = "hmdb_exp";
                testFile = ab.getApiResourcePath() + "mgf_samll_dda.mgf";
            }
        }
        setIsSingle(false);
        String name = DataUtils.getJustFileName(testFile);

        int res_fetdata = RSpectraUtils.FetchExampleMSP(RC, testFile);
        if (res_fetdata == 0) {
            sb.addMessage("Error", "Failed to get example msp data!");
        }

        // here, we need set useNL (regular) into mSet for further analysis
        RSpectraUtils.setMS2DBOpt(sb.getRConnection(), "regular");

        // do sainty/format check on the msp file
        int res_sainty = RSpectraUtils.SaintyCheckMSPfile(RC, name, mspFotmat);
        if (res_sainty == 0) {
            // TODO: add some error complain function
        } else {
            int count_ion = RSpectraUtils.GetCountsOfIncludedIons(sb.getRConnection());
            total_num_spectra = count_ion;
            useScheduler = count_ion > 20;
            return "MSMSSanity";
        }

        return null;
    }

    @JsonIgnore
    public DualListModel<String> getPrecIncluItems() {
        return precIncluItems;
    }

    public void setPrecIncluItems(DualListModel<String> precIncluItems) {
        this.precIncluItems = precIncluItems;
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public void prepareDataEditor() {
        if (precIncluItems == null) {
            RConnection RC = sb.getRConnection();
            //String[] all_precs = RSpectraUtils.GetAllPrecMZs(RC);
            String[] all_precs_included = RSpectraUtils.GetIncludedPrecMZRTs(RC);
            String[] all_precs_NonIncluded = RSpectraUtils.GetNonIncludedPrecMZRTs(RC);
            precIncluItems = new DualListModel(Arrays.asList(all_precs_included), Arrays.asList(all_precs_NonIncluded));
        }
    }

    public String editData() {

        String[] srcs = precIncluItems.getSource().toArray(String[]::new);
        String[] trgs = precIncluItems.getTarget().toArray(String[]::new);
        String included_str = "";

        for (String src : srcs) {
            included_str = included_str + src + "\n";
        }

        int res = RSpectraUtils.DataUpdatefromInclusionList(sb.getRConnection(), included_str);

        int count_ion = RSpectraUtils.GetCountsOfIncludedIons(sb.getRConnection());
        System.out.println("editData count_ion ===> " + count_ion);
        total_num_spectra = count_ion;
        useScheduler = count_ion > 20;

        if (res == 1) {
            return "MSMSSanity";
        }
        return null;
    }

    @JsonIgnore
    public String getPieModel() {
        return pieModel;
    }

    public void setPieModel(String pieModel1) {
        this.pieModel = pieModel1;
    }

    public void showSubResult() {

    }

    public void initSummaryChart() {
        if (!isIsSingle() & !resultinited) {

            updatePieChart();
            resultinited = true;
        }
        if (isIsSingle() & !resultinited) {
            resultinited = true;
            //currentcmdstatus = sb.isShowRcmdPane();
        }
    }

    private void updatePieChart() {
        List<String> bgColors = new ArrayList<>();
        List<Number> values = new ArrayList<>();
        List<String> labels = new ArrayList<>();

        int[] portions_ints = RSpectraUtils.SummarizeCMPDResults(sb.getRConnection(), high_cutoff, low_cutoff);
        String[] nodeIDs = {"High", "Middle", "Low"};
        String[] cols = {"#009E73", "#E69F00", "#999999"};
        for (int i = 0; i < portions_ints.length; i++) {
            bgColors.add(cols[i]);
            values.add(portions_ints[i]);
            labels.add(nodeIDs[i]);
        }

        pieModel = new PieChart()
                .setData(new PieData()
                        .addDataset(new PieDataset()
                                .setData(values)
                                //.setLabel("My First Dataset")
                                .addBackgroundColors(bgColors)
                        )
                        .setLabels(labels))
                .toJson();

    }

    public double getLow_cutoff() {
        return low_cutoff;
    }

    public void setLow_cutoff(double low_cutoff) {
        this.low_cutoff = low_cutoff;
    }

    public double getHigh_cutoff() {
        return high_cutoff;
    }

    public void setHigh_cutoff(double high_cutoff) {
        this.high_cutoff = high_cutoff;
    }

    /*
    public void updatePieChart() {

        PieChartModel pieModel1 = new PieChartModel();
        ChartData data = new ChartData();
        PieChartDataSet dataSet = new PieChartDataSet();

        List<String> bgColors = new ArrayList<>();
        List<Number> values = new ArrayList<>();
        List<String> labels = new ArrayList<>();

        int[] portions_ints = RSpectraUtils.SummarizeCMPDResults(sb.getRConnection(), high_cutoff, low_cutoff);
        String[] nodeIDs = {"High", "Middle", "Low"};
        String[] cols = {"#009E73", "#E69F00", "#999999"};
        for (int i = 0; i < portions_ints.length; i++) {
            bgColors.add(cols[i]);
            values.add(portions_ints[i]);
            labels.add(nodeIDs[i]);
        }
        dataSet.setBackgroundColor(bgColors);
        dataSet.setData(values);

        data.addChartDataSet(dataSet);
        data.setLabels(labels);
        pieModel1.setData(data);
        pieModel = pieModel1;
    }*/

    public void onRowToggle(org.primefaces.event.ToggleEvent event) {
        MS2FeatureBean sel = (MS2FeatureBean) event.getData();
        resNum = (int) sel.getFeatureNo();
        int n = expandedRows.length;
        List expRowList = Arrays.asList(expandedRows);
        if (!expRowList.contains(resNum)) {
            int newarr[] = new int[n + 1];
            System.arraycopy(expandedRows, 0, newarr, 0, n);
            newarr[n] = resNum;
            expandedRows = newarr;
        }

        setEleID("myid" + resNum);
        setMirrorwidget("mirrorwidget" + resNum);
        boolean visible = Visibility.VISIBLE.equals(event.getVisibility());
        if (visible) {
            plotMirrorMatching();
            //PrimeFaces.current().executeScript("plotintmirror();");
        }
    }

    public void plotmirrorint() {
        //plotMirrorMatching();
        System.out.println("==== plotmirrorint : resNum is --> " + resNum);
        //PrimeFaces.current().executeScript("plotintmirror('" + resNum + "');");
        //PrimeFaces.current().executeScript("PF('mirrorDialog').show();");
        //PrimeFaces.current().ajax().update("mpdialog");
    }

    public int getTotal_num_spectra() {
        return total_num_spectra;
    }

    public void setTotal_num_spectra(int total_num_spectra) {
        this.total_num_spectra = total_num_spectra;
    }

    public boolean isUseScheduler() {
        return useScheduler;
    }

    public void setUseScheduler(boolean useScheduler) {
        this.useScheduler = useScheduler;
    }

    public void submitBatchJob() {

        // create an external folder in /data/
        String guestName = sb.getCurrentUser().getName();
        System.out.println("===submitBatchJob==== guestName =======> " + guestName);
        File tmpExeFolder = new File("/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName);
        if (!tmpExeFolder.exists()) {
            tmpExeFolder.mkdirs();
        }

        // transfer entire R session (mSetObj + functions) outside (into the external folder created above)
        RSpectraUtils.saveCurrentSession(sb.getRConnection(), "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName);

        String database_path = "", fragmentDB_pth = "", NLdb_path = "", NLfragDB_path = "";
        if (ab.isOnQiangPc()) {
            database_path = "/data/glassfish/ms2databases/MS2ID_Complete.sqlite";
            fragmentDB_pth = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/data/COMPOUND_DBs/100_NL_DB/v09102023/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite";//to update later
        } else if (ab.isOnVipServer() || ab.isInDocker() || ab.isOnProServer()) {
            database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            ;//to update later
        } else {
            System.out.println("Now you must have the sqlite database at your local and specify the path here!!!");
        }

        if (useNeutralLoss) {
            database_path = NLdb_path;
            fragmentDB_pth = NLfragDB_path;
        }
        RSpectraUtils.saveParams4Processing(sb.getRConnection(), ppm_val1, ppm_val2,
                database_path, fragmentDB_pth,
                msmsDBOpt, simlarity_meth, precMZ, simi_cutoff, ionMode, unit1, unit2, "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName);

        // create job bash
        RSpectraUtils.createSLURMBash(sb.getRConnection(), "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName + "/");

        // submit job to SLURM
        String JobSubmission = "sbatch /data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName + "/ExecuteRawSpec.sh";

        try {
            if (ab.isOnProServer() || ab.isOnQiangPc() || ab.isInDocker() || ab.isOnVipServer()) {

                Process proc;
                List<String> commands = Arrays.asList(JobSubmission.split(" ")); // Split the command into arguments
                ProcessBuilder processBuilder = new ProcessBuilder(commands);
                proc = processBuilder.start();
                BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));

                String JobString = null;
                while ((JobString = stdInput.readLine()) != null) {
                    jobID = Integer.parseInt(JobString.replaceAll("[^0-9]", ""));
                    System.out.println("Raw spectra Job " + jobID + " has been submitted successfully !");
                }
            } else {
                // other case, like local testing purpose

            }
            setJobSubmitted(true);
        } catch (IOException | NumberFormatException e) {
            //System.out.println("IOException while trying to execute [JobSubmission: sbatch]" + JobSubmission);
        }

        boolean res = false;
        try {
            if (datasetController != null && !datasetController.stageCurrentRawWorkspace()) {
                return;
            }
            sb.addNaviTrack("MS2 Job status", "/Secure/spectra/Secure/spectra/MS2JobStatus.xhtml");
            res = fbc.saveProject("project");
        } catch (Exception ex) {
            java.util.logging.Logger.getLogger(SpectraControlBean.class.getName()).log(Level.SEVERE, null, ex);
        }

        if (!res) {
            sb.addMessage("error", "Project saving failed!");
        } else {
            DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/Secure/spectra/MS2JobStatus.xhtml", "info", "Project saving is successful, you can access your project in your <b>Project View</b> page later.");
        }

    }

    public int getJobID() {
        return jobID;
    }

    public void setJobID(int jobID) {
        this.jobID = jobID;
    }

    public String getJobStatus() {
        return jobStatus;
    }

    public void setJobStatus(String jobStatus) {
        this.jobStatus = jobStatus;
    }

    public double getJobProgress() {
        String guestName = sb.getCurrentUser().getName();
        String progres_file_path = "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName + "/progress_value.txt";
        String progres_file_path2 = "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName + "/progress_value_parallel.txt";
        int prg_count = 0;
        try {
            File myObj = new File(progres_file_path);
            File myObj2 = new File(progres_file_path2);
            if (!myObj.exists()) {
                return 0.0;
            }
            int i = 0;
            try (Scanner myReader = new Scanner(myObj)) {
                while (myReader.hasNextLine()) {
                    i++;
                    if (i > 10) {//to avoid infinite scanning
                        break;
                    }
                    double data = myReader.nextDouble();
                    if (data > 0) {
                        setJobStatus("Running");
                    }
                    if (data >= 100) {
                        setJobStatus("Completed");
                        setJobCompleted(true);
                        stopStatusCheck = true;
                    }
                    if (data > 20 & data < 90) {
                        setJobCompleted(false);
                        if (myObj2.exists()) {
                            prg_count = RSpectraUtils.readProgressSec(sb.getRConnection(), "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName);
                            data = prg_count * 20;
                            if (data < 20) {
                                data = 20;
                            }
                        }
                    }
                    jobProgress = data;
                    return data;
                }
            }
        } catch (FileNotFoundException e) {
            //LOGGER.error("updateProgress", e);
            System.out.println("An error occurred: Progress Log FILE NOT found " + progres_file_path);
        }

        return jobProgress;
    }

    public void setJobProgress(double jobProgress) {
        this.jobProgress = jobProgress;
    }

    public String getJobStatusText() throws IOException {

        String guestName = sb.getCurrentUser().getName();
        String filnm = "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName + "/metaboanalyst_ms2_search.txt";
        StringBuilder s = new StringBuilder();
        try {
            //Process p = Runtime.getRuntime().exec("tail -" + 12 + " " + filnm);
            Process p;
            String cmd = "tail -" + 12 + " " + filnm;
            List<String> commands = Arrays.asList(cmd.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            p = processBuilder.start();
            java.io.BufferedReader input = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream()));
            String line;
            while ((line = input.readLine()) != null) {
                if (line.contains("ERROR:")) {
                    line = line.replace("ERROR:", "<b>ERROR:</b>");
                } else if (line.contains("Step")) {
                    line = line.replace(line, "<b><font color=\"#ff8c00\">" + line + "</font></b>");
                } else if (line.contains("Everything has been finished successfully!")) {
                    line = line.replace("Everything has been finished successfully!", "<b>Everything has been finished successfully!</b>");
                }
                String newLine = line + "<br />";
                s.append(newLine);
            }
        } catch (java.io.IOException e) {
            System.out.println("An error occurred.");
        }
        return s.toString();
    }

    @JsonIgnore
    public StreamedContent getTextOutputFile() throws IOException {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/metaboanalyst_ms2_proc.txt");
    }

    public String timeStamp() {
        String timeStamp = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date());
        return timeStamp;
    }

    public void refreshJob() throws IOException {
        if (ab.shouldUseScheduler()) {
            setJobStatus("Status refreshing...");
        } else {
            if (jobProgress == 100) {
                setJobStatus("Running");
                setJobCompleted(true);
            }
        }
        ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
        ec.redirect(((HttpServletRequest) ec.getRequest()).getRequestURI());
    }

    public void cancelJob() {
        String JobKill = "scancel " + getJobID();
        String guestName = sb.getCurrentUser().getName();

        RSpectraUtils.cancelMS2Job(sb.getRConnection(), "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName);
        try {
            //Process proc = Runtime.getRuntime().exec(JobKill);
            processExec(JobKill);
            sb.addMessage("info", "Current job is cancelled!");

            setJobStatus("Killed");
            setJobSubmitted(false);
            setStopStatusCheck(false);
        } catch (Exception e) {
            sb.addMessage("error", "Unable to terminate the current job!");
        }
    }

    public boolean isJobSubmitted() {
        return jobSubmitted;
    }

    public void setJobSubmitted(boolean jobSubmitted) {
        this.jobSubmitted = jobSubmitted;
    }

    public boolean isStopStatusCheck() {
        return stopStatusCheck;
    }

    public void setStopStatusCheck(boolean stopStatusCheck) {
        this.stopStatusCheck = stopStatusCheck;
    }

    public String finishProgress() {

        // Internalize results
        String guestName = sb.getCurrentUser().getName();
        String exDir = "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName;
        String myDir = ab.getRealUserHomePath() + guestName;
        File res1 = new File(exDir + "/MS2_searching_results.qs");
        File res10 = new File(myDir + "/MS2_searching_results.qs");
        DataUtils.copyFile(res1, res10);

        File res2 = new File(exDir + "/metaboanalyst_ms2_search.txt");
        File res20 = new File(myDir + "/metaboanalyst_ms2_search.txt");
        DataUtils.copyFile(res2, res20);

        File res3 = new File(exDir + "/progress_value.txt");
        File res30 = new File(myDir + "/progress_value.txt");
        DataUtils.copyFile(res3, res30);

        File res4 = new File(exDir + "/MS2_rsession.RData");
        File res40 = new File(myDir + "/MS2_rsession.RData");
        DataUtils.copyFile(res4, res40);

        File res5 = new File(exDir + "/MS2_searching_params.rda");
        File res50 = new File(myDir + "/MS2_searching_params.rda");
        DataUtils.copyFile(res5, res50);
        // load mSet
        RSpectraUtils.loadMS2ResultmSet(sb.getRConnection());

        // Perform pupulating the results
        double[] allprecurs_ori = RSpectraUtils.GetMSMSPrecMZvec_msp(sb.getRConnection());
        precmzs = new SelectItem[allprecurs_ori.length];
        ArrayList<MS2FeatureBean> MS2FeatureBeans = new ArrayList<>();

        MS2FeatureBean mfb;
        int d_idx = 0;
        for (int d = 0; d < allprecurs_ori.length; d++) {
            precmzs[d] = new SelectItem(d, allprecurs_ori[d] + "");

            String[] allcmpds = RSpectraUtils.GetMSMSCompoundNames_single(sb.getRConnection(), d + 1);
            String[] allformulas = RSpectraUtils.GetMSMSFormulas_single(sb.getRConnection(), d + 1);
            String[] allsmiles = RSpectraUtils.GetMSMSSmiles_single(sb.getRConnection(), d + 1);
            String[] allinchikeys = RSpectraUtils.GetMSMSInchiKeys_single(sb.getRConnection(), d + 1);
            double[] allscores = RSpectraUtils.GetMSMSSimScores_single(sb.getRConnection(), d + 1);
            double[] alldotscores = RSpectraUtils.GetMSMSDot_single(sb.getRConnection(), d + 1);
            double[] allprecurs = RSpectraUtils.GetMSMSPrecs_single(sb.getRConnection(), d + 1);
            String featurelabel = RSpectraUtils.GetMSMSFeatureLabel(sb.getRConnection(), d + 1);

            for (int i = 0; i < allcmpds.length; i++) {
                mfb = new MS2FeatureBean(i,
                        allprecurs[i], allscores[i], alldotscores[i],
                        allformulas[i], allcmpds[i],
                        allsmiles[i], allinchikeys[i], allprecurs_ori[d], featurelabel);
                MS2FeatureBeans.add(mfb);
            }
            if (allcmpds.length > 0) {
                if (firstPrecMZ > allprecurs_ori[d]) {
                    precMZ = firstPrecMZ = allprecurs_ori[d];
                    d_idx = d;
                }
            }
        }

        RSpectraUtils.Setcurrentmsmsidx(sb.getRConnection(), d_idx + 1);
        FeatureModel = new ListDataModel(MS2FeatureBeans);
        plotMirrorMatching();
        return "MSMSResult";
    }

    public void checkStatus() {
        checkcount++;
        if ("Running".equals(jobStatus) && checkcount > 20) {
            if (checkcount % 15 == 0) {
                // query status from slurm every 15 seconds when it is running & submitted 20 sec later
                jobStatus = SchedulerUtils.getJobStatusMS2(jobID, sb.getCurrentUser().getName());
            } else {
                jobStatus = "RUNNING";
            }
        } else {
            // In other cases, keep querying until failed or finished
            jobStatus = SchedulerUtils.getJobStatusMS2(jobID, sb.getCurrentUser().getName());
        }
        if (jobStatus.equalsIgnoreCase("failed")) {
            stopStatusCheck = false;
            jobCompleted = false;
            setJobStatus("Failed");
        }

        System.out.println("==== checkStatus: Progress ==> " + jobProgress + " | jobStatus==> " + jobStatus + " | -> " + jobID);
    }

    public boolean isJobCompleted() {
        return jobCompleted;
    }

    public void setJobCompleted(boolean jobCompleted) {
        this.jobCompleted = jobCompleted;
    }

    public void reloadResults() {
        if ((FeatureModel == null) & (!isIsSingle())) {
            // PrimeFaces.current().executeScript("PF('statusDialog').show();");
            System.out.println("=== Now the reloadResults FeatureModel is = ==  => " + FeatureModel);
            // Internalize results
            String guestName = sb.getCurrentUser().getName();
            String exDir = "/data/glassfish/projects/metaboanalyst/ms2_tmp/" + guestName;
            String myDir = ab.getRealUserHomePath() + guestName;
            File res1 = new File(exDir + "/MS2_searching_results.qs");
            File res10 = new File(myDir + "/MS2_searching_results.qs");
            if (!res10.exists() & res1.exists()) {
                DataUtils.copyFile(res1, res10);
            }
            File res2 = new File(exDir + "/metaboanalyst_ms2_search.txt");
            File res20 = new File(myDir + "/metaboanalyst_ms2_search.txt");
            if (!res20.exists() & res2.exists()) {
                DataUtils.copyFile(res2, res20);
            }
            File res3 = new File(exDir + "/progress_value.txt");
            File res30 = new File(myDir + "/progress_value.txt");
            if (!res30.exists() & res3.exists()) {
                DataUtils.copyFile(res3, res30);
            }
            File res4 = new File(exDir + "/MS2_rsession.RData");
            File res40 = new File(myDir + "/MS2_rsession.RData");
            if (!res40.exists() & res4.exists()) {
                DataUtils.copyFile(res4, res40);
            }
            File res5 = new File(exDir + "/MS2_searching_params.rda");
            File res50 = new File(myDir + "/MS2_searching_params.rda");
            if (!res50.exists() & res5.exists()) {
                DataUtils.copyFile(res5, res50);
            }

            // Perform pupulating the results
            double[] allprecurs_ori = RSpectraUtils.GetMSMSPrecMZvec_msp(sb.getRConnection());
            precmzs = new SelectItem[allprecurs_ori.length];
            ArrayList<MS2FeatureBean> MS2FeatureBeans = new ArrayList<>();

            MS2FeatureBean mfb;
            for (int d = 0; d < allprecurs_ori.length; d++) {
                precmzs[d] = new SelectItem(d, allprecurs_ori[d] + "");

                String[] allcmpds = RSpectraUtils.GetMSMSCompoundNames_single(sb.getRConnection(), d + 1);
                String[] allformulas = RSpectraUtils.GetMSMSFormulas_single(sb.getRConnection(), d + 1);
                String[] allsmiles = RSpectraUtils.GetMSMSSmiles_single(sb.getRConnection(), d + 1);
                String[] allinchikeys = RSpectraUtils.GetMSMSInchiKeys_single(sb.getRConnection(), d + 1);
                double[] allscores = RSpectraUtils.GetMSMSSimScores_single(sb.getRConnection(), d + 1);
                double[] alldotscores = RSpectraUtils.GetMSMSDot_single(sb.getRConnection(), d + 1);
                double[] allprecurs = RSpectraUtils.GetMSMSPrecs_single(sb.getRConnection(), d + 1);
                String featurelabel = RSpectraUtils.GetMSMSFeatureLabel(sb.getRConnection(), d + 1);

                for (int i = 0; i < allcmpds.length; i++) {
                    mfb = new MS2FeatureBean(i,
                            allprecurs[i], allscores[i], alldotscores[i],
                            allformulas[i], allcmpds[i],
                            allsmiles[i], allinchikeys[i], allprecurs_ori[d], featurelabel);
                    MS2FeatureBeans.add(mfb);
                }
            }

            FeatureModel = new ListDataModel(MS2FeatureBeans);
        } else if ((FeatureModel == null) & (isIsSingle())) {

            String[] allcmpds = RSpectraUtils.GetMSMSCompoundNames_single(sb.getRConnection(), 1);
            String[] allformulas = RSpectraUtils.GetMSMSFormulas_single(sb.getRConnection(), 1);
            String[] allsmiles = RSpectraUtils.GetMSMSSmiles_single(sb.getRConnection(), 1);
            String[] allinchikeys = RSpectraUtils.GetMSMSInchiKeys_single(sb.getRConnection(), 1);
            double[] allscores = RSpectraUtils.GetMSMSSimScores_single(sb.getRConnection(), 1);
            double[] alldotscores = RSpectraUtils.GetMSMSDot_single(sb.getRConnection(), 1);
            double[] allprecurs = RSpectraUtils.GetMSMSPrecs_single(sb.getRConnection(), 1);

            ArrayList<MS2FeatureBean> MS2FeatureBeans = new ArrayList<>();

            MS2FeatureBean mfb;
            for (int i = 0; i < allcmpds.length; i++) {
                mfb = new MS2FeatureBean(i,
                        allprecurs[i], allscores[i], alldotscores[i],
                        allformulas[i], allcmpds[i],
                        allsmiles[i], allinchikeys[i], precMZ, "");
                MS2FeatureBeans.add(mfb);
            }
            FeatureModel = new ListDataModel(MS2FeatureBeans);
            precmzs = new SelectItem[1];
            precmzs[0] = new SelectItem(0, precMZ + "");
        }
    }

}
