/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.spectra;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.primefaces.model.StreamedContent;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetSetBean;
import pro.metaboanalyst.models.RawFeatureBean;
import pro.metaboanalyst.models.RawResBean;
import pro.metaboanalyst.models.SpecBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RSpectraUtils;
import pro.metaboanalyst.utils.DataUtils;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.ListDataModel;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import pro.metaboanalyst.models.MS2ResutlsBean;
import pro.metaboanalyst.models.SwathBean;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author qiang
 */
@SessionScoped
@Named("spectraProcessor")
@JsonIgnoreProperties(ignoreUnknown = true)

public class SpectraProcessBean implements Serializable {

    @Inject
    @JsonIgnore
    WorkflowBean wb;

    @Inject
    @JsonIgnore
    private SpectraControlBean sc;

    @Inject
    @JsonIgnore
    private SessionBean1 sb;

    @Inject
    @JsonIgnore
    private SpectraParamBean sparam;

    @Inject
    @JsonIgnore
    private ApplicationBean1 ab;

        @JsonIgnore
    @Inject
    private JavaRecord jrd;
        
    /// 1.3 PCA -3D Json File
    String featureNM = "";
    // Section 1 : Plotting function -----------------------
    private int figureCount = 0;
    /// 1.1 TIC -singal
    private String ticName;
    /// 1.2 XIC
    private int featureNum;
    @JsonIgnore
    private int featureNum0;
    private boolean showlabel = true;
    private String featureimageNM;
    private String XICSImgName;
    private String XICGImgName;
    // Section 2 : Table Model function -----------------------
    @JsonIgnore
    private List<SpecBean> specBeans;
    @JsonIgnore
    private List<String> spectraFiles;
    private int centroidFileCount = 0;
    /// Select data files as inclusions
    @JsonIgnore
    private SpecBean selectedData;
    @JsonIgnore
    private SpecBean selectedDataSet;
    @JsonIgnore
    private List<RawResBean> resBeans;
    /// 3. Used to save raw feature information
    @JsonIgnore
    private ListDataModel<RawFeatureBean> FeatureModel = null;
    @JsonIgnore
    private List<String> uploadedFileNamesSaved = new ArrayList<>();
    @JsonIgnore
    private ListDataModel<MetSetBean> fomula2cmpdSet = null;
    ///4. Used to save the compound matching results
    private int featureOrder = 0;
    private int resNum_exp = 0;
    private int resNum_exp_current = 0;
    private int subidx_exp = 0;
    private int subidx_exp_current = 0;

    public int getResNum_exp() {
        return resNum_exp;
    }

    public void setResNum_exp(int resNum_exp) {
        this.resNum_exp = resNum_exp;
    }

    public int getResNum_exp_current() {
        return resNum_exp_current;
    }

    public void setResNum_exp_current(int resNum_exp_current) {
        this.resNum_exp_current = resNum_exp_current;
    }

    public int getSubidx_exp() {
        return subidx_exp;
    }

    public void setSubidx_exp(int subidx_exp) {
        this.subidx_exp = subidx_exp;
    }

    public int getSubidx_exp_current() {
        return subidx_exp_current;
    }

    public void setSubidx_exp_current(int subidx_exp_current) {
        this.subidx_exp_current = subidx_exp_current;
    }

    @JsonIgnore
    private String exposome_details = null;

    @JsonIgnore
    public String getExposome_details() {
        if ((subidx_exp != subidx_exp_current) || (resNum_exp != resNum_exp_current)) {
            exposome_details = get_specific_exposome_details(resNum_exp, subidx_exp + 1, current_ft_label, sb.getRConnection());
            subidx_exp_current = subidx_exp;
            resNum_exp_current = resNum_exp;
        }
        return exposome_details;
    }

    public void setExposome_details(String exposome_details) {
        this.exposome_details = exposome_details;
    }

    private static String get_specific_exposome_details(int resnum, int subidx, String ft_label, RConnection RC) {
        System.out.println("get_specific_exposome_details --  resnum ====> " + resnum);
        System.out.println("get_specific_exposome_details --  subidx ====> " + subidx);
        //current_ft_label
        System.out.println("get_specific_exposome_details --  ft_label ====> " + ft_label);
        String res = null;
        if (resnum >= 0) {
            res = RDataUtils.getExposomeInfo(RC, ft_label, subidx);
            return res;
        }
        return "this is the details of exposome clss!";
    }
    // Section 3 : message section --------------------    
    private String msgText;
    private String errMsgText = "";
    @JsonIgnore
    private List<String> msgList = new ArrayList<>();
    // Section 5 : data image and inspect ----------------------
    @JsonIgnore
    private StreamedContent image;
    private String DataStrucImageNM = "";
    private String FileNameImage = null;
    private double rtl = 0.0;
    private double rth = 0.0;
    private double mzl = 0.0;
    private double mzh = 0.0;
    private int res = 100;
    private String dimen = "3D";
    private String svgStringBox = "";
    private String singleXICPlot = "/resources/images/EICalt.png";
    //// Section 6 : Other Utils - button's control ----------------------
    private boolean bnDisabled = false;
    private boolean singlegroup = false;
    private boolean recordCMD = false;
    private boolean containsMetaVal = true;

    @JsonIgnore
    private HashMap<String, String> jsonHashMap, refspecHMap, cmpdinfHMap, imgsHashMap;

    @JsonIgnore
    public SpectraProcessBean() {
        this.jsonHashMap = new HashMap<>();
        this.imgsHashMap = new HashMap<>();
        this.refspecHMap = new HashMap<>();
        this.cmpdinfHMap = new HashMap<>();
    }

    public static String humanReadableByteCountBin(long bytes) {
        long absB = bytes == Long.MIN_VALUE ? Long.MAX_VALUE : Math.abs(bytes);
        if (absB < 1024) {
            return bytes + " B";
        }
        long value = absB;
        CharacterIterator ci = new StringCharacterIterator("KMGTPE");
        for (int i = 40; i >= 0 && absB > 0xfffccccccccccccL >> i; i -= 10) {
            value >>= 10;
            ci.next();
        }
        value *= Long.signum(bytes);
        return String.format("%.1f", value / 1024.0);
        //return String.format("%.1f %ciB", value / 1024.0, ci.current());
    }

    public int getFigureCount() {
        return figureCount;
    }

    public void setFigureCount(int figureCount) {
        this.figureCount = figureCount;
    }

    public String getTicName() {
        return ticName;
    }

    public void setTicName(String ticName) {
        this.ticName = ticName;
    }

    public void plotSingleTIC() {
        RSpectraUtils.plotSingleTIC(sb, ticName, -1, "png", 150, "NA");
    }

    public String plotTIC() {
        String imageName = RSpectraUtils.plotSingleTIC(
                sb,
                ticName,
                figureCount,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        return imageName;
    }

    public String getTicImgName() {
        return ticName + ".png";
    }

    public int getFeatureNum() {
        return featureNum;
    }

    public void setFeatureNum(int featureNum) {
        this.featureNum = featureNum;
    }

    public int getFeatureNum0() {
        return featureNum0;
    }

    public void setFeatureNum0(int featureNum0) {
        setFeatureNum(RSpectraUtils.convertFTno2Num(sb.getRConnection(), featureNum0));
    }

    public boolean isShowlabel() {
        return showlabel;
    }

    public void setShowlabel(boolean showlabel) {
        this.showlabel = showlabel;
    }

    public List<String> getUploadedFileNamesSaved() {
        return uploadedFileNamesSaved;
    }

    public void setUploadedFileNamesSaved(List<String> uploadedFileNamesSaved) {
        this.uploadedFileNamesSaved = uploadedFileNamesSaved;
    }

    public void addUploadedFileNamesSaved(String uploadedFileNames) {
        uploadedFileNamesSaved.add(uploadedFileNames);
    }

    /// plot summary of the single feature
    public String plotMSfeature(String type) {
        if (type.equals("png")) {
            featureimageNM = RSpectraUtils.plotMSfeature(sb, featureNum, "png", 150, "NA");
            internalizeImage(featureimageNM);
            String rcmd = "plotMSfeature(\"" + featureNum + "\", \"png\", 150, NA" + ")";
            String newfnm = featureimageNM.replaceAll(".png", "");
            sb.addGraphicsCMD("raw_spec_msf_" + newfnm, rcmd);
            sb.addGraphicsMapLink("raw_spec_msf_" + newfnm, "/Secure/spectra/SpectraResult.xhtml");

            return featureimageNM;
        } else if (type.equals("svg")) {
            featureimageNM = RSpectraUtils.plotMSfeature(sb, featureNum, "png", 150, "NA");
            String rcmd = "plotMSfeature(\"" + featureNum + "\", \"png\", 150, NA" + ")";
            String newfnm = featureimageNM.replaceAll(".png", "");
            sb.addGraphicsCMD("raw_spec_msf_" + newfnm, rcmd);
            sb.addGraphicsMapLink("raw_spec_msf_" + newfnm, "/Secure/spectra/SpectraResult.xhtml");

            svgStringBox = RSpectraUtils.plotMSfeature(sb, featureNum, "svg2", 150, "NA");
            internalizeEIC(featureNum);
            return featureNum + "";
        } else {
            return ""; //return nothing for now. TODO: to optimize this point.
        }
    }

    public String plotMSfeatureUpdate() {
        String featureimageNew = RSpectraUtils.plotMSfeature(
                sb,
                featureNum,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        internalizeImage(featureimageNew);
        return featureimageNew;
    }

    public String getFeatureimageNM() {
        return featureimageNM;
    }

    public void setFeatureimageNM(String featureimageNM) {
        this.featureimageNM = featureimageNM;
    }

    public String getXICGImgName() {
        return XICGImgName;
    }

    public void setXICGImgName(String XICGImgName) {
        this.XICGImgName = XICGImgName;
    }

    public String getXICSImgName() {
        return XICSImgName;
    }

    public void setXICSImgName(String XICSImgName) {
        this.XICSImgName = XICSImgName;
    }

    public void plotSingleXIC() {
        String featureXICTitle = RSpectraUtils.plotXIC(sb.getRConnection(),
                featureNum,
                "png",
                150,
                "NA");
        this.XICSImgName = "EIC_" + featureXICTitle + "_sample_150.png";
        XICGImgName = "EIC_" + featureXICTitle + "_group_150.png";
    }

    public String plotXICUpdate() {
        int dpi = 150;
        if (sb.getFormatOpt().equals("png")) {
            dpi = sb.getDpiOpt();
        }
        String featureNAME = RSpectraUtils.plotXIC(sb.getRConnection(),
                featureNum,
                sb.getFormatOpt(),
                dpi,
                sb.getSizeOpt());
        return featureNAME;
    }

    public void setFeatureNM(String featureNM) {
        this.featureNM = featureNM;
    }

    public String getScoreJson() {
        return "/resources/users/" + sb.getCurrentUser().getName() + File.separator + "spectra_3d_score" + featureNM + ".json";
    }

    public String getLoadingJson() {
        return "/resources/users/" + sb.getCurrentUser().getName() + File.separator + "spectra_3d_loading" + featureNM + ".json";
    }

    /// 1.4 Plotting TIC - multiple
    public String plotTICs() {
        String imageName = RSpectraUtils.plotTICs(
                sb.getRConnection(),
                figureCount,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        return imageName;
    }

    /// 1.5 Ploting BPI - multiple
    public String plotBPIs() {
        String imageName = RSpectraUtils.plotBPIs(
                sb.getRConnection(),
                figureCount,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        return imageName;
    }

    /// 1.6 Ploting RTcor
    public String plotRTcor() {
        String imageName = RSpectraUtils.plotRTcor(
                sb.getRConnection(),
                figureCount,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        return imageName;
    }

    /// 1.7 Plotting BPI - corrected
    public String plotBPIcor() {
        String imageName = RSpectraUtils.plotBPIcor(
                sb.getRConnection(),
                figureCount,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        return imageName;
    }

    /// 1.8 Plotting Intensity Stats
    public String plotIntenStats() {
        String imageName = RSpectraUtils.plotIntenStats(
                sb.getRConnection(),
                figureCount,
                sb.getFormatOpt(),
                sb.getDpiOpt(),
                sb.getSizeOpt());
        return imageName;
    }

    public List<String> getSpectraFiles() {
        return spectraFiles;
    }

    public void setSpectraFiles(List<String> spectraFiles) {
        this.spectraFiles = spectraFiles;
    }

    public int getCentroidFileCount() {
        return centroidFileCount;
    }

    public void populateSpecBeans() throws REXPMismatchException {
        specBeans = new ArrayList<>();

        RConnection RC = sb.getRConnection();
        String[] groupnames;

        // Creates a new File instance by converting the given pathname string
        // into an abstract pathname
        String homeDir = sb.getCurrentUser().getHomeDir();
        if (sc.isExecutExample()) {
            if (Files.isDirectory(Paths.get("/Users/xia/Dropbox/"))) {
                homeDir = "/Users/xia/Dropbox/Current/Test/MetaboDemoRawData/"; //xia office
            } else if (Files.isDirectory(Paths.get("/Users/jeffxia/Dropbox/"))) {
                homeDir = "/Users/jeffxia/Dropbox/Current/Test/MetaboDemoRawData/"; //xia laptop
            } else if (Files.isDirectory(Paths.get("/home/zgy/projects/MetaboDemoRawData/"))) {
                homeDir = "/home/zgy/projects/MetaboDemoRawData/"; //xia laptop
            } else { //public server
                homeDir = "/home/glassfish/projects/MetaboDemoRawData/";
            }
        }

        File f;
        f = new File(homeDir + File.separator + "upload");

        // Populates the array with names of files and directories
        groupnames = f.list();

        // For each pathname in the pathnames array
        String rCommand = "CentroidCheck(\'" + "REPLACE_WITH_YOUR_SINGLE_FILE_PATH" + "\')";
        RCenter.recordRCommand(RC, rCommand);

        spectraFiles = new ArrayList<>();
        centroidFileCount = 0;

        for (String groupname : groupnames) {
            File grp = new File(homeDir + File.separator + "upload/" + groupname);
            if (grp.isFile()) {
                continue;
            }
            String[] spectranames = grp.list();
            for (String spectraname : spectranames) {
                File spec = new File(homeDir + "/upload/" + groupname + "/" + spectraname);
                String fileSize = humanReadableByteCountBin(spec.length());
                String filepath = homeDir + "/upload/" + groupname + "/" + spectraname;

                spectraFiles.add(spectraname);

                boolean mode = RDataUtils.checkCentroid(RC, filepath);
                boolean include = true;
                boolean disabled = false;

                String modec = null;
                if (mode) {
                    modec = "True";
                    centroidFileCount = centroidFileCount + 1;
                } else {
                    modec = "<b><font color=\"red\">False</font></b>";
                    include = false;
                    disabled = true;
                }

                String mslevel = "MS1";
                if ("MS2".equals(groupname)) {
                    mslevel = "MS2";
                }

                specBeans.add(new SpecBean(spectraname, modec, fileSize, groupname, "intensity", include, disabled, mslevel));
            }
        }

        bnDisabled = centroidFileCount <= 2;
        setSpectraFiles(spectraFiles);
    }

    public void populateSpecBeansGG() {
        specBeans = new ArrayList<>();

        RConnection RC = sb.getRConnection();
        String[] groupnames = RSpectraUtils.GetAllSpectraGroups(RC);

        // For each pathname in the pathnames array
        String rCommand = "CentroidCheck(\'" + "REPLACE_WITH_YOUR_SINGLE_FILE_PATH" + "\')";
        RCenter.recordRCommand(RC, rCommand);

        spectraFiles = new ArrayList<>();
        centroidFileCount = 0;
        String[] spectranames;
        double filesize; // in MB

        for (String groupname : groupnames) {
            spectranames = RSpectraUtils.GetSampleNMsofGroup(RC, groupname);

            for (String spectraname : spectranames) {
                filesize = RSpectraUtils.GetFileSizesofSpectra(RC, spectraname);
                spectraFiles.add(spectraname);

                boolean mode = filesize < 1024; //less than 1024MB (1GB)
                boolean include = true;
                boolean disabled = false;

                String modec = null;
                if (mode) {
                    modec = "True";
                    centroidFileCount = centroidFileCount + 1;
                } else {
                    modec = "<b><font color=\"red\">False</font></b>";
                    include = false;
                    disabled = true;
                }

                String mslevel = "MS1";
                if ("MS2".equals(groupname)) {
                    mslevel = "MS2";
                }

                specBeans.add(new SpecBean(spectraname, modec, filesize + "", groupname, "intensity", include, disabled, mslevel));
            }
        }

        bnDisabled = false;
        setSpectraFiles(spectraFiles);
    }

    public List<SpecBean> getSpecBeans() {
        return Collections.unmodifiableList(specBeans);
    }

    public void setSpecBeans(List<SpecBean> specBeans) {
        this.specBeans = specBeans;
    }

    public String prepareSpecProc() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            jrd.record_prepareSpecProc(this);
            return null;
        }

        ArrayList<String> incFiles = new ArrayList<>();
        ArrayList<String> groupArr = new ArrayList<>();

        Set<String> uniqueClasses = new HashSet<>();
        for (SpecBean sample : specBeans) {
            if (sample.isInclude()) {
                String grp = sample.getGroup();
                if (!grp.equals("QC") && !grp.equals("BLANK")) {
                    groupArr.add(grp);
                    uniqueClasses.add(grp);
                } else if (grp.equals("BLANK")) {
                    sparam.setButtonEnableBS(false);
                    sparam.setBlksub(true);
                }
                incFiles.add(sample.getName());
            }
        }

        for (String grp : uniqueClasses) {
            int occurrences = Collections.frequency(groupArr, grp);
            if (occurrences < 3) {
                sb.addMessage("Error", "Please make sure that there are at least three samples per group, except MS2 group!");
                return "";
            }
        }

        RConnection RC = sb.getRConnection();
        String FilesInclusion = DataUtils.createStringVector(incFiles.toArray(new String[0]));
        RDataUtils.saveFilesInclusion(sb, FilesInclusion, incFiles.size());

        sparam.initializeAdductList();

        sc.setIncludedFileNamesString(FilesInclusion);
        sc.setDataConfirmed(true);
        jrd.record_prepareSpecProc(this);
        if (ms2DataOpt.equals("dda")) {
            return "MS2 spectra";
        }
        return "Spectra processing";
    }

    public String prepareDIASpecProc() {

        // we need to save the swath file as a text first (via R)
        int swb_len = swathBeans.size();

        double[] mz1 = new double[swb_len];//, mz2;
        double[] mz2 = new double[swb_len];

        for (int i = 0; i < swb_len; i++) {
            SwathBean swbb = swathBeans.get(i);
            mz1[i] = swbb.getLomz();
            mz2[i] = swbb.getUpmz();
        }
        String str_mz1 = Arrays.toString(mz1);
        str_mz1 = str_mz1.replace("[", "c(");
        str_mz1 = str_mz1.replace("]", ")");

        String str_mz2 = Arrays.toString(mz2);
        str_mz2 = str_mz2.replace("[", "c(");
        str_mz2 = str_mz2.replace("]", ")");

        RSpectraUtils.exportSwathDesign(sb.getRConnection(), str_mz1, str_mz2);

        // then process to param page
        ArrayList<String> incFiles = new ArrayList<>();
        ArrayList<String> groupArr = new ArrayList<>();

        Set<String> uniqueClasses = new HashSet<>();
        for (SpecBean sample : specBeans) {
            if (sample.isInclude()) {
                String grp = sample.getGroup();
                if (!grp.equals("QC") && !grp.equals("BLANK")) {
                    groupArr.add(grp);
                    uniqueClasses.add(grp);
                } else if (grp.equals("BLANK")) {
                    sparam.setButtonEnableBS(false);
                    sparam.setBlksub(true);
                } else if (grp.equals("MS2")) {
                    // TODO: to add more settings here for MS2
                }
                incFiles.add(sample.getName());
            }
        }

        for (String grp : uniqueClasses) {
            int occurrences = Collections.frequency(groupArr, grp);
            if (occurrences < 3) {
                sb.addMessage("Error", "Please make sure that there are at least three samples per group, except MS2 group!");
                return "";
            }
        }

        String FilesInclusion = DataUtils.createStringVector(incFiles.toArray(String[]::new));
        RDataUtils.saveFilesInclusion(sb, FilesInclusion, incFiles.size());

        sparam.initializeAdductList();

        sc.setIncludedFileNamesString(FilesInclusion);
        sc.setDataConfirmed(true);
        return "MS2 spectra";
    }

    @JsonIgnore
    private List<SwathBean> swathBeans = null;

    public List<SwathBean> getSwathBeans() {
        return swathBeans;
    }

    public void setSwathBeans(List<SwathBean> swathBeans) {
        this.swathBeans = swathBeans;
    }

    public String prepareDIASpec() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            jrd.record_prepareDIASpec(this);
            return null;
        }

        // we are check DIA swath design here
        String homeDir = sb.getCurrentUser().getHomeDir();
        if (sc.isExecutExample()) {
            if (Files.isDirectory(Paths.get("/Users/xia/Dropbox/"))) {
                homeDir = "/Users/xia/Dropbox/Current/Test/DIARawData/"; //xia office
            } else if (Files.isDirectory(Paths.get("/Users/jeffxia/Dropbox/"))) {
                homeDir = "/Users/jeffxia/Dropbox/Current/Test/DIARawData/"; //xia laptop
            } else { //public server
                homeDir = "/home/glassfish/projects/DIARawData/";
            }
        }

        // if is from google drive, download one ms2 file
        if (fromGoogleDrive) {
            RSpectraUtils.DownloadAnMS2File(sb.getRConnection());
        }

        File f;
        f = new File(homeDir + File.separator + "upload/MS2");
        String[] ms2files = f.list();

        for (String ms2file : ms2files) {
            System.out.println(" ====== ms2file ====--->  " + homeDir + File.separator + "upload/MS2/" + ms2file);
        }
        String ms2file = ms2files[0];
        String ms2file2detect = homeDir + File.separator + "upload/MS2/" + ms2file;

        int resx = RSpectraUtils.PerformSWATHDesignDetection(sb, ms2file2detect);
        if (resx == 1) {
            jrd.record_prepareDIASpec(this);
            double[] alllowMZs = RSpectraUtils.GetSWATHDesginLow(sb.getRConnection());
            double[] allhigMZs = RSpectraUtils.GetSWATHDesginUp(sb.getRConnection());

            List<SwathBean> swathbeans = new ArrayList();
            for (int i = 0; i < alllowMZs.length; i++) {
                swathbeans.add(new SwathBean(i + 1, alllowMZs[i], allhigMZs[i]));
            }
            swathBeans = swathbeans;
        } else {
            System.out.println(" ===== PerformSWATHDesignDetection ====---> " + resx);
            return "";
        }

        return "SWATH check";

        //return null;
    }

    @JsonIgnore
    private ListDataModel<MS2ResutlsBean> FeatureModelMS2 = null;

    public ListDataModel<MS2ResutlsBean> getFeatureModelMS2() {
        return FeatureModelMS2;
    }

    public void setFeatureModelMS2(ListDataModel<MS2ResutlsBean> FeatureModelMS2) {
        this.FeatureModelMS2 = FeatureModelMS2;
    }

    private String fragDB_path = "";

    public int plotMirrorMatching() {

        if (ab.isOnQiangPc()) {
            fragDB_path = "/data/COMPOUND_DBs/MSBUDDY/FragsAnnotateDB_v02042024.sqlite";
        } else if (ab.isOnProServer() || ab.isOnVipServer() || ab.isInDocker()) {
            fragDB_path = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
        }

        mirrorplot_imgNM = "mirror_plotting_" + resNum + "_" + subidx + "_150.png";
        mirrorplot_jsonNM = "mirror_plotting_" + resNum + "_" + subidx + "_150.json";
        int resx = RSpectraUtils.PerformMirrorPlottingWeb(sb.getRConnection(), fragDB_path,
                current_ft_label, resNum, subidx, 10, mirrorplot_imgNM, 150,
                "png", 10, 6);

        internalizeImage(mirrorplot_jsonNM);

        jsonHashMap.put(resNum + "", mirrorplot_jsonNM);
        imgsHashMap.put(resNum + "", mirrorplot_imgNM);
        refspecHMap.put(resNum + "", "reference_spectrum_" + resNum + "_" + subidx + ".txt");
        cmpdinfHMap.put(resNum + "", "compound_info_" + resNum + "_" + subidx + ".txt");

        System.out.println("mirror=====" + resNum);

        return resx;
    }

    private int resNum = 0;

    public int getResNum() {
        return resNum;
    }

    private int subidx = 0;

    public int getSubidx() {
        return subidx;
    }

    public void setSubidx(int subidx) {
        this.subidx = subidx;
    }

    public void setResNum(int resNum) {
        this.resNum = resNum;
    }

    private String current_ft_label;

    public String getCurrent_ft_label() {
        return current_ft_label;
    }

    public void setCurrent_ft_label(String current_ft_label) {
        this.current_ft_label = current_ft_label;
    }

    /// Select data files as inclusions
    public SpecBean getSelectedData() {
        return selectedData;
    }

    public void setSelectedData(SpecBean selectedData) {
        this.selectedData = selectedData;
    }

    public SpecBean getSelectedDataSet() {
        return selectedDataSet;
    }

    public void setSelectedDataSet(SpecBean dm) {

        if (!selectedDataSet.getName().equals(dm.getName())) {
            this.selectedDataSet = dm;
            sb.addMessage("info",
                    "Current data is: " + selectedDataSet.getName() + ", ready for analysis.");
        }
    }

    public void checkTablePopulated() throws FileNotFoundException {
        if (resBeans == null) {
            populateRawResBeans();
        }
        if (FeatureModel == null) {
            populateRawFeatureBeans();
        }
        if (FeatureModelMS2 == null) {
            populateRawMS2Beans();
        }
    }

    public void checkResultsInternalized() {

        // check results have been internalized or not
        File LoadingJson = new File(sb.getCurrentUser().getHomeDir() + "/spectra_3d_loading.json");

        if (LoadingJson.exists()) {
            String guestName = sb.getCurrentUser().getName();
            String myDir = ab.getRealUserHomePath() + guestName;

            File guestFolder = new File(myDir);
            if (!guestFolder.exists()) {
                guestFolder.mkdir();
            }

            //String currentScoreJson = myDir + "/spectra_3d_score" + NmIdx + ".json";
            String currentLoadingJson = myDir + "/spectra_3d_loading.json";

            File cjson = new File(currentLoadingJson);
            if (!cjson.exists()) {
                DataUtils.copyFile(LoadingJson, new File(currentLoadingJson));
            } else {
                System.out.println("Previous spectra_3d_loading found!");
            }
        } else {
            System.out.println("Job not finished yet!");
        }
    }

    public void reloadRawSpecBean() {
        // reload raw spec bean when it is empty [usually after project loading]
        if (specBeans == null) {
            if (containsMetaVal) {
                try {
                    wb.getCalledWorkflows().add("Spectra Check");
                    populateSpecBeans();
                } catch (Exception e) {
                    wb.getCalledWorkflowsError().add("Spectra Check");
                    System.out.println("CANNOT populateSpecBeans 1!");
                }
            } else {
                try {
                    wb.getCalledWorkflows().add("Spectra Check");
                    populateSpecBeansNoMeta();
                } catch (Exception e) {
                    wb.getCalledWorkflowsError().add("Spectra Check");
                    System.out.println("CANNOT populateSpecBeans 2!");
                }
            }
        }

    }

    ///Following part is used to handle the results display tasks
    /// 1. show the general statistics of the processing results
    public boolean populateRawResBeans() throws FileNotFoundException {
        resBeans = new ArrayList<>();

        String[] groupnames;

        // Creates a new File instance by converting the given pathname string
        // into an abstract pathname
        String homeDir = sb.getCurrentUser().getHomeDir();
        String path = "";

        if (ab.shouldUseScheduler() && !ab.isOnQiangPc() && !ab.isOnZgyPc() && !ab.isInDocker()) {
            path = ab.getProjectsHome() + sb.getCurrentUser().getName() + File.separator;
        } else if (ab.isOnLocalServer()) {
            path = homeDir + File.separator;
        } else if (ab.isInDocker()) {
            path = homeDir + File.separator;
        }

        if (sc.isExecutExample()) {
            path = "/home/glassfish/projects/MetaboDemoRawData/";
        }

        if (Files.isDirectory(Paths.get("/Users/xia/Dropbox/"))) {
            // Handle Jeff's local case. Never change or remove !
            path = "/Users/xia/Dropbox/Current/Test/MetaboDemoRawData/"; //xia local
        } else if (Files.isDirectory(Paths.get("/home/zgy/projects/MetaboDemoRawData"))) {
            path = "/home/zgy/projects/MetaboDemoRawData/";
        }
        System.out.println(path + "=====path");
        File f = new File(path + "upload/");
        // Populates the array with names of files and directories
        groupnames = f.list();

        int num = 0;

        // For each pathname in the pathnames array
        for (String groupname : groupnames) {
            File grp = new File(path + "upload/" + groupname);
            if (grp.isDirectory()) {
                num = num + grp.list().length;
            } else {
                num = groupnames.length;
            }
        }

        // Read in the peak_result_summary.txt
        String[][] peaksum = new String[num][];
        int index = 0;
        File file = new File(homeDir + "/peak_result_summary.txt");
        //File file = new File("/home/qiang/peak_result_summary.txt");
        if (!file.exists()) {
            sb.addMessage("Error", "The file (peak_result_summary.txt) is not found!");
            return false;
        }

        Scanner input = new Scanner(file);

        while (input.hasNextLine() && index < peaksum.length) {
            peaksum[index] = input.nextLine().split(" "); //split returns an array
            index++;
        }

        for (String[] a : peaksum) {
            try {
                resBeans.add(new RawResBean(a[0], a[1], a[2], a[3], Integer.parseInt(a[4]), Double.parseDouble(a[5])));
            } catch (Exception e) {
                System.out.println("certain file has been omitted for processing !");
            }
        }
        return true;
    }

    /// 2. show the details of feature of spectra processing
    public boolean populateRawFeatureBeans() {

        RConnection RC = sb.getRConnection();
        String homeDir = sb.getCurrentUser().getHomeDir();
        // create a new ArrayList to store the RawFeatureBean information
        ArrayList<RawFeatureBean> RawFeatureBeans = new ArrayList<>();
        int resSumNum = -10;
        if (sparam.getAlgorithms().equals("asari")) {
            resSumNum = RDataUtils.generateAsariPeakList(RC, homeDir);
        } else {
            resSumNum = RDataUtils.generatePeakList(RC, homeDir);
        }

        if (resSumNum == -10) {

            File file0 = new File(homeDir + "/peak_feature_summary.tsv");
            int index0 = 0;
            Scanner input0 = null;

            try {
                input0 = new Scanner(file0);
            } catch (FileNotFoundException ex) {
                Logger.getLogger(SpectraProcessBean.class.getName()).log(Level.SEVERE, null, ex);
            }

            while (input0.hasNextLine()) {
                String str = input0.nextLine().split("\t")[0];
                if (str.equals("")) {
                    break;
                }
                index0++;
            }
            resSumNum = index0 - 1;
        }

        if (!RSpectraUtils.ensureDataExits(RC, "peak_feature_summary.tsv")) {
            sb.addMessage("Error", "Results writing not finished yet, please try again!");
            return false;
        }
        if (resSumNum == -10) {
            sb.addMessage("Error", "Results populating failed, please try again!");
            return false;
        }

        int feature_num = resSumNum;
        String[][] feature_sum = new String[feature_num + 1][];

        File file = new File(homeDir + "/peak_feature_summary.tsv");
        int index = 0;
        Scanner input = null;
        try {
            input = new Scanner(file);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(SpectraProcessBean.class.getName()).log(Level.SEVERE, null, ex);
        }

        while (input.hasNextLine() && index < feature_sum.length + 1) {
            feature_sum[index] = input.nextLine().split("\t"); //split returns an array
            index++;
        }

        RawFeatureBean rfb;
        for (int i = 1; i < feature_sum.length; i++) {
            rfb = new RawFeatureBean(
                    i,
                    Double.parseDouble(feature_sum[i][0]), //mz
                    Double.parseDouble(feature_sum[i][1]), //rt
                    feature_sum[i][3] + " " + feature_sum[i][2], //iso + adduct
                    feature_sum[i][4], //formula
                    feature_sum[i][5], //cmpd
                    feature_sum[i][6], //HMDB
                    Double.parseDouble(feature_sum[i][7]), //intensity
                    Double.parseDouble(feature_sum[i][8]), //p val
                    Double.parseDouble(feature_sum[i][9]), //fdr
                    Double.parseDouble(feature_sum[i][10])); //cv
            RawFeatureBeans.add(rfb);
        }

        if (feature_sum[1][8].equals("-10")) {
            singlegroup = true;
        }
        FeatureModel = new ListDataModel(RawFeatureBeans);

        return true;
    }

    /// 3. show the details of MS2 database searching results
    public boolean populateRawMS2Beans() {

        if (ms2DataOpt.equals("ms1")) {
            return true;
        }
        // into an abstract pathname
        String homeDir = sb.getCurrentUser().getHomeDir();

        File file = new File(homeDir + "/compound_msn_results.tsv");

        if (!file.exists()) {
            sb.addMessage("Error", "The file (compound_msn_results.tsv) is not found!");
            return false;
        }
        ArrayList<MS2ResutlsBean> MS2ResutlsBeans = new ArrayList<>();
        try {
            Scanner input = new Scanner(file);
            String line_content;
            int row_idx = 0;

            String[] thisline_vec;
            double mz_min = 0, mz_max = 0, rt_min = 0, rt_max = 0, score_val = 0, mz, rt;
            String feature_label, unit_rt, compound_nm, formula_nm, inchikey_nm, db_nm;
            MS2ResutlsBean mfb;
            while (input.hasNextLine()) {

                line_content = input.nextLine();
                if (row_idx == 0) {
                    row_idx++;
                    continue;
                }

                thisline_vec = line_content.split("\t");

                if (sparam.getAlgorithms().equals("asari")) {
                    unit_rt = "min";
                } else {
                    unit_rt = "sec";
                }
                for (int i = 0; i < 4; i++) {
                    mz_min = Double.parseDouble(thisline_vec[0]);
                    mz_max = Double.parseDouble(thisline_vec[1]);
                    rt_min = Double.parseDouble(thisline_vec[2]);
                    rt_max = Double.parseDouble(thisline_vec[3]);
                }

                for (int jj = 0; jj < 5; jj++) {
                    mz = (mz_min + mz_max) / 2;
                    rt = (rt_min + rt_max) / 2;
                    feature_label = "mz" + String.format("%.4f", mz) + "@" + String.format("%.2f", rt) + unit_rt;
                    compound_nm = thisline_vec[jj * 5 + 4];
                    //System.out.println(" Now we are ===> populateRawMS2Beans ----- 840 ------====----> " + jj + compound_nm);
                    if (compound_nm.equals("NA")) {
                        break;
                    }
                    inchikey_nm = thisline_vec[jj * 5 + 5];
                    formula_nm = thisline_vec[jj * 5 + 6];
                    score_val = Double.parseDouble(thisline_vec[jj * 5 + 7]);
                    db_nm = thisline_vec[jj * 5 + 8];
                    mfb = new MS2ResutlsBean(row_idx, jj,
                            mz_min, mz_max, rt_min, rt_max,
                            score_val, 0, formula_nm, compound_nm, "", inchikey_nm,
                            0, feature_label, db_nm);
                    MS2ResutlsBeans.add(mfb);
                }

                row_idx++;
            }

            FeatureModelMS2 = new ListDataModel(MS2ResutlsBeans);
        } catch (Exception e) {
            sb.addMessage("Error", "Failed to read MS/MS identification results!");
            return false;
        }

        return true;
    }

    public List<RawResBean> getResBeans() {
        return resBeans;
    }

    public void setResBeans(List<RawResBean> resBeans) {
        this.resBeans = resBeans;
    }

    public ListDataModel<RawFeatureBean> getRawFeatureBean() {
        return FeatureModel;
    }

    public ListDataModel<MetSetBean> getFomula2cmpdSet() {
        RConnection RC = sb.getRConnection();
        Iterator<RawFeatureBean> it = FeatureModel.iterator();
        int fmls_len = 0;
        String[] formus = {""};
        ArrayList<MetSetBean> MetSetBeans = new ArrayList<>();
        MetSetBean msb;
        while (it.hasNext()) {
            RawFeatureBean thisIt = it.next();
            int fno = thisIt.getFeatureNo();
            if (featureOrder == 0) {
                break;
            }
            if (fno == featureOrder) {
                String fmls = thisIt.getFormulas();
                formus = fmls.split("; ");
                fmls_len = formus.length;
                break;
            }
        }
        if (fmls_len != 0) {
            for (int i = 0; i < fmls_len; i++) {
                String cmpds = RSpectraUtils.extractHMDBCMPD(RC, formus[i], featureOrder);
                String formu = "<a href=https://pubchem.ncbi.nlm.nih.gov/#query=" + formus[i] + " target='_blank'>" + formus[i] + "</a>" + "; ";
                msb = new MetSetBean(
                        formu, cmpds, "");
                MetSetBeans.add(msb);
            }
        }

        //ArrayList<MetSetBean> libVec = new ArrayList();
        //libVec.add(new MetSetBean(details[0], details[1], ""));
        //return libVec.toArray(new MetSetBean[0]);
        fomula2cmpdSet = new ListDataModel(MetSetBeans);
        return fomula2cmpdSet;
    }

    public int getFeatureOrder() {
        return featureOrder;
    }

    public void setFeatureOrder(int featureOrder) {
        this.featureOrder = featureOrder;
    }

    public String getMsgText() {
        return msgText;
    }

    public String getErrMsgText() {
        return errMsgText;
    }

    public void setErrMsgText(String errMsgText) {
        this.errMsgText = errMsgText;
    }

    public void clearErrorMsg() {
        errMsgText = "";
    }

    public void populateSpecBeansNoMeta() throws REXPMismatchException {
        specBeans = new ArrayList();

        RConnection RC = sb.getRConnection();

        // Creates a new File instance by converting the given pathname string
        // into an abstract pathname
        String homeDir = sb.getCurrentUser().getHomeDir();

        // Populates the array with names of files and directories
        // For each pathname in the pathnames array
        String rCommand = "CentroidCheck(\'" + "REPLACE_WITH_YOUR_SINGLE_FILE_PATH" + "\')";
        RCenter.recordRCommand(RC, rCommand);
        spectraFiles = new ArrayList<>();
        centroidFileCount = 0;

        File grp = new File(homeDir + File.separator + "upload");
        String[] spectranames = grp.list();
        for (String spectraname : spectranames) {

            File spec = new File(homeDir + "/upload/" + spectraname);
            String fileSize = humanReadableByteCountBin(spec.length());
            String filepath = homeDir + "/upload/" + spectraname;
            if (!spec.isFile()) {
                continue;
            }
            boolean mode = RDataUtils.checkCentroid(RC, filepath);

            spectraFiles.add(spectraname);

            String modec = null;
            boolean include = true;
            boolean disabled = false;
            if (mode) {
                modec = "True";
                centroidFileCount = centroidFileCount + 1;
            } else {
                modec = "<b><font color=\"red\">False</font></b>";
                include = false;
                disabled = true;
                //bnDisabled = true;
            }
            String classnm = "Sample";
            if (spectraname.startsWith("QC_")) {
                classnm = "QC";
            } else if (spectraname.startsWith("BLANK_")) {
                classnm = "BLANK";
            }

            String mslevel = "MS1";
            if (spectraname.startsWith("MS2_")) {
                mslevel = "MS2";
            }

            specBeans.add(new SpecBean(spectraname, modec, fileSize, classnm, "intensity", include, disabled, mslevel));
        }

        bnDisabled = centroidFileCount <= 2;
        setSpectraFiles(spectraFiles);

    }

    public List<String> getMsgList() {
        return msgList;
    }

    public void setMsgList(List<String> msgList) {
        this.msgList = msgList;
    }

    public String getJobStatusText() throws IOException {

        String filnm = sb.getCurrentUser().getHomeDir() + "/metaboanalyst_spec_proc.txt";
        StringBuilder s = new StringBuilder();
        String real_path = ab.getRealPath();
        try {
            //Process p = Runtime.getRuntime().exec("tail -" + 18 + " " + filnm);
            Process p;
            String cmd = "tail -" + 18 + " " + filnm;
            List<String> commands = Arrays.asList(cmd.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            p = processBuilder.start();

            java.io.BufferedReader input = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream()));
            String line = null;
            while ((line = input.readLine()) != null) {
                if (line.contains("ERROR:")) {
                    line = line.replace("ERROR:", "<b>ERROR:</b>");
                } else if (line.contains("Step")) {
                    line = line.replace(line, "<b><font color=\"#ff8c00\">" + line + "</font></b>");
                } else if (line.contains("Everything has been finished successfully!")) {
                    line = line.replace("Everything has been finished successfully!", "<b>Everything has been finished successfully!</b>");
                } else if (line.contains(real_path)) {
                    line = line.replace(real_path, "");
                }
                if (line.contains("slurmstepd: error")
                        || line.startsWith(">")
                        || line.startsWith("+")
                        || line.startsWith("\\>")
                        || line.startsWith("\\+")
                        || line.contains("OptiLCMS")) {
                } else {
                    String newLine = line + "<br />";
                    s.append(newLine);
                }
            }
        } catch (java.io.IOException e) {
            System.out.println("An error occurred.");
        }
        return s.toString();
    }

    public void includeMessage(boolean val, String name) {
        // System.out.println(val + "===includemssg");
        String summary = val ? name + " is included." : name + " is excluded.";
        sb.addMessage("info", summary);
    }

    // Section 4 : output files section
    public StreamedContent getDefaultParamFile() throws IOException {
        RConnection RC = sb.getRConnection();
        boolean resparam = RSpectraUtils.generateParamFile(RC);
        if (resparam) {
            return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/param_file.txt");
        }
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/param_default.txt");
    }

    public StreamedContent getOptimizedParamFile() throws IOException {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/param_optimized.txt");
    }

    public StreamedContent getTextOutputFile() throws IOException {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/metaboanalyst_spec_proc.txt");
    }

    public void internalizeRes(int num) {
        if (ab.isOnProServer() || ab.isInDocker()) {
            String NmIdx = (num == 0) ? "" : Integer.toString(num);

            //File ScoreJson = new File(sb.getCurrentUser().getHomeDir() + "/spectra_3d_score" + NmIdx + ".json");
            File LoadingJson = new File(sb.getCurrentUser().getHomeDir() + "/spectra_3d_loading" + NmIdx + ".json");
            File mSetObj = new File(sb.getCurrentUser().getHomeDir() + "/mSet.rda");

            if (LoadingJson.exists()) {
                String guestName = sb.getCurrentUser().getName();
                String myDir = ab.getRealUserHomePath() + guestName;

                File guestFolder = new File(myDir);
                if (!guestFolder.exists()) {
                    guestFolder.mkdir();
                }

                //String currentScoreJson = myDir + "/spectra_3d_score" + NmIdx + ".json";
                String currentLoadingJson = myDir + "/spectra_3d_loading" + NmIdx + ".json";
                String currentmSetObj = myDir + "/mSet.rda";

                //DataUtils.copyFile(ScoreJson, new File(currentScoreJson));
                DataUtils.copyFile(LoadingJson, new File(currentLoadingJson));
                if (num == 0) {
                    DataUtils.copyFile(mSetObj, new File(currentmSetObj));
                }
            }
        }
    }

    public void internalizecomputeEncasingRes() {
        if (ab.isOnProServer() || ab.isInDocker()) {
            // encasing_mesh
            File encasingJson = new File(sb.getCurrentUser().getHomeDir() + "/");
            String[] fnames = encasingJson.list();
            String guestName = sb.getCurrentUser().getName();
            String myDir = ab.getRealUserHomePath() + guestName;

            for (String f : fnames) {
                if (f.startsWith("encasing_mesh") & f.endsWith(".json")) {
                    File meshJson = new File(sb.getCurrentUser().getHomeDir() + "/" + f);
                    if (meshJson.exists()) {
                        String currentmeshJson = myDir + "/" + f;
                        DataUtils.copyFile(meshJson, new File(currentmeshJson));
                    }
                }
            }
        }
    }

    public boolean summarizeProcessingMsg() {

        RConnection RC = sb.getRConnection();
        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = RSpectraUtils.getResSummaryMsg(RC);

        if (msgArray == null) {
            return false;
        }
        msgVec.addAll(Arrays.asList(msgArray));
        String msg;
        msg = "<table face=\"times\" size = \"3\">";
        msg = msg + "<tr><th> Raw Spectra Processing Result Summary: " + "</th></tr>";

        for (int i = 0; i < msgVec.size(); i++) {
            msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
        }
        msg = msg + "</table>";
        msgText = msg;

        return true;
    }

    public void internalizeImage(String imageNm) {
        if (ab.isOnProServer() || ab.isInDocker()) {

            File ImageFile = new File(sb.getCurrentUser().getHomeDir() + "/" + imageNm);
            if (ImageFile.exists()) {
                String guestName = sb.getCurrentUser().getName();
                String myDir = ab.getRealUserHomePath() + guestName;

                File guestFolder = new File(myDir);
                if (!guestFolder.exists()) {
                    guestFolder.mkdir();
                }

                String InternalImageFile = myDir + "/" + imageNm;
                DataUtils.copyFile(ImageFile, new File(InternalImageFile));

            }
        }

    }

    private void internalizeEIC(int featureNum) {
        if (ab.isOnProServer() || ab.isInDocker()) {
            File JsonFile = new File(sb.getCurrentUser().getHomeDir() + "/" + featureNum + ".json");
//            File svgFile = new File(sb.getCurrentUser().getHomeDir() + "/" + featureNum + ".svg");
//            File svgcoordFile = new File(sb.getCurrentUser().getHomeDir() + "/" + featureNum + ".svg.coords.js");
//            File svgmappingFile = new File(sb.getCurrentUser().getHomeDir() + "/" + featureNum + ".svg.mappings.js");
//            File svgutilsFile = new File(sb.getCurrentUser().getHomeDir() + "/" + featureNum + ".svg.utils.js");

            if (JsonFile.exists()) {
                String guestName = sb.getCurrentUser().getName();
                String myDir = ab.getRealUserHomePath() + guestName;

                File guestFolder = new File(myDir);
                if (!guestFolder.exists()) {
                    guestFolder.mkdir();
                }

                String InternalJsonFile = myDir + "/" + featureNum + ".json";
//                String InternalsvgFile = myDir + "/" + featureNum + ".svg";
//                String InternalsvgcoordFile = myDir + "/" + featureNum + ".svg.coords.js";
//                String InternalsvgmappingFile = myDir + "/" + featureNum + ".svg.mappings.js";
//                String InternalsvgutilsFile = myDir + "/" + featureNum + ".svg.utils.js";

                DataUtils.copyFile(JsonFile, new File(InternalJsonFile));
//                DataUtils.copyFile(svgFile, new File(InternalsvgFile));
//                DataUtils.copyFile(svgcoordFile, new File(InternalsvgcoordFile));
//                DataUtils.copyFile(svgmappingFile, new File(InternalsvgmappingFile));
//                DataUtils.copyFile(svgutilsFile, new File(InternalsvgutilsFile));
            }
        }
    }

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

    public String getDataStrucImageNM() {
        return DataStrucImageNM;
    }

    public void setDataStrucImageNM(String DataStrucImageNM) {
        this.DataStrucImageNM = DataStrucImageNM;
    }

    public String getFileNameImage() {
        return FileNameImage;
    }

    public void setFileNameImage(String FileNameImage) {
        this.FileNameImage = FileNameImage;
    }

    public double getRtl() {
        return rtl;
    }

    public void setRtl(double rtl) {
        this.rtl = rtl;
    }

    public double getRth() {
        return rth;
    }

    public void setRth(double rth) {
        this.rth = rth;
    }

    public double getMzl() {
        return mzl;
    }

    public void setMzl(double mzl) {
        this.mzl = mzl;
    }

    public double getMzh() {
        return mzh;
    }

    public void setMzh(double mzh) {
        this.mzh = mzh;
    }

    public int getRes() {
        return res;
    }

    public void setRes(int res) {
        this.res = res;
    }

    public String getDimen() {
        return dimen;
    }

    public void setDimen(String dimen) {
        this.dimen = dimen;
    }

    public void plotMSData() {
        if (res > 1000) {
            res = 1000;
            setRes(1000);
        }
        RConnection RC = sb.getRConnection();
        DataStrucImageNM = RDataUtils.plotMSData(RC, FileNameImage, mzl, mzh, rtl, rth, res, dimen);
        setDataStrucImageNM(DataStrucImageNM);
    }

    public String getSvgStringBox() {
        return svgStringBox;
    }

    public void setSvgStringBox(String svgStringBox) {
        this.svgStringBox = svgStringBox;
    }

    public String getSingleXICPlot() {
        return singleXICPlot;
    }

    public void setSingleXICPlot(String singleXICPlot) {
        this.singleXICPlot = singleXICPlot;
    }

    public void resetSingleXIC() {
        singleXICPlot = "/resources/images/EICalt.png";
        RSpectraUtils.cleanEIClayer(sb.getRConnection(), featureNum);
    }

    public boolean isBnDisabled() {
        return bnDisabled;
    }

    public boolean getBnDisabled() {
        return bnDisabled;
    }

    public void setBnDisabled(boolean bnDisabled) {
        this.bnDisabled = bnDisabled;
    }

    public boolean isSinglegroup() {
        return singlegroup;
    }

    public void setSinglegroup(boolean singlegroup) {
        this.singlegroup = singlegroup;
    }

    public boolean isRecordCMD() {
        return recordCMD;
    }

    public void setRecordCMD(boolean recordCMD) {
        this.recordCMD = recordCMD;
    }

    private String ms2DataOpt = "ms1";

    public String getMs2DataOpt() {
        return ms2DataOpt;
    }

    public void setMs2DataOpt(String ms2DataOpt) {
        this.ms2DataOpt = ms2DataOpt;
    }

    private boolean isms2 = !"ms1".equals(ms2DataOpt);

    @JsonIgnore
    public boolean isIsms2() {
        return !"ms1".equals(ms2DataOpt);
    }

    public void setIsms2(boolean isms2) {
        this.isms2 = isms2;
    }

    @JsonIgnore
    public boolean isIsms2DIA() {
        return "swath".equals(ms2DataOpt);
    }

    private String mirrorplot_jsonNM;

    public String getMirrorplot_jsonNM() {
        return mirrorplot_jsonNM;
    }

    public void setMirrorplot_jsonNM(String mirrorplot_jsonNM) {
        this.mirrorplot_jsonNM = mirrorplot_jsonNM;
    }

    @JsonIgnore
    private String jsonDir;

    @JsonIgnore
    public String getJsonDir() {
        String image_json_name = mirrorplot_jsonNM;
        if (!jsonHashMap.isEmpty()) {
            if (jsonHashMap.containsKey(resNum + "")) {
                image_json_name = jsonHashMap.get(resNum + "");
            }
        }

        if (ab.isOnProServer() || ab.isOnVipServer() || ab.isInDocker() || ab.isOnQiangPc()) {
            String guestName = sb.getCurrentUser().getName();
            String myDir = "/resources/users/" + guestName;
            jsonDir = myDir + "/" + image_json_name;
        } else {
            jsonDir = sb.getCurrentUser().getRelativeDir() + "/" + image_json_name;
        }

        return jsonDir;
    }

    public void setJsonDir(String jsonDir) {
        this.jsonDir = jsonDir;
    }

    private String mirrorplot_imgNM;

    public String getMirrorplot_imgNM() {
        return mirrorplot_imgNM;
    }

    public void setMirrorplot_imgNM(String mirrorplot_imgNM) {
        this.mirrorplot_imgNM = mirrorplot_imgNM;
    }

    @JsonIgnore
    private StreamedContent singleMirrorImage, refSpecTxt, cmpdInfoTxt;

    public StreamedContent getSingleMirrorImage() {
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        try {
            singleMirrorImage = DataUtils.getStreamedImage(filepath, mirrorplot_imgNM);
        } catch (Exception e) {
        }
        return singleMirrorImage;
    }

    public StreamedContent getRefSpecTxt() {
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        try {
            refSpecTxt = DataUtils.getStreamedImage(filepath, "reference_spectrum_" + resNum + "_" + subidx + ".txt");
        } catch (Exception e) {
        }
        return refSpecTxt;
    }

    public StreamedContent getCmpdInfoTxt() {
        String filepath = sb.getCurrentUser().getHomeDir() + "/";
        try {
            cmpdInfoTxt = DataUtils.getStreamedImage(filepath, "compound_info_" + resNum + "_" + subidx + ".txt");
        } catch (Exception e) {
        }
        return cmpdInfoTxt;
    }

    public boolean isContainsMetaVal() {
        return containsMetaVal;
    }

    public void setContainsMetaVal(boolean containsMetaVal) {
        this.containsMetaVal = containsMetaVal;
    }

    private boolean fromGoogleDrive = false;

    public boolean isFromGoogleDrive() {
        return fromGoogleDrive;
    }

    public void setFromGoogleDrive(boolean fromGoogleDrive) {
        this.fromGoogleDrive = fromGoogleDrive;
    }

    @JsonIgnore
    public String getCentroidColName() {
        if (fromGoogleDrive) {
            return "Size Accept";
        } else {
            return "Centroid";
        }
    }

}
