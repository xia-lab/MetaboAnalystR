/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
//clean memory arraylist uploaded files...
//explicitly assign arraylist to null
package pro.metaboanalyst.spectra;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import jakarta.annotation.PreDestroy;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.inject.Named;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.primefaces.event.FileUploadEvent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.ActionEvent;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.ResourceSemaphore;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RSpectraUtils;
import org.primefaces.PrimeFaces;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.workflows.JavaRecord;

/**
 *
 * @author qiang
 */
@SessionScoped
@Named("specLoader")
public class SpectraUploadBean implements Serializable {

    @Inject
    private ApplicationBean1 ab;

    @Inject
    private SessionBean1 sb;

    @Inject
    private SpectraProcessBean pkb;

    @Inject
    private SpectraControlBean pcb;

    @Inject
    private ResourceSemaphore resourceSemaphore;

    @Inject
    private SpectraParamBean spb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private boolean spectraUploaded = false;

    public boolean isSpectraUploaded() {
        return spectraUploaded;
    }

    public void setSpectraUploaded(boolean spectraUploaded) {
        this.spectraUploaded = spectraUploaded;
    }

    private String selectedExample = "fastdata";

    public String getSelectedExample() {
        return selectedExample;
    }

    public void setSelectedExample(String selectedExample) {
        this.selectedExample = selectedExample;
    }

    private List<String> uploadedFileNames = new ArrayList<>();

    public List<String> getUploadedFileNames() {
        return uploadedFileNames;
    }

    public void setUploadedFileNames(List<String> uploadedFileNames) {
        this.uploadedFileNames = uploadedFileNames;
    }

    public void doSpectraReset() {

        if (sb.isLoggedIn()) {
            if (sb.getRConnection() != null) {
                sb.getRConnection().close();
            }
            sb.setLoggedIn(false);
        }
        FacesContext.getCurrentInstance().getExternalContext().invalidateSession();
        FacesContext.getCurrentInstance().getViewRoot().getViewMap().clear();
        DataUtils.doRedirect("/MetaboAnalyst/Secure/upload/SpectraUpload.xhtml", ab);
    }

    private String[] fileNmsFromMeta;
    private String[] classNmsFromMeta;
    private String metaName;
    private HashMap<String, String> fileAndClass = new HashMap();

    private boolean zeroMeta = false;

    private int upLoadingCount = 0;

    private boolean containsMeta = false;

    public boolean isContainsMeta() {
        return containsMeta;
    }

    public void setContainsMeta(boolean containsMeta) {
        this.containsMeta = containsMeta;
    }

    public void updateText() {

        if (pkb.getMsgList().isEmpty()) {
            sb.addMessage("info", "File upload is complete! Click the <b>Proceed</b> button to the next page.");
        } else {
            String msg = "<table face=\"times\" size = \"3\">";
            for (int i = 0; i < pkb.getMsgList().size(); i++) {
                msg = msg + "<tr><td align=\"left\">" + pkb.getMsgList().get(i) + "</td></tr>";
            }
            msg = msg + "</table>";
            if (!pkb.getMsgList().isEmpty()) {
                pkb.setErrMsgText(msg);
            }
            sb.addMessage("Error", pkb.getErrMsgText());
            // System.out.println("console.log(\"" + msg + "\")");
            // PrimeFaces.current().executeScript("console.log(\"" + msg + "\")");
        }
    }

    private final int ACQUIRE_TIMEOUT = 10;

    public void handleFileUpload(FileUploadEvent event) {

        Semaphore semaphore = resourceSemaphore.getUploadSemaphore();

        try {
            if (semaphore.tryAcquire(ACQUIRE_TIMEOUT, TimeUnit.SECONDS)) {
                // can upload
                UploadedFile file = event.getFile();

                String fileName = file.getFileName();
                if (file.getSize() == 0) {
                    pkb.getMsgList().add("File is empty + " + fileName);
                    sb.addMessage("Error", "File is empty + " + fileName);
                    return;
                }

                //max 200 MB per zip file
                if (file.getSize() > ab.getMAX_SPEC_SIZE()) {
                    pkb.getMsgList().add(fileName + " is too large. Use MetaboAnalystR for processing high-resolution files (over 200 MB).");
                    sb.addMessage("Error", fileName + " is too large. Use MetaboAnalystR for processing high-resolution files (over 200 MB).");
                    return;
                }

                sb.addMessage("info", fileName + " is uploaded.");

                String homeDir = sb.getCurrentUser().getHomeDir();

                if (fileName.contains(".txt") || fileName.contains(".csv")) {
                    pkb.getMsgList().removeAll(pkb.getMsgList());
                    pkb.setErrMsgText("");
                    pcb.setMetaOk(false);

                    metaName = DataUtils.uploadFile(sb, file, homeDir, null, false);
                    int rawOK = RDataUtils.readRawMeta(sb.getRConnection(), metaName);

                    if (rawOK == 0) {
                        sb.addMessage("Error", "There are errors in reading you meta-data file!");
                        return;
                    }

                    fileNmsFromMeta = RDataUtils.getRawFileNames(sb.getRConnection());
                    classNmsFromMeta = RDataUtils.getRawClassNames(sb.getRConnection());

                    //map fileName to Class
                    for (int i = 0; i < fileNmsFromMeta.length; i++) {
                        fileAndClass.put(fileNmsFromMeta[i], classNmsFromMeta[i]);
                    }

                    File destDir = new File(homeDir + File.separator + "upload");
                    if (!destDir.exists()) {
                        destDir.mkdir();
                    }
                    pcb.setMetaOk(true);
                    containsMeta = true;

                } else {
                    if (containsMeta) {
                        if (metaName.equals("")) {
                            pkb.getMsgList().add("Please make sure that meta-data file in the .txt format is provided!");
                        }
                        String trimmedFileNm = fileName.replace(".zip", "");
                        if (!Arrays.asList(fileNmsFromMeta).contains(trimmedFileNm)) {
                            pkb.getMsgList().add("'<b>" + trimmedFileNm + "</b>' is not found in Meta-data!");
                        }
                        String className = fileAndClass.get(trimmedFileNm);
                        File classFolder = new File(homeDir + File.separator + "upload" + File.separator + className);
                        if (!classFolder.exists()) {
                            classFolder.mkdir();
                        }
                        fileName = DataUtils.uploadFile(sb, file, homeDir, null, false);

                        int res = DataUtils.unzipDataRaw(homeDir + File.separator + fileName, homeDir + File.separator + "upload" + File.separator + className);
                        switch (res) {
                            case -1:
                                pkb.getMsgList().add("<b>'" + DataUtils.getRawUploadErrorFile() + "'</b> must contain .mzXML, .mzML or .mzData files only.");
                            case 0:
                                pkb.getMsgList().add("Unable to unzip the files.");
                            case 1:
                                File zipToBeDeleted = new File(homeDir + File.separator + file.getFileName());
                                zipToBeDeleted.delete();
                                break;
                            default:
                                break;
                        }
                    } else {
                        fileName = DataUtils.uploadFile(sb, file, homeDir, null, false);
                        int res = DataUtils.unzipDataRaw(homeDir + File.separator + fileName, homeDir + File.separator + "upload");
                        switch (res) {
                            case -1:
                                pkb.getMsgList().add("<b>'" + DataUtils.getRawUploadErrorFile() + "'</b> must contain .mzXML, .mzML or .mzData files only.");
                            case 0:
                                pkb.getMsgList().add("Unable to unzip the files.");
                            case 1:
                                File zipToBeDeleted = new File(homeDir + File.separator + file.getFileName());
                                zipToBeDeleted.delete();
                                break;
                            default:
                                break;
                        }
                    }

                    uploadedFileNames.add(fileName);
                    pkb.addUploadedFileNamesSaved(fileName);
                }

                if (upLoadingCount == 0) {
                    zeroMeta = containsMeta;
                } else {
                    if (zeroMeta != containsMeta) {
                        pkb.getMsgList().add("Uplaoding with metadata or not must be consistent for mutiple uploading. Click 'Reset' to re-upalod!");
                        return;
                    }
                }
                upLoadingCount++;

                //return permission
                semaphore.release();

            } else {
                // code for timed-out
                sb.addMessage("warn",
                        "Too many users are uploading spectra at this time. Please try again later!");

            }
        } catch (InterruptedException e) {
            // code for timed-out
            sb.addMessage("error",
                    "Too many users are uploading spectra at this time. Please try again later!");

        }

    }

    public String uploadExampleSpectra() throws Exception {

        if (pcb.checkJobRunning()) {
            PrimeFaces.current().executeScript("PF('uploadSessionDialog').show()");
            return null;
        }

        return switch (selectedExample) {
            case "malaria" ->
                uploadMalariaExample();
            case "dda_example" ->
                uploadDDAExample();
            case "dia_example" ->
                uploadDIAExample();
            case "exposome_example" ->
                uploadExposomeExample();
            default ->
                uploadIBDExample();
        };

    }

    private String uploadIBDExample() throws REXPMismatchException {

        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return null;
        }

        String datadir = "/home/glassfish/projects/MetaboDemoRawData/upload";
        String homeDir = sb.getCurrentUser().getHomeDir();

        File srcDir = new File(datadir);
        System.out.println("srcDir =====> " + srcDir.exists());
        if (srcDir.exists()) {
            //DataUtils.copyDir(datadir, homeDir + File.separator + "upload");
            try {
                Path symbolic = Paths.get(datadir);
                Path target = Paths.get(homeDir + File.separator + "upload");
                Files.createSymbolicLink(target, symbolic);
            } catch (IOException ex) {
                System.out.println("This data is not found in your local, will start a downloading!");
            }
        }

        sb.setDataUploaded();
        pcb.setExecutExample(true);
        pkb.populateSpecBeans();
        pcb.setTotalNumberOfSamples(8);
        sb.setSaveEnabled(true);
        spb.setPolarity("negative");
        spectraUploaded = true;
        return "Spectra check";
    }

    private String uploadMalariaExample() throws REXPMismatchException {

        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return null;
        }

        String datadir = "/home/glassfish/projects/MetaboMalariaRawData/upload";
        String homeDir = sb.getCurrentUser().getHomeDir();

        File srcDir = new File(datadir);

        if (srcDir.exists()) {
            //DataUtils.copyDir(datadir, homeDir + File.separator + "upload");
            try {
                Path symbolic = Paths.get(datadir);
                Path target = Paths.get(homeDir + File.separator + "upload");
                Files.createSymbolicLink(target, symbolic);
            } catch (IOException ex) {
                System.out.println("This data is not found in your local, will start a downloading!");
            }
        }

        File markerfile = new File(homeDir + File.separator + "upload/QC/QC_001.mzML");
        if (!markerfile.exists()) {
            int res = RSpectraUtils.getMalariaRawData(sb.getRConnection(), homeDir);
            if (res == 0) {
                sb.addMessage("error", "Sorry! Cannot find your data! Please ask zhiqiang.pang@xialab.ca!");
            }
        }

        pcb.setMetaOk(true);
        sb.setDataUploaded();
        containsMeta = true;
        pkb.setContainsMetaVal(containsMeta);
        pkb.populateSpecBeans();
        pcb.setTotalNumberOfSamples(30);
        spb.setPolarity("positive");
        spectraUploaded = true;
        return "Spectra check";
    }

    private String uploadDDAExample() throws REXPMismatchException {

        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return null;
        }
        String datadir = "/home/glassfish/projects/DDARawData/upload";
        String homeDir = sb.getCurrentUser().getHomeDir();

        File srcDir = new File(datadir);

        if (srcDir.exists()) {
            //DataUtils.copyDir(datadir, homeDir + File.separator + "upload");
            try {
                Path symbolic = Paths.get(datadir);
                Path target = Paths.get(homeDir + File.separator + "upload");
                Files.createSymbolicLink(target, symbolic);
            } catch (IOException ex) {
                System.out.println("This data is not found in your local, will start a downloading!");
            }
        }

        File markerfile = new File(homeDir + File.separator + "upload/MS2/QCDDA_SCAN1_01.mzML");
        if (!markerfile.exists()) {
            int res = RSpectraUtils.getBloodRawData(sb.getRConnection(), homeDir);
            if (res == 0) {
                sb.addMessage("error", "Sorry! Cannot find your data! Please ask zhiqiang.pang@xialab.ca!");
            }
        }

        pcb.setMetaOk(true);
        sb.setDataUploaded();
        containsMeta = true;
        pkb.setContainsMetaVal(containsMeta);
        pkb.setIsms2(true);
        pkb.setMs2DataOpt("dda");
        pkb.populateSpecBeans();
        sb.setSaveEnabled(true);
        pcb.setTotalNumberOfSamples(30);
        spb.setPolarity("positive");
        spectraUploaded = true;

        return "Spectra check";
    }

    private String uploadDIAExample() throws REXPMismatchException {

        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return null;
        }
        String datadir = "/home/glassfish/projects/DIARawData/upload";
        String homeDir = sb.getCurrentUser().getHomeDir();

        File srcDir = new File(datadir);

        if (srcDir.exists()) {
            //DataUtils.copyDir(datadir, homeDir + File.separator + "upload");
            try {
                Path symbolic = Paths.get(datadir);
                Path target = Paths.get(homeDir + File.separator + "upload");
                Files.createSymbolicLink(target, symbolic);
            } catch (IOException ex) {
                System.out.println("This data is not found in your local, will start a downloading!");
            }
        }

        File markerfile = new File(homeDir + File.separator + "upload/MS2/Covid_Cov_16_MS2.mzML");
        if (!markerfile.exists()) {
            int res = RSpectraUtils.getCOVIDRawData(sb.getRConnection(), homeDir);
            if (res == 0) {
                sb.addMessage("error", "Sorry! Cannot find this data! Please ask zhiqiang.pang@xialab.ca!");
            }
        }

        pcb.setMetaOk(true);
        pcb.setIsSWATHExample(true);
        sb.setDataUploaded();
        containsMeta = true;
        pkb.setContainsMetaVal(containsMeta);
        pkb.setIsms2(true);
        pkb.setMs2DataOpt("swath");
        pkb.populateSpecBeans();
        sb.setSaveEnabled(true);
        pcb.setTotalNumberOfSamples(16);
        spb.setPolarity("negative");
        spectraUploaded = true;

        return "Spectra check";
    }

    public void updateMS2status() {
        System.out.println("pkb.isIsms2() == updateMS2status => " + pkb.isIsms2());
        if (ab.isInDocker()) {
            String database_path = "/home/glassfish/sqlite/MS2ID_Complete_v09102023.sqlite";
            String fragmentDB_pth = "/home/glassfish/sqlite/FragsAnnotateDB_v02042024.sqlite";
            String NLdb_path = "/home/glassfish/sqlite/MS2ID_CompleteNL_v09102023.sqlite";
            String NLfragDB_path = "/home/glassfish/sqlite/FragsAnnotateDBNL_v02042024.sqlite";
            File db_sqlite = new File(database_path);
            if (!db_sqlite.exists()) {
                pkb.setIsms2(false);
                pkb.setMs2DataOpt("ms1");
                sb.addMessage("Error", "Please make sure MS2ID_Complete_v09102023.sqlite has been successfully attached into your docker!");
                return;
            }
            File db_sqlite2 = new File(fragmentDB_pth);
            if (!db_sqlite2.exists()) {
                pkb.setIsms2(false);
                pkb.setMs2DataOpt("ms1");
                sb.addMessage("Error", "Please make sure FragsAnnotateDB_v02042024.sqlite has been successfully attached into your docker!");
                return;
            }
            File db_sqlite3 = new File(NLdb_path);
            if (!db_sqlite3.exists()) {
                pkb.setIsms2(false);
                pkb.setMs2DataOpt("ms1");
                sb.addMessage("Error", "Please make sure MS2ID_CompleteNL_v09102023.sqlite has been successfully attached into your docker!");
                return;
            }
            File db_sqlite4 = new File(NLfragDB_path);
            if (!db_sqlite4.exists()) {
                pkb.setIsms2(false);
                pkb.setMs2DataOpt("ms1");
                sb.addMessage("Error", "Please make sure FragsAnnotateDBNL_v02042024.sqlite has been successfully attached into your docker!");
                return;
            }
            System.out.println("All required database exits!");
        }
    }

    
    private String uploadExposomeExample() throws REXPMismatchException {
        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return null;
        }
        String datadir = "/home/glassfish/projects/Exposome_example/upload";
        String homeDir = sb.getCurrentUser().getHomeDir();

        File srcDir = new File(datadir);

        if (srcDir.exists()) {
            //DataUtils.copyDir(datadir, homeDir + File.separator + "upload");
            try {
                Path symbolic = Paths.get(datadir);
                Path target = Paths.get(homeDir + File.separator + "upload");
                Files.createSymbolicLink(target, symbolic);
            } catch (IOException ex) {
                System.out.println("This data is not found in your local, will start a downloading!");
            }
        }

        File markerfile = new File(homeDir + File.separator + "upload/Worker/Sample010.mzML");
        if (!markerfile.exists()) {
            int res = RSpectraUtils.getExposomeRawData(sb.getRConnection(), homeDir);
            if (res == 0) {
                sb.addMessage("error", "Sorry! Cannot find this data! Please ask zhiqiang.pang@xialab.ca!");
            }
        }

        pcb.setMetaOk(true);
        sb.setDataUploaded();
        containsMeta = true;
        pkb.setIsms2(true);
        pkb.setMs2DataOpt("dda");
        pkb.setContainsMetaVal(containsMeta);
        pkb.populateSpecBeans();
        sb.setSaveEnabled(true);
        pcb.setTotalNumberOfSamples(19);
        spb.setPolarity("negative");
        spectraUploaded = true;

        return "Spectra check";
    }

    
    public String goToProcessing() throws REXPMismatchException {
        // check if ms2 database exists, need to detect if the ms2 database exists for docker only        
        if (ab.isInDocker() & pkb.isIsms2()) {
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
            System.out.println("All required database exits in this docker!");
        }

        if (uploadedFileNames.isEmpty()) {
            uploadedFileNames = pkb.getUploadedFileNamesSaved();
        }

        if (!sb.isLoggedIn() || uploadedFileNames.isEmpty()) {
            pkb.getMsgList().add("Please upload your data before proceeding!");
            updateText();
            return "";
        }

        if (uploadedFileNames.size() < 3) {
            sb.addMessage("Error", "At least 3 samples are required for processing! Please keep uploading!");
            return "";
        }

        if (sanityCheck()) {
            pcb.setTotalNumberOfSamples(uploadedFileNames.size());
            sb.setDataUploaded();
            pkb.setContainsMetaVal(containsMeta);
            if (containsMeta) {
                pkb.populateSpecBeans();
            } else {
                pkb.populateSpecBeansNoMeta();
            }
            sb.setSaveEnabled(true);
            pkb.setRecordCMD(true);
            jrd.record_goToProcessing(this);
            return "Spectra check";
        } else {
            return "";
        }
    }

    public void projectProcessing() throws REXPMismatchException {

        pcb.OptiLCMSCheck();

        if (uploadedFileNames.isEmpty()) {
            uploadedFileNames = pkb.getUploadedFileNamesSaved();
        }

        if (!sb.isLoggedIn() || uploadedFileNames.isEmpty()) {
            pkb.getMsgList().add("Please upload your data before proceeding!");
            updateText();
        }

        if (sanityCheck()) {
            pcb.setTotalNumberOfSamples(uploadedFileNames.size());
            sb.setDataUploaded();
            pkb.setContainsMetaVal(containsMeta);
            if (containsMeta) {
                pkb.populateSpecBeans();
            } else {
                pkb.populateSpecBeansNoMeta();
            }

            sb.setSaveEnabled(true);
        }
    }

    public int doSpecLogin() {
        pcb.OptiLCMSCheck();
        if (!sb.isLoggedIn() || !sb.getAnalType().equals("raw")) {
            boolean ok = sb.doLogin("spec", "raw", false, false);
            if (!ok) {
                return 0;
            }
        }
        return 1;
    }

    public void showMetaError() {

        String homeDir = sb.getCurrentUser().getHomeDir();
        File meta = new File(homeDir + File.separator + metaName);
        meta.delete();
        updateText();
        sb.addMessage("Error", "The content of meta-data file is not formatted correctly! Please check!");
    }

    public void moreThanOneMeta() {
        sb.addMessage("Error", "Only one meta-data file can be present!");
    }

    public void doMetaIntegrityRC(ActionEvent e) {
        String fileNms = e.getFacesContext().getExternalContext().getRequestParameterMap().get("fileNms");
        String res;

        boolean metaFormatOk = pcb.isMetaOk();
        if (metaFormatOk) {
            boolean isMetaOk = checkMetaIntegrity(fileNms);
            if (!isMetaOk) {
                //su.updateText();
            }
            res = isMetaOk + "";
        } else {
            res = "false";
        }
        PrimeFaces.current().ajax().addCallbackParam("result", res + "");

    }

    public boolean checkMetaIntegrity(String fileNms) {
        pkb.getMsgList().removeAll(pkb.getMsgList());
        containsMeta = true;
        pkb.setContainsMetaVal(containsMeta);
        boolean metaok = pcb.isMetaOk();

        boolean res = true;

        if (!metaok) {
            pkb.getMsgList().add("The content of meta-data file is not formatted correctly!");
            res = false;
        }

        fileNmsFromMeta = RDataUtils.getRawFileNames(sb.getRConnection());
        classNmsFromMeta = RDataUtils.getRawClassNames(sb.getRConnection());
        Set<String> uniqueClasses = new HashSet<String>();

        uniqueClasses.addAll(Arrays.asList(classNmsFromMeta));

        int classSize = uniqueClasses.size();

        List<String> classList = Arrays.asList(classNmsFromMeta);
        for (String grp : uniqueClasses) {
            if (grp.toUpperCase().equals("QC")) {
                classSize--;
            } else if (grp.toUpperCase().equals("BLANK")) {
                classSize--;
            } else {
                int occurrences = Collections.frequency(classList, grp);
                if (occurrences < 3) {
                    sb.addMessage("Error", "Please make sure that there are at least three samples per group:");
                    res = false;
                    break;
                }
            }
        }

        if (classNmsFromMeta.length < 6 || classNmsFromMeta.length / classSize < 3) {
            pkb.getMsgList().add("Please make sure at least 2 groups of 3 replicates are present!");
            res = false;
        }

        String[] nmsArr = fileNms.split("; ");

        if (nmsArr.length != fileNmsFromMeta.length) {
            pkb.getMsgList().add("Inconsistency between selected files and meta-data provided!");
            res = false;
        }

        for (int i = 0; i < nmsArr.length; i++) {
            nmsArr[i] = nmsArr[i].split(".zip")[0];
        }

        for (String fileNm : fileNmsFromMeta) {
            String fileNmTrim = fileNm.split(".zip|.mzXML|.mzML|.mzData")[0];
            if (!Arrays.asList(nmsArr).contains(fileNmTrim)) {
                pkb.getMsgList().add("'<b>" + fileNmTrim + "</b>' is missing from uploaded files!");
                res = false;
            }
        }

        for (String fileNm : nmsArr) {
            String fileNmTrim = fileNm.split(".zip|.mzXML|.mzML|.mzData")[0];
            if (!Arrays.asList(fileNmsFromMeta).contains(fileNmTrim)) {
                if (!fileNmTrim.equals("")) {
                    pkb.getMsgList().add("'<b>" + fileNmTrim + "</b>' is not present in meta-data file!");
                    res = false;
                }
            }
        }
        return res;
    }

    public boolean sanityCheck() {
        updateText();
        if (!containsMeta) {
            pkb.getMsgList().removeAll(pkb.getMsgList());
            return true;
        }
        //Check file one by one to see whether they match to names from meta-file

        return pkb.getMsgList().isEmpty();
    }

    int numOfUploadFiles = 0;

    public void minusUploadingCount() {
        numOfUploadFiles--;
        if (numOfUploadFiles == 0) {
            //ab.minusUploadCount();
            resourceSemaphore.getUploadSemaphore().release();
            sanityCheck();
        }
    }

    @PreDestroy
    public void onSessionLeaveUploadReset() {
        try {
            // clean up resources
            if (numOfUploadFiles > 0) {
                resourceSemaphore.getUploadSemaphore().release();
            }
        } catch (Exception e) {
            // Just be silent
            // log the exception
            //e.printStackTrace();
        }

    }

    public void addNumOfUploadFiles(ActionEvent e) {
        String length = e.getFacesContext().getExternalContext().getRequestParameterMap().get("size");
        if (length != null && Integer.parseInt(length) > 0) {
            numOfUploadFiles = numOfUploadFiles + Integer.parseInt(length);
        }
    }

    //// Section : Centroid MS Data
    public void doCentroidMSdata(String Name, String group) throws REXPMismatchException {

        RConnection RC = sb.getRConnection();
        File f = new File(sb.getCurrentUser().getHomeDir() + "/upload/" + group);
        int res = 0;

        if (f.isDirectory() && f.exists()) {
            containsMeta = true;
            res = RDataUtils.centroidMSData(RC, Name, sb.getCurrentUser().getHomeDir() + "/upload/" + group);
        } else {
            containsMeta = false;
            res = RDataUtils.centroidMSData(RC, Name, sb.getCurrentUser().getHomeDir() + "/upload/");
        }
        //pkb.getSelectedData().setFormat("True");

        switch (res) {
            case 1 ->
                sb.addMessage("info", "Your data has been centroid now!");
            case -1 ->
                sb.addMessage("Error", "Sorry, This data cannot be centroid, please contact with administrator!");
            case -2 ->
                sb.addMessage("Error", "Sorry, only mzML and mzXML are supported for now. Other formats will be supported soon!");
        }

        if (containsMeta) {
            pkb.populateSpecBeans();
        } else {
            pkb.populateSpecBeansNoMeta();
        }

        if (pkb.getCentroidFileCount() >= 3) {
            pkb.setBnDisabled(false);
        } else {
            pkb.setBnDisabled(true);
        }
    }

    // variables and functions for google drive option
    private String googledriveURL = "";

    public String getGoogledriveURL() {
        return googledriveURL;
    }

    public void setGoogledriveURL(String googledriveURL) {
        this.googledriveURL = googledriveURL;
    }

    public void confirmSeqURL() {

        if (!googledriveURL.contains("https://drive.google.com/drive/folders/")) {
            sb.addMessage("error", "Invalid URL! The Google Drive is not in a correct format. Please read the instruction below!");
            return;
        } else {
            System.out.println("This googledriveURL looks good: -> " + googledriveURL);
        }

        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return;
        }
        RConnection RC = sb.getRConnection();
        boolean res1 = RSpectraUtils.validateGoogleDriveURL(RC, googledriveURL);

        if (!res1) {
            sb.addMessage("error", "No access to this URL! Not able to access the URL. Please check your URL link!");
            return;
        }
        int res2 = RSpectraUtils.ExtractGoogleDriveURL(RC, googledriveURL);

        //System.out.println(" ==== confirmSeqURL ===> " + res2);
        switch (res2) {
            case 0 -> {
                sb.addMessage("error", "No files detected! Please make sure no files are not included in multiple-level sub-folders!");
                return;
            }
            case 2 -> {
                sb.addMessage("error", "No metadata.txt found! Please make sure no files are not included in multiple-level sub-folders!");
                return;
            }
            case 3 -> {
                sb.addMessage("error", "No spectra files found! Please make sure no files are not included in multiple-level sub-folders!");
                return;
            }
            case 6 -> {
                sb.addMessage("error", "Multiple spectral formats found! Please make sure including only one format of spectral formats!");
                return;
            }
            default -> {
            }
        }

        // Now, let's check if metadata file matches to the files
        int res3 = RSpectraUtils.CheckMetadataMatching(RC);
        //System.out.println(" ==== CheckMetadataMatching ===> " + res3);
        if (res3 == 0) {
            String missing_files = RSpectraUtils.GetMissingFiles(RC);
            sb.addMessage("error", "Spectra files are not matched!" + missing_files);
            return;
        }

        // Let check the total size 
        boolean res4 = RSpectraUtils.GetFileTotalSizeBool(RC);
        if (!res4) {
            sb.addMessage("error", "Spectra files too large! You dataset is over 20GB. We cannot process it at this time.");
            return;
        } else {
            String[] allfiles = RSpectraUtils.GetAllSpectraFiles(RC);
            uploadedFileNames.addAll(Arrays.asList(allfiles));

            sb.addMessage("info", "Your dataset has been accepted for processing!");
        }
        allowGoogleDriveContinue = true;

    }

    private boolean allowGoogleDriveContinue = false;

    public boolean isAllowGoogleDriveContinue() {
        return allowGoogleDriveContinue;
    }

    public void setAllowGoogleDriveContinue(boolean allowGoogleDriveContinue) {
        this.allowGoogleDriveContinue = allowGoogleDriveContinue;
    }

    public String processSaintyCheck() {

        pcb.setTotalNumberOfSamples(uploadedFileNames.size());
        sb.setDataUploaded();
        pkb.setContainsMetaVal(containsMeta);
        pkb.populateSpecBeansGG();

        pkb.setFromGoogleDrive(true);
        sb.setSaveEnabled(true);
        pkb.setRecordCMD(true);
        spectraUploaded = true;

        return "Spectra check";
    }

}
