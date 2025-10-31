/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.project;

import jakarta.annotation.PostConstruct;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.DataUtils;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import pro.metaboanalyst.models.User;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RSpectraUtils;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.spectra.SpectraParamBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.spectra.SpectraUploadBean;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.RList;
import org.rosuda.REngine.Rserve.RConnection;
import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 *
 * @author soufanom
 */
@SessionScoped
@Named("projectBean")
public class ProjectBean implements Serializable {

    @Inject
    private ApplicationBean1 ab;

    @Inject
    private SessionBean1 sb;

    @Inject
    private UserLoginBean ulb;

    @Inject
    private SpectraProcessBean spb;

    @Inject
    private SpectraUploadBean sub;

    @Inject
    private SpectraControlBean scb;

    @Inject
    private SpectraParamBean spmb;
    
    private final static int PROJECT_LIMITS = 10;

    //Project details
    private String title;
    private String description;
    private String projectType = "raw";

    //Project data table
    private List<ProjectModel> projectTable = null;
    private HashMap<Long, ProjectModel> projectMap = null;
    private ProjectModel selectedProject = null;
    private String dataPlace = "";

    private ProjectModel currentProject = null;

    public ProjectModel getCurrentProject() {
        return currentProject;
    }

    public void setCurrentProject(ProjectModel currentProject) {
        this.currentProject = currentProject;
    }

    // initialize mysql - from ulb bean
    private MariaDBController mdbb;

    @PostConstruct
    public void init() {
        mdbb = ulb.getMdbb();
    }

    public MariaDBController getMdbb() {
        return mdbb;
    }

    // Section I --- Project operation section
    public void prepareProject() {
        //System.out.println("preparing selectedProject: " + selectedProject.getId());

        if (selectedProject == null) {
            sb.addMessage("error", "No project is selected! Please select a project to perform action.");
        } else {

            String reDirectLink;
            String folderNum = selectedProject.getFolder().substring(5, 25);
            long UserIden = selectedProject.getUserNM();

            reDirectLink = switch (selectedProject.getHostname()) {
                case "dev" ->
                    "https://www.metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml?funcNm=LoadProject&ID=" + UserIden + "_" + folderNum;
                case "genap" ->
                    "https://genap.metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml?funcNm=LoadProject&ID=" + UserIden + "_" + folderNum;
                default ->
                    ab.getBaseUrlDyn() + "faces/AjaxHandler.xhtml?funcNm=LoadProject&ID=" + UserIden + "_" + folderNum;
            }; //  if (ab.isOnGenapPublicDev()) {
            //      reDirectLink = "http://206.12.89.229:8080/MetaboAnalyst/faces/AjaxHandler.xhtml?funcNm=LoadProject?ID=" + UserIden + "_" + folderNum;
            //  }
            DataUtils.doRedirect(reDirectLink, ab);
        }
    }

    public String initializeProject(String guestFolder) {
        try {
            // Here is used to restore the selected project class after jump into the Nodes
            selectedProject = obtainCertainProject(ulb.getUserNM(), guestFolder);
            if (selectedProject != null) {
                return "done";
            }
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Project managing server is down, report this code: pxx00046 to administrator !");
        }

        return null;
    }

    public String loadnewProject() {
        //Process Naive status
        boolean ok = sb.doLogin("spec", "raw", false, false);
        if (!ok) {
            return null;
        }
        //Here we get the guest folder name
        String guestProFolder = sb.getCurrentUser().getName();
        selectedProject.setFolder(guestProFolder);

        //Here we get the server name
        if (ab.isOnProServer()) {
            dataPlace = "dev";
        } else if (ab.isOnQiangPc()) {
            dataPlace = "qiang";
        } else {
            dataPlace = "unk"; // an unknown place, maybe genap private
        }

        //Here we udpate mysql db                    
        String updateQury = "update devUsers.projects set status = 'started', projectFolderNM = '"
                + guestProFolder + "', dataplace = '" + dataPlace + "' where userNM = "
                + ulb.getUserNM() + " AND title = '" + selectedProject.getTitle() + "';";
        try {
            mdbb.runUpdate(updateQury);
            sb.addMessage("info", "Project started successfully!");
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Project managing server is down, report this code: pxx00014 to administrator !");
            return null;
        }

        return "spec";
    }

    public String loadProject() throws SQLException {

        //confirm the project exists
        if (ab.isOnProServer()) {
            dataPlace = "dev";
        } else if (ab.isOnQiangPc()) {
            dataPlace = "qiang";
        } else {
            dataPlace = "unk"; // an unknown place, maybe genap private
        }

        String ConfirmQuery = "SELECT EXISTS(select * from devUsers.projects where userNM="
                + ulb.getUserNM()
                + " AND title = '"
                + selectedProject.getTitle()
                + "' AND dataplace = '"
                + dataPlace
                + "');";
        ResultSet res;

        try {
            res = mdbb.runQuery(ConfirmQuery);
            if (res.next()) {
                if (res.getInt(1) != 1) {
                    sb.addMessage("error", "Project damaged! The report this error code xxxp00015 and your project information to administrator!");
                    return null;
                }
            }
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Project server is down, report this code: xxxp00016 to administrator !");
            return null;
        } finally {
            mdbb.disconnect();
        }

        //Initialize some variables and beans
        String status = null, proFolder = null;
        SpectraControlBean scb = null;
        long jobID = 0;
        String jobStatus = ""; //NOTE: status is the recorded 'status' from mysql db, while the 'jobStatus' is the real status of a certain job from slurm.

        //read job status + guest folder
        String readQuery = "select status,projectFolderNM,dataplace from devUsers.projects where userNM ="
                + ulb.getUserNM()
                + " AND title = '"
                + selectedProject.getTitle()
                + "' AND dataplace = '"
                + dataPlace
                + "';";

        try {
            res = mdbb.runQuery(readQuery);
            if (res.next()) {
                status = res.getString(1);
                proFolder = res.getString(2);
                selectedProject.setFolder(proFolder);
                //dataPlace = res.getString(3);
            }
        } catch (SQLException ex) {
            sb.addMessage("error", "Project server is down, report this code: xxxp00016 to administrator !");
            return null;
        } finally {
            mdbb.disconnect();
        }

        //Set java variable for this status (started, uploaded, submitted, finished)
        if ("started".equals(status.toLowerCase()) || "uploaded".equals(status.toLowerCase()) || "submitted".equals(status.toLowerCase()) || "finished".equals(status.toLowerCase())) {
            //Basic Jave setting
            setCurrentProject(selectedProject);
            sb.setDataType("spec");
            User user = new User();
            user.setName(proFolder);
            if (ab.isOnProServer()) {
                user.setHomeDir("/data/glassfish/projects/metaboanalyst/" + proFolder);
            } else {
                user.setHomeDir(ab.getRealUserHomePath() + File.separator + proFolder);
            }

            sb.setCurrentUser(user);
            doProjectLogin("raw", proFolder, false);
        }

        if ("uploaded".equals(status.toLowerCase()) || "submitted".equals(status.toLowerCase()) || "finished".equals(status.toLowerCase())) {
            //File uploaded Setting
            sb.setDataUploaded();
            List<String> uploadedFiles = new ArrayList<>();
            File folder;

            if (ab.isOnProServer()) {
                folder = new File("/data/glassfish/projects/metaboanalyst/" + proFolder + "/upload");
            } else {
                folder = new File(ab.getRealUserHomePath() + File.separator + proFolder + "/upload");
            }

            File[] listOfFiles = folder.listFiles();
            int groups = 0;

            for (File listOfFile : listOfFiles) {
                if (listOfFile.isFile()) {
                    //handle files directly shown in the folder
                    //System.out.println("File " + listOfFiles[i].getName());
                    if (listOfFile.getName().endsWith(".mzml") | listOfFile.getName().toLowerCase().endsWith(".mzxml") | listOfFile.getName().toLowerCase().endsWith(".cdf") | listOfFile.getName().toLowerCase().endsWith(".mzdata")) {
                        //System.out.println("FOund file: " + listOfFiles[i].getName());
                        uploadedFiles.add(listOfFile.getName());
                    }
                } else if (listOfFile.isDirectory()) {
                    //handle files shown in multiple subfolders
                    //System.out.println("FOund folder: " + listOfFiles[i].getName());
                    groups++;
                    File subFolder = listOfFile;
                    File[] newlistOfFile = subFolder.listFiles();
                    for (File newlistOfFile1 : newlistOfFile) {
                        if (newlistOfFile1.getName().toLowerCase().endsWith(".mzml") | newlistOfFile1.getName().toLowerCase().endsWith(".mzxml") | newlistOfFile1.getName().toLowerCase().endsWith(".cdf") | newlistOfFile1.getName().toLowerCase().endsWith(".mzdata")) {
                            //System.out.println("FOund file: " + newlistOfFile[j].getName());
                            uploadedFiles.add(newlistOfFile1.getName());
                        }
                    }
                }
            }

            sub.setUploadedFileNames(uploadedFiles);

            if (groups > 0) {
                sub.setContainsMeta(true);
            }

            try {
                sub.projectProcessing();
            } catch (REXPMismatchException ex) {
                sb.addMessage("error",
                        "Project data processing failed, please create a new project and re-upload your data!");
            }
        }

        if ("submitted".equals(status.toLowerCase()) || "finished".equals(status.toLowerCase())) {

            scb.setDataConfirmed(true);

            // Obtain file inclusion info
            RConnection RC = sb.getRConnection();

            try {
                if (ab.isOnProServer()) {
                    RC.voidEval("setwd('" + ab.getProjectsHome() + proFolder + "')");
                } else {
                    RC.voidEval("setwd('" + ab.getRealUserHomePath() + File.separator + proFolder + "')");
                }

                String ModeInfo = RSpectraUtils.retrieveModeInfo(RC);
                if ("auto".equals(ModeInfo)) {
                    spmb.setMeth("auto");
                }

                RList reslist = RDataUtils.readFilesInclusion(RC);
                scb.setIncludedFileNamesString(reslist.at(0).asString());
                scb.setTotalNumberOfSamples(reslist.at(1).asInteger());
            } catch (Exception ex) {
                scb.setIncludedFileNamesString("");
                sb.addMessage("error", "Rserve seems not working correctly. The result might be influenced !");
            }

            // Obtain job ID from mysql db
            String retrieveQuery = "select jobID from devUsers.jobs where jobFolder = '"
                    + proFolder
                    + "' AND jobPosition = '"
                    + dataPlace
                    + "';";
            try {
                ResultSet ressub = mdbb.runQuery(retrieveQuery);
                while (ressub.next()) {
                    jobID = ressub.getLong(1);
                }
            } catch (Exception ex) {
                sb.addMessage("error", "Project server is down, report this code: xxxp00018 to administrator !");
            } finally {
                mdbb.disconnect();
            }

            // Obtain job status from slurm and progress (if progress = 100, finished : read from slurm[Can be slurmdb future])
            double progress = scb.updateProgress();

            if (progress == 100) {
                jobStatus = "Finished";

                scb.setCurrentJobStatus(jobStatus);
                scb.setCurrentJobId(jobID);

                scb.setJobSubmitted(true);
                scb.setPerformedPlan(true);
                scb.setKilled(false);
                scb.setStopStatusCheck(false);
                scb.setProgress2(progress);
            }

            if (progress < 100) {
                if (jobID == 0) {
                    sb.addMessage("error", "Project server is down, report this code: xxxp00019 to administrator !");
                } else {
                    jobStatus = SchedulerUtils.getJobStatus(jobID, sb.getCurrentUser().getHomeDir());
                }

                scb.setCurrentJobStatus(jobStatus);
                scb.setCurrentJobId(jobID);

                scb.setJobSubmitted(true);
                scb.setPerformedPlan(true);
                scb.setProgress2(progress);

                if ("running".equals(jobStatus.toLowerCase())) {
                    // if jobs are running
                    scb.setKilled(false);
                    scb.setStopStatusCheck(true);

                } else {
                    // if jobs are killed or failed
                    scb.setKilled(true);
                    scb.setStopStatusCheck(false);
                }
            }
        }

        if ("finished".equals(status.toLowerCase()) || "finished".equals(jobStatus.toLowerCase())) {

            try {
                spb.internalizeRes(0);
                spb.populateRawResBeans();
                spb.populateRawFeatureBeans();
            } catch (FileNotFoundException ex) {
                sb.addMessage("error", "Your previous result is missing, please re-do the process !");
            }

            //System.out.println("Processing has been finished already!");
            sb.setDataUploaded();
            scb.setStopStatusCheck(false);
            scb.setFinishedJobId(jobID);
            scb.setFinishedProgress2(100);
            scb.setFinishedJobStatus(jobStatus);
        }

        //add navii tree & redirect the corresponding page according to the status
        sb.initNaviTree("spec");
        if ("started".equals(status)) {
            sb.addNaviTrack("Upload", null);
            return "spec";
        } else if ("uploaded".equals(status)) {
            sb.addNaviTrack("Upload", null);
            sb.addNaviTrack("Spectra check", null);
            return "Spectra check";
        } else if ("submitted".equals(status)) {
            sb.addNaviTrack("Upload", null);
            sb.addNaviTrack("Spectra check", null);
            sb.addNaviTrack("Spectra processing", null);
            sb.addNaviTrack("Job status", null);
            return "Job status";
        } else if ("finished".equals(status.toLowerCase())) {
            sb.addNaviTrack("Upload", null);
            sb.addNaviTrack("Spectra check", null);
            sb.addNaviTrack("Spectra processing", null);
            sb.addNaviTrack("Job status", null);
            sb.addNaviTrack("Spectra result", null);
            return "Spectra result";
        } else {
            sb.addMessage("error",
                    "Something weird happened, report this code: xxxp00017 to administrator !");
            return null;
        }
    }

    public boolean updateProjectStatus(String status, String proFolder) {

        if (ab.isOnProServer()) {
            dataPlace = "dev";
        } else if (ab.isOnQiangPc()) {
            dataPlace = "qiang";
        } else {
            dataPlace = "unk"; // an unknown place, maybe genap private
        }

        String UpQuery = "update devUsers.projects set status = '"
                + status + "' where projectFolderNM = '"
                + proFolder + "' AND dataplace = '" + dataPlace + "';";

        try {
            mdbb.runUpdate(UpQuery);
            sb.addMessage("info", "Project status was updated successfully!");
            return true;
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Project update failed: report this error code pxx00016 to administrator !");
        }

        return false;
    }

    public ProjectModel obtainCertainProject(long userNM, String projectFolderNum) throws SQLException {

        try {
            if (!ulb.MysqlDBAvailabilityCheck()) {
                sb.addMessage("error",
                        "Project server down: report this error code pxx00088 to administrator !");
                return null;
            }
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Project server down: report this error code pxx00088 to administrator !");
        }

        // Retrieve the data from db
        String query = "select * from devUsers.projects where userNM=" + userNM + ";";
        ResultSet res;
        ProjectModel project = null;

        try {
            res = mdbb.runQuery(query);
            while (res.next()) {
                if (res.getString(7).contains(projectFolderNum)) {
                    project = new ProjectModel();

                    project.setUserNM(userNM);
                    project.setTitle(res.getString(2));
                    project.setDescription(res.getString(3));
                    project.setType(res.getString(4));
                    project.setCreationDate(res.getDate(5));
                    project.setStatus(res.getString(6));
                    project.setFolder(res.getString(7));
                    project.setHostname(res.getString(8));
                    break;
                }
            }

        } catch (SQLException ex) {
            Logger.getLogger(ProjectController.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            mdbb.disconnect();
        }

        return project;
    }

    // Section II -----partial operation section
    public String loadPartial() throws SQLException {

        // initialize/grab some parameters and beans
        String partialID = sb.getPartialId();
        String jobStatus = null;
        String jobFolder;
        //verify partial link expiration
        if (ab.isInDocker()) {
            jobFolder = checkParitialLinkinDocker(partialID);
        } else {
            jobFolder = checkPartialvalid(partialID);
        }

        //String jobFolder = checkPartialvalid(partialID);
        if (jobFolder == null) { // A invalid/expired partial link
            //System.out.println("This is an invalid link to job: " + jobFolder);
            return null;
        }

        //get parial job id and initialize sessionbean's current user
        String pJobID = partialID.split("_")[1];

        sb.setDataType("spec");
        User user = new User();
        user.setName(jobFolder);
        if (ab.isOnProServer()) {
            user.setHomeDir("/data/glassfish/projects/metaboanalyst/" + jobFolder);
        } else {
            user.setHomeDir(ab.getRealUserHomePath() + File.separator + jobFolder);
        }

        sb.setCurrentUser(user);
        doProjectLogin("raw", jobFolder, false);
        sb.setDataUploaded();
        RConnection RC = sb.getRConnection();

        //obtain the spectra file info
        List<String> uploadedFiles = new ArrayList<>();
        File folder;

        if (ab.isOnProServer()) {
            folder = new File("/data/glassfish/projects/metaboanalyst/" + jobFolder + "/upload");
        } else {
            folder = new File(ab.getRealUserHomePath() + File.separator + jobFolder + "/upload");
        }

        File[] listOfFiles = folder.listFiles();
        int groups = 0;

        for (File listOfFile : listOfFiles) {
            if (listOfFile.isFile()) {
                //handle files directly shown in the folder
                //System.out.println("File " + listOfFile.getName());
                if (listOfFile.getName().endsWith(".mzml") | listOfFile.getName().toLowerCase().endsWith(".mzxml") | listOfFile.getName().toLowerCase().endsWith(".cdf") | listOfFile.getName().toLowerCase().endsWith(".mzdata")) {
                    //System.out.println("FOund file: " + listOfFiles[i].getName());
                    uploadedFiles.add(listOfFile.getName());
                }
            } else if (listOfFile.isDirectory()) {
                //handle files shown in multiple subfolders
                //System.out.println("FOund folder: " + listOfFiles[i].getName());
                groups++;
                File subFolder = listOfFile;
                File[] newlistOfFile = subFolder.listFiles();
                for (File newlistOfFile1 : newlistOfFile) {
                    if (newlistOfFile1.getName().toLowerCase().endsWith(".mzml") | newlistOfFile1.getName().toLowerCase().endsWith(".mzxml") | newlistOfFile1.getName().toLowerCase().endsWith(".cdf") | newlistOfFile1.getName().toLowerCase().endsWith(".mzdata")) {
                        //System.out.println("FOund file: " + newlistOfFile[j].getName());
                        uploadedFiles.add(newlistOfFile1.getName());
                    }
                }
            }
        }

        sub.setUploadedFileNames(uploadedFiles);
        scb.setDataConfirmed(true);
        if (groups > 0) {
            sub.setContainsMeta(true);
        }

        try {
            sub.projectProcessing();
        } catch (REXPMismatchException ex) {
            sb.addMessage("error",
                    "Project data processing failed, please create a new project and re-upload your data!");
        }

        try {
            if (ab.isOnProServer()) {
                RC.voidEval("setwd('" + ab.getProjectsHome() + jobFolder + "')");
            } else {
                RC.voidEval("setwd('" + ab.getRealUserHomePath() + File.separator + jobFolder + "')");
            }

            String ModeInfo = RSpectraUtils.retrieveModeInfo(RC);
            if ("auto".equals(ModeInfo)) {
                spmb.setMeth("auto");
            }

            RList reslist = RDataUtils.readFilesInclusion(RC);

            scb.setIncludedFileNamesString(reslist.at(0).asString());
            scb.setTotalNumberOfSamples(reslist.at(1).asInteger());
        } catch (Exception ex) {
            scb.setIncludedFileNamesString("");
            sb.addMessage("error",
                    "Rserve seems not working correctly. The result might be influenced !");
        }

        // Obtain job status from slurm and progress (if progress = 100, finished : read from slurm[Can be slurmdb future])
        double progress = scb.updateProgress();
        int jobID = Integer.parseInt(pJobID);

        if (progress == 100) {
            jobStatus = "Finished";

            scb.setCurrentJobStatus(jobStatus);
            scb.setCurrentJobId(jobID);

            scb.setJobSubmitted(true);
            scb.setPerformedPlan(true);
            scb.setKilled(false);
            scb.setStopStatusCheck(false);
            scb.setProgress2(progress);
        }

        if (progress < 100) {
            if (jobID == 0) {
                sb.addMessage("error",
                        "Project server is down, report this code: xxxp00019 to administrator !");
            } else {
                jobStatus = SchedulerUtils.getJobStatus(jobID, sb.getCurrentUser().getHomeDir());
            }

            scb.setCurrentJobStatus(jobStatus);
            scb.setCurrentJobId(jobID);

            scb.setJobSubmitted(true);
            scb.setPerformedPlan(true);
            scb.setProgress2(progress);

            if ("running".equals(jobStatus.toLowerCase())) {
                // if jobs are running
                scb.setKilled(false);
                scb.setStopStatusCheck(true);

            } else {
                // if jobs are killed or failed
                scb.setKilled(true);
                scb.setStopStatusCheck(false);
            }
        }

        //done
        //System.out.println(" --- Loading partial link finished! <----");
        return "Job status";
    }

    private String checkPartialvalid(String partialID) throws SQLException {
        String folder = null;
        long diff = 100;
        try {
            ulb.MysqlDBAvailabilityCheck();
        } catch (Exception ex) {
            sb.addMessage("error",
                    "URL managing server is down, report this code: uxx00010 to administrator !");
        }

        String checkQuery = "select jobSubTime,jobFolder from devUsers.jobs where partialLink = '" + partialID + "';";

        mdbb = ulb.getMdbb();

        try {
            ResultSet jobRes = mdbb.runQuery(checkQuery);
            if (jobRes.next()) {
                Date creatDate = jobRes.getDate(1);
                Date currentDate = new Date();
                long timed = currentDate.getTime() - creatDate.getTime();
                diff = TimeUnit.DAYS.convert(timed, TimeUnit.MILLISECONDS);
                //System.out.println("---------- time of days has pased since the url was created: " + diff);
                if (diff < 14) {
                    folder = jobRes.getString(2);
                }
            }
        } catch (Exception ex) {
            sb.addMessage("error",
                    "URL managing server is down, report this code: uxx00011 to administrator !");
        } finally {
            mdbb.disconnect();
        }

        return folder;
    }

    public boolean checkLink() throws SQLException {

        // initialize/grab some parameters and beans
        String partialID = sb.getPartialId();

        //verify partial link expiration
        String jobFolder = checkPartialvalid(partialID);

        if (jobFolder == null) { // A invalid/expired partial link
            return false;
        } else {
            return true;
        }
    }

    public boolean checkLinkinDocker() {

        // initialize/grab some parameters and beans
        String partialID = sb.getPartialId();

        //verify partial link expiration
        String jobFolder = null;
        boolean tmpRCon = false;
        try {
            RConnection rxc = sb.getRConnection();
            if (rxc == null) {
                rxc = RCenter.getCleanRConnection();
            }
            jobFolder = RDataUtils.checkParitialLinkinDocker(rxc, partialID, 0);
            if (tmpRCon) {
                rxc.close();
            }
        } catch (Exception ex) {
            //Logger.getLogger(SeqProcesser.class.getName()).log(Level.SEVERE, null, ex);
        }

        return jobFolder != null; // A invalid/expired partial link
    }

    public String checkParitialLinkinDocker(String partialID) {
        String jobFolder = null;
        boolean tmpRCon = false;
        try {
            RConnection rxc = sb.getRConnection();
            if (rxc == null) {
                rxc = RCenter.getCleanRConnection();
                tmpRCon = true;
            }
            jobFolder = RDataUtils.checkParitialLinkinDocker(rxc, partialID, 0);
            if (tmpRCon) {
                rxc.close();
            }
        } catch (Exception ex) {
            //Logger.getLogger(SeqProcesser.class.getName()).log(Level.SEVERE, null, ex);
        }
        return jobFolder;
    }

    // Section IV --- Project variable section
    public SelectItem[] getProjectTypes() {
        SelectItem[] projectTypes = new SelectItem[1];
        projectTypes[0] = new SelectItem("raw", "Spectra processing");
        return projectTypes;
    }

    public int getProjectLimit() {
        return PROJECT_LIMITS;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getProjectType() {
        return projectType;
    }

    public void setProjectType(String projectType) {
        this.projectType = projectType;
    }

    public HashMap<Long, ProjectModel> getProjectMap() {
        return projectMap;
    }

    public void setProjectMap(HashMap<Long, ProjectModel> projectMap) {
        this.projectMap = projectMap;
    }

    public ProjectModel getSelectedProject() {
        return selectedProject;
    }

    public void setSelectedProject(ProjectModel selectedProject) {
        setCurrentProject(selectedProject);
        this.selectedProject = selectedProject;
    }

    public List<ProjectModel> getProjectTable() {
        return projectTable;
    }

    public void setProjectTable(List<ProjectModel> projectTable) {
        this.projectTable = projectTable;
    }

    @JsonIgnore
    public String getProjectRelativeDir() {
        //set relative dir
        String relativeDir = ab.getProjectsHome() + File.separator + getCurrentProject().getFolder();
        return relativeDir;
    }

    //load existing folder
    public String doProjectLogin(String analType, String guestName, boolean isPartial) {

        if (!ab.isCompiled()) {
            ab.compileRScripts(analType);
        }
        if (sb.getCurrentUser() != null) {
            if (sb.getRConnection() != null) {
                sb.getRConnection().close();
            }
            sb.setCurrentUser(null);
        }

        User currentUser;
        RConnection RC;
        if (!isPartial) {
            currentUser = DataUtils.loadRawUser(guestName, ab);
            RC = RCenter.getRConnectionRawSharing(currentUser.getHomeDir(), ab.getRscriptLoaderPath(), analType);
        } else if (analType.equals("raw") && ab.shouldUseScheduler()) {
            currentUser = DataUtils.loadRawUser(guestName, ab);
            RC = RCenter.getRConnectionRawSharing(currentUser.getHomeDir(), ab.getRscriptLoaderPath(), analType);
        } else {
            currentUser = DataUtils.loadUser(guestName, ab.getRealUserHomePath());
            RC = RCenter.getCleanRConnection();
            RCenter.recordRserveConnection(RC, currentUser.getHomeDir() + guestName);
        }

        if (RC == null) {
            sb.addMessage("error", "Cannot connect to Rserver! Please start your Rserver with the right permission!");
            return null;
        } else {
            sb.setCurrentUser(currentUser);
            sb.setRConnection(RC);
        }

        ab.performResourceCleaning(RC);
        RDataUtils.initDataObjects(RC, sb.getDataType(), analType, sb.isPaired());
        sb.setLoggedIn(true);
        if (sb.isRegisteredLogin()) {
            sb.setSaveEnabled(true);
        }

        //job scheduler init path for output files
        String path = "";
        if (ab.shouldUseScheduler()) {
            path = ab.getRaw_spec_folder();
        } else if (ab.isOnLocalServer()) {
            path = RDataUtils.getPathForScheduler(RC);
        }
        path = path + currentUser.getName() + "/";
        RDataUtils.setUserPathForScheduler(RC, path);
        if (isPartial) {
            RDataUtils.initDataPlan(RC); //record to R
        }

        sb.setAnalType(analType);
        return analType;
    }
}
