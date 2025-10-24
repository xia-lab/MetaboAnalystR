/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.spectra;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.omnifaces.util.Faces;
import org.primefaces.PrimeFaces;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.project.MariaDBController;
import pro.metaboanalyst.project.SchedulerUtils;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;

import jakarta.annotation.PreDestroy;
import jakarta.enterprise.context.SessionScoped;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import java.io.*;
import java.sql.*;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Scanner;
import java.util.logging.Level;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.lts.FireBaseController;
import static pro.metaboanalyst.utils.DataUtils.processExec;
import pro.metaboanalyst.workflows.DiagramView;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author qiang
 */
@SessionScoped
@Named("spectraController")
@JsonIgnoreProperties(ignoreUnknown = true)

public class SpectraControlBean implements Serializable {

    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(SpectraControlBean.class);
    @Inject
    @JsonIgnore
    private ApplicationBean1 ab;

    @Inject
    @JsonIgnore
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @Inject
    @JsonIgnore
    private SpectraParamBean sparam;

    @Inject
    @JsonIgnore
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private FireBaseController fbc;

    @Inject
    @JsonIgnore
    private DatabaseClient db;

    @JsonIgnore
    @Inject
    private SpectraProcessBean spb;

    @JsonIgnore
    @Inject
    private DiagramView dv;

    private boolean createdShareLink = false;
    private LinkedHashMap<String, String> javaHistory = new LinkedHashMap<>();
    // Section 0 : Variable Section - count--------------
    private int count = 0;
    // Section 2 : Job ID & pid section -------------------
    private int examplePid = -1;
    private long currentJobId = 0;
    private long finishedJobId = 0;
    private String currentJobStatus = "Submitting...";
    private boolean stopStatusCheck = true;
    private boolean Killed = false;
    private boolean jobSubmitted = false;
    private String finishedJobStatus = "Not Started";
    // Section 4 : Job priority section -------------------
    private String currentJobPriority = "Level 1";
    // Section 5 : Job progress section -------------------
    private Double progress2 = 0.0;
    private double finishedProgress2 = 100.0;
    // Section 6 : sample number section -------------------
    private int totalNumberOfSamples = 0;
    private String includedFileNamesString = "";
    /// Section 7 : Other utils section ---------------
    // data confirmed or not
    private boolean dataConfirmed = false;
    // job perform or not
    private boolean performedPlan = false;
    // use example or not
    private boolean executExample = false;
    // parameter changed or not
    private boolean paramNotChanged = true;
    // Metadata is right or not
    private boolean metaOk = false;
    /// Section 9 : mysql db section
    // Initialize mysql connection
    private MariaDBController mdbb;

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    // MS1 or MS2 data job
    private boolean isms2 = false;

    public boolean isIsms2() {
        return isms2;
    }

    public void setIsms2(boolean isms2) {
        this.isms2 = isms2;
    }

    public boolean isCreatedShareLink() {
        return createdShareLink;
    }

    public void setCreatedShareLink(boolean createdShareLink) {
        this.createdShareLink = createdShareLink;
    }

    public LinkedHashMap<String, String> getJavaHistory() {
        return javaHistory;
    }

    public void setJavaHistory(LinkedHashMap<String, String> javaHistory) {
        this.javaHistory = javaHistory;
    }

    
    // Section 1 : General Controller -------------------
    public void performPlan(String workflowBoolString) throws REXPMismatchException {

        int pid;

        if (isJobSubmitted() || performedPlan || getCurrentJobId() != 0) {
            count = count + 1;
            return;
        }

        if (count == 0) {
            count = count + 1;
            return;
        }

        if (progress2 != 0 || isCreatedShareLink() || !isDataConfirmed()) { //used to strongly block the job resubmit
            return;
        }

        RConnection RC = sb.getRConnection();
        setJobSubmitted(true);

        boolean res = false;
        String params0_path = ab.getParams0();

        if ("customized".equals(sparam.getMeth())) {

            try {
                int res2 = RDataUtils.paramChangingDetection(RC, params0_path, sb.getCurrentUser().getHomeDir());
                if (res2 == 1) {
                    setParamCNothanged(true);
                } else {
                    setParamCNothanged(false);
                }
            } catch (REXPMismatchException ex) {
                LOGGER.error("performPlan-customized", ex);
                //Logger.getLogger(SpectraProcessBean.class.getName()).log(Level.SEVERE, null, ex);
            }

        }

        if ("auto".equals(sparam.getMeth())) { // to make sure the previous running will not interfere the demo of auto

            setParamCNothanged(true);

            try {
                int res2 = RDataUtils.paramChangingDetection(RC, params0_path, sb.getCurrentUser().getHomeDir());
                if (res2 == 1) {
                    setParamCNothanged(true);
                } else {
                    setParamCNothanged(false);
                }
            } catch (REXPMismatchException ex) {
                LOGGER.error("performPlan-auto", ex);
                //Logger.getLogger(SpectraProcessBean.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        if (sparam.getAlgorithms().equals("asari")) {
            setParamCNothanged(false);
            executExample = false;
        }

        if (executExample) {
            // Running the fast show - demo things
            //System.out.print("executExample====worked");
            RConnection RCDemo = RCenter.getRConnectionRawSharing(sb.getCurrentUser().getHomeDir(),
                    ab.getRscriptLoaderPath(), sb.getAnalType());
            pid = RDataUtils.demoPlanProcessDefine(RC, RCDemo, sb.getCurrentUser().getName(),
                    getIncludedFileNamesString(), sparam.getMeth(), ab, sb.getCurrentUser().getHomeDir(), paramNotChanged);
            setExamplePid(pid);
            res = pid != -1;
        } else {
            // Running the real datasets

            boolean ms2Opt = spb.getMs2DataOpt().equals("ms1");
            isms2 = !ms2Opt;

            if (sparam.getAlgorithms().equals("asari")) {
                //AsariPlanProcessDefine
                // this is specifically used for asari
                String param_str = "";
                if (sparam.getPolarity().equals("positive")) {
                    param_str = param_str + "--mode pos";
                } else {
                    param_str = param_str + "--mode neg";
                }
                if (isSWATHExample) {
                    param_str = param_str + " --autoheight true";
                }
                double ppm_val = sparam.getPpm();
                int ppm_val_int = (int) ppm_val;
                param_str = param_str + " --cores 8" + " --ppm " + ppm_val_int + " --input ";

                res = RDataUtils.AsariPlanProcessDefine(RC, sb.getCurrentUser().getName(),
                        getIncludedFileNamesString(), ms2Opt, ab,
                        sb.getCurrentUser().getHomeDir(), param_str, ab.getRscriptsHomePath());

            } else {
                res = RDataUtils.planProcessDefine(RC, sb.getCurrentUser().getName(),
                        getIncludedFileNamesString(), sparam.getMeth(), ab,
                        sb.getCurrentUser().getHomeDir());
            }

            if (isms2) {
                String db_str = sparam.getMsmsDBOpt();
                //System.out.println(" db_str 1 ====> " + db_str);
                if (!db_str.startsWith("[")) {
                    String[] db_strs = {db_str};
                    sparam.setMsmsDBOpt_multi(db_strs);
                    db_str = sparam.getMsmsDBOpt();
                }
                //System.out.println(" db_str 2 ====> " + db_str);
                if (db_str.equals("all")) {
                    db_str = "[all]";
                }
                String params_str = "ppm1:" + sparam.getPpm() + ";ppm2:" + sparam.getPpm2() + ";filtering:"
                        + sparam.getFiltering_val() + ";targets_opt:" + sparam.getTargeted_peaks()
                        + ";db_opt:" + db_str + ";win_size:" + sparam.getWindow_size()
                        + ";similarity_method:" + sparam.getSimi_method() + ";ionMode:" + sparam.getPolarity()
                        + ";intensity_threshold:" + sparam.getIntensity_threshold() + ";enabledDDADeco:" + sparam.isEnableDDADeco();
                if (sparam.getTarget_omics().equals("exposomics")) {
                    params_str = params_str + ";omics_type:exposomics";
                } else {
                    params_str = params_str + ";omics_type:metabolomics";
                }
                // ms2DataOpt is -> dda
                if (spb.getMs2DataOpt().equals("dda")) {
                    RDataUtils.createMS2ScriptForScheduler(RC, sb.getCurrentUser().getName(), params_str, "dda");
                }

                // ms2DataOpt is -> swath
                if (spb.getMs2DataOpt().equals("swath")) {
                    RDataUtils.createMS2ScriptForScheduler(RC, sb.getCurrentUser().getName(), params_str, "swath");
                }
            }

        }

        if (ab.shouldUseScheduler()) { // deal with the case ondev: 1. run real data or 2. example data with parameters or included files changed

            String fileusrNM = "c(\"QC_PREFA02.mzML\",\"QC_PREFB02.mzML\",\"CD-9WOBP.mzML\",\"CD-6KUCT.mzML\",\"CD-77FXR.mzML\",\"CD-9OS5Y.mzML\",\"HC-9SNJ4.mzML\",\"HC-AMR37.mzML\",\"HC-9X47O.mzML\",\"HC-AUP8B.mzML\")";

            if (getIncludedFileNamesString().length() != fileusrNM.length()) {
                paramNotChanged = false;
            }

            if (executExample && paramNotChanged) {
                setCurrentJobStatus("Demo Running");
            } else if (res) {
                String JobSubmission = "sbatch " + sb.getCurrentUser().getHomeDir() + "/ExecuteRawSpec.sh";
                int JobID = 0;
                try {
                    if (ab.isOnProServer() || ab.isOnQiangPc() || ab.isInDocker()) {
                        //Process proc = Runtime.getRuntime().exec(JobSubmission);
                        Process proc;
                        List<String> commands = Arrays.asList(JobSubmission.split(" ")); // Split the command into arguments
                        ProcessBuilder processBuilder = new ProcessBuilder(commands);
                        proc = processBuilder.start();

                        BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));

                        String JobString;
                        while ((JobString = stdInput.readLine()) != null) {
                            JobID = Integer.parseInt(JobString.replaceAll("[^0-9]", ""));
                            System.out.println("Raw spectra Job " + JobID + " has been submitted successfully !");
                        }
                    }
                } catch (Exception e) {
                    LOGGER.error("[JobSubmission: sbatch]" + JobSubmission, e);
                    //System.out.println("IOException while trying to execute [JobSubmission: sbatch]" + JobSubmission);
                }

                setCurrentJobId(JobID);
                if (JobID != 0) {
                    if (ab.isOnProServer() || ab.isOnQiangPc() || ab.isInDocker()) {
                        String jobStatus = SchedulerUtils.getJobStatus(JobID, sb.getCurrentUser().getHomeDir());
                        setCurrentJobStatus(jobStatus);
                    } else {
                        String jobStatus = "Local submitted";
                        setCurrentJobStatus(jobStatus);
                    }
                }
                if (ab.isOnProServer() || ab.isOnQiangPc() || ab.isInDocker()) {
                    dv.setRawJobId(JobID + "");
                    /*
                    System.out.println("fub.getEmail  ---> " + fub.getEmail());
                    System.out.println("    JobID     ---> " + JobID);
                    System.out.println("  getHomeDir  ---> " + sb.getCurrentUser().getHomeDir());
                    System.out.println("  getName     ---> " + sb.getCurrentUser().getName());
                    System.out.println("getRelativeDir---> " + sb.getCurrentUser().getRelativeDir());
                    */
                    //recordJob(JobID);
                    String JobPos;
                    if (ab.isOnVipServer()) {
                        JobPos = "vip";
                    } else if (ab.isOnProServer()) {
                        JobPos = "pro";
                    } else if (ab.isOnQiangPc()) {
                        JobPos = "qiang";
                    } else if (ab.isInDocker()) {
                        JobPos = "docker";
                    } else {
                        JobPos = "unknown";
                    }
                    db.recordRawJob(JobID, fub.getEmail(), sb.getCurrentUser().getName(), JobPos);
                    RDataUtils.recordspecjob2local(RC, fub.getEmail(), JobID + "", "UNCOMPLETE", sb.getCurrentUser().getName(), workflowBoolString);
                }
            }

        } else if (!executExample) {
            //TODO: activate cookie later
            setCurrentJobStatus("Running");
        } else {
            setCurrentJobStatus("Demo Running");
        }

        setPerformedPlan(true);
        //progress bar starts
        PrimeFaces.current().executeScript("PF('pbAjax1').start()");

        setFinishedJobId(0);
        setFinishedProgress2(0.0);
        setFinishedJobStatus("Not Started");
        //WorkflowBean wb = CDI.current().select(WorkflowBean.class).get();
        wb.getCalledWorkflows().add("Spectra Processing");

    }

    public void cancelPlan() {

        // just call scancel to cancel a certain job ID
        if (executExample && paramNotChanged) {

            if (examplePid == -1) {
                sb.addMessage("error", "Unable to cancel the job!");
            }

            String cmd = "kill " + examplePid;

            //Runtime.getRuntime().exec(cmd);
            processExec(cmd);
            sb.addMessage("info", "Current job is cancelled!");
            setJobSubmitted(false);
            setKilled(true);
            setStopStatusCheck(false);

        } else if (ab.shouldUseScheduler()) {

            String JobKill = "scancel " + getCurrentJobId();

            try {
                //Process proc = Runtime.getRuntime().exec(JobKill);
                processExec(JobKill);
                sb.addMessage("info", "Current job is cancelled!");

                setKilled(true);
                setCurrentJobStatus("Killed");
                setJobSubmitted(false);
                //setStopStatusCheck(false);
            } catch (Exception e) {
                sb.addMessage("error", "Unable to terminate the current job!");
            }

            /*
            boolean res = SchedulerUtils.killJob(getCurrentJobId());
            String status = SchedulerUtils.getJobStatus(getCurrentJobId());
             */
        } else {

            setKilled(true);
            setJobSubmitted(false);
            setCurrentJobStatus("Killed");
            setStopStatusCheck(false);
            sb.addMessage("info", "Current job is cancelled!");
        }
    }

    public void refreshJob() throws IOException {
        if (ab.shouldUseScheduler()) {
            setCurrentJobStatus("Status refreshing...");
        } else {
            if (progress2 == 100) {
                setCurrentJobStatus("Running");
            }
        }
        ExternalContext ec = FacesContext.getCurrentInstance().getExternalContext();
        ec.redirect(((HttpServletRequest) ec.getRequest()).getRequestURI());
    }

    public void checkStatus() throws IOException {
        if(sb.getCurrentUser() == null){
            return;
        }
        String jobStatus = null;
        if ("Running".equals(currentJobStatus) && count > 20) {
            if (count % 15 == 0) {
                // query status from slurm every 15 seconds when it is running & submitted 20 sec later
                jobStatus = SchedulerUtils.getJobStatus(currentJobId, sb.getCurrentUser().getHomeDir());
            } else {
                jobStatus = "RUNNING";
            }
        } else {
            // In other cases, keep querying until failed or finished
            jobStatus = SchedulerUtils.getJobStatus(currentJobId, sb.getCurrentUser().getHomeDir());
        }

        double jobProgress = getProgress2();

        if (jobProgress == 0) {
            currentJobStatus = "Pending";
        } else if (jobProgress == 100 || jobStatus.equals("COMPLETED")) {
            currentJobStatus = "Finished";
        } else if ("FAILED".equals(jobStatus)) {
            currentJobStatus = "Failed";
        } else if ("OUT_OF_ME+".equals(jobStatus)) {
            currentJobStatus = "Failed: Out OF MEMORY";
        } else {
            if (!isKilled() || "RUNNING".equals(jobStatus)) {
                currentJobStatus = "Running";
            } else if (isKilled() || "CANCELLED".equals(jobStatus)) {
                currentJobStatus = "Killed";
            } else {
                currentJobStatus = "Suspending";
            }
        }

        //setCurrentJobStatus(currentJobStatus);
        if (executExample && paramNotChanged) {
            setStopStatusCheck(true);
        } else if (ab.shouldUseScheduler()) {

            if (currentJobStatus.contains("Failed")) {
                sb.addMessage("error", "Processing Faild, unable to finish the job!");
                setStopStatusCheck(false);
            }

            if (currentJobStatus.equals("Killed") || currentJobStatus.equals("Finished")) {
                setStopStatusCheck(false);
            }

        } else {
            setStopStatusCheck(true);
        }
    }

    public String finishProgress() throws FileNotFoundException {
        // add graphics information before returning results page
        // TIC
        String rCommand = "OptiLCMS::plotTICs(mSet = NULL, imgName = \"raw_spec_tic.png\", format = \"png\", dpi = 150, width = 12);";
        sb.addGraphicsCMD("raw_spec_tic", rCommand);
        sb.addGraphicsMapLink("raw_spec_tic", "/Secure/spectra/SpectraResult.xhtml");

        // BPI
        rCommand = "OptiLCMS::plotBPIs(mSet = NULL, imgName = \"raw_spec_bpi.png\", format = \"png\", dpi = 150, width = 12);";
        sb.addGraphicsCMD("raw_spec_bpi", rCommand);
        sb.addGraphicsMapLink("raw_spec_bpi", "/Secure/spectra/SpectraResult.xhtml");

        // Intensity raw_spec_int
        rCommand = "OptiLCMS::PlotSpectraInsensityStistics(mSet = NULL, imgName = \"raw_spec_int.png\", format = \"png\", dpi = 150, width = 12);";
        sb.addGraphicsCMD("raw_spec_int", rCommand);
        sb.addGraphicsMapLink("raw_spec_int", "/Secure/spectra/SpectraResult.xhtml");

        // PCA
        rCommand = "OptiLCMS::PlotSpectraPCA(mSet = NULL, imgName = \"raw_spec_pca.png\", format = \"png\", dpi = 150, width = 12);";
        sb.addGraphicsCMD("raw_spec_pca", rCommand);
        sb.addGraphicsMapLink("raw_spec_pca", "/Secure/spectra/SpectraResult.xhtml");

        isms2 = !"ms1".equals(spb.getMs2DataOpt());

        if (ab.shouldUseScheduler() && !(executExample && paramNotChanged)) {

            String jobStatus = getCurrentJobStatus();

            if (jobStatus.equals("Finished") && progress2 == 100) {

                spb.setRecordCMD(true);
                if (!spb.populateRawResBeans()) {
                    return null;
                }
                if (!spb.populateRawFeatureBeans()) {
                    return null;
                }

                if (!spb.populateRawMS2Beans()) {
                    //return null;
                }

                spb.internalizeRes(0);
                if (!spb.summarizeProcessingMsg()) {
                    sb.addMessage("error", "Job is not totally ready yet! Please try again later.");
                    return null;
                }
                String omics_type = RDataUtils.retrieveOmicsType(sb.getRConnection());
                sparam.setTarget_omics(omics_type);
                if (isms2) {
                    if (omics_type.equals("metabolomics")) {
                        spb.preparePiechart(0);
                    } else {
                        spb.preparePiechart(0);
                        spb.preparePiechart(1);
                    }
                }

                return "Spectra result";
            } else {
                sb.addMessage("error", "Job is not completed yet! Please wait until the Job Status becomes Finished.");
                return null;
            }

        } else {
            /*
            if (!getCurrentJobStatus().contains("Finished") && progress2 != 100) {
                sb.addMessage("Error", "Please wait until the job is finished!");
                return null;
            }
             */
            if (!spb.populateRawResBeans()) {
                return null;
            }
            if (!spb.populateRawFeatureBeans()) {
                return null;
            }
            spb.internalizeRes(0);
            spb.summarizeProcessingMsg();
            String omics_type = RDataUtils.retrieveOmicsType(sb.getRConnection());
            sparam.setTarget_omics(omics_type);
            //System.out.println("===== isms2==2==> " + isms2);
            if (isms2) {
                if (omics_type.equals("metabolomics")) {
                    spb.preparePiechart(0);
                } else {
                    spb.preparePiechart(0);
                    spb.preparePiechart(1);
                }
            }

            return "Spectra result";
        }

    }

    public int getExamplePid() {
        return examplePid;
    }

    public void setExamplePid(int examplePid) {
        this.examplePid = examplePid;
    }

    public long getCurrentJobId() {
        return currentJobId;
    }

    public void setCurrentJobId(long currentJobId) {
        this.currentJobId = currentJobId;
    }

    public long getFinishedJobId() {
        return finishedJobId;
    }

    public void setFinishedJobId(long finishedJobId) {
        this.finishedJobId = finishedJobId;
    }

    // Section 3 : Job status section -------------------
    public String goToJobStatus(boolean saveBool) {

        if (!checkJobRunning()) {
            //update peak params
            updatePeakParam();
            //forcedly zero progress log
            Writer wr;
            try {
                wr = new FileWriter(sb.getCurrentUser().getHomeDir() + "/log_progress.txt");
                wr.write("0");
                wr.close();
            } catch (IOException ex) {
                LOGGER.error("goToJobStatus", ex);
                //  Logger.getLogger(SpectraControlBean.class.getName()).log(Level.SEVERE, null, ex);
            }

            //Initialize the controller status
            setStopStatusCheck(true);
            setJobSubmitted(false);
            setPerformedPlan(false);
            setKilled(false);
            setCurrentJobId(0);
            setCurrentJobStatus("Submitting...");
            setProgress2(0.0);
            setCreatedShareLink(false);
            //return to the job status web page
            if (saveBool) {
                boolean res = false;
                try {
                    sb.addNaviTrack("Job Status", "/Secure/spectra/JobStatusView.xhtml");
                    res = fbc.saveProject("project");
                } catch (Exception ex) {
                    java.util.logging.Logger.getLogger(SpectraControlBean.class.getName()).log(Level.SEVERE, null, ex);
                }

                if (!res) {
                    sb.addMessage("error", "Project saving failed!");
                } else {
                    DataUtils.doRedirectWithGrowl(sb, "/MetaboAnalyst/Secure/spectra/JobStatusView.xhtml", "info", "Project saving is successful, you can access your project in your <b>Project View</b> page later.");

                }
            } else {
                return "Job status";
            }
        } else {
            PrimeFaces.current().executeScript("PF('uploadSessionDialog').show()");
        }
        return null;
    }

    // KIll job on session destroy
    @PreDestroy
    public void onSessionLeaveJobKill() {
        if (!isCreatedShareLink() && !sb.isRegisteredLogin()) {
            long id = getCurrentJobId();
            if (id == 0) {
                return;
            }
            String status = SchedulerUtils.getJobStatus(id, sb.getCurrentUser().getHomeDir());
            if (status.equals("Pending") || status.equals("Running")) {
                SchedulerUtils.killJob(getCurrentJobId());
            }
        }
    }

    public String getCurrentJobStatus() {
        return currentJobStatus;
    }

    public void setCurrentJobStatus(String currentJobStatus) {
        this.currentJobStatus = currentJobStatus;
    }

    public boolean isStopStatusCheck() {
        return stopStatusCheck;
    }

    public void setStopStatusCheck(boolean stopStatusCheck) {
        this.stopStatusCheck = stopStatusCheck;
    }

    public boolean isKilled() {
        return Killed;
    }

    public void setKilled(boolean Killed) {
        this.Killed = Killed;
    }

    public boolean isJobSubmitted() {
        return jobSubmitted;
    }

    public void setJobSubmitted(boolean jobSubmitted) {
        this.jobSubmitted = jobSubmitted;
    }

    public String getFinishedJobStatus() {
        return finishedJobStatus;
    }

    public void setFinishedJobStatus(String finishedJobStatus) {
        this.finishedJobStatus = finishedJobStatus;
    }

    public String getCurrentJobPriority() {
        return currentJobPriority;
    }

    public void setCurrentJobPriority(String currentJobPriority) {
        this.currentJobPriority = currentJobPriority;
    }

    public Double getProgressProcessing() {
        Double myPro = updateProgress();

        if (myPro > 100) {
            myPro = myPro / 2.0;
        }

        if (myPro > 100) {
            myPro = 100.0;
        }

        if (myPro == null) {
            progress2 = 0.0;
            return progress2;
        }

        if (myPro > progress2) {
            progress2 = myPro;
        }
        //System.out.println(" === getProgressProcessing====> " + myPro + "=== progress2===> " + progress2);
        if (progress2 == 100) {
            if (ab.shouldUseScheduler()) {
                setCurrentJobStatus("Finished");
            } else {
                setCurrentJobStatus("Finished");
                setJobSubmitted(false);
            }
            setStopStatusCheck(false);
            setFinishedJobId(currentJobId);
            setFinishedProgress2(progress2);
            setFinishedJobStatus(currentJobStatus);
            if (ab.isOnProServer() || ab.isOnQiangPc()) {
                String JobPos;
                if (ab.isOnVipServer()) {
                    JobPos = "vip";
                } else if (ab.isOnProServer()) {
                    JobPos = "pro";
                } else if (ab.isOnQiangPc()) {
                    JobPos = "qiang";
                } else if (ab.isInDocker()) {
                    JobPos = "docker";
                } else {
                    JobPos = "unknown";
                }
                db.updateRawJobStatus(currentJobId, currentJobStatus, JobPos);
                //updateJobStatus(currentJobId);
            }
        }
        return progress2;
    }

    public Double updateProgress() {

        try {
            File myObj;
            if (isms2) {
                myObj = new File(sb.getCurrentUser().getHomeDir() + "/log_progress2.txt");
            } else {
                myObj = new File(sb.getCurrentUser().getHomeDir() + "/log_progress.txt");
            }

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
                    return data;
                }
            }
        } catch (FileNotFoundException e) {
            //LOGGER.error("updateProgress", e);
            System.out.println("An error occurred: Progress Log FILE NOT found " + sb.getCurrentUser().getHomeDir() + "/log_progress.txt");
        }
        return null;
    }

    public Double getProgress2() {
        return progress2;
    }

    public void setProgress2(Double progress2) {
        this.progress2 = progress2;
    }

    public double getFinishedProgress2() {
        return finishedProgress2;
    }

    public void setFinishedProgress2(double finishedProgress2) {
        this.finishedProgress2 = finishedProgress2;
    }

    public int getTotalNumberOfSamples() {
        return totalNumberOfSamples;
    }

    public void setTotalNumberOfSamples(int totalNumberOfSamples) {
        this.totalNumberOfSamples = totalNumberOfSamples;
    }

    public String getIncludedFileNamesString() {
        return includedFileNamesString;
    }

    public void setIncludedFileNamesString(String includedFileNamesString) {
        this.includedFileNamesString = includedFileNamesString;
    }

    public boolean isDataConfirmed() {
        return dataConfirmed;
    }

    public void setDataConfirmed(boolean dataConfirmed) {
        this.dataConfirmed = dataConfirmed;
    }

    public boolean isPerformedPlan() {
        return performedPlan;
    }

    public void setPerformedPlan(boolean performedPlan) {
        this.performedPlan = performedPlan;
    }

    public boolean isExecutExample() {
        return executExample;
    }

    public void setExecutExample(boolean executExample) {
        this.executExample = executExample;
    }

    private boolean isSWATHExample = false;

    public boolean isIsSWATHExample() {
        return isSWATHExample;
    }

    public void setIsSWATHExample(boolean isSWATHExample) {
        this.isSWATHExample = isSWATHExample;
    }

    public boolean getParamNotChanged() {
        return paramNotChanged;
    }

    public void setParamCNothanged(boolean paramNotChanged) {
        this.paramNotChanged = paramNotChanged;
    }

    public boolean isMetaOk() {
        return metaOk;
    }

    public void setMetaOk(boolean metaOk) {
        this.metaOk = metaOk;
    }

    public String timeStamp() {

        String timeStamp = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date());
        return timeStamp;
    }

    //to prevent multiple uploads in one session/browser
    public boolean checkJobRunning() {

        if (sb.isRegisteredLogin()) {
            return false;
        }

        String job = Faces.getRequestCookie("jobId");
        long jobId;
        if (job == null) {
            jobId = getCurrentJobId();
            //PrimeFaces.current().executeScript("console.log(000000000000000)");
        } else {
            jobId = Long.parseLong(job);
            //PrimeFaces.current().executeScript("console.log(111111111111111)");
        }

        String status = "";
        if (ab.shouldUseScheduler()) {
            if (jobId == 0) {
                status = "Submitting...";
            } else {
                status = SchedulerUtils.getJobStatus(jobId, sb.getCurrentUser().getHomeDir());
            }
        } else {
            if (getCurrentJobStatus().equals("Submitting...")) {
                status = "Submitting...";
            } else {
                double progress = 0.0;
                progress = getProgressProcessing();
                if (progress == 100) {
                    status = "Local Finished";
                } else if (progress > 0) {
                    status = "Running";
                } else if (progress == 0) {
                    status = "Submitting...";
                }
            }
        }

        return (status.contains("Running") || status.contains("Pending"));
    }

    public void killCurrentJob() {
        long id = getCurrentJobId();
        if (id == 0) {
            sb.addMessage("Error", "Unable to delete running job!");
            return;
        }
        String status = SchedulerUtils.getJobStatus(id, sb.getCurrentUser().getHomeDir());
        if (status.equals("Pending") || status.equals("Running")) {
            boolean res = SchedulerUtils.killJob(getCurrentJobId());
            if (res) {
                setKilled(true);
                setCurrentJobStatus("Killed");
                setJobSubmitted(false);
                sb.addMessage("Info", "Job (" + id + ") has been successfully terminated!");
            } else {
                sb.addMessage("Error", "Unable to delete running job!");
            }
        }
    }

    /// Section 8 : Parameters updating section ---------------
    // update parameters for processing
    public void updatePeakParam() {

        RConnection RC = sb.getRConnection();

        RDataUtils.updateParams(RC, sparam.getPeakmeth(), sparam.getRtmeth(), sparam.getPolarity(),
                sparam.getPpm(), sparam.getMin_peakwidth(), sparam.getMax_peakwidth(), sparam.getMzdiff(),
                sparam.getSnthresh(), sparam.getNoise(), sparam.getPrefilter(), sparam.getValue_of_prefilter(),
                sparam.getBw(), sparam.getMinFraction(), sparam.getMinSamples(),
                sparam.getMaxFeatures(), sparam.getIntegrate(), sparam.getExtra(), sparam.getSpan(),
                sparam.getProfStep(), sparam.getFwhm(), sparam.getSigma(), sparam.getSteps(),
                sparam.getMax(), sparam.getFwhmThresh(), sparam.getMzmabsmiso(),
                sparam.getMax_charge(), sparam.getMax_iso(), sparam.getCorr_eic_th(),
                sparam.getMz_abs_add(), sparam.getAdducts(), sparam.isRmConts(), sparam.isBlksub());
    }

    public boolean MysqlDBAvailabilityCheck() throws Exception {

        if (ab.isOnQiangPc()) {
            mdbb = new MariaDBController("jdbc:mysql://localhost:3306/devUsers", "qiangPC", "1qazxDR%");
            //return true;
        } else if (ab.isOnProServer()) {
            mdbb = new MariaDBController("jdbc:mysql://198.168.185.149:3306/devUsers", "devglassfish", "1qazxDR%");
            //mdbb = new MariaDBController("jdbc:mysql://10.240.0.12:3306/devUsers", "devglassfish", "1qazxDR%");
        } else if (ab.isInDocker()) {
            return (false);
        } else {
            //For real application (dev + genap public)
            mdbb = new MariaDBController("jdbc:mysql://198.168.185.149:3306/devUsers", "devglassfish", "1qazxDR%");
        }

        boolean res = mdbb.connect();
        boolean res2 = mdbb.disconnect();
        //System.out.println("MysqlDBAvailabilityCheck result: (double true is working)" + res + "|" + res2);
        return res;
    }

    public void updateJobPartialLink() {

        if (currentJobId == 0) {
            sb.addMessage("error", "Save example as a URL job is not allowed!");
        } else {

            try {
                boolean dbAviRes = MysqlDBAvailabilityCheck();
                if (!dbAviRes) {
                    sb.addMessage("error",
                            "Job Saving server is down, report this code: xxj00002 to administrator !");
                }
            } catch (Exception ex) {
                sb.addMessage("error",
                        "Job Saving server is down, report this code: xxj00003 to administrator !");
            }

            String dataPlace = "dev";
            if (ab.isOnProServer()) {
                dataPlace = "dev";
            } else if (ab.isOnQiangPc()) {
                dataPlace = "qiang";
            } else {
                dataPlace = "unk";
            }

            String partialID = sb.getPartialId();
            String updateQuery = "update devUsers.jobs set partialLink = '"
                    + partialID
                    + "' where jobID = "
                    + currentJobId
                    + " AND jobPosition ='"
                    + dataPlace + "';";

            try {
                mdbb.runUpdate(updateQuery);
                sb.addMessage("info", "Job URL created successfully!");
            } catch (SQLException ex) {
                sb.addMessage("error",
                        "Job Saving server is down, report this code: xxj00019 to administrator !");
            }
        }
    }

    public void updateJobPartialLink_docker() {
        if (currentJobId == 0) {
            sb.addMessage("error", "Please wait until job finially submitted!");
        } else {
            String partialID = sb.getPartialId();
            RDataUtils.recordPartialLinkinDocker(sb.getRConnection(), partialID, (int) currentJobId, sb.getCurrentUser().getName(), timeStamp());
        }
    }

    public void exampleJobPartialWarning() {
        if (currentJobId == 0) {
            sb.addMessage("error", "Save example as a URL job is not allowed! Please try this with your own data.");
        }
    }

    ///Section 10: OptiLCMS auto update controller
    //set to enable auto update
    public void OptiLCMSCheck() {
//        RConnection RC = sb.getRConnection();
//        String res = RDataUtils.checkOptiLCMS(RC, ab.getOptiLCMSversion());
//        if (!res.equals("TRUE")) {
//            System.out.println("Updating your local OPtiLCMS now....");
//            //RDataUtils.updateOptiLCMS(RC);
//            System.out.println("Updating finished!");
//        }
    }
}
