/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.util.Scanner;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.List;
import org.primefaces.model.StreamedContent;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.DownloadBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;

/**
 *
 * @author qiang
 */
@ViewScoped
@Named("batchprocess")
public class BatchProcesser implements Serializable {

    @Inject
    private SessionBean1 sb;
    @Inject
    private ApplicationBean1 ab;
    @Inject
    private FireProjectBean pbb;

    @Inject
    private DownloadBean dwb;
    
    @Inject
    private FireBaseController fbc;
    
    private String homeDir;
    private boolean jobSubmitted = false;

    private int progress_val = 0;

    public int getProgress_val() {
        return progress_val;
    }

    public void setProgress_val(int progress_val) {
        this.progress_val = progress_val;
    }

    private int delay = 0;

    public void proceed2report() {

        RCenter.LoadRLoadImg(sb.getRConnection(), "Rload_batch.RData");
        RCenter.saveRLoadImg(sb.getRConnection());
        dwb.generateReport("html");

    }

    private String status_val = "Pending";

    public String getStatus_val() {
        //if(progress_val<1)
        return status_val;
    }

    public void setStatus_val(String status_val) {
        this.status_val = status_val;
    }

    private boolean stopCheckProgress = false;

    public boolean isStopCheckProgress() {
        return stopCheckProgress;
    }

    public void setStopCheckProgress(boolean stopCheckProgress) {
        this.stopCheckProgress = stopCheckProgress;
    }

    public boolean batchProcess_running() {
        return progress_val > 0 & progress_val < 100;
    }

    public boolean exeBatchProcessing() {

        // This function include four steps :  
        // 0). save the project first;
        // 1). organize and generate exe_batch.sh; 
        // 2). Submit to SLURM; 
        // 3). Pop-up prgress bar; 
        // 4). generate and show report page

        sb.setIsBatchProject(true);
        fbc.saveBatchProject("batch_project", pbb.getTemplateToken());

        String home_dir = sb.getCurrentUser().getHomeDir();
        String script_path = ab.getRscriptLoaderPath();
        String analType = sb.getAnalType();
        homeDir = home_dir;
        String temp_path = pbb.getBatchTemplatePath();

        //System.out.println("exeBatchProcessing -- home_dir    ==> " + home_dir);
        //System.out.println("exeBatchProcessing -- script_path ==> " + script_path);
        //System.out.println("exeBatchProcessing -- analType    ==> " + analType);
        //System.out.println("exeBatchProcessing -- temp_path   ==> " + temp_path);

        String JobSubmission;
        //int JobID = 0;
        if (ab.isOnProServer() || ab.isOnQiangPc()) {
            RDataUtils.generateSLURMbash(sb.getRConnection(), analType, "", temp_path, home_dir, script_path, "TRUE");
            // submit to slurm
            JobSubmission = "sbatch " + home_dir + "/exe_batch.sh";
        } else {
            RDataUtils.generateSLURMbash(sb.getRConnection(), analType, "", temp_path, home_dir, script_path, "FALSE");
            JobSubmission = "nohup Rscript " + home_dir + "/exe_batch.R > exe_output 2>&1 &";
        }

        jobSubmitted = DataUtils.SubmitJob(JobSubmission);
        // redirect to the job status page
        String url = "/" + ab.getAppName() + "/xialabpro/BatchProgress.xhtml";
        DataUtils.doRedirect(url, ab);

        return jobSubmitted;
    }

    public void checkprogress() {

        initprogress();
        pbb.setBatchModeEnabled(true);
        File file0 = new File(homeDir + "/progress_value");
        Scanner input0;
        String str = "0";

        try {
            input0 = new Scanner(file0);
            while (input0.hasNextLine()) {
                str = input0.nextLine();
            }
        } catch (FileNotFoundException ex) {
            System.out.println("Now the FileNotFoundException " + str);
        }

        progress_val = Integer.parseInt(str);
        if (progress_val == 0 & jobSubmitted) {
            status_val = "Submitted! Waiting to start...";
        }

        if (progress_val < 99 & progress_val > 0) {
            status_val = "Running...";
        }

        if (progress_val == 99) {
            status_val = "Almost finished. Please wait patiently..";
        }

        if (progress_val == 100) {
            status_val = "Completed! Click 'Proceed' to generate report.";
            stopCheckProgress = true;
            delay++;
            if (delay == 1) {
                sb.addMessage("info", "Batch processing successfully completed!");
            }
            if (delay > 5) {
                // give a little bit delay to ensure the hard drive I/O is completed
                RCenter.LoadRLoadImg(sb.getRConnection(), "Rload_batch.RData");
                RCenter.saveRLoadImg(sb.getRConnection());

                if (delay == 6) {
                    dwb.generateReport("html");
                }
            }
        }
    }

    public void initprogress() {

        if (sb.getCurrentUser() == null) {
            return;
        }

        String home_dir = sb.getCurrentUser().getHomeDir();
        homeDir = home_dir;

        File file0 = new File(homeDir + "/progress_value");
        Scanner input0 = null;
        String str = "0";

        try {
            input0 = new Scanner(file0);
            while (input0.hasNextLine()) {
                str = input0.nextLine();
            }
        } catch (FileNotFoundException ex) {
            System.out.println("Now the FileNotFoundException " + str + " Job not submitted yet!");
            return;
        }

        progress_val = Integer.parseInt(str);

        if (progress_val == 0) {
            status_val = "Pending...";
        }

        if (progress_val < 99 & progress_val > 0) {
            status_val = "Running...";
        }

        if (progress_val == 99) {
            status_val = "Almost finished. Please wait patiently..";
        }

        if (progress_val == 100) {
            status_val = "Completed! Click 'Proceed' to generate report.";
        }
    }

    public void cancelJob() {
        stopCheckProgress = true;

    }

    public String getJobStatusText() throws IOException {

        String filnm = sb.getCurrentUser().getHomeDir() + "/slurm_exe_res.out";
        StringBuilder s = new StringBuilder();
        try {
            Process p;
            //String cmd = "tail -" + 18 + " " + filnm;
            //Process p = Runtime.getRuntime().exec("tail -" + 18 + " " + filnm);
            //List<String> commands = Arrays.asList(cmd.split(" ")); // Split the command into arguments
            List<String> commands = List.of("tail", "-n", "18", filnm);
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            p = processBuilder.start();
            java.io.BufferedReader input = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream()));
            String line;
            while ((line = input.readLine()) != null) {
                if (line.contains("ERROR:")) {
                    line = line.replace("ERROR:", "<b>ERROR:</b>");
                } else if (line.contains("> ")) {
                    line = line.replace("> ", "");
                } else if (line.contains("\"Everything has been finished successfully!\"")) {
                    line = line.replace("\"Everything has been finished successfully!\"", "<b style='color: green'>Everything has been finished successfully!</b>");
                }
                if (line.contains("generateProgressMarker") || line.startsWith("print")) {
                    line = line.replace(line, "");
                }
                if (line.length() > 250) {
                    line = line.replace(line, "");
                }
                if (line.equals("[1] 1")) {
                    line = "<b>Done !</b>\n";
                }
                if (line.startsWith("[1] ")) {
                    line = line.replace("[1] ", "");
                }
                if (!line.equals("")) {
                    String newLine = line + "<br />";
                    s.append(newLine);
                }
            }
        } catch (java.io.IOException e) {
            System.out.println("An error occurred.");
        }
        return s.toString();
    }

    public StreamedContent getTextOutputFile() throws IOException {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/slurm_exe_res.out");
    }

}
