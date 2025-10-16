/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import pro.metaboanalyst.controllers.general.ApplicationBean1;

/**
 *
 * @author qiang
 */
@ApplicationScoped
@Named("jobMonitor")
public class JobMonitor {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    private MailService ms;

    public void init() {
        if (ab.isOnQiangPc() || ab.isOnZgyPc() || ab.isOnVipServer() || ab.isOnVipServer2()) {
            Executors.newSingleThreadScheduledExecutor()
                    .scheduleAtFixedRate(this::run, 0, 15, TimeUnit.SECONDS);
            System.out.println("JobMonitor scheduled.");
        } else {
            System.out.println("JobMonitor skipped on this machine.");
        }
    }

    public void run() {
        System.out.println("Running into job monitor....");
        int num = 0;
        String rCommand = "", rCmd = "", job_status = "", job_email = "";
        String dataPath = "/data/glassfish/projects/data/all_slurm_jobs.csv";

        boolean switch_trigger = false;

        if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))) {
            switch_trigger = true;
        } else if (Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
            switch_trigger = true;
        } else if (Files.isDirectory(Paths.get("/home/zgy/"))) {
            switch_trigger = true;
        }

        try {
            while (switch_trigger) {
                num++;
                RConnection RC = new RConnection("127.0.0.1", 6311);

                // Ensure the all_slurm_jobs.csv file exists
                String rCMD = " if(!dir.exists(\"/data/glassfish/projects/data/\")){dir.create(\"/data/glassfish/projects/data/\", recursive = T)}";
                try {
                    RC.voidEval(rCMD);
                } catch (RserveException ex1) {
                    //java.util.logging.Logger.getLogger(RDataUtils.class.getName()).log(Level.SEVERE, null, ex1);
                }
                rCommand = "if(!file.exists(\"" + dataPath + "\")){"
                        + "write.csv(data.frame(jobid=0, emailed=T, email='', folder='', wfstatus='', wfBool=''), "
                        + "file = \"" + dataPath + "\", row.names = FALSE)}";
                RC.voidEval(rCommand);

                // Get job IDs that haven't been emailed
                rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE);"
                        + "all_unemailed_job_ids <- as.character(dt[!dt$emailed,1]); all_unemailed_job_ids";
                String jobIDs[] = RC.eval(rCommand).asStrings();

                // Get corresponding folder names
                rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE);"
                        + "all_unemailed_job_ids <- as.character(dt[!dt$emailed,4]); all_unemailed_job_ids";
                String folderNms[] = RC.eval(rCommand).asStrings();

                Map<String, String> jobFolderMap = new HashMap<>();
                for (int i = 0; i < jobIDs.length; i++) {
                    jobFolderMap.put(jobIDs[i], folderNms[i]);
                }

                for (String jid : jobIDs) {
                    rCmd = "system(\"sacct -j " + jid + " --format=state\", intern = T)[3]";
                    job_status = RC.eval(rCmd).asString();

                    rCmd = "dt <- read.csv(\"" + dataPath + "\", header = TRUE);"
                            + "this_email <- as.character(dt[dt$jobid==" + jid + ",3]); this_email";
                    job_email = RC.eval(rCmd).asString();

                    if (job_email == null || job_email.length() < 4) {
                        continue;
                    }

                    /* ── 2 · inside the for-loop, fetch wfBool for this job id ──────────────── */
                    rCmd
                            = "dt <- read.csv('" + dataPath + "', header = TRUE); "
                            + "as.logical(dt[dt$jobid == " + jid + ", 'wfBool'])";
                    boolean wfBool = RC.eval(rCmd).asInteger() == 1; // TRUE → 1, FALSE → 0

                    System.out.println("==== this job_email====> " + job_email);
                    System.out.println("==== this job_status====> " + job_status);

                    boolean res = false;
                    if (job_status.contains("CANCELLED") || job_status.contains("FAILED")) {
                        res = sendReminderEmail(job_status, jid, job_email);
                    } else if (job_status.contains("COMPLETED")) {

                        String folderName = jobFolderMap.get(jid);
                        String filePath = "/data/glassfish/projects/metaboanalyst/" + folderName + "/metaboanalyst_input.csv";
                        File file = new File(filePath);

                        if (!file.exists() & !Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
                            continue;
                        }

                        // ── NEW: read wfstatus from CSV for this job (default to "UNCOMPLETE" if missing/NA/empty)
                        String wfStatusCsv;
                        try {
                            rCmd = "dt <- read.csv('" + dataPath + "', header=TRUE);"
                                    + "x <- as.character(dt$wfstatus[dt$jobid==" + jid + "]);"
                                    + "if (length(x)==0 || is.na(x[1]) || nchar(x[1])==0) 'UNCOMPLETE' else x[1]";
                            wfStatusCsv = RC.eval(rCmd).asString();
                        } catch (Exception e) {
                            Logger.getLogger(JobMonitor.class.getName())
                                    .log(Level.WARNING, "Failed to read wfstatus for job " + jid, e);
                            wfStatusCsv = "UNCOMPLETE";
                        }
                        System.out.println("==== current wfstatus in CSV for job " + jid + " => " + wfStatusCsv);

                        if (!wfBool) {
                            res = sendReminderEmail(job_status, jid, job_email);

                        } else {
                            

                                if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))
                                        & Files.isRegularFile(Paths.get("/home/glassfish/payara6_micro/useVIP_2025R2"))) {
                                    sendPostRequest("vip2", folderName, jid, job_email);
                                    res=true;
                                } else if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))) {
                                    res = sendPostRequest("vip", folderName, jid, job_email);
                                } else if (Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
                                    // Qiang's local for testing purpose
                                    System.out.println("before sendPostRequest Qiang's local <=========");
                                    res = sendPostRequest("localhost", folderName, jid, job_email);
                                }

                                rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE);"
                                        + "idx <- dt$jobid==" + jid + ";"
                                        + "if (any(idx)) dt$wfstatus[idx] <- \"WORKFLOW_PROGRESS\";"
                                        + "write.csv(dt, file = \"" + dataPath + "\", row.names = FALSE)";
                                RC.voidEval(rCommand);
                            
                        }
                    }

                    if (res) {
                        rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE);"
                                + "dt[dt$jobid==" + jid + ",2] <- TRUE;"
                                + "write.csv(dt, file = \"" + dataPath + "\", row.names = FALSE)";
                        RC.voidEval(rCommand);
                    }
                }

                RC.close();
                try {
                    System.out.println("loopJobMonitor=====");
                    Thread.sleep(15000);
                } catch (InterruptedException ex) {
                    Logger.getLogger(JobMonitor.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        } catch (RserveException | REXPMismatchException ex) {
            Logger.getLogger(JobMonitor.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public static String readWfStatusOnceByFolder(String folderName) {
        RConnection rc = null;
        try {
            rc = new RConnection("127.0.0.1", 6311);
            String DATA_PATH = "/data/glassfish/projects/data/all_slurm_jobs.csv";

            // Read wfstatus for the given folder; return "" if NA/missing
            String read
                    = "dt <- read.csv('" + DATA_PATH + "', header=TRUE);\n"
                    + "x <- dt$wfstatus[dt$folder == '" + folderName + "'];\n"
                    + "if (length(x)==0 || is.na(x[1])) '' else as.character(x[1])";
            String wf = rc.eval(read).asString();
            return (wf == null || "NA".equals(wf)) ? "" : wf;
        } catch (Exception e) {
            e.printStackTrace();
            return "";
        } finally {
            if (rc != null) try {
                rc.close();
            } catch (Exception ignore) {
            }
        }
    }

    private boolean sendResumeEmailTest(String job_status, String jid, String email, String node, String folderName) {

        // Construct the resume job URL
        String resumeUrl = "https://" + node + ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml"
                + "?funcNm=resumeRawProject"
                + "&folderName=" + URLEncoder.encode(folderName, StandardCharsets.UTF_8)
                + "&jobId=" + URLEncoder.encode(jid, StandardCharsets.UTF_8)
                + "&email=" + URLEncoder.encode(email, StandardCharsets.UTF_8);

        // HTML message with resume URL
        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<body style=\"font-family: Arial; font-size: 12px;\">\n"
                + "<div>\n"
                + "    <p>\n"
                + "        Your spectra processing job (ID: " + jid + ") status has become <b>" + job_status + "</b>.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        You can resume your job by clicking the link below:\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        <a href=\"" + resumeUrl + "\" target=\"_blank\">Resume Job</a>\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        Please ignore this email if you did not submit any jobs to MetaboAnalyst.\n"
                + "    </p>\n"
                + "\n"
                + "    <p style=\"color:gray; font-size:10px;\">Do NOT reply to this email.</p>\n"
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        boolean res = false;
        try {
            res = ms.sendEmail(email, "MetaboAnalyst - Job Status Update", "text/html", htmlMsg);
        } catch (IOException ex) {
            Logger.getLogger(JobMonitor.class.getName()).log(Level.SEVERE, null, ex);
        }
        return res;
    }

    private boolean sendReminderEmail(String job_status, String jid, String email) {

        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<body style=\"font-family: Arial; font-size: 12px;\">\n"
                + "<div>\n"
                + "    <p>\n"
                + "        Your spectra processing job (ID: " + jid + ") status has become <b>" + job_status + "</b>.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        Please ignore this email if you did not submit any jobs to MetaboAnalyst.\n"
                + "    </p>\n"
                + "\n"
                + "\n Do NOT reply this email."
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        boolean res = false;
        try {
            res = ms.sendEmail(email, "MetaboAnalyst - Job Status Update", "text/html", htmlMsg);
        } catch (IOException ex) {
            Logger.getLogger(JobMonitor.class.getName()).log(Level.SEVERE, null, ex);
        }
        return res;
    }

    public boolean sendPostRequest(String node, String folderName, String jobId, String email) {
        String baseUri = "https://" + node + ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml";
        HttpClient client = HttpClient.newHttpClient();
        System.out.println("SENDPOSTREQUEST=============" + baseUri);

        if (node.equals("localhost")) {
            baseUri = ab.getBaseUrlDyn() + "/faces/AjaxHandler.xhtml";
        }

        try {
            // Construct URI with query parameter
            URI uri = new URI(baseUri + "?funcNm=resumeRawProject");

            // Prepare the URL parameters with encoding
            String urlParameters = Map.of(
                    "folderName", folderName,
                    "jobId", jobId,
                    "email", email
            ).entrySet().stream()
                    .map(entry -> URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8) + "="
                    + URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8))
                    .collect(Collectors.joining("&"));

            // Build HTTP POST Request
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(uri)
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .POST(HttpRequest.BodyPublishers.ofString(urlParameters))
                    .build();

            // Send request and handle response
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            System.out.println("SENDPOSTREQUEST===========urlParameters=====" + urlParameters);

            if (response.statusCode() == 200) { // HTTP OK
                // Process the response content
                System.out.println("Response: " + response.body());
                return true;
            } else {
                System.err.println("Request failed. Response code: " + response.statusCode());
                return false;
            }

        } catch (URISyntaxException | IOException | InterruptedException e) {
            // Handle exceptions
            e.printStackTrace();
            Thread.currentThread().interrupt(); // Handle InterruptedException properly
            return false;
        }

    }

    private void checkJobs() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    /**
     * public boolean sendPostRequest(String node, String folderName, String
     * jobId, String email) { String urlString = "https://" + node +
     * ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml?funcNm=resumeRawProject";
     *
     * try { // Create a URL object URL url = new URL(urlString);
     *
     * // Open an HTTP connection HttpURLConnection conn = (HttpURLConnection)
     * url.openConnection();
     *
     * // Set request method and headers conn.setRequestMethod("POST");
     * conn.setRequestProperty("Content-Type",
     * "application/x-www-form-urlencoded"); conn.setDoOutput(true); // Enable
     * output for the request body
     *
     * // Prepare the URL parameters String urlParameters = "folderName=" +
     * URLEncoder.encode(folderName, "UTF-8") + "&jobId=" +
     * URLEncoder.encode(jobId, "UTF-8") + "&email=" + URLEncoder.encode(email,
     * "UTF-8");
     *
     * // Write parameters to the request body try (DataOutputStream wr = new
     * DataOutputStream(conn.getOutputStream())) { wr.writeBytes(urlParameters);
     * wr.flush(); }
     *
     * // Check the response code int responseCode = conn.getResponseCode(); if
     * (responseCode == HttpURLConnection.HTTP_OK) { // 200 // Read the response
     * try (BufferedReader reader = new BufferedReader(new
     * InputStreamReader(conn.getInputStream()))) { String responseLine;
     * StringBuilder responseContent = new StringBuilder();
     *
     * while ((responseLine = reader.readLine()) != null) {
     * responseContent.append(responseLine); }
     *
     * // Process the response content System.out.println("Response: " +
     * responseContent.toString()); } } else { System.err.println("Request
     * failed. Response code: " + responseCode); return false; }
     *
     * // Disconnect the connection conn.disconnect(); return true;
     *
     * } catch (IOException e) { // Handle IOException e.printStackTrace();
     * return false; } }*
     */
}
