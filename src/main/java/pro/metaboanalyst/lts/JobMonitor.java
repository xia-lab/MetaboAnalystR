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
import java.util.stream.Collectors;
import pro.metaboanalyst.controllers.general.ApplicationBean1;

/**
 *
 * @author qiang
 */
@ApplicationScoped
@Named("jobMonitor")
public class JobMonitor extends Thread {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    private MailService ms;

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
                rCommand = "if(!file.exists(\"" + dataPath + "\")){"
                        + "write.csv(data.frame(jobid=0, emailed=T, email='', folder='', wfstatus=''), "
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

                    if (job_email.length() < 4) {
                        continue;
                    }

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

                        if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))
                                & Files.isRegularFile(Paths.get("/home/glassfish/payara6_micro/useVIP2"))) {
                            res = sendPostRequest("vip2", folderName, jid, job_email);
                            //sendResumeEmailTest("RAW_COMPLETED", jid, job_email, "vip2", folderName);
                        } else if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))) {
                            res = sendPostRequest("vip", folderName, jid, job_email);
                            //sendResumeEmailTest("RAW_COMPLETED", jid, job_email, "vip", folderName);
                        } else if (Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
                            // Qiang's local for testing purpose
                            System.out.println("before sendPostRequest Qiang's local <=========");
                            res = sendPostRequest("localhost", folderName, jid, job_email);
                        }

                        //res = sendReminderEmail(job_status, jid, job_email);
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
