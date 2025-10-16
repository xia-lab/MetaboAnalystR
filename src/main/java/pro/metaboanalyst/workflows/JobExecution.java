/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.IOException;
import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.ResultSet;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.primefaces.PrimeFaces;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.lts.JobMonitor;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;

/**
 *
 * @author zgy
 */
@SessionScoped
@Named("jobExecution")
public class JobExecution implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private DiagramView dv;

    private boolean stopStatusCheck = true;
    private String statusMsg = "Job not submitted.";

    private String currentJobId = "";
    private String currentStartTime = "";

    public String getCurrentStartTime() {
        return currentStartTime;
    }

    public void setCurrentStartTime(String currentStartTime) {
        this.currentStartTime = currentStartTime;
    }

    public String getCurrentJobId() {
        return currentJobId;
    }

    public void setCurrentJobId(String currentJobId) {
        this.currentJobId = currentJobId;
    }

    public boolean isStopStatusCheck() {
        return stopStatusCheck;
    }

    public void setStopStatusCheck(boolean stopStatusCheck) {
        this.stopStatusCheck = stopStatusCheck;
    }

    public String getStatusMsg() {
        return statusMsg;
    }

    public void setStatusMsg(String statusMsg) {
        this.statusMsg = statusMsg;
    }
// Inject your persistent timer service
    @Inject
    private JobTimerService jobTimers;

    public void checkJobStatus() {
        if (sb.getCurrentUser() == null) {
            return;
        }

        // Keep your special RAW path if needed
        if ("raw".equalsIgnoreCase(sb.getAnalType())) {
            run();
            return;
        }

        final String userId = sb.getCurrentUser().getName();
        final JobTimerService.Status st = jobTimers.getStatus(userId);

        if (st == null) {
            statusMsg = "No job found for job_" + userId + " (not started or not initialized yet).";
            return;
        }
        String jobId = userId;

        switch (st) {
            case IN_PROGRESS -> {
                statusMsg = "Job is running...";
            }
            case COMPLETED -> {
                statusMsg = "<span style='color: green'>Job has finished!</span>";
                stopStatusCheck = true;

                // Build the finish URL (same as before, but redirect on the CLIENT)
                String token = jobTimers.getTokenByJobId(jobId);  // ← fetch what you stored
                String url = DataUtils.constructNavigationURL(
                        ab.getToolLocation(), ab.getAppName(), token, "finishWorkflowJob", ab
                );

                // IMPORTANT: client-side redirect during Ajax poll
                PrimeFaces.current().executeScript("window.location.href='" + url + "';");
            }
            case FAILED -> {
                statusMsg = "<span style='color: red'>Job failed or encountered an error.</span>";
                stopStatusCheck = true;
            }
            default -> {
                statusMsg = "Job status: " + st;
            }
        }

        System.out.println("statusMsg==" + statusMsg);
    }

    public void run() {

        System.out.println("Running into job monitor session....POLL  - " + LocalDateTime.now());
        int num = 0;
        String rCommand = "", job_status = "", job_email = "";
        String dataPath = "/data/glassfish/projects/data/all_slurm_jobs.csv";

        try {
            num++;
            RConnection RC = new RConnection("127.0.0.1", 6311);

            // Ensure the all_slurm_jobs.csv file exists
            rCommand = "if(!file.exists(\"" + dataPath + "\")) {"
                    + "write.csv(data.frame(jobid=0, emailed=T, email='', folder='', wfstatus=''), "
                    + "file = \"" + dataPath + "\", row.names = FALSE)}";
            RC.voidEval(rCommand);

            // Fetch all unemailed job IDs, their folders, statuses, and emails
            rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE); "
                    + "as.character(dt[!dt$emailed, 'jobid'])";
            String jobIDs[] = RC.eval(rCommand).asStrings();

            rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE); "
                    + "as.character(dt[!dt$emailed, 'folder'])";
            String folderNms[] = RC.eval(rCommand).asStrings();

            rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE); "
                    + "as.character(dt[!dt$emailed, 'wfstatus'])";
            String jobStatuses[] = RC.eval(rCommand).asStrings();

            rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE); "
                    + "as.character(dt[!dt$emailed, 'email'])";
            String jobEmails[] = RC.eval(rCommand).asStrings();

            rCommand = "dt <- read.csv(\"" + dataPath + "\", header = TRUE); "
                    + "as.character(dt[!dt$wfBool, 'email'])";
            String jobWfBools[] = RC.eval(rCommand).asStrings();

            Map<String, String> jobFolderMap = new HashMap<>();
            Map<String, String> jobStatusMap = new HashMap<>();
            Map<String, String> jobEmailMap = new HashMap<>();
            Map<String, String> jobWfBoolMap = new HashMap<>();

            for (int i = 0; i < jobIDs.length; i++) {
                jobFolderMap.put(jobIDs[i], folderNms[i]);
                jobStatusMap.put(jobIDs[i], jobStatuses[i] != null ? jobStatuses[i] : "UNKNOWN");
                jobEmailMap.put(jobIDs[i], jobEmails[i]);
                jobWfBoolMap.put(jobIDs[i], jobWfBools[i]);

            }

            String currentUserFolder = sb.getCurrentUser().getName();

            for (String jid : jobIDs) {
                String folderName = jobFolderMap.get(jid);
                String status = jobStatusMap.get(jid);
                String email = jobEmailMap.get(jid);
                String wfBool = jobWfBoolMap.get(jid);

                // Ensure the job is associated with the current user's folder
                if (!currentUserFolder.equals(folderName)) {
                    continue; // Skip jobs that do not belong to the current user
                }

                if ("RAW_FINISHED".equalsIgnoreCase(status)) {
                    System.out.println("REACHED=======================RAW_FINISHED");

                    sb.getNotice().add(DataUtils.obtainTimestampText());
                    sb.getNotice().add("Raw processing is <b>COMPLETED</b>. Now computing the rest of the workflow.");
                    dv.setShowNotif(true);
                    RDataUtils.updateRawJobStatusByFolder(sb.getRConnection(), folderName, "RAW_NOTIFIED");
                    PrimeFaces.current().executeScript("PF('progressDialog').hide()");
                    PrimeFaces.current().executeScript("PF('rawWorkflowProgressDialog').show()");

                } else if ("WORKFLOW_FINISHED".equalsIgnoreCase(status)) {
                    PrimeFaces.current().executeScript("PF('rawWorkflowProgressDialog').hide()");
                    System.out.println("REACHED=======================WORKFLOW_FINISHED");
                    String node = "vip";
                    String funcName = "finishRawProject";
                    Map<String, String> params = Map.of(
                            "folderName", folderName,
                            "jobId", jid,
                            "email", email
                    );

                    if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))
                            & Files.isRegularFile(Paths.get("/home/glassfish/payara6_micro/useVIP_2025R2"))) {
                        node = "vip2";
                    } else if (Files.isDirectory(Paths.get("/home/glassfish/payara6_micro"))) {
                        node = "vip";
                    } else if (Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
                        node = "localhost";
                    }

                    String shareLink = buildUrl(node, funcName, params);
                    sb.addMessage("Info", "Your workflow processing job (ID: " + jid + ") status has become <b>COMPLETED</b>.");
                    sb.addMessage("Info", "You can access the following link to resume your project: "
                            + "<a href=\"" + shareLink + "\">click here</a>.\n");
                    dv.setJobId(jid);
                    dv.setFinishLink(shareLink);
                    RDataUtils.updateRawJobStatusByFolder(sb.getRConnection(), folderName, "ALL_NOTIFIED");
                    PrimeFaces.current().executeScript("PF('rawWorkflowProgressDialog').hide()");
                    sb.setNoticeSize(sb.getNoticeSize() + 1);
                    PrimeFaces.current().ajax().update(":formBell");

                    for (Map.Entry<String, Boolean> entry : dv.getSelectionMap().entrySet()) {
                        if (entry.getValue() != null && entry.getValue()) {
                            if (!dv.getSuccessExecutionMap().getOrDefault(entry.getKey(), true)) {
                                dv.getExecutionMap().put(entry.getKey(), false);

                            } else {
                                dv.getExecutionMap().put(entry.getKey(), true);
                            }
                            System.out.println(entry.getKey() + "====trueExecution");
                        }
                    }
                    PrimeFaces.current().ajax().update(":finishDialogForm");
                    PrimeFaces.current().executeScript("PF('rawWorkflowFinishDialog').show()");

                    stopStatusCheck = true;
                }
            }

            RC.close();

        } catch (RserveException | REXPMismatchException ex) {
            Logger.getLogger(JobMonitor.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public boolean sendPostRequest(String node, String folderName, String jobId, String email, ApplicationBean1 ab) {
        String baseUri = "https://" + node + ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml";
        HttpClient client = HttpClient.newHttpClient();
        System.out.println("SENDPOSTREQUEST=============" + baseUri);

        if (node.equals("localhost")) {
            // enforce url for local testing
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

    public static String buildUrl(String node, String funcName, Map<String, String> params) {
        // Base URL
        String baseUrl = "https://" + node + ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml";

        // Append function name as a query parameter
        String queryString = "funcNm=" + URLEncoder.encode(funcName, StandardCharsets.UTF_8);

        // Append additional query parameters if provided
        if (params != null && !params.isEmpty()) {
            String paramString = params.entrySet().stream()
                    .map(entry -> URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8) + "="
                    + URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8))
                    .collect(Collectors.joining("&"));
            queryString += "&" + paramString;
        }

        return baseUrl + "?" + queryString;
    }

}
