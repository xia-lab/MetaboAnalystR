package pro.metaboanalyst.workflows;

import jakarta.annotation.Resource;
import jakarta.ejb.Singleton;
import jakarta.ejb.Startup;
import jakarta.ejb.Timeout;
import jakarta.ejb.Timer;
import jakarta.ejb.TimerConfig;
import jakarta.ejb.TimerService;
import jakarta.enterprise.concurrent.ManagedExecutorService;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.nio.file.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.enterprise.context.control.RequestContextController;
import jakarta.enterprise.inject.Instance;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.models.JobInfo;
import pro.metaboanalyst.utils.DataUtils;

@Startup
@Singleton
@Named("jobTimerService")
public class JobTimerService {

    private static final Logger LOG = Logger.getLogger(JobTimerService.class.getName());

    public enum Status { IN_PROGRESS, COMPLETED, FAILED, ERROR }

    @Resource private TimerService timerService;
    @Resource private ManagedExecutorService executor;

    private final Map<String, Status> statuses = new ConcurrentHashMap<>();
    private final Map<String, String>  tokens = new ConcurrentHashMap<>();
    private final Queue<JobInfo> pendingJobs = new ConcurrentLinkedQueue<>();
    private final AtomicBoolean workerRunning = new AtomicBoolean(false);

    @Inject private ApplicationBean1 ab;
    @Inject private FireBase fb;
    @Inject private RequestContextController requestContextController;
    @Inject private Instance<DatabaseClient> databaseClientProvider;
    /* ============ Public API ============ */

    /** Schedule a persistent single-shot job (fires immediately). */
    public void schedule(JobInfo info) {
        final String jobId = info.getJobId();
        //System.out.println("[JobTimerService] schedule() jobId=" + jobId
        //        + " node=" + info.getNode()
        //        + " type=" + info.getType()
         //       + " folder=" + info.getFolderName());

        updateJobStatus(jobId, Status.IN_PROGRESS);
        syncWorkflowRunStatus(info, "running");

        if (info.getToken() != null) {
            rememberToken(jobId, info.getToken());
        } else {
            System.out.println("[JobTimerService] schedule() no token supplied for jobId=" + jobId);
        }

        timerService.createSingleActionTimer(0, new TimerConfig(info, false));
        //System.out.println("[JobTimerService] schedule() timer created (persistent) for jobId=" + jobId);
    }

    /** Legacy-style insert + schedule helper. */
    public void insertJob(String jobId, String token, String email,
                          String appName, String node, String type,
                          String folderName, String baseUrl) {
        //System.out.println("[JobTimerService] insertJob() jobId=" + jobId + " node=" + node + " type=" + type);
        schedule(new JobInfo(jobId, token, email, appName, node, type, folderName, baseUrl));
    }

    public void rememberToken(String jobId, String token) {
        //System.out.println("[JobTimerService] rememberToken() jobId=" + jobId + " token.len=" + (token == null ? 0 : token.length()));
        tokens.put(jobId, token);
        writeMarker(jobId, statuses.get(jobId), token);
    }

    public String getTokenByJobId(String jobId) {
        String t = tokens.get(jobId);
        if (t != null) {
           // System.out.println("[JobTimerService] getTokenByJobId() (mem) jobId=" + jobId);
            return t;
        }
        Marker m = readMarker(jobId);
        //System.out.println("[JobTimerService] getTokenByJobId() (file) jobId=" + jobId + " hit=" + (m != null && m.token != null));
        return m != null ? m.token : null;
    }

    public void updateJobStatus(String jobId, Status status) {
        //System.out.println("[JobTimerService] updateJobStatus() jobId=" + jobId + " -> " + status);
        statuses.put(jobId, status);
        writeMarker(jobId, status, tokens.get(jobId));
    }

    public void updateJobStatus(String jobId, String status) {
        try {
            updateJobStatus(jobId, Status.valueOf(status));
        } catch (IllegalArgumentException e) {
            System.out.println("[JobTimerService] updateJobStatus(string) invalid status=" + status + " jobId=" + jobId + " -> ERROR");
            updateJobStatus(jobId, Status.ERROR);
        }
    }

    public Status getStatus(String jobId) {
         //   System.out.println("[JobTimerService] getStatus() (mem) jobId=" + jobId);
        Status s = statuses.get(jobId);
        if (s != null) {
            // noisy but useful when debugging polling
         //   System.out.println("[JobTimerService] getStatus() (mem) jobId=" + jobId + " = " + s);
            return s;
        }
        Marker m = readMarker(jobId);
        if (m != null) {
            if (m.status != null) statuses.put(jobId, m.status);
            if (m.token  != null) tokens.put(jobId,  m.token);
         //   System.out.println("[JobTimerService] getStatus() (file) jobId=" + jobId + " = " + m.status);
            return m.status;
        }
        //System.out.println("[JobTimerService] getStatus() not found jobId=" + jobId);
        return null;
    }

    /* ============ Timer callback ============ */

    @Timeout
    public void onTimeout(Timer timer) {
        Object payload = timer.getInfo();
        if (!(payload instanceof JobInfo info)) {
            LOG.warning("Timer fired without JobInfo payload");
         //   System.out.println("[JobTimerService] onTimeout() missing JobInfo payload");
            return;
        }
        final String jobId = info.getJobId();
       // System.out.println("[JobTimerService] onTimeout() fired for jobId=" + jobId);

        enqueue(info);
    }

    /* ============ Marker files ============ */

    private record Marker(Status status, String token) {}

    private Path markerPath(String jobId) {
        try {
            String base = fb.getProjectPath() + "/appdata/";  // writable base directory
            Path dir = Paths.get(base, "job-markers");
            Files.createDirectories(dir);
            Path resolved = dir.resolve(jobId + ".marker");
          //  System.out.println("[JobTimerService] markerPath() jobId=" + jobId + " path=" + resolved);
            return resolved;
        } catch (Exception e) {
            Path fallback = Paths.get(System.getProperty("java.io.tmpdir"), "job_" + jobId + ".marker");
            System.out.println("[JobTimerService] markerPath() FALLBACK tmp path=" + fallback);
            return fallback;
        }
    }

    private void writeMarker(String jobId, Status s, String token) {
        try {
            Path p = markerPath(jobId);
            Files.createDirectories(p.getParent());
            String content = (s == null ? "" : s.name()) + "\n" + (token == null ? "" : token) + "\n";
            Files.writeString(p, content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
          //  System.out.println("[JobTimerService] writeMarker() jobId=" + jobId + " status=" + s + " token.len=" + (token == null ? 0 : token.length()));
        } catch (Exception e) {
            System.out.println("[JobTimerService] writeMarker() ERROR jobId=" + jobId + " msg=" + e.getMessage());
        }
    }

    private Marker readMarker(String jobId) {
        try {
            Path p = markerPath(jobId);
            if (!Files.exists(p)) {
            //    System.out.println("[JobTimerService] readMarker() NOT FOUND jobId=" + jobId);
                return null;
            }
            var lines = Files.readAllLines(p);
            Status s = (lines.size() > 0 && !lines.get(0).isBlank())
                    ? Status.valueOf(lines.get(0).trim()) : null;
            String t = (lines.size() > 1) ? lines.get(1).trim() : null;
          //  System.out.println("[JobTimerService] readMarker() jobId=" + jobId + " status=" + s + " token.len=" + (t == null ? 0 : t.length()));
            return new Marker(s, t);
        } catch (Exception e) {
            System.out.println("[JobTimerService] readMarker() ERROR jobId=" + jobId + " msg=" + e.getMessage());
            return null;
        }
    }

    private void enqueue(JobInfo info) {
        pendingJobs.add(info);
        if (workerRunning.compareAndSet(false, true)) {
            executor.submit(this::drainQueue);
        }
    }

    private void drainQueue() {
        try {
            JobInfo next;
            while ((next = pendingJobs.poll()) != null) {
                runJob(next);
            }
        } finally {
            workerRunning.set(false);
            if (!pendingJobs.isEmpty() && workerRunning.compareAndSet(false, true)) {
                executor.submit(this::drainQueue);
            }
        }
    }

    private void runJob(JobInfo info) {
        String jobId = info.getJobId();
        try {
            syncWorkflowRunStatus(info, "running");
            boolean ok = DataUtils.sendPostRequest(
                    info.getNode(),
                    info.getAppName(),
                    info.getToken(),
                    "executeWorkflowJob",
                    info.getEmail(),
                    info.getType(),
                    info.getFolderName(),
                    info.getJobId(),
                    info.getBaseUrl()
            );
            syncWorkflowRunStatus(info, ok ? "completed" : "failed");
            updateJobStatus(jobId, ok ? Status.COMPLETED : Status.FAILED);
        } catch (Exception e) {
            System.out.println("[JobTimerService] execute() EXCEPTION jobId=" + jobId + " msg=" + e.getMessage());
            syncWorkflowRunStatus(info, "failed");
            updateJobStatus(jobId, Status.FAILED);
        }
    }

    private void syncWorkflowRunStatus(JobInfo info, String status) {
        if (info == null || status == null) {
            return;
        }
        Integer runId = info.getWorkflowRunId();
        if (runId == null) {
            return;
        }
        boolean activated = false;
        try {
            if (databaseClientProvider == null || databaseClientProvider.isUnsatisfied()) {
                LOG.log(Level.WARNING,
                        "[JobTimerService] DatabaseClient bean not available; cannot update workflow_run id={0}",
                        runId);
                return;
            }

            activated = requestContextController != null && requestContextController.activate();
            DatabaseClient client = databaseClientProvider.get();
            if ("completed".equalsIgnoreCase(status)) {
                Map<String, Object> existing = client.getWorkflowRunById(String.valueOf(runId));
                Object currentStatus = existing != null ? existing.get("status") : null;
                if (currentStatus != null && "failed".equalsIgnoreCase(currentStatus.toString())) {
                    LOG.log(Level.INFO,
                            "[JobTimerService] Skipping status update to completed for workflow_run id={0} because it is already failed.",
                            runId);
                    return;
                }
            }
            Map<String, Object> payload = new HashMap<>();
            payload.put("status", status);
            String res = client.updateWorkflowRunFields(String.valueOf(runId), payload);
            if (res == null || res.isBlank() || !res.toLowerCase().contains("ok") && !res.toLowerCase().contains("\"updated\":true")) {
                LOG.log(Level.WARNING, "[JobTimerService] workflow_run update returned ''{0}'' for id={1}, status={2}",
                        new Object[]{res, runId, status});
            }
        } catch (Exception e) {
            LOG.log(Level.WARNING,
                    "[JobTimerService] workflow_run update failed for id=" + runId + ", status=" + status,
                    e);
        } finally {
            if (activated && requestContextController != null) {
                requestContextController.deactivate();
            }
        }
    }
}
