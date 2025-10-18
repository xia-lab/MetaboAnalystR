/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
// WorkflowJobTimerService.java
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
import java.util.logging.Logger;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.models.JobInfo;

@Startup
@Singleton
public class WorkflowJobTimerService {

    private static final Logger LOG = Logger.getLogger(WorkflowJobTimerService.class.getName());

    @Resource
    private TimerService timerService;

    @Resource
    private ManagedExecutorService executor;

    @Inject
    private JobTimerService jobTimerService; // the small registry/status service we built

    /** Schedule the job to start immediately (or pass a delay if you prefer). */
    public void schedule(JobInfo info) {
        // mark IN_PROGRESS (like your Quartz insert)
        jobTimerService.updateJobStatus(info.getJobId(), JobTimerService.Status.IN_PROGRESS);
        jobTimerService.rememberToken(info.getJobId(), info.getToken());

        // persistent=true so it survives server restarts
        timerService.createSingleActionTimer(0, new TimerConfig(info, true));
    }

    /** Called by the container when the timer fires. */
    @Timeout
    public void onTimeout(Timer timer) {
        Object payload = timer.getInfo();
        if (!(payload instanceof JobInfo info)) {
            LOG.warning("Timer fired without JobInfo payload");
            return;
        }

        // Hand off to a managed thread (best practice; don’t block @Timeout thread)
        executor.submit(() -> executeWorkflow(info));
    }

    private void executeWorkflow(JobInfo info) {
        String jobId = info.getJobId();
        try {
            System.out.println("Executing WorkflowJob via TimerService: " + jobId);

            boolean ok = DataUtils.sendPostRequest(
                info.getNode(),
                info.getAppName(),
                info.getToken(),
                "executeWorkflowJob",
                info.getEmail(),
                info.getType(),
                info.getFolderName(),
                jobId,
                info.getBaseUrl()
            );

            if (ok) {
                 //System.out.println("WorkflowJob completed: " + jobId);
                //jobTimerService.updateJobStatus(jobId, JobTimerService.Status.COMPLETED);
            } else {
                 //System.out.println("WorkflowJob failed (sendPostRequest returned false): " + jobId);
                //jobTimerService.updateJobStatus(jobId, JobTimerService.Status.FAILED);
            }
        } catch (Exception e) {
             System.out.println("WorkflowJob exception for " + jobId + ": " + e.getMessage());
            jobTimerService.updateJobStatus(jobId, JobTimerService.Status.FAILED);
        }
    }
}
