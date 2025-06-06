/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import org.quartz.Job;
import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.JobKey;
import org.quartz.SchedulerException;
import pro.metaboanalyst.utils.DataUtils;

/**
 *
 * @author qiang
 */
public class WorkflowJob implements Job {

    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        System.out.println("Executing a WorkflowJob .... ");

        JobDataMap dataMap = context.getJobDetail().getJobDataMap();
        String jobId = (String) dataMap.get("jobId");
        String token = (String) dataMap.get("token");
        String email = (String) dataMap.get("email");

        String appName = (String) dataMap.get("appName");
        String node = (String) dataMap.get("node");
        String type = (String) dataMap.get("type");
        String folderName = (String) dataMap.get("folderName");
        QuartzDbUtils.insertJobStatus(jobId, token, "IN_PROGRESS");

        boolean res = DataUtils.sendPostRequest(node, appName, token, "executeWorkflowJob", email, type, folderName, jobId);

        if (res) {
            System.out.println("Job Finished...");

            JobKey jobKey = context.getJobDetail().getKey();
            try {
                context.getScheduler().deleteJob(jobKey);
            } catch (SchedulerException e) {
                e.printStackTrace();
            }
            //if (client != null) {
            String message = String.format(
                    "Job Completed: { jobId: %s, token: %s, email: %s, appName: %s, node: %s, type: %s, folderName: %s }",
                    jobId, token, email, appName, node, type, folderName
            );
            //     client.sendMessage(message);
            //}
            QuartzDbUtils.updateJobStatus(jobId, "COMPLETED");
        } else {
            /*
            if (client != null) {

                client.sendMessage("Job failed.");
            }
             */
        }
    }

}
