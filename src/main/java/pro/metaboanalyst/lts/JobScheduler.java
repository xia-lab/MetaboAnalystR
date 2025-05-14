/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.quartz.Job;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;
import static org.quartz.JobBuilder.*;
import org.quartz.JobDetail;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import static org.quartz.TriggerBuilder.*;
import static org.quartz.SimpleScheduleBuilder.*;
import org.quartz.Trigger;

/**
 *
 * @author qiang
 */
@ApplicationScoped
@Named("jobScheduler")
public class JobScheduler implements Serializable {

    private Scheduler scheduler;// = StdSchedulerFactory.getDefaultScheduler();

    public void initScheduler() {
        System.out.println("Starting initializing Quartz Scheduler...");
        try {
            if (scheduler == null) {
                scheduler = StdSchedulerFactory.getDefaultScheduler();
            }
        } catch (SchedulerException ex) {
            Logger.getLogger(JobScheduler.class.getName()).log(Level.SEVERE, null, ex);
        }
        try {
            if (!scheduler.isStarted()) {
                scheduler.start();
            }
        } catch (SchedulerException ex) {
            Logger.getLogger(JobScheduler.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void shutdownScheduler() {
        try {
            scheduler.shutdown();
        } catch (SchedulerException ex) {
            Logger.getLogger(JobScheduler.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public Scheduler getScheduler() {
        if (scheduler == null) {
            initScheduler();
        }
        return scheduler;
    }

    public void setScheduler(Scheduler scheduler) {
        this.scheduler = scheduler;
    }

    public void testQuartzScheduler() {
//        System.out.println("====== testing QuartzScheduler=== 1");

        try {
//            System.out.println("====== testing QuartzScheduler=== 2");
            // Grab the Scheduler instance from the Factory
            Scheduler scheduler = StdSchedulerFactory.getDefaultScheduler();

            // and start it off
            scheduler.start();
            Random rand = new Random();

            // Generate random integers in range 0 to 999
            int rand_int1 = rand.nextInt(1000);
            String jobnm = "job" + rand_int1;
            String triggernm = "trigger1" + rand_int1;

            JobDetail job = newJob(testJobx.class)
                    .withIdentity(jobnm, "group1")
                    .build();

            // Trigger the job to run now, and then repeat every 40 seconds
            Trigger trigger = newTrigger()
                    .withIdentity(triggernm, "group1")
                    .startNow()
                    .withSchedule(simpleSchedule()
                            .withIntervalInSeconds(5))
                    .build();

            // Tell quartz to schedule the job using our trigger
            scheduler.scheduleJob(job, trigger);

        } catch (SchedulerException se) {
            se.printStackTrace();
        }
    }

    public class testJobx implements Job {

        @Override
        public void execute(JobExecutionContext jec) throws JobExecutionException {
            try {
                System.out.println("====== testing HelloJobxxx=== 1");
                Thread.sleep(15000);
                System.out.println("====== testing HelloJobxxx=== 2");
            } catch (InterruptedException ex) {
                Logger.getLogger(testJobx.class.getName()).log(Level.SEVERE, null, ex);
            }
            //throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
        }

    }
}
