/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.project;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.ListDataModel;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.RawJobsBean;

/**
 *
 * @author qiang
 */
@SessionScoped
@Named("JobManagerBean")
public class JobManager implements Serializable {

    @Inject
    UserLoginBean ulb;

    @Inject
    @JsonIgnore
    private SessionBean1 sb;

    private boolean managerPage = false;

    public boolean isManagerPage() {
        HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
        String url = request.getRequestURL().toString();
        managerPage = url.contains("SpectraUpload") || url.contains("ProjectView");
        return managerPage;
    }

    private String killPassword = "";

    public String getKillPassword() {
        return killPassword;
    }

    public void setKillPassword(String killPassword) {
        this.killPassword = killPassword;
    }

    private final String jobPwd = "Big*Data>Info";

    public String enterManager() {
        if (killPassword.equals(jobPwd)) {
            return "Job Manager";
        }
        return null;
    }

    private ListDataModel<RawJobsBean> listModel = null;

    public void JobStatusRead() throws ParseException {

        try {
            List<JobDetails> jobList = obtainJobsInfor();
            ListIterator<JobDetails> j = jobList.listIterator();

            ArrayList<Long> jobid = new ArrayList<>();
            ArrayList<Integer> jobUserNM = new ArrayList<>();
            ArrayList<String> jobstatus = new ArrayList<>();
            ArrayList<String> jobfolder = new ArrayList<>();
            ArrayList<String> jobDate = new ArrayList<>();
            ArrayList<Integer> jobSampleNum = new ArrayList<>();
            ArrayList<Integer> jobProgress = new ArrayList<>();
            ArrayList<String> jobNode = new ArrayList<>();

            SimpleDateFormat format = new SimpleDateFormat("YYYY-MM-dd HH:mm:ss");

            while (j.hasNext()) {
                JobDetails p = j.next(); // must be called before you can call i.remove()

                jobid.add(p.getId());
                jobUserNM.add(p.getUserNM());
                jobstatus.add(p.getStatus());
                jobfolder.add(p.getFolder());
                jobDate.add(format.format(p.getCreationDate()));
                jobSampleNum.add(p.getSampleNum());
                jobNode.add(p.getNodes());
                jobProgress.add(SchedulerUtils.getJobProgress(p.getFolder()));

            }

            List<RawJobsBean> JobInform = new ArrayList<>();

            RawJobsBean rawjob;
            for (int i = jobid.size() - 1; i > 0; i--) {

                System.out.println(i + " | jobProgress.get(i) ---->" + jobProgress.get(i));
                rawjob = new RawJobsBean(
                        jobid.get(i),
                        jobUserNM.get(i),
                        "mass",
                        jobDate.get(i),
                        jobSampleNum.get(i),
                        jobstatus.get(i),
                        jobProgress.get(i),
                        1,
                        "NA",
                        "NA",
                        jobfolder.get(i),
                        jobNode.get(i)
                );

                JobInform.add(rawjob);

            }

            listModel = new ListDataModel(JobInform);
        } catch (SQLException ex) {
            Logger.getLogger(JobManager.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    private MariaDBController mdbb;

    public List<JobDetails> obtainJobsInfor() throws SQLException {
        mdbb = ulb.getMdbb();

        List<JobDetails> jobTable = new ArrayList();
        // Retrieve the data from db
        String query = "select * from devUsers.jobs;";
        try {
            ResultSet res = mdbb.runQuery(query);
            int count = 0;
            while (res.next()) {
                JobDetails jobs = new JobDetails();
                //System.out.println(count + "checking projects res: " + res.getString(2) + res.getString(3) + res.getString(4) + res.getDate(5) + res.getString(6) + res.getString(7) + res.getString(8));

                jobs.setUserNM(res.getInt(1));
                jobs.setId(res.getLong(2));
                jobs.setCreationDate(res.getDate(3));
                jobs.setStatus(res.getString(5));
                jobs.setFolder(res.getString(6));
                jobs.setNodes(res.getString(7));

                jobTable.add(count, jobs);
                count++;
            }
        } catch (Exception ex) {
            sb.addMessage("error","Job server is down, report this code: Jobstp00021 to administrator !");
        } finally {
            mdbb.disconnect();
        }
        return jobTable;
    }

    public ListDataModel<RawJobsBean> getRawJobsBean() {
        return listModel;
    }

    public SelectItem[] getTypes() {
        SelectItem[] typesOptions = new SelectItem[1];
        typesOptions[0] = new SelectItem("mass", "mass");
        return typesOptions;
    }

    public SelectItem[] getNodes() {
        SelectItem[] nodesOptions = new SelectItem[2];
        nodesOptions[0] = new SelectItem("dev", "dev");
        nodesOptions[1] = new SelectItem("genap", "genap");
        return nodesOptions;
    }

    public SelectItem[] getStatuses() {
        SelectItem[] StatusOptions = new SelectItem[5];
        StatusOptions[0] = new SelectItem("submitted", "submitted");
        StatusOptions[1] = new SelectItem("Running", "Running");
        StatusOptions[2] = new SelectItem("Error", "Error");
        StatusOptions[3] = new SelectItem("Finished", "Finished");
        StatusOptions[4] = new SelectItem("Killed", "Killed");
        return StatusOptions;
    }

    public SelectItem[] getPrioritys() {
        SelectItem[] priorityOptions = new SelectItem[3];
        priorityOptions[0] = new SelectItem("low", "low");
        priorityOptions[1] = new SelectItem("medium", "medium");
        priorityOptions[2] = new SelectItem("high", "high");
        return priorityOptions;
    }

    public void changePriority(RawJobsBean job) {

        System.out.println(job.getPriority());
        System.out.println(job.getId());
        System.out.println(listModel.getRowData().getId());
        System.out.println(listModel.getRowData().getPriority());

        sb.addMessage("error", "This function is still under development!");
        //for(int i=0; i < listModel.getWrappedData; i++){
        //System.out.println("=============== changePriority RUNNING +++++++++++++++++++++++++++++++++++++++");
        //SchedulerUtils.updateJobPriority(job.getId(), job.getPriority());
        //}

    }

    public int killAllJobs() {
        sb.addMessage("error", "This function is still under development!");
        int res = 0;
        return res;
    }

}
