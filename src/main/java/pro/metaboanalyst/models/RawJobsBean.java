/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author qiang
 */
public class RawJobsBean implements Serializable {
            
    private long id;
    private int userNM;
    private String time;
    private String type;
    private int num;
    private String status;
    private int progress;
    private int queue;
    private String priority;
    private String action;
    private String jobFolder;    
    private String node;

    public String getNode() {
        return node;
    }

    public void setNode(String node) {
        this.node = node;
    }
    
    public long getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getUserNM() {
        return userNM;
    }

    public void setUserNM(int userNM) {
        this.userNM = userNM;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getTime() {
        return time;
    }

    public void setTime(String time) {
        this.time = time;
    }

    public int getNum() {
        return num;
    }

    public void setNum(int num) {
        this.num = num;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public int getProgress() {
        return progress;
    }

    public void setProgress(int progress) {
        this.progress = progress;
    }

    public int getQueue() {
        return queue;
    }

    public void setQueue(int queue) {
        this.queue = queue;
    }
    
    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }
    
    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getJobFolder() {
        return jobFolder;
    }

    public void setJobFolder(String jobFolder) {
        this.jobFolder = jobFolder;
    }
    
    
    public RawJobsBean(
            long id, 
            int userNM,
            String type, 
            String time, 
            int num, 
            String status, 
            int progress, 
            int queue, 
            String priority, 
            String action, 
            String jobFolder,
            String node){
        this.id = id;
        this.userNM = userNM;
        this.type = type;
        this.time = time;
        this.num = num;
        this.status = status;
        this.progress = progress;
        this.queue = queue;
        this.priority = priority;
        this.action = action;
        this.jobFolder = jobFolder;
        this.node = node;
    }
    
    
}
