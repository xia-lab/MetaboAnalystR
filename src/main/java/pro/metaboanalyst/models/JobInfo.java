/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.models;

/**
 *
 * @author zgy
 */
// JobInfo.java
import java.io.Serializable;


public class JobInfo implements Serializable {
    private String jobId;
    private String token;
    private String email;
    private String appName;
    private String node;
    private String type;
    private String folderName;
    private String baseUrl;

    public JobInfo() {}

    public JobInfo(String jobId, String token, String email,
                   String appName, String node, String type,
                   String folderName, String baseUrl) {
        this.jobId = jobId;
        this.token = token;
        this.email = email;
        this.appName = appName;
        this.node = node;
        this.type = type;
        this.folderName = folderName;
        this.baseUrl = baseUrl;
    }

    public String getJobId() { return jobId; }
    public String getToken() { return token; }
    public String getEmail() { return email; }
    public String getAppName() { return appName; }
    public String getNode() { return node; }
    public String getType() { return type; }
    public String getFolderName() { return folderName; }
    public String getBaseUrl() { return baseUrl; }
}
