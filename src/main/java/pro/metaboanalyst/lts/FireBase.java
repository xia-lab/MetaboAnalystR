/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Named;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.ConcurrentHashMap;
import jakarta.annotation.PostConstruct;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.servlet.ServletContext;

/**
 * @author zgy
 */
@ApplicationScoped
@Named("fireBase")
public class FireBase implements Serializable {

    @Inject
    ApplicationBean1 ab;

    private String projectPath = "";

    private ConcurrentHashMap<String, FireUserBean> userMap = new ConcurrentHashMap<>();
    private String appLocation = "";
    private ConcurrentHashMap<String, FireUserBean> loginUserMap = new ConcurrentHashMap<>();

    public ConcurrentHashMap<String, FireUserBean> getLoginUserMap() {
        return loginUserMap;
    }

    @PostConstruct
    public void initResources() {
        // Your initialization logic here
        FacesContext facesContext = FacesContext.getCurrentInstance();
        ExternalContext externalContext = facesContext.getExternalContext();
        ServletContext servletContext = (ServletContext) externalContext.getContext();

        appLocation = servletContext.getInitParameter("APP_LOCATION");
        System.out.println("App Location: " + appLocation); // Debugging line

        projectPath = servletContext.getInitParameter("projectPath");
        if (Files.isDirectory(Paths.get("/mnt/disks/launchpad"))) {
            projectPath = "/mnt/disks/launchpad/"; //lab workstation
        } else if (Files.isDirectory(Paths.get("/Users/xialab/Dropbox/"))) {
            projectPath = "/Users/xialab/Dropbox/OmicSquareProject/"; //xia local1
        } else {
            projectPath = servletContext.getInitParameter("PROJECT_PATH");
        }
        //System.out.println("App Location: " + projectPath); // Debugging line
        if (ab.isOnQiangPc() || ab.isOnZgyPc() ||ab.isOnVipServer() || ab.isOnVipServer2()) {
            initJobMonitor();
        }
    }

        @JsonIgnore
    @Inject
    private JobMonitor jm;
    public void initJobMonitor() {
        System.out.println("Trying to init job monitor....");
        jm.init();
    }

    public String getRscriptsDBPath() {
        return ab.getRealPath() + "/rscripts/XiaLabPro/R/project_management.R";
    }

    public String getProjectDBPath() {
        return projectPath + "omicsquare_account.sqlite";
    }

    public String getProjectPath() {
        return projectPath;
    }

    public String getAppLocation() {
        return appLocation;
    }

    public ConcurrentHashMap<String, FireUserBean> getUserMap() {
        return userMap;
    }

    public void setUserMap(ConcurrentHashMap<String, FireUserBean> userMap) {
        this.userMap = userMap;
    }

}
