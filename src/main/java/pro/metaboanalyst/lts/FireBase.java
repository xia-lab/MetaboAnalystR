/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import pro.metaboanalyst.controllers.general.ApplicationBean1;
import com.google.cloud.storage.Storage;
import com.google.cloud.storage.StorageOptions;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Named;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
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

    private FirebaseApp fireApp = null;
    private Storage storage;
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
        } else if (Files.isDirectory(Paths.get("/Users/jeffxia/Dropbox/"))) {
            projectPath = "/Users/jeffxia/Dropbox/OmicSquareProject/"; //xia local1
        } else {
            projectPath = servletContext.getInitParameter("PROJECT_PATH");
        }
        System.out.println("App Location: " + projectPath); // Debugging line
        if (ab.isOnQiangPc() || ab.isOnZgyPc() ||ab.isOnVipServer() || ab.isOnVipServer2()) {
            initJobMonitor();
        }
    }

    public void initJobMonitor() {
        System.out.println("Trying to init job monitor....");
        JobMonitor jobMoni = new JobMonitor();
        jobMoni.start();
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

    public void initFirebase() {
        /*
        GoogleCredentials credentials;

        try (FileInputStream serviceAccountStream = new FileInputStream(ab.getFirebaseInitFile())) {
            credentials = GoogleCredentials.fromStream(serviceAccountStream);

            FirebaseOptions options = FirebaseOptions.builder()
                    .setCredentials(credentials)
                    .setProjectId("omicsquare-dashboard-2282c")
                    .setStorageBucket("omicsquare-dashboard-2282c.appspot.com")
                    .build();
            fireApp = FirebaseApp.initializeApp(options);

            storage = StorageOptions.newBuilder().setCredentials(credentials)
                    .setProjectId("omicsquare-dashboard-2282c").build().getService();
        } catch (FileNotFoundException ex) {
            Logger.getLogger(FireBase.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(FireBase.class.getName()).log(Level.SEVERE, null, ex);
        }
        */
    }

    public FirebaseApp getFireApp() {
        if (fireApp == null) {
            initFirebase();
        }
        return fireApp;
    }

    public String getAppLocation() {
        return appLocation;
    }

    public void setFireApp(FirebaseApp fireApp) {
        this.fireApp = fireApp;
    }

    public Storage getStorage() {
        if (storage == null) {
            initFirebase();
        }
        return storage;
    }

    public void setStorage(Storage storage) {
        this.storage = storage;
    }

    public ConcurrentHashMap<String, FireUserBean> getUserMap() {
        return userMap;
    }

    public void setUserMap(ConcurrentHashMap<String, FireUserBean> userMap) {
        this.userMap = userMap;
    }

    public String getFirebaseInitFile() {
        return ab.getRealPath() + "/firebase/firebase-init.json";
    }

}
