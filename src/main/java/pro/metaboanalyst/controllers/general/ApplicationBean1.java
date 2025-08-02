/*
 * ApplicationBean1.java
 *
 * Created on Oct 21, 2008, 9:37:17 AM
 */
package pro.metaboanalyst.controllers.general;

import jakarta.annotation.PostConstruct;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.inject.Named;
import jakarta.faces.context.FacesContext;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.lts.DatabaseConnectionPool;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.utils.DataUtils;

@ApplicationScoped
@Named("applicationBean1")
public class ApplicationBean1 implements Serializable {

    private String app_url = "https://pro.metaboanalyst.ca";

    private final String apiResourcePath = "https://api2.xialab.ca/api/download/metaboanalyst/";

    public String getApiResourcePath() {
        return apiResourcePath;
    }

    private final String appName = "MetaboAnalyst";
    private boolean onProServer = true;
    private boolean onLocalServer = false;
    private boolean onVipServer = false;
    private boolean onVipServer2 = false;
    private boolean onQiangPc = false;
    private boolean onZgyPc = false;
    private boolean onYaoPc = false;
    private boolean inDocker = false;
    private boolean dockerAuthed = false;
    private String domainUrl;
    private String resourcePath;
    private String raw_spec_folder = "";
    private String sysCleaningCmd;
    private String baseExpDate = "2000-01-01 00:00:00";
    // 50M
    public static final int MAX_UPLOAD_SIZE = 50000000;
    public static int MAX_SPEC_SIZE = 214958080;
    public static int MAX_SPEC_NUM = 200;

    private boolean compiled = false;

    private String toolLocation = "pro";

    public String getToolLocation() {
        return toolLocation;
    }

    /*
    All relative paths below are below /resources
     */
    private static final String usr_home = "/users/";
    private String projectsHome = "/data/glassfish/projects/metaboanalyst/";


    public String getAppName() {
        return appName;
    }

    @PostConstruct
    public void initDirectories() {
        String domain_url = ((HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRe‌​quest()).getRequestURL().toString();
        //System.out.println("here 1: domain_url====" + domain_url + "==========");

        ServletContext context = (ServletContext) FacesContext.getCurrentInstance().getExternalContext().getContext();
        resourcePath = context.getRealPath("/resources");
        if (domain_url.contains("pro.metaboanalyst.ca")) {
            app_url = "https://pro.metaboanalyst.ca";
            onProServer = true;
            raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            toolLocation = "pro";
        } else if (domain_url.contains("eu.metaboanalyst.ca")) {
            app_url = "https://eu.metaboanalyst.ca";
            onProServer = true;
            raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            toolLocation = "eu";

        } else if (domain_url.contains("as.metaboanalyst.ca")) {
            app_url = "https://as.metaboanalyst.ca";
            onProServer = true;
            raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            toolLocation = "as";

        } else if (domain_url.contains("vip.metaboanalyst.ca")) {
            app_url = "https://vip.metaboanalyst.ca";
            onProServer = true;
            raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            toolLocation = "vip";
            onVipServer = true;

        } else if (domain_url.contains("vip2.metaboanalyst.ca")) {
            app_url = "https://vip2.metaboanalyst.ca";
            onProServer = true;
            raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            toolLocation = "vip2";
            onVipServer = true;
            onVipServer2 = true;

        } else if (Files.isRegularFile(Paths.get("/docker_marker"))) {
            System.out.println("domain_url=======> " + domain_url);
            app_url = domain_url.replace("/MetaboAnalyst/", "");
            System.out.println("Now the app url is xxx ===> " + app_url);
            inDocker = true;
            onProServer = false;
            raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            toolLocation = "docker";

        } else { //all others will be local

            //if (DataUtils.isLocalNetwork(domain_url)) {
            app_url = DataUtils.getDomainURL(domain_url);
            onProServer = false;
            onLocalServer = true;
            if (Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
                onQiangPc = true;
                projectsHome = "/home/qiang/Downloads/results/"; //qiang's lab workstation
                raw_spec_folder = "/data/glassfish/projects/metaboanalyst/";
            } else if (Files.isDirectory(Paths.get("/Users/xia/Dropbox/projects/metaboanalyst/"))) {
                projectsHome = "/Users/xia/Dropbox/projects/metaboanalyst/"; //xia local
            } else if (Files.isDirectory(Paths.get("/home/zgy/"))) {
                onZgyPc = true;
            } else if (Files.isDirectory(Paths.get("/Users/lzy"))) {
                onYaoPc = true;
            } else {
                projectsHome = "/resources/projects/metaboanalyst/"; //tmp folder for testing
            }
            if (resourcePath.contains("C:")) {
                resourcePath = resourcePath.replace("\\", "/");
            }
            toolLocation = "localhost";
            //} else {
            //    System.out.println("==========UNKNOWN LOCATION ======= " + domain_url);
            //}
        }

        //if (toolLocation.equals("localhost") && !(onQiangPc)) {
        if (toolLocation.equals("localhost") && !(onQiangPc || onZgyPc)) {

        } else {
            if (inDocker) {
                File psql_conf_file = new File("/home/psql_tool.conf");
                if (psql_conf_file.exists()) {
                    if (validate_psql_conf()) {
                        DatabaseConnectionPool.setupDataSource(9);
                    } else {
                        DatabaseConnectionPool.setupDataSource(0);
                    }
                } else {
                    DatabaseConnectionPool.setupDataSource(0);
                }
            }
        }

        if (onQiangPc || onZgyPc) {
            raw_spec_folder = resourcePath + usr_home;
        }

        //System.out.println("here 2: app_url====" + app_url);
        domainUrl = app_url + "/MetaboAnalyst";
    }

    private static boolean validate_psql_conf() {

        try (BufferedReader br = new BufferedReader(new FileReader("/home/psql_tool.conf"))) {
            String line;
            br.readLine();  // Skip the header line
            boolean bool1, bool2, bool3, bool4, bool5;
            bool1 = false;
            bool2 = false;
            bool3 = false;
            bool4 = false;
            bool5 = false;

            while ((line = br.readLine()) != null) {
                System.out.println("Now this validate_psql_conf line is ==> " + line);
                String[] values = line.split(":");
                if (values[0].equals("PSQL_DB_Name")) {
                    bool1 = true;
                }
                if (values[0].equals("PSQL_DB_Password")) {
                    bool2 = true;
                }
                if (values[0].equals("PSQL_DB_ADDR")) {
                    bool3 = true;
                }
                if (values[0].equals("PSQL_DB_Port")) {
                    bool4 = true;
                }
                if (values[0].equals("PSQL_USER_Name")) {
                    bool5 = true;
                }
            }
            return (bool1 & bool2 & bool3 & bool4 & bool5);
        } catch (IOException e) {
            e.printStackTrace();
        }

        return false;
    }

    public int getFILE_SIZE_BYTE() {
        return 200 * 1048576;
    }

    //The compiling will be created upon application start up
    //@PostConstruct
    //The compiling will be created upon first user query
    public boolean isCompiled() {
        return compiled;
    }

    //need internal check for intialization due to concurrent access
    public synchronized boolean compileRScripts(String analType) {
        if (!compiled) {
            if (analType.equals("raw") && shouldUseScheduler()) {
                compiled = RCenter.compileRScripts(raw_spec_folder, resourcePath + "/rscripts/_script_loader.R");
            } else {
                compiled = RCenter.compileRScripts(resourcePath + usr_home, resourcePath + "/rscripts/_script_loader.R");
            }
        }
        return compiled;
    }

    private boolean cleaningOn = false;
    private ExecutorService streamHandlers;
    private long lastCleaningTime = System.currentTimeMillis();

    //running job every 10 min triggered by user
    public synchronized void performResourceCleaning(RConnection RC) {

        if (sysCleaningCmd == null) {
            sysCleaningCmd = RCenter.getBashFullPath(RC) + " " + resourcePath + "/rscripts/_clean_jobs.sh";
        }

        if (streamHandlers == null) {
            streamHandlers = Executors.newFixedThreadPool(2);
        }

        try {
            // Ensure scripts are executable
            File cleanJobsScript = new File(resourcePath + "/rscripts/_clean_jobs.sh");
            if (!cleanJobsScript.canExecute()) {
                boolean madeExecutable = cleanJobsScript.setExecutable(true);
                System.out.println("_clean_jobs.sh executable: " + madeExecutable);
            }

            File cleanFoldersScript = new File(resourcePath + "/rscripts/_clean_folders.sh");
            if (!cleanFoldersScript.canExecute()) {
                boolean madeExecutable = cleanFoldersScript.setExecutable(true);
                System.out.println("_clean_folders.sh executable: " + madeExecutable);
            }

            cleaningOn = true;

            if (DataUtils.runExternalCommand(sysCleaningCmd, streamHandlers)) {
                lastCleaningTime = System.currentTimeMillis();
                //cleaningOn = false;
            }

            String userPath = resourcePath + "/users";
            String sysCleaningCmd2 = RCenter.getBashFullPath(RC) + " " + resourcePath + "/rscripts/_clean_folders.sh " + userPath;
            System.out.println("=== sysCleaningCmd2 ==> " + sysCleaningCmd2);
            DataUtils.runExternalCommand(sysCleaningCmd2, streamHandlers);

        } catch (Exception e) {
            System.out.println("Exception in resource cleaning -  ");
            // LOGGER.error("performResourceCleaning", e);
        } finally {
            cleaningOn = false;
        }
    }

    public long getLastCleaningTime() {
        return lastCleaningTime;
    }

    public boolean isCleaningOn() {
        return cleaningOn;
    }

    public void setCleaningOn(boolean cleaningOn) {
        this.cleaningOn = cleaningOn;
    }

    public String getRaw_spec_folder() {
        return raw_spec_folder;
    }

    public int getMAX_UPLOAD_SIZE() {
        if (inDocker) {
            return 20 * MAX_UPLOAD_SIZE;
        }
        return MAX_UPLOAD_SIZE;
    }

    public int getMAX_SPEC_SIZE() {
        if (inDocker) {
            return 2145958080;
        }
        return MAX_SPEC_SIZE;
    }

    public int getMAX_SPEC_NUM() {
        if (inDocker) {
            return 1000000;
        }
        return MAX_SPEC_NUM;
    }

    //vmOpt, jobs burdn sharing by dispactching among www, new and genap
    // 0, use www only => compute canada (new and genap) down
    // 1, use www and new only => maintain genap node
    // 2, use www and genap only => maintain new node)
    // 3, use www, new and genap concurrently (basic mode)
    private int vmOpt = 0;

    public int getVmOpt() {
        return vmOpt;
    }

    public void setVmOpt(int vmOpt) {
        this.vmOpt = vmOpt;
    }

    //dispactch between cloud and dev
    //odd to cloud, even to dev
    private int jobCount = 0;
    private String devOpt = "cloud";

    public String getDevOpt() {
        return devOpt;
    }

    public void setDevOpt(String devOpt) {
        this.devOpt = devOpt;
    }

    private String myID;

    public String getMyID() {
        return myID;
    }

    public void setMyID(String myID) {
        this.myID = myID;
    }

    public void updateRedirection() {
        if (myID.equals("server@burn_side")) {
            //add your funciton here
        }
    }

    public String getAppUrlPath() {
        return app_url;
    }

    public String getRootContext() {
        return "/MetaboAnalyst";
    }

    public String getDomainURL() {
        return domainUrl;
    }

    public String getModuleURL() {
        jobCount++;
        return (domainUrl);
    }

    public boolean isOnProServer() {
        return onProServer;
    }

    public boolean isOnZgyPc() {
        return onZgyPc;
    }

    public boolean isOnQiangPc() {
        return onQiangPc;
    }

    public boolean isOnYaoPc() {
        return onYaoPc;
    }

    public boolean isOnVipServer2() {
        return onVipServer2;
    }

    public boolean shouldUseScheduler() {
        return onProServer || onZgyPc || onQiangPc || inDocker;
    }

    public boolean isOnLocalServer() {
        return onLocalServer;
    }

    public boolean isInDocker() {
        return inDocker;
    }

    public boolean isDockerAuthed() {
        if (!inDocker) {
            return true;
        } else {
            return dockerAuthed;
        }
    }

    public void setDockerAuthed(boolean dockerAuthed) {
        this.dockerAuthed = dockerAuthed;
    }

    public boolean isOnVipServer() {
        return onVipServer;
    }

    public void setOnVipServer(boolean onVipServer) {
        this.onVipServer = onVipServer;
    }

    public String getRealUserHomePath() {
        //return realUserHomePath;
        return resourcePath + usr_home;
    }

    public String getRscriptsHomePath() {
        return resourcePath + "/rscripts";
    }

    public String getRscriptsLoaderPath() {
        return resourcePath + "/rscripts/_script_loader.R";
    }

    public String getAutoCache() {
        if (onProServer || onQiangPc || inDocker || onZgyPc) {
            return "/home/glassfish/projects/spectra_example_cache/auto";
        } else {
            System.out.println("WARNING: If there is no cache file found in your local, please COPY it from dev server and add your path HERE!!!!");
        }
        return "/home/glassfish/projects/spectra_example_cache/auto";
    }

    public String getCustomizedCache() {
        if (onProServer || onQiangPc || inDocker || onZgyPc) {
            return "/home/glassfish/projects/spectra_example_cache/customized";
        } else {
            System.out.println("WARNING: If there is no cache file found in your local, please COPY it from dev server and add your path HERE!!!!");
        }
        return "/home/glassfish/projects/spectra_example_cache/customized";
    }

    public String getProjectsHome() {
        return projectsHome;
    }

    private final String OptiLCMSversion = "1.1.0";

    public String getOptiLCMSversion() {
        return OptiLCMSversion;
    }

    public String getRscriptLoaderPath() {
        return resourcePath + "/rscripts/_script_loader.R";
    }

    public String getBgImgPath() {
        return resourcePath + "/images/background.png";
    }

    public String getParams0() {
        return resourcePath + "/data/params0.rda";
    }


    /*
    * Handling resources
     */
    public String getResourceByAPI(String fileName) {
        return apiResourcePath + fileName;
    }

    public String getInternalData(String fileName) {
        return resourcePath + "/data/" + fileName;
    }

    public String getRealPath() {
        return resourcePath;
    }

    public String getApp_url() {
        System.out.println("NOw the app url is ===> " + app_url);
        return app_url;
    }

    public String getBaseExpDate() {
        return baseExpDate;
    }

    public void setBaseExpDate(String baseExpDate) {
        this.baseExpDate = baseExpDate;
    }

    public boolean validateExpiry() {
        LocalDateTime expDate = LocalDateTime.parse(baseExpDate, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        LocalDateTime currentDateTime = LocalDateTime.now();

        if (!expDate.isBefore(currentDateTime)) {
            long daysleft = currentDateTime.until(expDate, ChronoUnit.DAYS);
            if (daysleft < 30) {
                FacesContext.getCurrentInstance().addMessage("warn", new FacesMessage(FacesMessage.SEVERITY_WARN, "Warning", "Your license will be expired in " + daysleft + " days! Please contact to renew!"));
            }
            return true;
        }
        return false;
    }

    public String getBaseUrlDyn() {
        HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance()
                .getExternalContext().getRequest();

        String scheme = request.getScheme();             // http or https
        String serverName = request.getServerName();     // localhost or domain
        int serverPort = request.getServerPort();        // 8080
        String contextPath = request.getContextPath();   // /ExpressAnalyst
        System.out.println("getBaseUrlDyn===" + scheme + "://" + serverName + ":" + serverPort + contextPath);
        return scheme + "://" + serverName + ":" + serverPort + contextPath;
    }

}
