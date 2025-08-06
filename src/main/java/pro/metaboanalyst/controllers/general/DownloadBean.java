/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.gson.Gson;
import jakarta.faces.context.FacesContext;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import jakarta.inject.Named;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.models.ResultBean;
import pro.metaboanalyst.models.User;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.model.StreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.rosuda.REngine.REXPString;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.enrich.MsetBean;
import pro.metaboanalyst.controllers.meta.MetaLoadBean;
import pro.metaboanalyst.controllers.meta.MetaResBean;
import pro.metaboanalyst.controllers.meta.MetaStatBean;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.mgwas.MgwasBean;
import pro.metaboanalyst.controllers.multifac.LimmaBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.lts.FireProjectBean;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.models.GalleryImage;
import pro.metaboanalyst.project.ProjectModel;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.utils.FigureCaptionUtils;
import pro.metaboanalyst.workflows.RunSummary;
import pro.metaboanalyst.workflows.WorkflowBean;
import pro.metaboanalyst.workflows.WorkflowParameters;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("downloader")
public class DownloadBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private FireBaseController fbc;

    @JsonIgnore
    @Inject
    private MsetBean mstb;

    @JsonIgnore
    @Inject
    private FigureCaptionUtils fcu;

    @JsonIgnore
    @Inject
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private FireBase fbb;

    @JsonIgnore
    @Inject
    private DatabaseClient dbc;

    @JsonIgnore
    @Inject
    private FireProjectBean fpb;

    @JsonIgnore
    @Inject
    private SpectraProcessBean spb;

    @JsonIgnore
    @Inject
    private MummiAnalBean mab;

    @JsonIgnore
    @Inject
    private DetailsBean dtb;

    @JsonIgnore
    @Inject
    private DoseResponseBean drb;

    @JsonIgnore
    @Inject
    private MetaPathStatBean mpb;

    @JsonIgnore
    @Inject
    private MetaResBean mrb;

    @JsonIgnore
    @Inject
    private MetaLoadBean mlb;

    @JsonIgnore
    @Inject
    private MgwasBean mgwb;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;
    @JsonIgnore
    @Inject
    private LimmaBean lmb;

    @JsonIgnore
    @Inject
    private MetaLoadBean ldb;
    @JsonIgnore
    @Inject
    private MetaStatBean msb;

    private ResultBean[] downloads;

    public ResultBean[] getDownloads() {
        return downloads;
    }
    private String bestNormOpt = "";
    private static final Logger LOGGER = LogManager.getLogger(DownloadBean.class);
    private boolean tableInit = false;
    private List<String> galleryStatImages;
    private List<String> galleryIntImages;
    private String galleryStat;
    private String imageStat = "";
    private boolean hideImages = true;
    private final String imgPathUrl = "/resources/users/";
    private int activeIndex = 0;

    public List<RunSummary> getRunSummaries() {
        return runSummaries;
    }

    public void setRunSummaries(List<RunSummary> runSummaries) {
        this.runSummaries = runSummaries;
    }

    public int getActiveIndex() {
        return activeIndex;
    }

    public void setActiveIndex(int activeIndex) {
        this.activeIndex = activeIndex;
    }

    public List<String> getGalleryStatImages() {
        return galleryStatImages;
    }

    public void setGalleryStatImages(List<String> galleryStatImages) {
        this.galleryStatImages = galleryStatImages;
    }

    public List<String> getGalleryIntImages() {
        return galleryIntImages;
    }

    public void setGalleryIntImages(List<String> galleryIntImages) {
        this.galleryIntImages = galleryIntImages;
    }

    public void setupDownloadTableWrapper() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }

        sb.buildCustomNaviTree();

        if (!tableInit) {

            if (wb.getCalledWorkflows().contains("ORA")) {
                if (mstb.getOraBeans() == null) {
                    mstb.populateOraBean();
                }
            }
            if (wb.getCalledWorkflows().contains("QEA")) {
                if (mstb.getQeaBeans() == null) {
                    mstb.populateQeaBean();
                }
            }

            if (wb.getResultPageDisplay().equals("default")) {
                setupDownloadTable("");
                setupGalleryStat("");
                System.out.println("getResultPageDisplay === default");
            } else {
                RCenter.setWd(sb.getRConnection(), sb.getCurrentUser().getOrigHomeDir() + "/");

                populateRunSummaries();
                if (!wb.getCurrentSubFolder().equals("NA")) {
                    loadResults(wb.getCurrentSubFolder());
                } else {
                    String[] folderNames = wb.getWorkflowOptions().stream()
                            .map(WorkflowParameters::getFolderName)
                            .toArray(String[]::new);
                    loadResults(folderNames[0]);
                }
            }
        }
    }

    public void setupGalleryStat(String subFolder) {

        galleryImages = new ArrayList<>();

        // Copy files from subFolder to main folder
        if (!subFolder.isEmpty()) {
            String mainFolderPath = sb.getCurrentUser().getHomeDir();
            String subFolderPath = mainFolderPath + File.separator + subFolder;
            File subFolderDir = new File(subFolderPath);
            if (subFolderDir.exists() && subFolderDir.isDirectory()) {
                File[] subFolderFiles = subFolderDir.listFiles();
                if (subFolderFiles != null) {
                    for (File file : subFolderFiles) {
                        try {
                            File targetFile = new File(mainFolderPath, file.getName());
                            Files.copy(file.toPath(), targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                        } catch (IOException e) {
                            System.out.println("Error copying file: " + file.getName() + " - " + e.getMessage());
                        }
                    }
                }
            }
        }

        if (galleryStatImages == null || galleryStatImages.isEmpty()) {
            return;
        }

        // Map to track highest-numbered version of each base name
        Map<String, String> highestNumberedImages = new HashMap<>();

        // Map to track highest-numbered version of each base name
        for (String picture : galleryStatImages) {
            if (picture == null || "null".equals(picture)) {
                continue;
            }

            Matcher mBase = BASE_PATTERN.matcher(picture);
            String baseName = mBase.find() ? mBase.group(1) : picture;

            int number = 0;
            Matcher mIdx = INDEX_PATTERN.matcher(picture);
            if (mIdx.find()) {
                number = Integer.parseInt(mIdx.group(1));
            }

            System.out.printf("[SCAN] %-35s → base=%-20s idx=%d%n",
                    picture, baseName, number);

            if (!highestNumberedImages.containsKey(baseName)) {

                System.out.printf("[KEEP] first time for %-20s → %s%n",
                        baseName, picture);
                highestNumberedImages.put(baseName, picture);

            } else {

                String prevPic = highestNumberedImages.get(baseName);
                int prevIdx = getImageNumber(prevPic);

                System.out.printf("[COMP] %-20s prev=%-30s(idx=%d)  vs  new=%s(idx=%d)%n",
                        baseName, prevPic, prevIdx, picture, number);

                if (prevIdx < number) {
                    System.out.printf("[REPL] replacing %-20s with %s%n",
                            baseName, picture);
                    highestNumberedImages.put(baseName, picture);
                } else {
                    System.out.printf("[SKIP] keeping existing for %-20s%n", baseName);
                }
            }
        }

        List<String> sortedImages = new ArrayList<>(highestNumberedImages.values());
        sortedImages.sort(Comparator.comparingInt(f -> getAnalysisOrderIndex(f)));

        galleryImages.clear();

        for (String picture : sortedImages) {
            String path;

            String directory = sb.getCurrentUser().getRelativeDir() + File.separator;
            if (sb.getAnalType().equals("raw") || sb.getAnalType().equals("spec")) {
                String guestName = sb.getCurrentUser().getName();
                String myDir = ab.getRealUserHomePath() + guestName;
                directory = "/resources/users/" + guestName;
            }
            path = directory + File.separator + picture;

            String key = getMatchingKey(picture, sb.getGraphicsMapLink());
            String interactiveUrl = (key != null) ? sb.getGraphicsMapLink().get(key) : "#";

            if (interactiveUrl.equals("#")) {
                key = picture.replaceAll("(_\\d+_dpi150)?\\.png$", "");

                boolean res = fbc.handleNaviCases(key.replaceAll("_demo", ""), "");
                if (res) {
                    interactiveUrl = sb.getCurrentNaviUrl();
                } else {
                    interactiveUrl = "/Secure/xialabpro/DashboardView.xhtml?growlWarning=Unable%20to%20find%20corresponding%20page";
                }
            }

            System.out.println("= setupGalleryStat === key ---> " + key);
            if (picture.contains("_demo")) {
                galleryImages.add(new GalleryImage(fcu.obtainLegend(key) + " (visit to update)", path, "/MetaboAnalyst" + interactiveUrl));
            } else {
                galleryImages.add(new GalleryImage(fcu.obtainLegend(key), path, "/MetaboAnalyst" + interactiveUrl));
            }
        }
    }

    // Helper method to extract the image number
    private static int getImageNumber(String filename) {
        Matcher m = INDEX_PATTERN.matcher(filename);
        return m.find() ? Integer.parseInt(m.group(1)) : -1;   // -1 → “no numeric suffix”
    }

    String getMatchingKey(String picture, Map<String, String> graphicsMapLink) {
        for (String key : graphicsMapLink.keySet()) {
            if (picture.startsWith(key)) { // Check if picture starts with the key
                return key;
            }
        }
        return null; // Return null if no match is found
    }

    // single, pre‑compiled patterns → faster and clearer
    private static final Pattern BASE_PATTERN //   «qc_multi_pca»
            = Pattern.compile("^(.+?)_\\d+(?:_dpi\\d+)?\\.png$", Pattern.CASE_INSENSITIVE);

    private static final Pattern INDEX_PATTERN //   «1» in qc_multi_pca_1_dpi150.png
            = Pattern.compile("_(\\d+)(?:_dpi\\d+)?\\.png$", Pattern.CASE_INSENSITIVE);

    public void setupDownloadTable(String subFolder) {
        if (sb.getRConnection() == null || sb.getCurrentUser() == null) {
            return;
        }

        galleryStatImages = new ArrayList<>();
        galleryIntImages = new ArrayList<>();
        hideImages = true;

        User usr = sb.getCurrentUser();
        String usrName = usr.getName();
        RConnection RC = sb.getRConnection();
        RDataUtils.saveRCommands(RC);

        if (!"raw".equals(sb.getAnalType())) {
            RDataUtils.saveAllData(RC);
        }

        String mainFolderPath = usr.getHomeDir();
        String subFolderPath = mainFolderPath + (subFolder.isEmpty() ? "" : File.separator + subFolder);
        File subFolderDir = new File(subFolderPath);

        // Copy files from subFolder to the main folder
        if (!subFolder.isEmpty() && subFolderDir.exists() && subFolderDir.isDirectory()) {
            File[] subFolderFiles = subFolderDir.listFiles();
            if (subFolderFiles != null) {
                for (File file : subFolderFiles) {
                    try {
                        File targetFile = new File(mainFolderPath, file.getName());
                        Files.copy(file.toPath(), targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);

                    } catch (IOException e) {
                        System.out.println("Error copying file: " + file.getName() + " - " + e.getMessage());
                    }
                }
            }
        }

        // Process files in the main folder
        File folder = new File(mainFolderPath);
        DataUtils.deleteFile(usr, "Download.zip");
        File[] listOfFiles = folder.listFiles(new OnlyExt(true));

        if (listOfFiles == null || listOfFiles.length == 0) {
            downloads = new ResultBean[1];
            downloads[0] = new ResultBean("Empty Folder", "", null, null);
        } else {
            DataUtils.createZipFile(listOfFiles, "Download.zip", mainFolderPath);

            ArrayList<String> fileNames = new ArrayList<>();
            fileNames.add("Download.zip");

            listOfFiles = folder.listFiles(new OnlyExt(false));
            if (listOfFiles != null) {
                for (File listOfFile : listOfFiles) {
                    String filename = listOfFile.getName().toLowerCase();
                    if (filename.endsWith(".json")) {
                        continue; // Skip .json files
                    }
                    fileNames.add(listOfFile.getName());
                }
            }

            reportURL = null;

            if (fileNames.contains("Rhistory.R")) {
                fileNames.remove("Rhistory.R");
                fileNames.add(1, "Rhistory.R");
            }

            ArrayList<String> notImages = new ArrayList<>();
            for (String fileName : fileNames) {
                if ((fileName.contains(".png") || fileName.contains(".svg")) && !(fileName.startsWith("zoom") && fileName.endsWith(".png"))) {
                    if (fileName.contains("_gallery_")) {
                        galleryIntImages.add(fileName);
                    } else {
                        galleryStatImages.add(fileName);
                    }
                } else {
                    notImages.add(fileName);
                }
            }

            fileNames = notImages;

            int fileSize = fileNames.size();
            boolean added = false;
            if (fileSize % 2 > 0) {
                fileSize += 1;
                added = true;
            }

            int rowNum = fileSize / 2;
            downloads = new ResultBean[rowNum];
            String fileNMA, fileNMB, fileNMALink, fileNMBLink;

            if (ab.shouldUseScheduler() && sb.getAnalType().equals("raw")) {
                for (int i = 0; i < rowNum; i++) {
                    fileNMA = fileNames.get(i);
                    fileNMALink = mainFolderPath + File.separator + fileNMA;

                    if (i == rowNum - 1 && added) {
                        fileNMB = "";
                        fileNMBLink = null;
                    } else {
                        fileNMB = fileNames.get(rowNum + i);
                        fileNMBLink = mainFolderPath + File.separator + fileNMB;
                    }
                    downloads[i] = new ResultBean(fileNMA, fileNMB, fileNMALink, fileNMBLink);
                }
            } else {
                for (int i = 0; i < rowNum; i++) {
                    fileNMA = "<a target='_blank' href='/MetaboAnalyst/resources/users/" + usrName + "/" + fileNames.get(i) + "'>" + fileNames.get(i) + "</a>";
                    if (i == rowNum - 1 && added) {
                        fileNMB = "";
                    } else {
                        fileNMB = "<a target='_blank' href='/MetaboAnalyst/resources/users/" + usrName + "/" + fileNames.get(rowNum + i) + "'>" + fileNames.get(rowNum + i) + "</a>";
                    }
                    downloads[i] = new ResultBean(fileNMA, fileNMB, "", "");
                }
            }
        }

        tableInit = true;
    }

    public String getGalleryStat() {
        //setupGalleryStat();
        return galleryStat;
    }

    public String setupGalleryStatOld() {

        for (int i = 0; i < galleryStatImages.size(); i++) {
            if ("null".equals(galleryStatImages.get(i))) {
                galleryStatImages.remove(galleryStatImages.get(i));
            }
        }

        for (String picture : galleryStatImages) {

            String caption = picture;
            String temp;
            String path;
            if (sb.getAnalType().equals("rawSeq") || sb.getAnalType().equals("Salmon") || sb.getAnalType().equals("Seq2FunR")) {
                path = "/imageServlet?imageName=" + picture;
            } else {
                path = imgPathUrl + sb.getCurrentUser().getName() + File.separator + picture;
            }
            temp = "<div class=\"gallery\"> \n"
                    + "<a target=\"_blank\" href=\"/MetaboAnalyst" + path + "\"> \n"
                    + "<img src=\"/MetaboAnalyst"
                    + "" + path + "\" width=\"1300\"/> \n"
                    + "</a> \n"
                    + "<div class=\"desc\">" + caption + "</div>"
                    + "</div> \n";

            imageStat = imageStat + temp;
        }

        return imageStat;
    }

    public void setGalleryStat(String galleryStat) {
        this.galleryStat = galleryStat;
    }

    private String reportURL = null;

    public String getReportURL() {
        return reportURL;
    }

    public void setReportURL(String reportURL) {
        this.reportURL = reportURL;
    }

    private String mdlOpt = "stat";

    public String getMdlOpt() {
        return mdlOpt;
    }

    public void setMdlOpt(String mdlOpt) {
        this.mdlOpt = mdlOpt;
    }

    public void loadExistingReport() {
        if (sb.getCurrentUser() == null) {
            sb.addMessage("Error", "Please start analysis before generating report!");
            return;
        }

        Path path = Paths.get(sb.getCurrentUser().getHomeDir() + "/Analysis_Report.html");
        boolean reportAlreadyExists = Files.exists(path);
        if (reportAlreadyExists) {
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Existing report loaded, click on 'Update' to include the latest changes!");
        } else {
            generateReport("html");
        }
    }

    public void editReport() {
        sb.addMessage("Info", "This feature will be available in the next release!");
    }

    //try to redo it using bash, assuming Analysis_Report.tex exists
    public void generateReport(String format) {

        if (!sb.isLoggedIn() || sb.getCurrentUser() == null) {
            sb.addMessage("Error", "You need to first start your analysis.");
            return;
        }

        RConnection RC = sb.getRConnection();

        // report generation is not available for utilies modules
        //System.out.println("generateReport for this module ===> " + sb.getAnalType());
        RDataUtils.setAnalType(RC, sb.getAnalType());
        if (sb.getAnalType().equals("utils")) {
            sb.addMessage("Warn", "Report generation is not available for modules in utilies!");
            return;
        }

        //set live links
        Gson gson = new Gson();
        String jsonStr = gson.toJson(sb.getReportImgMap());
        //generate share link

        try {
            int res = dbc.checkMatchingFolderNameProject(sb.getCurrentUser().getName());
            if (res == -1) {
                fbc.saveProject("project");
            }
        } catch (Exception ex) {
            java.util.logging.Logger.getLogger(DownloadBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        //String rpath = ab.getRscriptsLoaderPath();
        RCenter.loadReporterFuns(sb, sb.getAnalType());

        RCenter.setReportImgMap(sb.getRConnection(), jsonStr);

        ProjectModel currentProject = fpb.getSelectedProject();
        boolean projectSaved = false;
        if (currentProject != null) {
            if (currentProject.getFolderName().equals(sb.getCurrentUser().getName())) {
                projectSaved = true;
            }
        }

        if (fpb.isBatchModeEnabled()) {
            String newJH = RCenter.updateJSFHistory(sb.getRConnection(), fbb.getProjectDBPath(), fpb.getTemplateToken());
            if (!newJH.equals("")) {
                System.out.println("Found a str to update the jsf history *_*");
            } else {
                System.out.println("Cannot find a str to update the jsf history");
            }
            String[] graphicsMap_str = RCenter.extractGraphicsMap(sb.getRConnection(), fbb.getProjectDBPath(), fpb.getTemplateToken());
            HashMap<String, String> graphicsMap = sb.getGraphicsMap();
            String[] kc; // key + cmd
            for (String s : graphicsMap_str) {
                kc = s.split("\t\t\t");
                if (!graphicsMap.containsKey(kc[0])) {
                    sb.addGraphicsCMD(kc[0], kc[1]);
                }
            }
            // resume image map
            String[] ImgMap_str = RCenter.extractImgMap(sb.getRConnection(), fbb.getProjectDBPath(), fpb.getTemplateToken());
            String[] kcx; // key + cmd
            HashMap<String, Integer> imgMap = sb.getImgMap();
            for (String s : ImgMap_str) {
                kcx = s.split("\t\t\t");
                if (!imgMap.containsKey(kcx[0])) {
                    imgMap.put(kcx[0], Integer.valueOf(kcx[1]));
                }
            }
            sb.setImgMap(imgMap);
        }

        fbc.setShareableLink(ab.getApp_url() + "/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=ShareLink&tokenId=" + DataUtils.generateToken(sb.getCurrentUser().getName()));
        String sharableLink = fbc.getShareableLink();
        RCenter.SetSharingLink(RC, sharableLink);
        String filePath;
        if (format.equals("slides")) {
            filePath = sb.getCurrentUser().getHomeDir() + "/Analysis_Presentation.pptx";
        } else {
            filePath = sb.getCurrentUser().getHomeDir() + "/Analysis_Report." + format;
        }
        Path path = Paths.get(filePath);
        boolean reportAlreadyExists = Files.exists(path);
        RCenter.setReportFormat(sb.getRConnection(), format);

        // boolean res = RCenter.prepareReport(RC, usr.getName(), fb.getShareableLink());
        boolean res = RCenter.prepareReport(RC, fub.getEmail(), fbc.getShareableLink());
        if (sb.getDataType().equals("spec")) {
            if (format.equals("slides")) {
                spb.internalizeImage("Analysis_Presentation.pptx");
            } else {
                spb.internalizeImage("Analysis_Report." + format);
            }
        }

        if (res) {
            if (!format.equals("html")) {

            } else {

                if (reportAlreadyExists) {
                    DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Report has been successfully updated!");
                } else {
                    if (projectSaved) {
                        DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Report has been successfully created!");
                    } else {
                        DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Report and project have been successfully created!");

                    }
                }
            }
        } else {
            reportURL = "";
            sb.addMessage("Error", "Unknown error happened during report generation.");
        }
    }

    public boolean generateReportByModule(String module, String format) {
        if (!module.equals("NA")) {
            wb.setReportModule(module);
            sb.setAnalType(module);
            if (format.equals("html")) {
                generateReportByModuleOld(module);
            } else {
                generateReport(format);
            }

        } else {
            wb.setReportModule(sb.getAnalType());
            generateReport(format);

        }
        return true;
    }

    public boolean generateReportByModuleOld(String module) {

        if (!sb.isLoggedIn() || sb.getCurrentUser() == null) {
            sb.addMessage("Error", "You need to first start your analysis.");
            return false;
        }

        RConnection RC = sb.getRConnection();

        // report generation is not available for utilies modules
        System.out.println("generateReport for this module ===> " + sb.getAnalType());
        RDataUtils.setAnalType(RC, sb.getAnalType());

        if (sb.getAnalType().equals("utils")) {
            sb.addMessage("Warn", "Report generation is not available for modules in utilies!");
            return false;
        }

        //set live links
        Gson gson = new Gson();
        String jsonStr = gson.toJson(sb.getReportImgMap());
        //generate share link

        try {
            int res = dbc.checkMatchingFolderNameProject(sb.getCurrentUser().getName());
            if (res == -1) {
                fbc.saveProject("project");
            }
        } catch (Exception ex) {
            java.util.logging.Logger.getLogger(DownloadBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        //String rpath = ab.getRscriptsLoaderPath();
        RCenter.loadReporterFuns(sb, module);

        RCenter.setReportImgMap(sb.getRConnection(), jsonStr);

        ProjectModel currentProject = fpb.getSelectedProject();
        boolean projectSaved = false;
        if (currentProject != null) {
            if (currentProject.getFolderName().equals(sb.getCurrentUser().getName())) {
                projectSaved = true;
            }
        }

        if (fpb.isBatchModeEnabled()) {
            String newJH = RCenter.updateJSFHistory(sb.getRConnection(), fbb.getProjectDBPath(), fpb.getTemplateToken());
            if (!newJH.equals("")) {
                System.out.println("Found a str to update the jsf history *_*");
            } else {
                System.out.println("Cannot find a str to update the jsf history");
            }
            String[] graphicsMap_str = RCenter.extractGraphicsMap(sb.getRConnection(), fbb.getProjectDBPath(), fpb.getTemplateToken());
            HashMap<String, String> graphicsMap = sb.getGraphicsMap();
            String[] kc; // key + cmd
            for (String s : graphicsMap_str) {
                kc = s.split("\t\t\t");
                if (!graphicsMap.containsKey(kc[0])) {
                    sb.addGraphicsCMD(kc[0], kc[1]);
                }
            }
            // resume image map
            String[] ImgMap_str = RCenter.extractImgMap(sb.getRConnection(), fbb.getProjectDBPath(), fpb.getTemplateToken());
            String[] kcx; // key + cmd
            HashMap<String, Integer> imgMap = sb.getImgMap();
            for (String s : ImgMap_str) {
                kcx = s.split("\t\t\t");
                if (!imgMap.containsKey(kcx[0])) {
                    imgMap.put(kcx[0], Integer.valueOf(kcx[1]));
                }
            }
            sb.setImgMap(imgMap);
        }

        fbc.setShareableLink(ab.getApp_url() + "/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=ShareLink&tokenId=" + DataUtils.generateToken(sb.getCurrentUser().getName()));
        String sharableLink = fbc.getShareableLink();
        //System.out.println("sharableLink ===> " + sharableLink);
        RCenter.SetSharingLink(RC, sharableLink);

        Path path = Paths.get(sb.getCurrentUser().getHomeDir() + "/Analysis_Report_" + module + ".html");
        boolean reportAlreadyExists = Files.exists(path);
        RCenter.setReportFormat(sb.getRConnection(), "html");

        // boolean res = RCenter.prepareReport(RC, usr.getName(), fb.getShareableLink());
        boolean res = RCenter.prepareReportByModule(RC, fub.getEmail(), fbc.getShareableLink(), module);
        wb.setReportModule(module);
        sb.setAnalType(module);
        RDataUtils.setAnalType(RC, module);
        if (sb.getDataType().equals("spec")) {
            spb.internalizeImage("Analysis_Report_" + module + ".html");
        }

        if (res) {

            if (reportAlreadyExists) {
                DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Report successfully updated!");
            } else {
                if (projectSaved) {
                    DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Report successfully created!");
                } else {
                    DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Report and project have been successfully created!");

                }
            }
            return true;

        } else {
            reportURL = "";
            sb.addMessage("Error", "Unknown error happened during report generation.");
            return false;
        }

    }

    public StreamedContent getAnalysisReport(String format) throws IOException {
        if (!sb.isLoggedIn() || sb.getCurrentUser() == null) {
            sb.addMessage("Error", "You need to first start your analysis.");
            return null;
        }

        String path = sb.getCurrentUser().getHomeDir() + "/Analysis_Report." + format;
        if (format.equals("pptx")) {
            path = sb.getCurrentUser().getHomeDir() + "/Analysis_Presentation." + format;
        }

        if (!wb.getReportModule().equals("")) {
            path = sb.getCurrentUser().getHomeDir() + "/Analysis_Report_" + wb.getReportModule() + "." + format;
            if (format.equals("pptx")) {
                path = sb.getCurrentUser().getHomeDir() + "/Analysis_Presentation_" + wb.getReportModule() + "." + format;
            }
        }

        File myReport = new File(path);
        if (myReport.exists()) {
            return DataUtils.getDownloadFile(path);
        } else {
            String path2 = sb.getCurrentUser().getHomeDir() + "/Analysis_Report." + format;
            if (format.equals("pptx")) {
                path2 = sb.getCurrentUser().getHomeDir() + "/Analysis_Presentation." + format;
            }

            File myReport2 = new File(path2);
            System.out.println(path2 + "=====path2");
            if (myReport2.exists()) {
                return DataUtils.getDownloadFile(path2);
            } else {
                sb.addMessage("Error", "No report was generated.");
                return null;
            }
        }
    }

    public class OnlyExt implements FilenameFilter {

        private boolean showAll = false;

        public OnlyExt(boolean isall) {
            showAll = isall;
        }

        @Override
        public boolean accept(File dir, String name) {
            if (showAll) {
                return name.endsWith(".csv")
                        || name.endsWith(".png")
                        || (name.endsWith(".R") & !name.endsWith("ExecuteRawSpec.R"))
                        || name.endsWith(".json")
                        || name.endsWith(".pdf") 
                        || name.endsWith("msn_result.zip");
            } else {
                return name.endsWith(".csv")
                        || name.endsWith(".png")
                        || (name.endsWith(".R") & !name.endsWith("ExecuteRawSpec.R"))
                        || name.endsWith(".json")
                        || name.endsWith("Report.pdf")
                        || name.endsWith("msn_result.zip");
            }
        }
    }

    private final List<String> untargetedDatas = Arrays.asList("spec", "specbin", "pktable", "nmrpeak", "mspeak");
    private final List<String> regresAnals = Arrays.asList("pathway", "enrich");
    private final List<String> compatibleDatas = Arrays.asList("conc", "spec", "specbin", "pktable", "nmrpeak", "mspeak", "mass_table");
    private final List<String> compatibleAnals = Arrays.asList("stat", "roc", "raw", "power", "mummichog", "pathqea", "msetqea", "pathway", "enrich");
    private final List<String> compatibleListAnals = Arrays.asList("pathora", "msetora", "pathway", "enrich");
    private final List<String> targetedAnals = Arrays.asList("pathora", "pathqea", "msetora", "msetqea", "pathway", "enrich");

    public String switchModule() {
        //need to send to sanity check page directly 
        //prepare the new model
        sb.setSwitchMode(false);
        String dataType = sb.getDataType();
        String analType = sb.getAnalType();

        //some sanity check
        //no switching to itself
        if (analType.equals(mdlOpt)) {
            sb.addMessage("Error", "You are switching to the same module.");
            return null;
        }

        //switching to targeted analysis
        if (targetedAnals.contains(mdlOpt)) {
            //cannot from untargeted data
            if (untargetedDatas.contains(dataType)) {
                sb.addMessage("Error", "This module requires annotated metabolomics data");
                return null;
                //others need to do name mapping first
            } else if (sb.getCmpdIDType().equals("na")) {
                //sb.addMessage("Error", "Please specify compound ID type to enter this module.");
                PrimeFaces.current().executeScript("PF('idDialog').show();");
                return null;
            }
        }
        //only allow spec to mummichog
        boolean allowUsemumi = false;
        if (mdlOpt.equals("mummichog") & !dataType.equals("spec")) {
            if (!RDataUtils.checkDataGenericFormat(sb.getRConnection())) {
                sb.addMessage("Error", "Due to its requirements on RT and m/z infomation, you can only switch to Functional Analysis from Raw Spectral Processing module.");
                return null;
            } else {
                //now allow other module if data format is generic (e.g. feature is mz__rt)
                allowUsemumi = true;
            }
        }

        //dealing with regression
        if (sb.isRegresion() & !regresAnals.contains(mdlOpt)) {
            sb.addMessage("Error", "This module does not support continuous class labels.");
            return null;
        }

        //between two input compound lists. Note the module for pathway and enrich cannot distingusih ora or qea. It will
        //depends on the current analtype
        String newAnalOpt = mdlOpt;
        String newDataType = dataType;
        String treeOpt = mdlOpt;
        String naviCode;

        //list input
        if (dataType.equals("conc") && compatibleListAnals.contains(analType) && compatibleListAnals.contains(mdlOpt)) {

            if (analType.equals("msetora")) {
                newAnalOpt = "pathora";
                treeOpt = "pathway";
                naviCode = "pathparam";
            } else {
                newAnalOpt = "msetora";
                treeOpt = "enrich";
                naviCode = "enrichparam";
            }
            prepData(newDataType, newAnalOpt, treeOpt);
            return naviCode;

            //table input
        } else if (compatibleDatas.contains(dataType) && compatibleAnals.contains(analType) && compatibleAnals.contains(mdlOpt)) {

            //from raw spec, need to update something
            if (dataType.equals("spec")) {

                if (ab.isOnProServer() || ab.isOnQiangPc() || ab.isInDocker()) {
                    //update the user dir to be under web application
                    User user = sb.getCurrentUser();
                    String guestName = user.getName();
                    String myDir = ab.getRealUserHomePath() + guestName;

                    File guestFolder = new File(myDir);
                    if (!guestFolder.exists()) {
                        guestFolder.mkdir();
                    }

                    user.setRelativeDir("/resources/users/" + guestName);
                    user.setHomeDir(myDir);

                    //copy the "metaboanalyst_input.csv" to the new user folder?    
                    String fileName = "metaboanalyst_input.csv";
                    String inPath = "/data/glassfish/projects/metaboanalyst/" + guestName + "/" + fileName;
                    String currentPath = sb.getCurrentUser().getHomeDir() + "/" + fileName;
                    DataUtils.copyFile(new File(inPath), new File(currentPath));

                    fileName = "params.rda"; //required for mummichog
                    inPath = "/data/glassfish/projects/metaboanalyst/" + guestName + "/" + fileName;
                    currentPath = sb.getCurrentUser().getHomeDir() + "/" + fileName;
                    DataUtils.copyFile(new File(inPath), new File(currentPath));

                    //set R working dirctory to new guestfolder
                    try {
                        sb.getRConnection().voidEval("setwd(\"" + sb.getCurrentUser().getHomeDir() + "\")");
                    } catch (RserveException ex) {
                        LOGGER.error("switchModule", ex);
                    }
                }

                //setup the mSet object
                RDataUtils.prepareSpec4Switch(sb.getRConnection());

                //update new data type
                if (newAnalOpt.equals("mummichog")) {
                    //update the data type
                    newDataType = "mass_table";
                    treeOpt = "mummichgo-table";
                    //mummi specif updates

                    mab.setDisabledV2(true);
                    mab.setModuleSwitch(true);
                } else {
                    newDataType = "pktable";
                }
            }

            if (allowUsemumi && newAnalOpt.equals("mummichog")) {
                //update the data type
                newDataType = "mass_table";
                //mummi specif updates
                mab.setDisabledV2(true);
                mab.setModuleSwitch(true);
            }

            if (mdlOpt.equals("pathway")) {
                newAnalOpt = "pathqea";
            } else if (mdlOpt.equals("enrich")) {
                newAnalOpt = "msetqea";
            }

            sb.setSwitchMode(true);
            prepData(newDataType, newAnalOpt, treeOpt);
            return "Data check";

        } else {
            sb.addMessage("Error", "The two modules are not compatible due to different input requirements.");
            return null;
        }
    }

    private void prepData(String newDataType, String newAnalType, String treeType) {
        sb.setDataPreprocessed();
        sb.setDataType(newDataType);
        sb.setAnalType(newAnalType);
        sb.setPaired(false);
        sb.setRegression(false);
        sb.getNaviTrack().clear();
        sb.initNaviTree(treeType);

        RConnection newRC = RCenter.updateRConnection(sb.getRConnection(), RCenter.getCleanRConnection(), ab.getRscriptLoaderPath(), newAnalType, sb.getCurrentUser().getHomeDir());
        RCenter.recordRserveConnection(newRC, sb.getCurrentUser().getHomeDir());
        sb.setRConnetion(newRC);

        RDataUtils.updateDataObjects(newRC, newDataType, newAnalType, false);
    }

    public String getReportPath() {
        if (sb.getCurrentUser() == null) {
            sb.addMessage("Error", "Please log in and perform analysis first.");
            return null;
        }
        File newFile = new File(sb.getCurrentUser().getHomeDir() + "/Analysis_Report.html");
        //System.out.println("/" + ab.getAppName() + sb.getCurrentUser().getRelativeDir() + "/Analysis_Report.html" + "====" + newFile.exists());

        if (newFile.exists()) {

            if (sb.getAnalType().equals("raw")) {
                String guestName = sb.getCurrentUser().getName();
                return "/" + ab.getAppName() + "/resources/users/" + guestName + "/Analysis_Report.html";
            }

            return "/" + ab.getAppName() + sb.getCurrentUser().getRelativeDir() + "/Analysis_Report.html";
        } else {
            if (!wb.getReportModule().equals("")) {
                return "/" + ab.getAppName() + sb.getCurrentUser().getRelativeDir() + "/Analysis_Report_" + wb.getReportModule() + ".html";
            }
        }

        sb.addMessage("Error", "No report found in your home directory.");
        return null;
    }

    private List<GalleryImage> galleryImages;

    public List<GalleryImage> getGalleryImages() {
        if (galleryImages == null) {
            setupGalleryStat("");
        }
        return galleryImages;
    }

    private List<RunSummary> runSummaries = new ArrayList<>();

    public void populateRunSummaries() {
        // Step 1: Fetch the summary from R
        String selectedContrast = wb.getSelectedGrp1() + " vs. " + wb.getSelectedGrp2();
        String[] folderNames = wb.getWorkflowOptions().stream()
                .map(WorkflowParameters::getFolderName)
                .toArray(String[]::new);

        Map<String, Object> summaryNormResults = RDataUtils.getSummaryNormResults(sb.getRConnection(), selectedContrast, folderNames);
        if (summaryNormResults == null) {
            System.err.println("No summaryNormResults returned from R.");
            runSummaries = Collections.emptyList();
            return;
        }

        // Step 2: Retrieve bestNormOpt
        if (summaryNormResults.containsKey("bestNormOpt")) {
            Object bestNormOptObj = summaryNormResults.get("bestNormOpt");
            if (bestNormOptObj instanceof REXPString rEXPString) {
                try {
                    bestNormOpt = rEXPString.asString(); // Convert REXPString to Java String
                } catch (Exception e) {
                    System.err.println("Error converting bestNormOpt to String: " + e.getMessage());
                }
            } else if (bestNormOptObj instanceof String string) {
                bestNormOpt = string;
            } else {
                System.err.println("Unexpected type for bestNormOpt: " + bestNormOptObj.getClass().getName());
            }
            System.out.println("Best Normalization Option: " + bestNormOpt);
        }

        // Step 3: Build RunSummary objects
        List<RunSummary> tempList = new ArrayList<>();

        for (Map.Entry<String, Object> entry : summaryNormResults.entrySet()) {
            String normOpt = entry.getKey(); // e.g., "Log2"

            // Skip the "bestNormOpt" and "scores" keys
            if ("bestNormOpt".equals(normOpt) || "scores".equals(normOpt)) {
                continue;
            }

            // "details" is the sub-map with { numDE => ..., pcaPermInfo => ..., ... }
            @SuppressWarnings("unchecked")
            Map<String, Object> details = (Map<String, Object>) entry.getValue();

            // A) numDE
            int numDE = -1;
            if (details.get("numDE") instanceof Number number) {
                numDE = number.intValue();
            }

            // B) normality description
            String normalityDesc = null;
            if (details.get("normalityInfo") instanceof String string) {
                normalityDesc = string;
            } else {
                normalityDesc = "No Mardia test results";
            }

            // C) PCA group separation info
            String pcaPermInfo = null;
            if (details.get("pcaPermInfo") instanceof String string) {
                pcaPermInfo = string;
            } else {
                pcaPermInfo = "No PCA PERMANOVA results";
            }

            int numDEpval = -1;
            if (details.get("numDEpval") instanceof Number number) {
                numDEpval = number.intValue();
            }

            int numDEfc = -1;
            if (details.get("numDEfc") instanceof Number number) {
                numDEfc = number.intValue();
            }

            // D) Param description (using normOpt)
            String paramDescription = normOpt;

            // E) Image path for PCA
            String imageName = sb.getCurrentUser().getOrigRelativeDir() + File.separator + normOpt + File.separator + sb.getCurrentImage("pca_score2d") + "dpi150.png";

            // Create a new RunSummary
            RunSummary summary = new RunSummary(
                    normOpt, // runId
                    normalityDesc, // normalityDescription
                    numDE, // # of DE features
                    pcaPermInfo, // PCA info
                    paramDescription,
                    numDEpval,
                    numDEfc
            );
            summary.setPcaImage(imageName);

            tempList.add(summary);
        }

        // Step 4: Assign to the bean field
        runSummaries = tempList;
    }

    public String getBestNormOpt() {
        return bestNormOpt;
    }

    public void setBestNormOpt(String bestNormOpt) {
        this.bestNormOpt = bestNormOpt;
    }

    private String selectedImage;

    public String getSelectedImage() {
        return selectedImage;
    }

    public void setSelectedImage(String selectedImage) {
        this.selectedImage = selectedImage;
    }
    // Method to set the selected image

    public void selectImage(String image) {
        this.selectedImage = image;
        System.out.println("Selected image: " + image); // Debug log
    }

    private String currentSubFolder = "";

    public String getCurrentSubFolder() {
        return currentSubFolder;
    }

    public void setCurrentSubFolder(String currentSubFolder) {
        this.currentSubFolder = currentSubFolder;
    }

    public void loadResults(String subFolder) {

        //Change path
        sb.getCurrentUser().setHomeDir(sb.getCurrentUser().getOrigHomeDir() + "/" + subFolder);
        sb.getCurrentUser().setRelativeDir(sb.getCurrentUser().getOrigRelativeDir() + "/" + subFolder);

        currentSubFolder = subFolder;
        wb.setCurrentSubFolder(subFolder);
        setupDownloadTable("");
        setupGalleryStat("");

        RCenter.setWd(sb.getRConnection(), sb.getCurrentUser().getHomeDir() + "/");
        RCenter.setResourceDir(sb.getRConnection(), ab.getRealPath());

        String javaHistory = fbc.readJsonStringFromFile(sb.getCurrentUser().getHomeDir() + File.separator + "java_history.json");
        int res1 = fbc.loadJavaHistory(javaHistory);
        for (String module : wb.getModuleNames()) {
            RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), module);
        }
        RCenter.loadHistory(sb.getRConnection());

        wb.setResultPageDisplay("multi");
        populateResTable();
        sb.addMessage("info", "Detail view has been updated to '" + subFolder + "'!");
    }

    public boolean containsModule(String module) {
        boolean res;
        if (sb.getAnalType().equals(module)) {
            res = true;
        } else {
            res = wb.getModuleNames().contains(module);
        }

        return res;
    }
    private WorkflowParameters selectedWorkflowParams;

    public WorkflowParameters getSelectedWorkflowParams() {
        return selectedWorkflowParams;
    }

    public void setSelectedWorkflowParams(WorkflowParameters selectedWorkflowParams) {
        this.selectedWorkflowParams = selectedWorkflowParams;
    }

    public void loadDetails(RunSummary selectedSummary) {
        System.out.println(selectedSummary.getPcaImage() + "======isFilterUnmapped");
        WorkflowParameters opt = wb.getWorkflowParameterByFolderName(selectedSummary.getRunId());
        selectedWorkflowParams = opt;
    }

    private int getAnalysisOrderIndex(String figure) {

        for (int i = 0; i < fcu.getANALYSIS_ORDER().size(); i++) {
            if (figure.startsWith(fcu.getANALYSIS_ORDER().get(i))) {
                return i;
            }
        }
        return Integer.MAX_VALUE; // Place unknown figures at the end
    }

    public void populateResTable() {

        if (containsModule("stat")) {
            //if (naviContainsUrl("/Secure/analysis/VolcanoView.xhtml")) {
            dtb.setInit(false);
            dtb.setupDetailsTable(true, "volcano");
            //}else if (naviContainsUrl("/Secure/analysis/PLSDAView.xhtml")) {
            dtb.setupDetailsTable(true, "pls.vip");
            //}else if (naviContainsUrl("/Secure/analysis/PatternView.xhtml")) {
            dtb.setupDetailsTable(true, "template");
            //}else if (naviContainsUrl("/Secure/analysis/RFView.xhtml")) {
            dtb.setupDetailsTable(true, "rf");
            //}
            dtb.setupDetailsTable(true, "opls.vip");
            dtb.setupDetailsTable(true, "spls.loadings");
            dtb.setupDetailsTable(true, "pca");

            dtb.setupDetailsTable(true, "svm");
            dtb.setupDetailsTable(true, "anova");

            dtb.setupDetailsTable(true, "ebam");
            dtb.setupDetailsTable(true, "sam");

        }
        if (containsModule("mf")) {
            dtb.setupDetailsTable(true, "cov");
            dtb.setupDetailsTable(true, "anova2");
            dtb.setupDetailsTable(true, "multirf");
        }
        if (containsModule("dose")) {
            dtb.setupDetailsTable(true, "dose-de");
            if (drb.isBmdAnal()) {
                drb.populateDoseResBeans();
            }
        }

        if (containsModule("network")) {
            List<String> analyses = List.of(
                    "keggGlobal_enr",
                    "gene_metabolites_enr",
                    "metabo_phenotypes_enr",
                    "metabo_metabolites_enr",
                    "global_enr",
                    "dspc_enr"
            );

            for (String a : analyses) {
                dtb.setupDetailsTable(true, a);
            }
            //bean.setupDetailsTable(true, "enrichment_network");
        }

        if (containsModule("metapaths")) {
            if (mpb.getResBeans() == null) {
                mpb.populateResBeans();
            }
        }
        if (containsModule("metadata")) {

            List<String> analyses = List.of(
                    "metap",
                    "votecount",
                    "merge"
            );
            for (String a : analyses) {
                mlb.setAnalMethod(a);
                mrb.populateResBeans();
            }

        }
        if (containsModule("mgwas")) {

            if (RDataUtils.checkDetailsTablePerformed(sb.getRConnection(), "harmonized.dat")) {
                mgwb.setupTable("harmonized.dat");
            }
            if (RDataUtils.checkDetailsTablePerformed(sb.getRConnection(), "mr_results_merge")) {
                mgwb.setupMRResTable();
            }
        }
        if (containsModule("pathinteg")) {
            dtb.setupDetailsTable(true, "match_integ");
        }

    }

    public boolean naviContainsUrl(String url) {
        Collection<String> values = sb.getNaviTrack().values();
        for (String value : values) {
            if (value.equals(url)) {
                return true;
            }
        }
        return false;
    }

    public String generateSummaryLM() {
        StringBuilder summary = new StringBuilder();

        // Retrieve selections from multifacBean and lmBean
        String comparisonOption = mfb.getNestCompOpt();
        String studyDesign = mfb.getCompDesign();
        String primaryMetadata = lmb.getAnalysisMeta();
        String secondaryMetadata = lmb.getAnalysisMeta2();
        String referenceGroup = lmb.getReferenceGroupFromAnalysisMeta();
        String contrast = lmb.getContrastFromAnalysisMeta();
        List<String> covariates = Arrays.asList(lmb.getAdjustedMeta());
        String blockingFactor = lmb.getBlockFac();
        String pValueCutoff = lmb.getCovPThresh();
        String pValueType = lmb.getCovPvalType();

        // Add comparison option
        switch (comparisonOption) {
            case "ref" ->
                summary.append("- Comparison: Against a common control\n");
            case "custom" ->
                summary.append("- Comparison: Specific comparison\n");
            case "inter" ->
                summary.append("- Comparison: Interactivity\n");
            case "nested" ->
                summary.append("- Comparison: Nested comparisons\n");
            default ->
                summary.append("- Comparison: Not specified\n");
        }

        if (null == studyDesign) {
            summary.append("- Study Design: Not specified\n");
        } else // Add study design
        {
            switch (studyDesign) {
                case "cov":
                    summary.append("- Study Design: Single Factor\n");
                    break;
                case "nest":
                    summary.append("- Study Design: Two Factors\n");
                    break;
                default:
                    summary.append("- Study Design: Not specified\n");
                    break;
            }
        }

        // Add primary metadata
        if (primaryMetadata != null && !primaryMetadata.isEmpty()) {
            summary.append("- Primary Metadata: ").append(primaryMetadata).append("\n");
        }

        // Add secondary factor if applicable
        if ("nest".equals(studyDesign) && secondaryMetadata != null && !secondaryMetadata.isEmpty()) {
            summary.append("- Secondary Factor: ").append(secondaryMetadata).append("\n");
        }

        // Add reference group if applicable
        if ("cov".equals(studyDesign) && referenceGroup != null && !referenceGroup.isEmpty()) {
            summary.append("- Reference Group: ").append(referenceGroup).append("\n");
        }

        // Add contrast if applicable
        if ("cov".equals(studyDesign) && contrast != null && !contrast.isEmpty()) {
            summary.append("- Contrast: ").append(contrast).append("\n");
        }

        // Add covariates if any
        if (covariates != null && !covariates.isEmpty()) {
            summary.append("- Covariates (Controlled for): ").append(String.join(", ", covariates)).append("\n");
        }

        // Add blocking factor
        if (blockingFactor != null && !"NA".equals(blockingFactor)) {
            summary.append("- Blocking Factor: ").append(blockingFactor).append("\n");
        }

        // Add P-value cutoff
        if (pValueCutoff != null && !pValueCutoff.isEmpty()) {
            summary.append("- P-value Cutoff: ").append(pValueCutoff).append(" (").append(pValueType.toUpperCase()).append(")\n");
        }

        return summary.toString();
    }

    public String generateMetaAnalysisSummary() {

        StringBuilder summary = new StringBuilder("<ul>"); // Start an HTML unordered list

        String analysisMethod = ldb.getAnalMethod(); // Get the selected meta-analysis method

        if (null == analysisMethod) {
            summary.append("<li><b>Analysis Method:</b> Not Specified</li>");
        } else {
            switch (analysisMethod) {
                case "metap" -> {
                    summary.append("<li><b>Analysis Method:</b> P-value Combination</li>");
                    summary.append("<li><b>Combination Method:</b> ").append(msb.getMetapMethod()).append("</li>");
                    summary.append("<li><b>P-value Significance Level:</b> ").append(msb.getMetpSigLvl()).append("</li>");
                }
                case "votecount" -> {
                    summary.append("<li><b>Analysis Method:</b> Vote Counting</li>");
                    summary.append("<li><b>Significance Level:</b> ").append(msb.getVcSigLvl()).append("</li>");
                    summary.append("<li><b>Minimum Votes Required:</b> ").append(msb.getMinVote()).append("</li>");
                }
                case "merge" -> {
                    summary.append("<li><b>Analysis Method:</b> Direct Merging</li>");
                    summary.append("<li><b>Significance Level:</b> ").append(msb.getDmSigLvl()).append("</li>");
                }
                default ->
                    summary.append("<li><b>Analysis Method:</b> Not Specified</li>");
            }
        }

        summary.append("</ul>"); // Close the unordered list
        return summary.toString();
    }

}
