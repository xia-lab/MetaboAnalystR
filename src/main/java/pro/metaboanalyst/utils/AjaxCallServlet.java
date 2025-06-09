/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.utils;

import jakarta.enterprise.context.spi.CreationalContext;
import jakarta.enterprise.inject.spi.Bean;
import jakarta.enterprise.inject.spi.BeanManager;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.inject.Inject;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Set;
import java.util.concurrent.Semaphore;
import javax.imageio.ImageIO;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.workflows.WorkflowView;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.ResourceSemaphore;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.enrich.MsetBean;
import pro.metaboanalyst.controllers.enrich.PathBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.mnet.MnetLoadBean;
import pro.metaboanalyst.controllers.mnet.MnetResBean;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;

import pro.metaboanalyst.project.ProjectBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.rwrappers.RGraphUtils;
import pro.metaboanalyst.rwrappers.RNetworkUtils;
import pro.metaboanalyst.rwrappers.RSpectraUtils;
import pro.metaboanalyst.spectra.SpectraControlBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;
import pro.metaboanalyst.spectra.SpectraUploadBean;

@WebServlet("/faces/AjaxCall")
public class AjaxCallServlet extends HttpServlet {

    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;

    private static final Logger LOGGER = LogManager.getLogger(AjaxCallServlet.class);

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        handleRequest(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        handleRequest(request, response);
    }

    protected void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        String funcName = request.getParameter("function");
        String res = "";
        String prefix = "/MetaboAnalyst"; //

        if (funcName.equalsIgnoreCase("redraw")) {
            //String analType = (String) request.getParameter("analType");
            double zoomPercent = Double.parseDouble(request.getParameter("zoompc"));
            double panXPercent = Double.parseDouble(request.getParameter("panXpc"));
            double panYPercent = Double.parseDouble(request.getParameter("panYpc"));
            int canvasDimX = Integer.parseInt(request.getParameter("width"));
            int canvasDimY = Integer.parseInt(request.getParameter("height"));
            //save the original value
            int canvasDimXo = canvasDimX;
            int canvasDimYo = canvasDimY;

            if (zoomPercent == 100) { //if no scaling needs to be done, pan is irrelevant
                panXPercent = 0;
                panYPercent = 0;
            } else {      //otherwise....
                panXPercent = ((100 - zoomPercent) * panXPercent) / (100 - zoomPercent);  //readjust X pan for larger image
                panYPercent = ((100 - zoomPercent) * panYPercent) / (100 - zoomPercent);  //readjust Y pan for larger image
            }

            canvasDimX = (int) ((zoomPercent / 100) * canvasDimX + 0.5);     //readjust canvas dim x based on scaling
            canvasDimY = (int) ((zoomPercent / 100) * canvasDimY + 0.5);   //readjust canvas dim y based on scaling

            long current = System.currentTimeMillis();
            String zoomImgName = "zoom" + current++ + ".png";

            res = "zoomImgURL=\'NA\'";
            RConnection RC = sb.getRConnection();

            boolean plotRes = RGraphUtils.drawMetPAGraph(RC, zoomImgName, canvasDimX, canvasDimY, zoomPercent);

            if (plotRes) {
                String convertPath = RGraphUtils.getConvertFullPath(RC);
                if (convertPath.equals("NA")) {

                } else {
                    int cropright = (int) ((canvasDimX - canvasDimXo) * panXPercent / 100);
                    int croptop = (int) ((canvasDimY - canvasDimYo) * panYPercent / 100);
                    if (croptop < 0) {
                        int wd = 0 - croptop;
                        croptop = 0;
                        cropright = cropright + wd;
                    }
                    if (cropright < 0) {
                        int wd = 0 - cropright;
                        cropright = 0;
                        croptop = croptop + wd;
                    }
                    String zmImgPath = sb.getCurrentUser().getHomeDir() + File.separator + zoomImgName;
                    String cropedImgName = zoomImgName.replace("zoom", "crop");
                    String targetPath = sb.getCurrentUser().getHomeDir() + File.separator + cropedImgName;
                    boolean cropOK = DataUtils.cropImage(convertPath, zmImgPath, targetPath, cropright, croptop, canvasDimXo, canvasDimYo, 100);
                    if (cropOK) {
                        String zmImgURL = "zoomImgURL=\'" + prefix + "/resources/users" + File.separator
                                + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + cropedImgName + "\'";
                        res = zmImgURL;
                    }
                }
            }
        } else if (funcName.equalsIgnoreCase("getusrdir")) {
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator;
        } else if (funcName.equalsIgnoreCase("getWorkflowUrl")) {
            String id = request.getParameter("id");
            res = getUrl(id);
            //System.out.println(res + "====getUrl");
        } else if (funcName.equalsIgnoreCase("plotPathway")) {
            String pathName = request.getParameter("pathname");
            int canvasDimX = Integer.parseInt(request.getParameter("width"));
            int canvasDimY = Integer.parseInt(request.getParameter("height"));
            String nodeTipInfo = RGraphUtils.plotKEGGPath(sb, pathName, canvasDimX + "", canvasDimY + "", "png", "NULL");

            String pathPrefix = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator;
            String imgURL = pathPrefix + pathName + ".png";

            String urlCode = "pathImgURL=\'" + imgURL + "\';";

            res = urlCode + "\n" + "pathPrefix=\'" + pathPrefix + "\';\n" + nodeTipInfo;

        } else if (funcName.equalsIgnoreCase("getImageMap")) {

            String leftImgURL = prefix + "/resources/users" + File.separator
                    + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + sb.getCurrentImage("path_view") + "dpi150.png";
            String urlCode = "leftImgURL=\"" + leftImgURL + "\"";

            String imgMapInfo = RGraphUtils.getOverviewImgMap(sb.getRConnection());
            res = sb.getAnalType() + "||" + urlCode + "\n" + imgMapInfo;

        } else if (funcName.equalsIgnoreCase("prepareMsetView")) {
            MsetBean msb = findBeanInstance("msetBean");

            String id = request.getParameter("id");
            msb.setMsetNm(id);
            res = "1";
        } else if (funcName.equalsIgnoreCase("getmnetinfo")) {
            //MnetResBean mnetb = findBeanInstance("mnetResBean");
            MnetLoadBean loadnetb = findBeanInstance("mnetLoader");
            String org = loadnetb.getNetOrg();
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + org;
            res = res + "||" + sb.getVisMode() + "||" + RNetworkUtils.getNetsNamesString(sb.getRConnection());

        } else if (funcName.equalsIgnoreCase("getUpsetInfo")) {
            if (sb.getAnalType().equals("metadata")) {// feature-meta
                res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + false + "||" + sb.getCurrentImage("upset_stat");
            } else {//path meta
                MetaPathLoadBean mplb = findBeanInstance("pLoadBean");

                res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + false + "||" + sb.getCurrentImage("upset_path") + "||"
                        + mplb.getSelectedDataNms().size() + "||" + String.join(";", mplb.getSelectedDataNms().toArray(new String[0]));
            }
        } else if (funcName.equalsIgnoreCase("getmummi")) {
            MummiAnalBean mbean = findBeanInstance("mummiAnalBean");

            String opts = Arrays.toString(mbean.getAlgOpts());
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + opts;
        } else if (funcName.equalsIgnoreCase("getheatmap")) {
            System.out.println("getheatmap========" + sb.getHeatmapType());
            if (sb.getHeatmapType().equals("mummichog")) {
                MummiAnalBean mbean = findBeanInstance("mummiAnalBean");

                res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator
                        + "||" + sb.getOrg() + "||" + mbean.getHeatmapName() + "||" + sb.getHeatmapType();
            } else { //pathway
                PathBean pb = (PathBean) findBeanInstance("pathBean");
                res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator
                        + "||" + pb.getLibOpt() + "||" + pb.getHeatmapName() + "||" + sb.getHeatmapType() + "||" + pb.getEnrType();
            }

        } else if (funcName.equalsIgnoreCase("getheatmappath")) {
            PathBean pb = (PathBean) findBeanInstance("pathBean");
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator
                    + "||" + sb.getOrg() + "||" + pb.getHeatmapName();

        } else if (funcName.equalsIgnoreCase("heatmapMummichog")) {
            String funDB = request.getParameter("funDB");
            String ids = request.getParameter("IDs");
            String nm = "mummichog_enrichment_" + sb.getFileCount();
            int myres = REnrichUtils.doHeatmapMummichogTest(sb.getRConnection(), nm, funDB, ids);
            //System.out.print(myres);
            if (myres == 1) {
                res = nm + ".json";
            } else {
                res = "ERROR!" + RDataUtils.getErrMsg(sb.getRConnection());
            }

        } else if (funcName.equalsIgnoreCase("heatmapPathway")) {
            String funDB = request.getParameter("funDB");
            String ids = request.getParameter("IDs");
            String nm = "pathway_enrichment_" + sb.getFileCount();
            int myres = REnrichUtils.doHeatmapPathwayTest(sb.getRConnection(), nm, funDB, ids);
            //System.out.print(myres);
            if (myres == 1) {
                res = nm + ".json";
            } else {
                res = "ERROR!" + RDataUtils.getErrMsg(sb.getRConnection());
            }
        } else if (funcName.equalsIgnoreCase("loadingSingleXICPlot")) {
            SpectraProcessBean spb = findBeanInstance("spectraProcessor");

            String featureNumber = request.getParameter("featureNumber");
            String sampleName = request.getParameter("sampleName");
            String fileName = RSpectraUtils.plotSingleXIC(sb, featureNumber, sampleName, spb.isShowlabel());

            sb.getCurrentUser().setRelativeDir("/resources/users/" + sb.getCurrentUser().getName());
            spb.setSingleXICPlot(sb.getCurrentUser().getRelativeDir() + File.separator + fileName);
            spb.internalizeImage(fileName);
            res = fileName;
        } else if (funcName.equalsIgnoreCase("loadingXICPlot")) {
            SpectraProcessBean spb = findBeanInstance("spectraProcessor");

            String id = request.getParameter("id");
            RConnection RC = sb.getRConnection();
            int idnew;
            spb.setSingleXICPlot("/resources/images/EICalt.png");
            if (!id.equals("na")) {
                idnew = RSpectraUtils.mzrt2ID2(RC, id);
                spb.setFeatureNum(idnew);
            } else {
                idnew = spb.getFeatureNum();
                //System.out.println("Now idnew equals: " + idnew);                
                spb.setFeatureNum(idnew);
            }

            String fileNm = spb.plotMSfeature("svg");
            res = fileNm;

        } else if (funcName.equalsIgnoreCase("loadingBoxPlot")) {
            String id = request.getParameter("id");
            sb.viewCmpdSummary(id);
            res = "1";

        } else if (funcName.equalsIgnoreCase("loadingTICPlot")) {
            SpectraProcessBean spb = findBeanInstance("spectraProcessor");

            String fileNM = request.getParameter("fileNM");

            /*Only work when Raw Info exists*/
            File mSetObj = new File(sb.getCurrentUser().getHomeDir() + File.separator + "mSet.rda");

            if (mSetObj.exists()) {
                spb.setTicName(fileNM);
                spb.plotSingleTIC();
                res = spb.getTicImgName();
            } else {
                res = "";
            }

        } else if (funcName.equalsIgnoreCase("updateLoading")) {
            SpectraProcessBean spb = findBeanInstance("spectraProcessor");

            spb.setFeatureNM(request.getParameter("number"));

            int nb = Integer.parseInt(request.getParameter("number"));
            int res1 = RDataUtils.update3DPCA(sb.getRConnection(), nb);

            if (res1 > 0) {
                spb.internalizeRes(nb);
                res = spb.getLoadingJson();
                spb.setFeatureNM("");
                res = res + "||" + spb.getLoadingJson();
            } else {
                res = "NOT okay";
            }
        } else if (funcName.equalsIgnoreCase("updateSpectraPCA")) {
            SpectraProcessBean spb = findBeanInstance("spectraProcessor");

            spb.setFeatureNM(request.getParameter("number"));

            int nb = Integer.parseInt(request.getParameter("number"));
            if (nb == 0) {
                spb.setFeatureNM("");
            }
            res = spb.getLoadingJson();

        } else if (funcName.equalsIgnoreCase("iPCALoadingBoxPlot")) {
            MultifacBean tb = findBeanInstance("multifacBean");

            String id = request.getParameter("id");
            tb.setBoxId(id);
            tb.updateBoxplotMeta();
            res = "1";
        } else if (funcName.equalsIgnoreCase("multiFacBoxPlot")) {
            MultifacBean tb = findBeanInstance("multifacBean");

            String id = request.getParameter("id");
            tb.setBoxId(id);
            tb.updateBoxplotMeta();
            res = "1";
        } else if (funcName.equalsIgnoreCase("getCovSvgInfo")) {
            MultifacBean tb = findBeanInstance("multifacBean");

            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + tb.getCovJsonName() + "||" + tb.getRawCovThresh();
        } else if (funcName.equalsIgnoreCase("getSpecSvgInfo")) {
            SpectraProcessBean sp = (SpectraProcessBean) findBeanInstance("spectraProcessor");
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + sp.getFeatureNum() + ".json";
        } else if (funcName.equalsIgnoreCase("decodeImageData")) {
            String data = request.getParameter("data");
            String fileNm = request.getParameter("name");
            String format = request.getParameter("format");

            try {
                if (format.equals("svg")) {
                    String dataString = data.split(",")[0];
                    byte[] dataBytes = jakarta.xml.bind.DatatypeConverter.parseBase64Binary(dataString);
                    // Convert bytes to string assuming the SVG is encoded in UTF-8
                    String svgString = new String(dataBytes, "UTF-8");
                    // Create the SVG file
                    File outputfile = new File(sb.getCurrentUser().getHomeDir() + "/" + fileNm + ".svg");
                    // Write SVG string to file
                    try (FileWriter fw = new FileWriter(outputfile)) {
                        fw.write(svgString);
                    }
                } else {
                    String dataString = data.split(",")[1];
                    byte[] dataBytes = jakarta.xml.bind.DatatypeConverter.parseBase64Binary(dataString);
                    BufferedImage img = ImageIO.read(new ByteArrayInputStream(dataBytes));
                    String dir = sb.getCurrentUser().getHomeDir();
                    File outputfile = new File(dir + "/" + fileNm + ".png");
                    ImageIO.write(img, "png", outputfile);
                }
                System.out.println("receiveimagedata====" + fileNm);
                sb.getReportImgMap().put(fileNm, fileNm + "." + format);

            } catch (IOException ex) {
                // Logger.getLogger(SessionBean1.class.getName()).log(Level.SEVERE, null, ex);
                LOGGER.error("decodeImageData", ex);
            }
        } else if (funcName.equalsIgnoreCase("receiveJsonData")) {
            String jsonData = request.getParameter("data");
            String nm = request.getParameter("name");

            String fileName = "";
            String outputName = "";
            System.out.println("receiveJsonData======" + nm);
            switch (nm) {
                case "ipca_3d" ->
                    fileName = sb.getCurrentImage("ipca_3d") + ".json";
                case "pca_3d" ->
                    fileName = sb.getCurrentImage("pca_loading3d") + ".json";
                case "plsda_3d" ->
                    fileName = sb.getCurrentImage("pls_loading3d") + ".json";
                case "scores_3d" ->
                    fileName = sb.getCurrentImage("scores_3d") + ".json";
                case "splsda_3d" ->
                    fileName = sb.getCurrentImage("spls_loading3d") + ".json";
                case "heatmap_mummichog" -> {
                    MummiAnalBean mb = (MummiAnalBean) findBeanInstance("mummiAnalBean");
                    fileName = mb.getHeatmapName();
                }
                case "heatmap_pathway" -> {
                    PathBean pb = findBeanInstance("pathBean");

                    fileName = pb.getHeatmapName();
                }
                case "network", "gene_metabolites", "metabo_phenotypes", "metabo_metabolites", "global", "dspc" -> {
                    fileName = "networkanalyst_" + nm + ".json";
                    outputName = "report_" + nm + ".json";
                }
                case "enrichment_network" -> {
                    fileName = "msea_network.json";
                }
                case "network_MetaboNet" ->
                    fileName = "mummichog_query.json";
                case "network_gsea", "network_mummichog", "network_integ" ->
                    fileName = nm + ".json";

                default -> {
                }

            }
            System.out.println("receiveJsonDataName======" + fileName);
            if (outputName.equals("")) {
                outputName = "report_" + fileName;
            }

            File outputFile = new File(sb.getCurrentUser().getHomeDir() + "/" + outputName);

            try (FileWriter fileWriter = new FileWriter(outputFile)) {
                fileWriter.write(jsonData);
                sb.getReportJsonMap().put(nm, fileName);
                res = fileName;
            } catch (IOException ex) {
                LOGGER.error("handleAjaxRequest-in", ex);
                res = "NA";
            }

        } else if (funcName.equalsIgnoreCase("getnetinfo")) {
            res = RNetworkUtils.getNetsNamesString(sb.getRConnection());
        } else if (funcName.equalsIgnoreCase("performCommunityDetection")) {
            String method = request.getParameter("method");
            res = RNetworkUtils.detectCommunities(sb.getRConnection(), method);
        } else if (funcName.equalsIgnoreCase("getShortestPaths")) {
            String ndA = request.getParameter("source");
            String ndB = request.getParameter("target");
            res = RNetworkUtils.getShortestPaths(sb.getRConnection(), ndA, ndB);
        } else if (funcName.equalsIgnoreCase("updateNetworkLayout")) {
            MnetResBean mnetb = findBeanInstance("mnetResBean");

            String algo = request.getParameter("layoutalgo");
            res = mnetb.updateNetworkLayout(algo);
        } else if (funcName.equalsIgnoreCase("performNodesFilter")) {
            MnetResBean mnetb = findBeanInstance("mnetResBean");

            String nodeIDs = request.getParameter("nodes");
            res = mnetb.performNodesFilter(nodeIDs);
        } else if (funcName.equalsIgnoreCase("networkEnrichment")) {
            MnetResBean mnetb = findBeanInstance("mnetResBean");

            String ids = request.getParameter("IDs");
            String funDB = request.getParameter("funDB");
            String vismode = sb.getVisMode();
            String nm = "metaboanalyst_enrichment_" + sb.getFileCount();
            int myres;
            if (funDB.equals("kegg")) {
                //Run enrichment analysis using KEGG genes library
                if (vismode.equals("gene_phenotypes")) {
                    myres = RNetworkUtils.doNetEnrichmentTest(sb.getRConnection(), nm, funDB, ids, vismode);
                } else {
                    //Run enrichment analysis using KEGG compounds-genes library
                    myres = mnetb.doIntegPathwayAnalysis(ids, nm);
                }
            } else {//GO, motif. reactiome
                myres = RNetworkUtils.doNetEnrichmentTest(sb.getRConnection(), nm, funDB, ids, vismode);
            }

            if (myres == 1) {
                res = nm + ".json";
            } else {
                res = "ERROR!" + RDataUtils.getErrMsg(sb.getRConnection());
            }

        } else if (funcName.equalsIgnoreCase("exportNetwork")) {
            MnetResBean mnetb = findBeanInstance("mnetResBean");

            String format = request.getParameter("format");
            res = mnetb.exportNetwork(format);

        } else if (funcName.equalsIgnoreCase("extractModule")) {
            MnetResBean mnetb = findBeanInstance("mnetResBean");

            String nodeIDs = request.getParameter("nodeIDs");
            res = mnetb.extractModule(nodeIDs);

        } else if (funcName.equalsIgnoreCase("viewNetwork")) {
            String name = request.getParameter("name");
            res = RDataUtils.genPathwayJSON(sb.getRConnection(), name);

        } else if (funcName.equalsIgnoreCase("download")) {
            DataUtils.setupFileDownloadZip(sb.getCurrentUser());
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "/Download.zip";

        } else if (funcName.equalsIgnoreCase("getCurrentPageUrl")) {
            res = request.getRequestURL().toString();
        } else if (funcName.equalsIgnoreCase("getErrorMsg")) {
            res = RDataUtils.getErrMsg(sb.getRConnection());
        } else if (funcName.equalsIgnoreCase("prepareNetwork")) {
            MnetResBean mnetb = findBeanInstance("mnetResBean");

            String netName = request.getParameter("netName");
            res = mnetb.loadNetwork(netName);
        } else if (funcName.equalsIgnoreCase("getcmpdinfo")) {
            String id = request.getParameter("cid");
            res = RDataUtils.getCmpdInfo(sb.getRConnection(), id);
        } else if (funcName.equalsIgnoreCase("savePartialRaw")) {

            String naviString = request.getParameter("naviString");
            String uid = request.getParameter("uid");

            SpectraControlBean spcb = (SpectraControlBean) findBeanInstance("spectraController");
            spcb.setCreatedShareLink(true);
            sb.setPartialId(uid);
            sb.setNaviString(naviString);

            if (ab.isInDocker()) {
                spcb.updateJobPartialLink_docker();
            } else {
                spcb.updateJobPartialLink();
            }

            res = ab.getAppUrlPath() + "||" + spcb.getCurrentJobId();

        } else if (funcName.equalsIgnoreCase("doSpecLogin")) {
            if (!sb.isLoggedIn() || !sb.getAnalType().equals("raw")) {
                boolean ok = sb.doLogin("spec", "raw", false, false);
                if (!ok) {
                    res = "false";
                } else {
                    res = "true";
                }
            } else {
                res = "true";
            }
        } else if (funcName.equalsIgnoreCase("executeWorkflow")) {
            WorkflowView wf = findBeanInstance("workflowView");
            String str = request.getParameter("functionsStr");

            int resInt = 0;
            try {
                resInt = wf.executeWorkflow(str);
            } catch (Exception ex) {
                LOGGER.error("executeWorkflow", ex);
                // java.util.logging.Logger.getLogger(MyPhaseListener.class.getName()).log(Level.SEVERE, null, ex);
            }
            if (resInt == 1) {
                res = "success";

            } else if (resInt == 0) {
                res = "failed";

            }
        } else if (funcName.equalsIgnoreCase("getMetaIntegrity")) {
            //SpectraControlBean spc = findBeanInstance("spectraControlBean");
            SpectraControlBean spc = CDI.current().select(SpectraControlBean.class).get();
            String filenms = request.getParameter("fileNms");
            boolean metaFormatOk = spc.isMetaOk();
            if (metaFormatOk) {
                //SpectraUploadBean su = findBeanInstance("spectraUploadBean");
                SpectraUploadBean su = CDI.current().select(SpectraUploadBean.class).get();

                boolean isMetaOk = su.checkMetaIntegrity(filenms);
                if (!isMetaOk) {
                    //su.updateText();
                }
                res = isMetaOk + "";
            } else {
                res = "false";
            }

        } else if (funcName.equalsIgnoreCase("checkUploadPermission")) {
            ResourceSemaphore resourceSemaphore = findBeanInstance("semaphore");

            Semaphore semaphore = resourceSemaphore.getUploadSemaphore();
            boolean canUpload = semaphore.availablePermits() > 0;
            res = canUpload + "";

        } else if (funcName.equalsIgnoreCase("checkSessionUploaded")) {
            //SpectraControlBean spc = findBeanInstance("spectraControlBean");
            SpectraControlBean spc = CDI.current().select(SpectraControlBean.class).get();
            boolean resB = spc.checkJobRunning();
            res = resB + "";

        } else if (funcName.equalsIgnoreCase("getPartialLinkInfo")) {
            SpectraControlBean spc = findBeanInstance("spectraControlBean");

            String url = ab.getAppUrlPath();
            res = url + "/MetaboAnalyst/faces/Share?ID=" + sb.getPartialId() + "||" + spc.getCurrentJobId();

        } else if (funcName.equalsIgnoreCase("loadpartialLink")) {
            try {
                ProjectBean pjb = findBeanInstance("projectBean");

                res = pjb.loadPartial();
            } catch (SQLException ex) {
                res = null;
            }

            if (res == null) {
                res = "null";
            } else {
                res = "/MetaboAnalyst/Secure/spectra/JobStatusView.xhtml";
            }

        } else if (funcName.equalsIgnoreCase("generateColorArray")) {
            int n = Integer.parseInt(request.getParameter("number"));

            int jsonCount = sb.getFileCount();
            String filenm = "color_array_" + jsonCount + ".json";
            res = RDataUtils.generateColorArray(sb.getRConnection(), n, "green", filenm);

        } else if (funcName.equalsIgnoreCase("computeEncasing")) {
            String type = (String) request.getParameter("type");
            String names = (String) request.getParameter("names");
            String level = (String) request.getParameter("level");
            String omicstype = (String) request.getParameter("omics");

            int jsonCount = sb.getFileCount();
            String filenm = "encasing_mesh_" + jsonCount + ".json";
            res = RDataUtils.computeEncasing(sb.getRConnection(), filenm, type, names, level, omicstype);

            if (sb.getAnalType().equals("raw") || sb.getDataType().equals("spec")) {
                SpectraProcessBean spb = findBeanInstance("spectraProcessor");

                spb.internalizecomputeEncasingRes();
            }

        } else if (funcName.equalsIgnoreCase("generatecoviddata")) {
            String platforms = (String) request.getParameter("platforms");
            String bloods = (String) request.getParameter("bloods");
            String polarities = (String) request.getParameter("polarities");
            String countries = (String) request.getParameter("countries");
            String populations = (String) request.getParameter("populations");
            String comparis = (String) request.getParameter("comparis");
            String url = ab.getAppUrlPath();

            String res0 = RDataUtils.generatecoviddata(sb.getRConnection(), platforms, bloods, polarities, countries, populations, comparis);

            res = url + "/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName() + "/" + res0;

        } else if (funcName.equalsIgnoreCase("dologout")) {
            System.out.println(" Reaching here dologout !!!!");
            sb.doLogout(0);
            res = "done";
        } else {
            //Nothing
        }

        //finally send back response
        response.setHeader("Cache-Control", "no-store, no-cache, must-revalidate");
        response.setDateHeader("Expires", 0);
        //Note: set plain text, not xml, since our response is basically free text & javascript code
        response.setContentType("text/plain");
        try {
            response.getWriter().write(res);
        } catch (Exception e) {
            LOGGER.error("handleAjaxRequest", e);
        }
    }

    @Inject
    private BeanManager beanManager;

    @SuppressWarnings("unchecked")
    public <T> T findBeanInstance(String beanName) {
        Set<Bean<?>> beans = beanManager.getBeans(beanName);
        if (beans == null || beans.isEmpty()) {
            throw new IllegalArgumentException("Unknown bean name: " + beanName);
        }
        Bean<?> bean = beanManager.resolve(beans);
        CreationalContext<?> ctx = beanManager.createCreationalContext(bean);
        return (T) beanManager.getReference(bean, bean.getBeanClass(), ctx);
    }

    public String getUrl(String func) {
        String url = "";
        switch (func) {
            case "Missing Values" -> {

            }
            case "Save Project" -> {

            }
            case "Data Processing", "Sanity Check", "Sanity Check Intensity", "Sanity Check Peak" -> {
                url = "/Secure/process/SanityCheck.xhtml";
            }
            case "Filtering", "Filtering_Table", "Filtering Intensity" -> {
                url = "/Secure/process/FilterView.xhtml";

            }
            case "Normalization", "Normalization_Table", "Normalization Intensity" -> {
                url = "/Secure/process/NormalizationView.xhtml";

            }
            case "Volcano" -> {
                url = "/Secure/analysis/VolcanoView.xhtml";

            }
            case "PCA" -> {
                url = "/Secure/analysis/PCAView.xhtml";
            }
            case "iPCA" -> {
                url = "/Secure/multifac/LivePCAView.xhtml";

            }
            case "ANOVA" -> {
                url = "/Secure/analysis/AnovaView.xhtml";
            }
            case "Fold change" -> {
                url = "/Secure/analysis/FoldChangeView.xhtml";
            }
            case "T-test" -> {
                url = "/Secure/analysis/TtestView.xhtml";
            }
            case "Pattern Search" -> {
                url = "/Secure/analysis/PatternView.xhtml";
            }
            case "Correlation Heatmap" -> {
                url = "/Secure/analysis/CorrelationView.xhtml";
            }
            case "PLSDA" -> {
                url = "/Secure/analysis/PLSDAView.xhtml";
            }
            case "sPLSDA" -> {
                url = "/Secure/analysis/SparsePLSDAView.xhtml";
            }
            case "OrthoPLSDA" -> {
                url = "/Secure/analysis/OrthoPLSDAView.xhtml";

            }
            case "SAM" -> {
                url = "/Secure/analysis/SAMView.xhtml";
            }
            case "EBAM" -> {
                url = "/Secure/analysis/EBAMView.xhtml";
            }
            case "Dendrogram" -> {
                url = "/Secure/analysis/TreeView.xhtml";
            }
            case "Heatmap" -> {
                url = "/Secure/analysis/HeatmapView.xhtml";
            }
            case "K-means" -> {
                url = "/Secure/analysis/KMView.xhtml";

            }
            case "SOM" -> {
                url = "/Secure/analysis/SOMView.xhtml";
            }
            case "Random Forest" -> {
                url = "/Secure/analysis/RFView.xhtml";

            }
            case "SVM" -> {
                url = "/Secure/analysis/RFView.xhtml";

            }
            case "SSP" -> {
                //sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                url = "/Secure/enrichment/OraView.xhtml";

            }
            case "QEA" -> {
                //sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                url = "/Secure/enrichment/QeaView.xhtml";

            }
            case "ORA" -> {
                //sb.addNaviTrack("Set parameter", "/Secure/enrichment/EnrichParamView.xhtml");
                url = "/Secure/enrichment/OraView.xhtml";

            }
            case "Functional Annotation" -> {
                url = "/Secure/mummichog/LibraryView.xhtml";

            }
            case "performPeaks2Fun", "Scatter" -> {
                MummiAnalBean ma = CDI.current().select(MummiAnalBean.class).get();

                if (ma.getAlgOpts().length > 1) {
                    url = "/Secure/mummichog/IntegMumResultView.xhtml";
                } else if (ma.getAlgOpts()[0].equals("mum")) {
                    url = "/Secure/mummichog/MummiResultView.xhtml";
                } else if (ma.getAlgOpts()[0].equals("gsea")) {
                    url = "/Secure/mummichog/GseaResultView.xhtml";
                }

            }
            case "Heatmap_mum" -> {
                url = "/Secure/viewer/HeatmapView.xhtml";
            }
            case "Network" -> {
                url = "/Secure/mummichog/KeggNetView.xhtml";
            }
            case "Name check", "Name check_List", "Name check_Table" -> {
                url = "/Secure/process/NameMapView.xhtml";

            }
            case "paBn_proceed_ora", "paBn_proceed_qea" -> {
                PathBean pab = CDI.current().select(PathBean.class).get();

                if (pab.getAnalOption().equals("Scatter")) {
                    if (pab.getLibOpt().startsWith("smpdb")) {
                        url = "/Secure/pathway/SMPDBResultView.xhtml";
                    } else {
                        url = "/Secure/pathway/PathResultView.xhtml";
                    }
                } else {
                    url = "/Secure/viewer/HeatmapView.xhtml";
                }
            }
            case "doMnetworkAnalysis_static", "doMnetworkAnalysis_metabo_phenotypes", "doMnetworkAnalysis_gene_metabolites", "doMnetworkAnalysis_metabo_metabolites", "doMnetworkAnalysis_global" -> {
                // Add specific handling if required
            }
            case "Network Selection" -> {
                url = "/Secure/network/MnetParamView.xhtml";
            }
            case "Network Builder_dspc" -> {
                url = "/Secure/network/MphenoStatsxhtml";

            }
            case "DSPC Network", "Network Viewer_dspc", "DSPC Networks" -> {
                url = "/Secure/network/MphenoNetView.xhtml";

            }
            case "KEGG Network" -> {
                url = "/Secure/network/MetaboNetView.xhtml";
            }
            case "doMnetworkAnalysis" -> {
                url = "/Secure/network/MphenoNetView.xhtml";

            }
            case "Multivariate ROC" -> {
                url = "/Secure/roc/MultiRocView.xhtml";

            }
            case "Model-based ROC" -> {

                url = "/Secure/roc/RocTestView.xhtml";
            }
            case "Univariate ROC" -> {

                url = "/Secure/roc/UnivRocView.xhtml";
            }
            case "Metadata check" -> {
                url = "/Secure/process/MetaDataCheck.xhtml";

            }
            case "Metadata Heatmap" -> {
                url = "/Secure/multifac/MetaDataView.xhtml";
            }
            case "Multifactor anova" -> {
                url = "/Secure/multifac/Anova2View.xhtml";
            }
            case "ASCA" -> {
                url = "/Secure/multifac/AscaView.xhtml";

            }
            case "Clustering heatmap" -> {
                url = "/Secure/multifac/Heatmap2View.xhtml";

            }
            case "Linear Models" -> {
                url = "/Secure/multifac/LinearModelView.xhtml";
            }
            case "MEBA" -> {
                url = "/Secure/multifac/TimeCourseView.xhtml";

            }
            case "Random Forest2" -> {
                url = "/Secure/multifac/MultifacRFView.xhtml";

            }
            case "Correlation Analysis" -> {
                url = "/Secure/multifac/PartialCorrView.xhtml";
            }
            case "Correlation Networks (DSPC)" -> {
                url = "/Secure/network/MphenoNetView.xhtml";
            }
            case "DE Analysis" -> {
                url = "/Secure/dose/SigFeatureView.xhtml";
            }
            case "Curve Fitting" -> {
                url = "/Secure/dose/ModelFitView.xhtml";
            }
            case "Result" -> {
                url = "/Secure/dose/FitResultView.xhtml";
            }
            case "Pathway Analysis Results_Table", "Pathway Analysis Results_List" -> {
                url = "/Secure/dose/FitResultView.xhtml";
            }
            case "Combine P-values" -> {
                url = "/Secure/metastat/MetaResultView.xhtml";
            }
            case "Vote Counting" -> {

                url = "/Secure/metastat/MetaResultView.xhtml";
            }
            case "Direct Merging" -> {

                url = "/Secure/metastat/MetaResultView.xhtml";
            }
            case "Upset Diagram" -> {

                url = "/Secure/metastat/UpsetDiagramView.xhtml";
            }
            case "metapaths_Method Selection" -> {
                url = "/Secure/metapath/MetaPathAnalView.xhtml";
            }
            case "Pathway-level integration" -> {
                url = "/Secure/metapath/MetaPathResultView.xhtml";
            }
            case "metapaths Network Explorer path" -> {
                url = "/Secure/network/MetaboNetView.xhtml";

            }
            case "metapaths Upset Diagram path" -> {

                url = "/Secure/metastat/UpsetDiagramView.xhtml";

            }
            case "Pooling peaks" -> {

                url = "/Secure/metapath/MetaPathResultView.xhtml";
            }
            case "metapaths Network Explorer pool" -> {
                url = "/Secure/network/MetaboNetView.xhtml";
            }
            case "metapaths Upset Diagram pool" -> {
                url = "/Secure/metastat/UpsetDiagramView.xhtml";
            }
            case "Spectra Check" -> {
                // Add specific handling if required
                url = "/Secure/spectra/SpectraCheck.xhtml";
            }
            case "Spectra Parameters Settings" -> {
                url = "/Secure/spectra/SpectraProcess.xhtml";
            }
            case "Spectra Processing" -> {
                url = "/Secure/spectra/JobStatusView.xhtml";
            }
            case "Spectra View Result" -> {
                url = "/Secure/spectra/SpectraResult.xhtml";

            }
            case "Annotation_Conc" -> {
                url = "/Secure/process/NameMapView.xhtml";
            }
            case "Comp. with Reference" -> {
                url = "/Secure/enrichment/SspProfileView.xhtml";
            }
            default -> {

            }
        }
        return url;
    }

    public String addLinkInfo(String imgName) {
        String url = "/Secure/analysis/CorrelationView.xhtml";
        switch (imgName) {
            case "heatmap_correlation" -> {
                url = "/Secure/analysis/CorrelationView.xhtml";
            }
            case "heatmap_static" -> {
                url = "/Secure/analysis/HeatmapView.xhtml";
            }
            case "dose_volcano" -> {
                url = "/Secure/dose/SigFeatureView.xhtml";
            }
            case "heatmap_multifac" -> {
                url = "/Secure/multifac/Heatmap2View.xhtml";
            }
            case "network_MetaboNet" -> {
                url = "/Secure/network/MetaboNetView.xhtml";
            }
            case "enrichment_network" -> {
                url = "/Secure/enrichment/OraView.xhtml";
            }
            case "upset" -> {
                url = "/Secure/metastat/UpsetDiagramView.xhtml";
            }
        }
        return "";
    }
}
