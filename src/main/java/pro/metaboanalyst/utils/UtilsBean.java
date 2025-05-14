/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import kong.unirest.Unirest;
import pro.metaboanalyst.models.NameMapBean;
import pro.metaboanalyst.models.User;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;

/**
 *
 * @author jianguox
 */
@SessionScoped
@Named("utilBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class UtilsBean implements Serializable {
    @JsonIgnore
    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    @JsonIgnore
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    @JsonIgnore
    private static final Logger LOGGER = LogManager.getLogger(UtilsBean.class);

    private boolean useApiCalls = true;

    public boolean isUseApiCalls() {
        return useApiCalls;
    }

    public void setUseApiCalls(boolean useApiCalls) {
        this.useApiCalls = useApiCalls;
    }

    public UtilsBean() {
        Unirest.config().setObjectMapper(new JacksonObjectMapper());
    }

    private String isomerOpt = "y";

    public String getIsomerOpt() {
        return isomerOpt;
    }

    public void setIsomerOpt(String isomerOpt) {
        this.isomerOpt = isomerOpt;
    }

    private UploadedFile lipidFile;

    public UploadedFile getLipidFile() {
        return lipidFile;
    }

    public void setLipidFile(UploadedFile lipidFile) {
        this.lipidFile = lipidFile;
    }

    private String[] selectedIDs;

    public String[] getSelectedIDs() {
        return selectedIDs;
    }

    public void setSelectedIDs(String[] selectedIDs) {
        this.selectedIDs = selectedIDs;
    }

    private String inputType;

    public String getInputType() {
        return inputType;
    }

    public void setInputType(String inputType) {
        this.inputType = inputType;
    }

    private String queryList = null;

    public String getQueryList() {
        return queryList;
    }

    public void setQueryList(String queryList) {
        this.queryList = queryList;
    }

    private UploadedFile nmFile;

    public UploadedFile getNmFile() {
        return nmFile;
    }

    public void setNmFile(UploadedFile nmFile) {
        this.nmFile = nmFile;
    }

    public NameMapBean[] formatHyperlinks(NameMapBean[] nameMaps, String inputType) {
        for (NameMapBean nameMap : nameMaps) {
            if (nameMap.getHit().equals("") && inputType.equals("name")) {
                nameMap.setQuery("<strong style=\"background-color:var(--orange-500); font-size=125%;>" + nameMap.getQuery() + "</strong>");
                nameMap.setDetails("View");
            } else if (nameMap.getHit().equals("")) {
                nameMap.setQuery("<strong style=\"background-color:var(--orange-500); font-size=125%;>" + nameMap.getQuery() + "</strong>");
            } else {
                if (!nameMap.getHmdb_id().equals("-")) {
                    nameMap.setHmdb_id("<a href=\"http://www.hmdb.ca/metabolites/" + nameMap.getHmdb_id() + "\" target=\"_blank\">" + nameMap.getHmdb_id() + "</a>");
                }
                if (!nameMap.getChebi_id().equals("-")) {
                    nameMap.setChebi_id("<a href=http://www.ebi.ac.uk/chebi/searchId.do?chebiId=" + nameMap.getChebi_id() + " target='_blank'>" + nameMap.getChebi_id() + "</a>");
                }
                if (!nameMap.getKegg_id().equals("-")) {
                    nameMap.setKegg_id("<a href=http://www.genome.jp/dbget-bin/www_bget?" + nameMap.getKegg_id() + " target='_blank'>" + nameMap.getKegg_id() + "</a>");
                }
                if (!nameMap.getPubchem_id().equals("-")) {
                    nameMap.setPubchem_id("<a href=http://pubchem.ncbi.nlm.nih.gov/summary/summary.cgi?cid=" + nameMap.getPubchem_id() + " target='_blank'>" + nameMap.getPubchem_id() + "</a>");
                }
                if (!nameMap.getMetlin_id().equals("-")) {
                    nameMap.setMetlin_id("<a href=http://metlin.scripps.edu/metabo_info.php?molid=" + nameMap.getMetlin_id() + " target='_blank'>" + nameMap.getMetlin_id() + "</a>");
                }
            }
        }
        return nameMaps;
    }

    public String performNameMapping() {

        String[] qVec;
        if (queryList != null && queryList.trim().length() > 0) {
            qVec = DataUtils.getQueryNames(queryList, null);
        } else if (nmFile.getSize() > 0) {
            qVec = DataUtils.getQueryNames(nmFile, null);
        } else {
            sb.addMessage("Error", "Empty input?");
            return null;
        }

        //this is required even for API call (could be removed)
        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
              if (!ok) {
                return null;
            }
        }

       // useApiCalls = !ab.isOnProServer();

      
        if (ab.isOnProServer() || ab.isInDocker()|| ab.isOnLocalServer()) { //local
            RConnection RC = sb.getRConnection();
            try {
                RDataUtils.setMapData(RC, qVec);
                SearchUtils.crossReferenceExactGeneral(RC, inputType);
                setupNameMaps();
                return "Map Result";
            } catch (Exception e) {
                //e.printStackTrace();
                LOGGER.error("performNameMapping - local", e);
                return null;
            }
        } else { // try API

            RConnection RC = sb.getRConnection();
            try {
                RDataUtils.setMapData(RC, qVec);
                SearchUtils.crossReferenceAPI(RC, inputType);
                setupNameMaps();
                return "Map Result";
            } catch (Exception e) {
                //this should run when the microservice is down!
                try {
                    RDataUtils.setMapData(RC, qVec);
                    SearchUtils.crossReferenceExactGeneral(RC, inputType);
                    setupNameMaps();
                    return "Map Result";
                } catch (Exception er) {
                    //er.printStackTrace();
                    LOGGER.error("performNameMapping-Local", er);
                    return null;
                }
            }
        }

    
    }

    private String resultInfo = "";

    public String getResultInfo() {
        return resultInfo;
    }

    public void setResultInfo(String resultInfo) {
        this.resultInfo = resultInfo;
    }

    public void performLipidAnalysis() {

        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        resultInfo = "";

        if (lipidFile == null) {
            sb.addMessage("Error", "Please upload your file");
            return;
        }

        if (lipidFile.getSize() == 0) {
            sb.addMessage("Error", "File is empty");
            return;
        }

        if (!lipidFile.getFileName().endsWith(".csv")) {
            sb.addMessage("Error", "Only comma separated format (*.csv) will be accepted!");
            return;
        }

        if (!sb.isLoggedIn() || !sb.getAnalType().equals("utils")) {
            boolean ok = sb.doLogin("NA", "utils", false, false);
              if (!ok) {
                return;
            }
        }

        RConnection RC = sb.getRConnection();
        String fileName = DataUtils.uploadFile(lipidFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
        try {
            String rcmd = "analyze.lipids(\"" + fileName + "\", iso=\"" + isomerOpt + "\")";
            RCenter.recordRCommand(RC, rcmd, true);
            RC.voidEval(rcmd);
            setupFileDownloadTable(sb.getCurrentUser().getHomeDir());
        } catch (Exception e) {
            //sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
            LOGGER.error("performLipidAnalysis", e);
        }
    }

    private void setupFileDownloadTable(String homeDir) {
        User usr = sb.getCurrentUser(); // BHAN: added for deleteFile
        ArrayList<String> fileNames = new ArrayList();
        String tmpName = DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir());
        File folder = new File(homeDir);
        DataUtils.deleteFile(usr, "Download.zip"); // BHAN, added because it makes diskfull
        File[] listOfFiles = folder.listFiles();

        DataUtils.createZipFile(listOfFiles,"Download.zip", homeDir);

        for (File listOfFile : listOfFiles) {
            fileNames.add(listOfFile.getName());
        }

        fileNames.add(0, "Download.zip");

        String str = "<table width=\"250\" border=\"1\" cellpadding=\"3\">";
        for (int i = 0; i < fileNames.size(); i++) {
            str = str + "<tr><td>" + "<a href = \"/MetaboAnalyst/resources/users/" + tmpName + File.separator + fileNames.get(i) + "\">" + fileNames.get(i) + "</a>" + "</td></tr>";
        }
        str = str + "</table>";
        str = str + "<p> "
                + "Note: the \"Download.zip\" contains all the data. The \"_uplimit.csv\" contains the upper limit concentrations "
                + "for each corresponding lipid class. The \"_prob.csv\" contains the most probable conetration values."
                + "</p>";
        resultInfo = str;
    }

    private NameMapBean[] nameMaps = null;

    public NameMapBean[] getNameMapResult() {
        return nameMaps;
    }

    private String downloadMsg = "";

    public String getDownloadMsg() {
        return downloadMsg;
    }

    public void setDownloadMsg(String downloadMsg) {
        this.downloadMsg = downloadMsg;
    }

    private void setupNameMaps() {
        RConnection RC = sb.getRConnection();
        String[] mapRes = SearchUtils.getNameMapTable(RC);
        int row_count = SearchUtils.getNameMapRowNumber(RC);

        nameMaps = new NameMapBean[row_count];
        NameMapBean nameMap;
        for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
            nameMap = new NameMapBean();

            //walk through in order
            int count = 0;
            nameMap.setQuery(mapRes[i + count * row_count]);

            count++;
            nameMap.setHit(mapRes[i + count * row_count]);

            count++;
            nameMap.setHmdb_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setPubchem_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setChebi_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setKegg_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setMetlin_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setDetails(mapRes[i + count * row_count]);
            nameMaps[i] = nameMap;
        }
        downloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private NameMapBean[] candidateMaps;
    private String preTarget = "";

    public NameMapBean[] getCandidateMaps() {
        if (!target.equals(preTarget)) {
            RConnection RC = sb.getRConnection();
            SearchUtils.performDetailSearch(RC, target);
            String[] canList = SearchUtils.getCandidateList(RC);
            int row_count = SearchUtils.getCanListRowNumber(RC);
            candidateMaps = new NameMapBean[row_count];

            for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
                NameMapBean nameMap = new NameMapBean();
                //walk through in order
                nameMap.setQuery(target);
                int count = 0;
                nameMap.setHit(canList[i + count * row_count]);
                count++;
                nameMap.setHmdb_id(canList[i + count * row_count]);
                count++;
                nameMap.setPubchem_id(canList[i + count * row_count]);
                count++;
                nameMap.setChebi_id(canList[i + count * row_count]);
                count++;
                nameMap.setKegg_id(canList[i + count * row_count]);
                count++;
                nameMap.setMetlin_id(canList[i + count * row_count]);

                candidateMaps[i] = nameMap;
            }

            preTarget = target;
        }
        return candidateMaps;
    }

    public String prepareNameMapLine(NameMapBean map) {

        String line = "\"" + map.getQuery() + "\",\"" + map.getHit() + "\",\"" + map.getHmdb_id();
        line = line + "\",\"" + map.getPubchem_id() + "\",\"" + map.getChebi_id() + "\",\"" + map.getKegg_id() + "\",\"" + map.getMetlin_id();
        line = line + "\",\"" + map.getSmiles() + "\",";
        if ("".equals(map.getHit())) {
            line += "\"0\"\n";
        } else {
            line += "\"1\"\n";
        }
        return line;
    }

    public void writeMapTable(NameMapBean[] nameMaps) {
        NameMapBean[] nameMaps2 = new NameMapBean[nameMaps.length];
        for (int i = nameMaps2.length - 1; i >= 0; --i) {
            NameMapBean p = nameMaps[i];
            if (p != null) {
                nameMaps2[i] = new NameMapBean(p);
            }
        }
        nameMaps2 = DataUtils.removeHyperlinks(nameMaps2);
        try {
            try (FileWriter myWriter = new FileWriter(sb.getCurrentUser().getHomeDir() + "/name_map.csv")) {
                if (nameMaps2 != null) {
                    myWriter.write("""
                                   "Query","Match","HMDB","PubChem","ChEBI","KEGG","METLIN","SMILES","Comment"
                                   """);
                    for (NameMapBean nameMaps21 : nameMaps2) {
                        myWriter.write(prepareNameMapLine(nameMaps21));
                    }
                }
            }
        } catch (IOException ex) {
            //Logger.getLogger(UtilsBean.class.getName()).log(Level.SEVERE, null, ex);
            LOGGER.error("writeMapTable", ex);
        }
        downloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private String selectedHit;

    public String getSelectedHit() {
        return selectedHit;
    }

    public void setSelectedHit(String selectedHit) {
        this.selectedHit = selectedHit;
    }

    private String target = "";

    public String getTarget() {
        return target;
    }

    public void setCurrentCmpd(String cmpd) {
        target = DataUtils.getStringHTMLTag(cmpd);
    }

    public void selectCandidate() {
        for (NameMapBean candidateMap : candidateMaps) {
            if (candidateMap.isSelected() && !(candidateMap.getHit().equals("None of the above") || candidateMap.getHit().equals(""))) {

                useApiCalls = false;

                if (useApiCalls) {
                    int idx = 0;
                    for (NameMapBean nameMap : nameMaps) {
                        if (nameMap.getQuery().contains(candidateMap.getQuery())) {
                            break;
                        }
                        idx += 1;
                    }
                    nameMaps[idx] = candidateMap;
                    writeMapTable(nameMaps);
                } else {
                    int current_hitInx = SearchUtils.setCandidate(sb.getRConnection(), target, candidateMap.getHit());
                    nameMaps[current_hitInx - 1] = candidateMap;
                }
                return;
            }
        }
    }

}
