/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.mgwas;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.rwrappers.RDataUtils;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.MgwasUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Inject;
import java.io.File;

/**
 *
 * @author le
 */
@SessionScoped
@Named("mgwasLoadBean")
public class MgwasLoadBean implements Serializable {

    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;
    @Inject
    MgwasBean mb;

    private boolean proceedDisabled = true;
    private String[] diseaseOpts;
    private String selectedPopulation = "All";
    private String selectedDisease = "Age asthma diagnosed | ukb-b-4575";

    public boolean isProceedDisabled() {
        return proceedDisabled;
    }

    public void setProceedDisabled(boolean proceedDisabled) {
        this.proceedDisabled = proceedDisabled;
    }

    public String getSelectedDisease() {
        return selectedDisease;
    }

    public void setSelectedDisease(String selectedDisease) {
        this.selectedDisease = selectedDisease;
    }

    public String getSelectedPopulation() {
        return selectedPopulation;
    }

    public void setSelectedPopulation(String selectedPopulation) {
        this.selectedPopulation = selectedPopulation;
    }

    public List<String> completeTextDis(String query) {
        String queryLowerCase = query.toLowerCase();

        if (disCompleteList.isEmpty()) {
            String filePath = ab.getRealPath() + "/libs/ieugwas_202210.csv";  // Replace with your CSV file path
            List<String> diseases = new ArrayList<>();

            try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
                String line;
                br.readLine();  // Skip the header line

                while ((line = br.readLine()) != null) {
                    String[] values = line.split(",");
                    if (values.length > 1) {
                        String metabolite = values[2].replace("\"", "").trim() + " | " + values[1].replace("\"", "").trim();  // Remove quotes and trim whitespace
                        diseases.add(metabolite);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            diseaseOpts = new String[diseases.size()];
            for (int i = 0; i < diseases.size(); i++) {
                String metabolite = diseases.get(i);
                diseaseOpts[i] = metabolite;
            }
            disCompleteList = Arrays.asList(diseaseOpts);
        }

        return disCompleteList.stream().filter(t -> t.toLowerCase().contains(queryLowerCase)).collect(Collectors.toList());
    }

    private String org = "hsa";

    public String getOrg() {
        return org;
    }

    public void setOrg(String org) {
        this.org = org;
        sb.setOrg(org);
    }

    private boolean useListExample = false;

    public boolean isUseListExample() {
        return useListExample;
    }

    public void setUseListExample(boolean useListExample) {
        this.useListExample = useListExample;
    }

    private String idOpt = "rsid";

    public String getIdOpt() {
        return idOpt;
    }

    public void setIdOpt(String idOpt) {
        this.idOpt = idOpt;
    }

    private String myList = "";
    private String myDisList = "Age asthma diagnosed | ukb-b-4575";

    public String getMyDisList() {
        return myDisList;
    }

    public void setMyDisList(String myDisList) {
        this.myDisList = myDisList;
    }
    private String mySNPList;
    private String myCmpd;

    public String getMyCmpd() {
        //System.out.println("metaboanalyst.controllers.mgwas.MgwasLoadBean.getMyCmpdList()" + myCmpd);
        return myCmpd;
    }

    public void setMyCmpd(String myCmpd) {
        this.myCmpd = myCmpd;
    }

    public String getMySNPList() {
        return mySNPList;
    }

    public void setMySNPList(String mySNPList) {
        this.mySNPList = mySNPList;
    }

    public String getMyList() {
        return myList;
    }

    public void setMyList(String myList) {
        this.myList = myList;
    }

    public String goToNet() {
        return "networkview";
    }

    private String exampleMetList = "met_test1";

    public String getExampleMetList() {
        return exampleMetList;
    }

    public void setExampleMetList(String exampleMetList) {
        this.exampleMetList = exampleMetList;
    }

    //Using example data
    public void updateMetInput() {
        switch (exampleMetList) {
            case "met_test2" -> {
                setTissueType("blood");
                setPopulationType("all");
                //sb.setInputType("metabolite");
                //sb.setM2snpMapping(true);
                cmpdIDType = "hmdb";
            }
            case "met_test1" -> {
                setTissueType("blood");
                setPopulationType("all");
                //sb.setInputType("metabolite");
                //sb.setM2snpMapping(true);
                cmpdIDType = "name";
            }
            case "met_test_Ellinghaus" -> {
                setTissueType("blood");
                setPopulationType("eur");
                //sb.setInputType("metabolite");
                cmpdIDType = "name";
                //sb.setM2gStatMapping(true);
                //sb.setM2snpMapping(true);
            }
            default -> {
                //sb.setInputType("na");
                snpIDType = "na";
                myList = "";
                return;
            }
        }
        myList = DataUtils.readTextFile(ab.getInternalData(exampleMetList));
    }

    private String netOrg = "hsa"; //only hsa is supported

    public String getNetOrg() {
        return netOrg;
    }

    public void setNetOrg(String netOrg) {
        this.netOrg = netOrg;
    }

    private String snpList;

    public void setSnpList(String snpList) {
        this.snpList = snpList;
    }

    public String getSnpList() {
        return snpList;
    }

    private String tissueType = "all";
    private String populationType = "all";
    private String searchType = "rsid";
    private String selectedMrres = "mr_results";

    public String getSearchType() {
        return searchType;
    }

    public String getSelectedMrres() {
        return selectedMrres;
    }

    public void setSelectedMrres(String selectedMrres) {
        this.selectedMrres = selectedMrres;
    }

    public void setSearchType(String searchType) {
        this.searchType = searchType;
    }

    public String getPopulationType() {
        return populationType;
    }

    public void setPopulationType(String populationType) {
        this.populationType = populationType;
    }

    private String idType;

    public String getIdType() {
        return idType;
    }

    public void setIdType(String idType) {
        this.idType = idType;
    }

    public String getTissueType() {
        return tissueType;
    }

    public void setTissueType(String tissueType) {
        this.tissueType = tissueType;
    }

    private String snpIDType = "rsid";

    private String cmpdIDType = "name";

    public String getSnpIDType() {
        return snpIDType;
    }

    public void setSnpIDType(String snpIDType) {
        this.snpIDType = snpIDType;
    }

    public String getCmpdIDType() {
        return cmpdIDType;
    }

    public void setCmpdIDType(String cmpdIDType) {
        this.cmpdIDType = cmpdIDType;
    }

    private String cmpdList;

    public String getCmpdList() {
        return cmpdList;
    }

    public void setCmpdList(String cmpdList) {
        this.cmpdList = cmpdList;
    }

    private boolean useExample = false;

    public boolean isUseExample() {
        return useExample;
    }

    public void setUseExample(boolean useExample) {
        this.useExample = useExample;
    }

    public String getNetOpt() {
        return netOpt;
    }

    public void setNetOpt(String netOpt) {
        this.netOpt = netOpt;
    }

    private String netOpt = "composite";

    private SelectItem[] filterItem;

    public void setFilterItem(SelectItem[] filterItem) {
        this.filterItem = filterItem;
    }

    public SelectItem[] getFilterItem() {
        filterItem = new SelectItem[7];
        filterItem[0] = new SelectItem("name", "Metabolite");
        filterItem[1] = new SelectItem("rsid", "rsID");
        filterItem[2] = new SelectItem("chr", "Chr");
        filterItem[3] = new SelectItem("p_value", "P-value");
        filterItem[4] = new SelectItem("consequence", "Consequence");
        filterItem[5] = new SelectItem("symbol", "Gene");
        filterItem[6] = new SelectItem("pmid", "PMID");

        return filterItem;
    }

    private boolean shouldExecute = true;

    public boolean isShouldExecute() {
        return shouldExecute;
    }

    public void setShouldExecute(boolean shouldExecute) {
        this.shouldExecute = shouldExecute;
    }

    public String doMrAnalysis() {
        //sb.doMrAnalysis();
        //DataUtils.doMRResComputing(sb);
        return "mrresult";
    }

    public void openSNPMapDlg() {
        if (netOpt.equals("composite")) {
            PrimeFaces.current().executeScript("PF('snpMapDialog').show()");
        } else if (netOpt.equals("snp2met2dis")) {
            PrimeFaces.current().executeScript("PF('snp2met2disDialog').show()");
        }
    }

    private List<String> metCompleteList;

    DualListModel<String> metDualList = new DualListModel<>();

    public DualListModel<String> getMetDualList() {
        boolean isSourceEmpty = metDualList.getSource().isEmpty();
        boolean isTargetEmpty = metDualList.getTarget().isEmpty();
        if (isSourceEmpty && isTargetEmpty) {
            String[] metNames = buildMetList();
            List<String> completeList = Arrays.asList(metNames);
            List<String> targetList = new ArrayList<>();
            metDualList = new DualListModel<>(completeList, targetList);
        }
        return metDualList;
    }

    public String[] buildMetList() {

        String filePath = ab.getRealPath() + "/libs/mr_met_nms.txt";  // Replace with your CSV file path
        List<String> metabolites = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            br.readLine();  // Skip the header line

            while ((line = br.readLine()) != null) {
                String[] values = line.split("\t");
                if (values.length > 1) {
                    String metabolite = values[1].replace("\"", "").trim();  // Remove quotes and trim whitespace
                    metabolites.add(metabolite);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Convert List to Array
        String[] metabolitesArray = metabolites.toArray(new String[0]);

        return metabolitesArray;
    }

    public void setMetDualList(DualListModel<String> metDualList) {
        this.metDualList = metDualList;
    }

    public List<String> getMetCompleteList() {
        if (metCompleteList == null) {
            String[] metNames = buildMetList();
            metCompleteList = new ArrayList<>(Arrays.asList(metNames));
        }
        return metCompleteList;
    }

    public void setMetCompleteList(List<String> metCompleteList) {
        this.metCompleteList = metCompleteList;
    }
    private List<String> disCompleteList = new ArrayList<>();

    public List<String> getDisCompleteList() {
        return disCompleteList;
    }

    public void setDisCompleteList(List<String> disCompleteList) {
        this.disCompleteList = disCompleteList;
    }

    public boolean searchMgwasMetabolites() {
        proceedDisabled = true;

        // need to check the database for docker
        if (ab.isInDocker()) {
            String plink_path = "/home/glassfish/plink/";
            String[] plink_files = {"plink", "AFR.bed", "AFR.bim", "AFR.fam", "AMR.bed", "AMR.bim", "AMR.fam",
                "EAS.bed", "EAS.bim", "EAS.fam", "EUR.bed", "EUR.bim", "EUR.fam", "SAS.bed", "SAS.bim", "SAS.fam"};
            for (String f : plink_files) {
                File db_sqlite = new File(plink_path + f);
                if (!db_sqlite.exists()) {
                    sb.addMessage("Error", "Please make sure plink file " + f + " has been successfully attached into your docker!");
                    return false;
                }
            }
            System.out.println("All required database for mgwas exits in this docker!");
        }

        sb.setCmpdIDType("name");
        sb.doLogin("NA", "mgwas", false, false);

        mb.setDisplayExposure(myCmpd);
        sb.setDataUploaded();
        Boolean res = searchExposureItem(myCmpd);
        if (res) {
            sb.addMessage("info", "Exposure has been selected, you can proceed!");
            return true;
        }
        return false;
    }

    public String handleMgwasSelection() {
        if (myCmpd == null || "".equals(myCmpd.trim())) {
            sb.addMessage("error", "Please select a metabolite / exposure!");
            return null;
        }

        if (selectedDisease == null || "".equals(selectedDisease.trim())) {
            sb.addMessage("error", "Please select a disease / outcome!");
            return null;
        }

        if (searchMgwasMetabolites()) {
            if (searchOutcomeItem()) {
                //sb.setReportAvailable(false);
                //if (!mb.getSnpListMultiMet().isEmpty()) {
                //    DataUtils.setupFlashGrowl("warn", "There are " + mb.getSnpListMultiMet().size() + " SNPs that are associated with multiple metabolites (Horizontal Pleiotropy). It is advised not to include them!");
                //}
                mb.performSnpFiltering();
                return "MgwasParamView";
            }
        }
        return null;
    }

    public boolean searchOutcomeItem() {
        if (selectedDisease == null) {
            sb.addMessage("error", "Input is empty!");
            return false;
        }

        //String[] myDisListSplit = selectedDisease.split("\\|");
        //String myDisName = myDisListSplit[0];
        String myDisId = selectedDisease.split("\\|")[1];
        //System.out.println(myDisName);
        //System.out.println(myDisId + "====disId");
        //System.out.println("myDisName");
        //sb.setDisplayOutcome(myDisName.trim());
        RConnection RC = sb.getRConnection();
        int res = MgwasUtils.performOutcomeSearch(RC, myDisId);

        if (res == 0) {
            String msg = RDataUtils.getCurrentMsg(RC);
            sb.addMessage("error", msg);
            return false;
        }
        //setupTable("outcome");
        //doMirNetComputing();
        return true;
    }

    public static <T> T[] append(T[] arr, T element) {
        final int N = arr.length;
        arr = Arrays.copyOf(arr, N + 1);
        arr[N] = element;
        return arr;
    }

    private String ldR2 = "0.8";
    private String ldR2P = "0.8";

    public String getLdR2P() {
        return ldR2P;
    }

    public void setLdR2P(String ldR2P) {
        this.ldR2P = ldR2P;
    }

    public String getLdR2() {
        return ldR2;
    }

    public void setLdR2(String ldR2) {
        this.ldR2 = ldR2;
    }

    private String ldProxy = "None";
    private String ldProxyP = "None";

    public String getLdProxyP() {
        return ldProxyP;
    }

    public void setLdProxyP(String ldProxyP) {
        this.ldProxyP = ldProxyP;
    }

    public String getLdProxy() {
        return ldProxy;
    }

    public void setLdProxy(String ldProxy) {
        this.ldProxy = ldProxy;
    }

    public boolean searchExposureItem(String qVec) {

        //if (mode == 0 && sb.getCurrentPageID().equals("SNP mapping")) {
        //     return;
        // }
        int res = MgwasUtils.performExposureSearch(sb.getRConnection(), qVec);
        //System.out.println("ress=======" + res);
        if (res == 0) {
            String msg = RDataUtils.getCurrentMsg(sb.getRConnection());
            sb.addMessage("error", msg);
            return false;
        }
        //System.out.println("ress2=======" + res);
        mb.setupTable("exposure");

        return true;
    }

    public String getOutcome() {
        String disease = selectedDisease.split("\\|")[0].trim();
        String study = selectedDisease.split("\\|")[1].trim();
        String link = "<a style=\"font-weight:normal\" href=\"" + "https://gwas.mrcieu.ac.uk/datasets/?gwas_id__icontains=" + study + "\" target=\"_blank\">" + study + "</a>";
        String outcome = disease + " (Study: " + link + ")";
        return outcome;
    }

}
