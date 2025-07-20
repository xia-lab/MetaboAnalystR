/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.controllers.mgwas;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.CompoundDataModel;
import pro.metaboanalyst.models.MRResult;
import pro.metaboanalyst.rwrappers.MgwasUtils;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Inject;
import java.util.Iterator;
import java.util.stream.Collectors;
import org.primefaces.PrimeFaces;

/**
 *
 * @author zgy
 */
@SessionScoped
@Named("mgwasBean")
public class MgwasBean implements Serializable {

    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;

    private boolean clumped = false;
    private boolean proxied = false;
    private boolean harmonized = false;

    private String ldclumpOpt = "no_ldclump";
    private String ldProxyOpt = "no_proxy";
    private boolean performClump = false;
    private boolean performProxy = false;

    private String harmonizeOpt = "1";
    private boolean pleiotropyOpt = false;
    private String[] methodOpts = {"mr_egger_regression", "mr_weighted_median", "mr_ivw", "mr_simple_mode", "mr_weighted_mode", "mr_wald_ratio"};
    private boolean ldProxies = true;
    private boolean pldSNPs = true;
    private double ldThresh = 0.8;
    private double mafThresh = 0.3;
    private boolean useSteiger = false;
    private String steigerOpt = "no_steiger";
    private boolean mrWaldRatio = true;
    private boolean mrTwoSampleMl = false;
    private boolean mrEggerRegression = true;
    private boolean mrEggerRegressionBootstrap = false;
    private boolean mrSimpleMedian = false;
    private boolean mrWeightedMedian = true;
    private boolean mrPenalisedWeightedMedian = false;
    private boolean mrIvw = true;
    private boolean mrIvwRadial = false;
    private boolean mrIvwMre = false;
    private boolean mrIvwFe = false;
    private boolean mrSimpleMode = true;
    private boolean mrWeightedMode = true;
    private boolean mrWeightedModeNome = false;
    private boolean mrSimpleModeNome = false;
    private boolean mrRaps = false;
    private boolean mrSign = false;
    private boolean mrUwr = false;
    private String filterCol = "pval";
    private String filterOpt = "min";
    private String filterActOpt = "remove";
    private String filterValue = "";
    private boolean filtPerformed = false;
    private String displayExposure;
    private String displayOutcome;

    public boolean isFiltPerformed() {
        return filtPerformed;
    }

    public void setFiltPerformed(boolean filtPerformed) {
        this.filtPerformed = filtPerformed;
    }

    public String getDisplayExposure() {
        return displayExposure;
    }

    public void setDisplayExposure(String displayExposure) {
        this.displayExposure = displayExposure;
    }

    public String getDisplayOutcome() {
        return displayOutcome;
    }

    public void setDisplayOutcome(String displayOutcome) {
        this.displayOutcome = displayOutcome;
    }

    public String getLdProxyOpt() {
        return ldProxyOpt;
    }

    public void setLdProxyOpt(String ldProxyOpt) {
        this.ldProxyOpt = ldProxyOpt;
    }

    public boolean isClumped() {
        return clumped;
    }

    public void setClumped(boolean clumped) {
        this.clumped = clumped;
    }

    public boolean isUseSteiger() {
        return useSteiger;
    }

    public void setUseSteiger(boolean useSteiger) {
        this.useSteiger = useSteiger;
        if (useSteiger) {
            steigerOpt = "use_steiger";
        } else {
            steigerOpt = "no_steiger";
        }
    }

    public boolean isProxied() {
        return proxied;
    }

    public void setProxied(boolean proxied) {
        this.proxied = proxied;
    }

    public boolean isHarmonized() {
        return harmonized;
    }

    public void setHarmonized(boolean harmonized) {
        this.harmonized = harmonized;
    }

    public boolean isPerformClump() {
        return performClump;
    }

    public void setPerformClump(boolean performClump) {
        this.performClump = performClump;
    }

    public boolean isPerformProxy() {
        return performProxy;
    }

    public void setPerformProxy(boolean performProxy) {
        this.performProxy = performProxy;
    }

    public String getSteigerOpt() {
        return steigerOpt;
    }

    public void setSteigerOpt(String steigerOpt) {
        this.steigerOpt = steigerOpt;
    }

    private ArrayList<HashMap> resTable = null;

    public ArrayList<HashMap> getResTable() {
        return resTable;
    }

    public void setResTable(ArrayList<HashMap> resTable) {
        this.resTable = resTable;
    }

    public boolean isMrWaldRatio() {
        return mrWaldRatio;
    }

    public void setMrWaldRatio(boolean mrWaldRatio) {
        this.mrWaldRatio = mrWaldRatio;
    }

    public boolean isMrTwoSampleMl() {
        return mrTwoSampleMl;
    }

    public void setMrTwoSampleMl(boolean mrTwoSampleMl) {
        this.mrTwoSampleMl = mrTwoSampleMl;
    }

    public boolean isMrEggerRegression() {
        return mrEggerRegression;
    }

    public void setMrEggerRegression(boolean mrEggerRegression) {
        this.mrEggerRegression = mrEggerRegression;
    }

    public boolean isMrEggerRegressionBootstrap() {
        return mrEggerRegressionBootstrap;
    }

    public void setMrEggerRegressionBootstrap(boolean mrEggerRegressionBootstrap) {
        this.mrEggerRegressionBootstrap = mrEggerRegressionBootstrap;
    }

    public boolean isMrSimpleMedian() {
        return mrSimpleMedian;
    }

    public void setMrSimpleMedian(boolean mrSimpleMedian) {
        this.mrSimpleMedian = mrSimpleMedian;
    }

    public boolean isMrWeightedMedian() {
        return mrWeightedMedian;
    }

    public void setMrWeightedMedian(boolean mrWeightedMedian) {
        this.mrWeightedMedian = mrWeightedMedian;
    }

    public boolean isMrPenalisedWeightedMedian() {
        return mrPenalisedWeightedMedian;
    }

    public void setMrPenalisedWeightedMedian(boolean mrPenalisedWeightedMedian) {
        this.mrPenalisedWeightedMedian = mrPenalisedWeightedMedian;
    }

    public boolean isMrIvw() {
        return mrIvw;
    }

    public void setMrIvw(boolean mrIvw) {
        this.mrIvw = mrIvw;
    }

    public boolean isMrIvwRadial() {
        return mrIvwRadial;
    }

    public void setMrIvwRadial(boolean mrIvwRadial) {
        this.mrIvwRadial = mrIvwRadial;
    }

    public boolean isMrIvwMre() {
        return mrIvwMre;
    }

    public void setMrIvwMre(boolean mrIvwMre) {
        this.mrIvwMre = mrIvwMre;
    }

    public boolean isMrIvwFe() {
        return mrIvwFe;
    }

    public void setMrIvwFe(boolean mrIvwFe) {
        this.mrIvwFe = mrIvwFe;
    }

    public boolean isMrSimpleMode() {
        return mrSimpleMode;
    }

    public void setMrSimpleMode(boolean mrSimpleMode) {
        this.mrSimpleMode = mrSimpleMode;
    }

    public boolean isMrWeightedMode() {
        return mrWeightedMode;
    }

    public void setMrWeightedMode(boolean mrWeightedMode) {
        this.mrWeightedMode = mrWeightedMode;
    }

    public boolean isMrWeightedModeNome() {
        return mrWeightedModeNome;
    }

    public void setMrWeightedModeNome(boolean mrWeightedModeNome) {
        this.mrWeightedModeNome = mrWeightedModeNome;
    }

    public boolean isMrSimpleModeNome() {
        return mrSimpleModeNome;
    }

    public void setMrSimpleModeNome(boolean mrSimpleModeNome) {
        this.mrSimpleModeNome = mrSimpleModeNome;
    }

    public boolean isMrRaps() {
        return mrRaps;
    }

    public void setMrRaps(boolean mrRaps) {
        this.mrRaps = mrRaps;
    }

    public boolean isMrSign() {
        return mrSign;
    }

    public void setMrSign(boolean mrSign) {
        this.mrSign = mrSign;
    }

    public boolean isMrUwr() {
        return mrUwr;
    }

    public void setMrUwr(boolean mrUwr) {
        this.mrUwr = mrUwr;
    }

    public double getMafThresh() {
        return mafThresh;
    }

    public void setMafThresh(double mafThresh) {
        this.mafThresh = mafThresh;
    }

    public double getLdThresh() {
        return ldThresh;
    }

    public void setLdThresh(double ldThresh) {
        this.ldThresh = ldThresh;
    }

    public boolean isPldSNPs() {
        return pldSNPs;
    }

    public void setPldSNPs(boolean pldSNPs) {
        this.pldSNPs = pldSNPs;
    }

    public boolean isLdProxies() {
        return ldProxies;
    }

    public void setLdProxies(boolean ldProxies) {
        this.ldProxies = ldProxies;
    }

    public String getLdclumpOpt() {
        return ldclumpOpt;
    }

    public void setLdclumpOpt(String ldclumpOpt) {
        this.ldclumpOpt = ldclumpOpt;
    }

    public String getHarmonizeOpt() {
        return harmonizeOpt;
    }

    public void setHarmonizeOpt(String harmonizeOpt) {
        this.harmonizeOpt = harmonizeOpt;
    }

    public String[] getMethodOpts() {
        return methodOpts;
    }

    public void setMethodOpts(String[] methodOpts) {
        this.methodOpts = methodOpts;
    }

    public String getFilterCol() {
        return filterCol;
    }

    public void setFilterCol(String filterCol) {
        this.filterCol = filterCol;
    }

    public String getFilterOpt() {
        return filterOpt;
    }

    public void setFilterOpt(String filterOpt) {
        this.filterOpt = filterOpt;
    }

    public String getFilterActOpt() {
        return filterActOpt;
    }

    public void setFilterActOpt(String filterActOpt) {
        this.filterActOpt = filterActOpt;
    }

    public String getFilterValue() {
        return filterValue;
    }

    public void setFilterValue(String filterValue) {
        this.filterValue = filterValue;
    }


    /*
    public void performLDClumping() {
        int res = MgwasUtils.performLDClumping(sb.getRConnection(), ldclumpOpt);
        if (ldclumpOpt.equals("no_ldclump")) {
            sb.addMessage("Info", "No LD clumping has been performed.");
            clumped = true;

        } else {
            if (res > -1) {
                sb.addMessage("Info", "Successful!, " + res + "SNP(s) had been removed after LD clumping!");
                if (res > 0) {
                    setupTable("exposure.ldp");
                }
                clumped = true;
            } else {
                sb.addMessage("Error", "LD Clumping has failed!");

            }
        }
    }
 
    public void performLDProxies() {
        int res = MgwasUtils.performLDProxies(sb.getRConnection(), ldProxies, ldThresh, pldSNPs, mafThresh);
        if (res > -1) {
            sb.addMessage("Info", "Successful!, " + res + "SNP(s) had been removed after LD proxy analysis!");
            if (res > 0) {
                setupTable("outcome.dat");
            }
            proxied = true;
        } else {
            sb.addMessage("Error", "LD proxy analysis has failed!");

        }

    }

    public void performHarmonization() {
        int res = MgwasUtils.performHarmonization(sb.getRConnection(), harmonizeOpt);
        if (res > 0) {
            sb.addMessage("Info", "Harmonization succeded!");
            harmonized = true;

        } else {
            sb.addMessage("Error", "Harmonization has failed!");
        }

    }
     */
    private String current_key = "";

    private boolean checkKey() {
        String ck = MgwasUtils.readOpenGWASKey(sb.getRConnection());
        if (!ck.equals("")) {
            current_key = ck;
            return true;
        }
        return false;
    }

    public void addMethodMessage(String value) {

        boolean include = false;
        switch (value) {
            case "Simple mode":
                include = mrSimpleMode;
                break;
            case "Inverse variance weighted (FE)":
                include = mrIvwFe;
                break;
            case "Inverse variance weighted (MRE)":
                include = mrIvwMre;
                break;
            case "Wald ratio":
                include = mrWaldRatio;
                break;
            case "Maximum likelihood":
                include = mrTwoSampleMl;
                break;
            case "MR Egger":
                include = mrEggerRegression;
                break;
            case "Simple median":
                include = mrSimpleMedian;
                break;
            case "Weighted median":
                include = mrWeightedMedian;
                break;
            case "Inverse variance weighted radial":
                include = mrIvwRadial;
                break;
            case "Weighted mode":
                include = mrWeightedMode;
                break;
            case "Weighted mode (NOME)":
                include = mrWeightedModeNome;
                break;
            case "Simple mode (NOME)":
                include = mrSimpleModeNome;
                break;
            case "Sign concordance test":
                include = mrSign;
                break;
            case "Unweighted regression":
                include = mrUwr;
                break;
            default:
                return;
        }

        if (include) {
            sb.addMessage("Info", "Will use this method: " + value + "!");
        } else {
            sb.addMessage("Info", "Will exclude this method: " + value + "!");
        }

    }

    private boolean userKeyGiven = false;

    public String getCurrent_key() {
        return current_key;
    }

    public void setCurrent_key(String current_key) {
        userKeyGiven = true;
        this.current_key = current_key;
    }

    private int rm_num_pleiotropy = 0;

    public void performSnpFiltering() {
        if (performClump) {
            ldclumpOpt = "use_ldclump";
        } else {
            ldclumpOpt = "no_ldclump";
        }

        if (performProxy) {
            ldProxyOpt = "use_proxy";
        } else {
            ldProxyOpt = "no_proxy";
        }

        int[] res;
        rm_num_pleiotropy = 0;
        if (ldProxyOpt.equals("use_proxy") & ((ldThresh != 0.8) | (mafThresh != 0.3))) {
            if (!userKeyGiven) {
                PrimeFaces.current().executeScript("PF('keyDialog').show()");
                return;
            } else {
                if (current_key.equals("")) {
                    sb.addMessage("Error", "The key are have uploaded is empty!");
                    return;
                }
                res = MgwasUtils.performSnpFiltering(sb.getRConnection(), ldclumpOpt, ldProxyOpt, ldProxies, ldThresh, pldSNPs, mafThresh, harmonizeOpt, steigerOpt, current_key);
           }

        } else {
            res = MgwasUtils.performSnpFiltering(sb.getRConnection(), ldclumpOpt, ldProxyOpt, ldProxies, ldThresh, pldSNPs, mafThresh, harmonizeOpt, steigerOpt, current_key);
      }

        if (res[0] > -1 | res[1] >-1) {
 
                setupTable("harmonized.dat");
                
            clumped = true;
            proxied = true;
            harmonized = true;

            if (!pleiotropyOpt) {
                int count = 0;
                ArrayList<String> snpRowIdsToRemove = new ArrayList<>();
                for (HashMap<String, Object> map : resTable) {

                    if (snpListMultiMet.contains(map.get("rsid").toString())) {
                        map.put("selected", false);
                        snpRowIdsToRemove.add(map.get("rowID").toString());
                        count++;
                    }
                }
                String[] entriesArray = snpRowIdsToRemove.toArray(new String[0]); // The argument specifies the type of the array

                int res2 = MgwasUtils.removeEntries(sb.getRConnection(), entriesArray);
                if (res2 > 0) {
                           sb.addMessage("Info", "Filtering & Harmonization succeeded! " + res[0] + " SNP(s) have been removed through LD-based and Allele Harmonization. "+ res[1] + " SNP(s) have been removed according to Steiger filtering. "  + rm_num_pleiotropy + " pleiotropic SNP(s) are excluded. You can furthur include/exclude SNPs below if necessary.");
                 } else if (res2 == 0) {
                    sb.addMessage("Info", "Filtering & Harmonization succeeded! No SNP has been removed. " + rm_num_pleiotropy + " pleiotropic SNP(s) are excluded. You can furthur include/exclude SNPs below if necessary.");
                 } else {
                    sb.addMessage("Error", "Unable to exclude " + Arrays.toString(entriesArray) + " from downstream analysis!");
                }
            } else {
                sb.addMessage("Info", "Harmonization has completed successfully! " + res + " SNP(s) had been removed!");
            }
        } else if (res[0] == -2) { // failed at proxy
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Harmonization has failed:" + err);
        } else {
            sb.addMessage("Error", "Harmonization has failed! Possible error: server is at capacity. Please try again later!");
        }

    }

    public String doMrAnalysis() {
        RConnection RC = sb.getRConnection();

//        String imgName1 = "mr_scatter_plot";
//        String imgName2 = "mr_forest_plot";
//        String imgName3 = "mr_leaveoneout_plot";
//        String imgName4 = "mr_funnel_plot";
        methodOpts = getSelectedMethods();
        MgwasUtils.setMRMethod(RC, methodOpts);

        int res = 0;

        res = MgwasUtils.performMRAnalysis(RC);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res == 0) {
            sb.addMessage("error", msg);
            return null;
        }

        setupMRResTable();
        // Loop through each exposure and generate the plots
        for (CompoundDataModel model : compoundDataModels.values()) {
            String exposure = model.getCompoundName();
            String imgName1 = sb.getNewImage(exposure + "_mr_scatter_plot");
            String imgName2 = sb.getNewImage(exposure + "_mr_forest_plot");
            String imgName3 = sb.getNewImage(exposure + "_mr_leaveoneout_plot");
            String imgName4 = sb.getNewImage(exposure + "_mr_funnel_plot");

            MgwasUtils.plotScatter(sb, exposure, imgName1, "png", 150);
            MgwasUtils.plotForest(sb, exposure, imgName2, "png", 150);
            MgwasUtils.plotLeaveOneOut(sb, exposure, imgName3, "png", 150);
            MgwasUtils.plotFunnel(sb, exposure, imgName4, "png", 150);
        }
        searchLiterature();
        return "MgwasResultView";
    }

    public void setupMRResTableOld() {
        String netType = "mr_results_merge";
        RConnection RC = sb.getRConnection();
        ArrayList<HashMap> resTable = new ArrayList();
        //String[] rowNms = MgwasUtils.getResRowNames(RC, netType);
        //Number of SNPs	Beta	SE	P value	Q	Q_df	Q_pval	Egger Intercept	SE	P value
        String[] col1 = MgwasUtils.getResCol(RC, netType, 1); // Number of SNPs
        String[] col2 = MgwasUtils.getResCol(RC, netType, 2); // Beta
        String[] col3 = MgwasUtils.getResCol(RC, netType, 3); // SE
        String[] col4 = MgwasUtils.getResCol(RC, netType, 4); // P value
        String[] col5 = MgwasUtils.getResCol(RC, netType, 5); // Q
        String[] col6 = MgwasUtils.getResCol(RC, netType, 6); // Q_df
        String[] col7 = MgwasUtils.getResCol(RC, netType, 7); // Q_pval
        String[] col8 = MgwasUtils.getResCol(RC, netType, 8); // Egger Intercept
        String[] col9 = MgwasUtils.getResCol(RC, netType, 9); // SE
        String[] col10 = MgwasUtils.getResCol(RC, netType, 10); // P value
        String[] rowNms = MgwasUtils.getResCol(RC, netType, 11); // Method
        String[] col12 = MgwasUtils.getResCol(RC, netType, 12); // CompoundName

        int rowNum = rowNms.length;
        String rowID;
        HashMap mb;
        for (int i = 0; i < rowNum; i++) {
            rowID = rowNms[i];
            mb = new HashMap<>();
            mb.put("rowID", rowID);
            // this needs to be a clickable link
            mb.put("col1", col1[i]); // number of SNPs
            mb.put("col2", col2[i]); // beta
            mb.put("col3", col3[i]); // se
            mb.put("col4", col4[i]); // p value
            mb.put("col5", col5[i]); // Q
            mb.put("col6", col6[i]);
            mb.put("col7", col7[i]);
            mb.put("col8", col8[i]);
            mb.put("col9", col9[i]);
            mb.put("col10", col10[i]);
            mb.put("compoundName", col12[i]);

            resTable.add(mb);
        }
        setResmrTable(resTable);
    }

    HashMap<String, CompoundDataModel> compoundDataModels = new HashMap<>();

    public HashMap<String, CompoundDataModel> getCompoundDataModels() {
        return compoundDataModels;
    }

    public void setCompoundDataModels(HashMap<String, CompoundDataModel> compoundDataModels) {
        this.compoundDataModels = compoundDataModels;
    }

    public void setupMRResTable() {
        String netType = "mr_results_merge";
        RConnection RC = sb.getRConnection();
        compoundDataModels = new HashMap<>();
        //String[] rowNms = MgwasUtils.getResRowNames(RC, netType);
        //Number of SNPs	Beta	SE	P value	Q	Q_df	Q_pval	Egger Intercept	SE	P value

        String[] col1 = MgwasUtils.getResCol(RC, netType, 1); // Number of SNPs
        String[] col2 = MgwasUtils.getResCol(RC, netType, 2); // Beta
        String[] col3 = MgwasUtils.getResCol(RC, netType, 3); // SE
        String[] col4 = MgwasUtils.getResCol(RC, netType, 4); // P value
        String[] col5 = MgwasUtils.getResCol(RC, netType, 5); // Q
        String[] col6 = MgwasUtils.getResCol(RC, netType, 6); // Q_df
        String[] col7 = MgwasUtils.getResCol(RC, netType, 7); // Q_pval
        String[] col8 = MgwasUtils.getResCol(RC, netType, 8); // Egger Intercept
        String[] col9 = MgwasUtils.getResCol(RC, netType, 9); // SE
        String[] col10 = MgwasUtils.getResCol(RC, netType, 10); // P value
        String[] rowNms = MgwasUtils.getResCol(RC, netType, 11); // Method
        String[] col12 = MgwasUtils.getResCol(RC, netType, 12); // CompoundName

        int rowNum = rowNms.length;
        String rowID;
        HashMap mb;
        for (int i = 0; i < rowNum; i++) {
            MRResult mrResult = new MRResult();
            mrResult.setRowID(i + "");
            mrResult.setNumberOfSNPs(col1[i]);
            mrResult.setBeta(col2[i]);
            mrResult.setSe(col3[i]);
            mrResult.setPvalue(col4[i]);
            mrResult.setQ(col5[i]);
            mrResult.setQdf(col6[i]);
            mrResult.setQpval(col7[i]);
            mrResult.setEggerIntercept(col8[i]);
            mrResult.setMethod(rowNms[i]);
            mrResult.setSe2(col9[i]);
            mrResult.setPvalue2(col10[i]);
            String compoundName = col12[i]; // Assuming this is the compound name
            compoundDataModels.putIfAbsent(compoundName, new CompoundDataModel(compoundName));
            compoundDataModels.get(compoundName).addMrResult(mrResult);
        }

    }

    private ArrayList<HashMap> resmrTable = null;

    public ArrayList<HashMap> getResmrTable() {
        return resmrTable;
    }

    public void setResmrTable(ArrayList<HashMap> resmrTable) {
        this.resmrTable = resmrTable;
    }

    public String[] getSelectedMethods() {
        List<String> selectedMethods = new ArrayList<>();

        if (mrWaldRatio) {
            selectedMethods.add("mr_wald_ratio");
        }
        if (mrTwoSampleMl) {
            selectedMethods.add("mr_two_sample_ml");
        }
        if (mrEggerRegression) {
            selectedMethods.add("mr_egger_regression");
        }
        if (mrEggerRegressionBootstrap) {
            selectedMethods.add("mr_egger_regression_bootstrap");
        }
        if (mrSimpleMedian) {
            selectedMethods.add("mr_simple_median");
        }
        if (mrWeightedMedian) {
            selectedMethods.add("mr_weighted_median");
        }
        if (mrPenalisedWeightedMedian) {
            selectedMethods.add("mr_penalised_weighted_median");
        }
        if (mrIvw) {
            selectedMethods.add("mr_ivw");
        }
        if (mrIvwRadial) {
            selectedMethods.add("mr_ivw_radial");
        }
        if (mrIvwMre) {
            selectedMethods.add("mr_ivw_mre");
        }
        if (mrIvwFe) {
            selectedMethods.add("mr_ivw_fe");
        }
        if (mrSimpleMode) {
            selectedMethods.add("mr_simple_mode");
        }
        if (mrWeightedMode) {
            selectedMethods.add("mr_weighted_mode");
        }
        if (mrWeightedModeNome) {
            selectedMethods.add("mr_weighted_mode_nome");
        }
        if (mrSimpleModeNome) {
            selectedMethods.add("mr_simple_mode_nome");
        }
        if (mrRaps) {
            selectedMethods.add("mr_raps");
        }
        if (mrSign) {
            selectedMethods.add("mr_sign");
        }
        if (mrUwr) {
            selectedMethods.add("mr_uwr");
        }

        return selectedMethods.toArray(new String[0]);
    }

    private ArrayList<HashMap> harmonizedTable = null;

    public ArrayList<HashMap> getHarmonizedTable() {
        return harmonizedTable;
    }

    public void setHarmonizedTable(ArrayList<HashMap> harmonizedTable) {
        this.harmonizedTable = harmonizedTable;
    }

    private String tableType = "";

    public String getTableType() {
        return tableType;
    }

    public void setTableType(String tableType) {
        this.tableType = tableType;
    }

    private ArrayList<String> snpListMultiMet = new ArrayList<>();

    public void setupTable(String netType) {
        RConnection RC = sb.getRConnection();

        resTable = new ArrayList();
        tableType = netType;
        String snpNm;
        String hmdbIdNm;
        String metNm;
        String metsNm = "metabolites";
        String symbolNm = "genes";
        String entrezNm = "gene_id";
        String pvalNm = "P-value";
        String pvalOut = "pval.outcome";
        String[] keepInx = null;
        String[] ifCheck = null;
        String[] pvaloutcome = null;
        String[] pvalStn = null;
        switch (netType) {
            case "exposure" -> {
                snpNm = "SNP";
                hmdbIdNm = "HMDB";
                metNm = "Common Name";
            }
            case "harmonized.dat" -> {
                snpNm = "SNP";
                hmdbIdNm = "id.exposure";
                metNm = "exposure";
                keepInx = MgwasUtils.getResColByName(RC, netType, "mr_keep");
                pvalNm = "pval.exposure";
                pvalOut = "pval.outcome";
            }
            case "outcome.dat" -> {
                snpNm = "SNP";
                hmdbIdNm = "id.exposure";
                metNm = "exposure";
                keepInx = MgwasUtils.getResColByName(RC, netType, "mr_keep.outcome");
                 if (filtPerformed) {
                    pvalOut = "pval.outcome";
                }
            }
            case "tableView" -> {
                snpNm = "SNP";
                hmdbIdNm = "HMDB";
                metNm = "exposure";
                keepInx = MgwasUtils.getResColByName(RC, netType, "mr_keep");
                ifCheck = MgwasUtils.getResColByName(RC, netType, "ifCheck");
            }
            default -> {
                snpNm = "SNP";
                hmdbIdNm = "id.exposure";
                metNm = "exposure";
            }
        }

        String[] rowNms = MgwasUtils.getResRowNames(RC, netType);
        //"Metabolite", "SNP ID", "Chr", "Position", "A1", "A2", "Beta","SE", "P-value", "PMID"
        //String[] col1 = MgwasUtils.getResColByName(RC, netType, metNm); // metabolite name
        String[] col2 = MgwasUtils.getResColByName(RC, netType, hmdbIdNm); // hmdb id
        String[] snp = MgwasUtils.getResColByName(RC, netType, snpNm); // rs id
        String[] col8 = MgwasUtils.getResColByName(RC, netType, metNm); // common name
        String[] metabolites = MgwasUtils.getResColByName(RC, netType, "metabolites"); // se
        String[] symbol = MgwasUtils.getResColByName(RC, netType, "genes"); // se
        String[] entrez = MgwasUtils.getResColByName(RC, netType, "gene_id"); // se
        String[] pval = MgwasUtils.getResColByName(RC, netType, pvalNm); // p
        String[] url = MgwasUtils.getResColByName(RC, netType, "URL"); // p
        String[] pmid = MgwasUtils.getResColByName(RC, netType, "PMID"); // p
        String[] pop = MgwasUtils.getResColByName(RC, netType, "pop_code"); // p
        String[] biofluid = MgwasUtils.getResColByName(RC, netType, "biofluid"); // p
       
        if (filtPerformed) {

            pvaloutcome = MgwasUtils.getResColByName(RC, netType, pvalOut); // p
            if ("use_steiger".equals(steigerOpt)) {

                pvalStn = MgwasUtils.getResColByName(RC, netType, "steiger_pval");
            }

        }


        int rowNum = rowNms.length;
        //sb.setEdgeNum(rowNms.length);
        String rowID;
        //String lastGroupKey = null;
        HashMap mb;
        snpListMultiMet = new ArrayList<>();
        groupCounts = new HashMap<>();

        for (int i = 0; i < rowNum; i++) {
            rowID = rowNms[i];
            mb = new HashMap<>();

            if (netType.equals("harmonized.dat") || netType.equals("outcome.dat") || netType.equals("tableView")) {
                //  System.out.println("==============.MgwasBean.setupTable()"+keepInx[i]);
                if (keepInx[i].equals("FALSE") & !pleiotropyOpt) {
                    //rm_num_pleiotropy++;
                    continue;
                } else if (keepInx[i].equals("FALSE")) {
                    mb.put("selected", false);
                    mb.put("expanded", false);
                } else {
                    mb.put("selected", true);
                    mb.put("expanded", false);
                }
            }
            if (ifCheck != null) {
                mb.put("selected", ifCheck[i]);

            }

            mb.put("rowID", rowID);
            mb.put("group", col8[i]);
            // if HMDB ID is NA, then not link to HMDB; otherwise link to HMDB
            if ("NA".equals(col2[i])) {
                mb.put("col1", col8[i]);
            } else {
                mb.put("col1", "<a href=\"https://hmdb.ca/metabolites/" + col2[i] + "\" target=\"_blank\">" + col8[i] + "</a>");
            }
            mb.put("col2", "<a href=\"http://www.ncbi.nlm.nih.gov/snp/" + snp[i] + "\" target=\"_blank\">" + snp[i] + "</a>");

            mb.put("pval", pval[i]); // p
            if (filtPerformed) {
                mb.put("pvaloutcome", pvaloutcome[i]); // p
                if ("use_steiger".equals(steigerOpt)) {
                    mb.put("pvalsteiger", pvalStn[i]); // p
                    mb.put("dir", "TRUE"); // p
                }
            }

            mb.put("study", "<a style=\"font-weight:normal\" href=\"" + url[i] + "\" target=\"_blank\">" + pmid[i] + "</a>");
            mb.put("col11", processCommaDelimitedString(metabolites[i], snp[i])); // associated metabolites
            mb.put("col12", "<a href=\"https://www.ncbi.nlm.nih.gov/gene/" + entrez[i] + "\" target=\"_blank\">" + symbol[i] + "</a>"); // associated genes
            mb.put("pop", makeReadable(pop[i])); // p
            mb.put("biofluid", makeReadable(biofluid[i])); // p

            mb.put("rsid", snp[i]); // chr
            resTable.add(mb);

            String groupKey = mb.get("col1") + ""; // Replace with your grouping field
            // If this is the first group key, set expanded to true
            groupCounts.put(groupKey, groupCounts.getOrDefault(groupKey, 0) + 1);

        }
        // Set the expanded property for the last group
    }

    private String makeReadable(String popCode) {
        String popName;
        switch (popCode) {
            case "eas" ->
                popName = "East Asian";
            case "afr" ->
                popName = "African";
            case "hsp" ->
                popName = "Hispanic";
            case "eur" ->
                popName = "European";
            case "sas" ->
                popName = "South Asian";
            case "ami" ->
                popName = "Amish";
            case "mde" ->
                popName = "Middle Eastern";
            case "amr", "mix" ->
                popName = "Mixed";
            default -> {
                popName = popCode.substring(0, 1).toUpperCase() + popCode.substring(1);
            }

        }
        return popName;
    }

    private Map<String, Integer> groupCounts = new HashMap<>();

    public int getGroupCount(String groupKey) {
        return groupCounts.getOrDefault(groupKey, 0);
    }

    public String processCommaDelimitedString(String input, String snp) {
        if (!input.contains(",")) {
            return input; // Return the string as-is if only one metabolite 
        }

        String[] elements = input.split(",");
        int elementCount = elements.length;
        int charCount = 0;
        int includedElements = 0;
        rm_num_pleiotropy++;
        // Determine how many elements can be included without exceeding 50 characters
        for (String element : elements) {
            if (charCount + element.length() + (includedElements > 0 ? 1 : 0) > 50) {
                break;
            }
            charCount += element.length() + (includedElements > 0 ? 1 : 0); // Add 1 for the comma
            includedElements++;
        }

        // Construct the truncated string with the additional element count
        String truncated = String.join(",", Arrays.copyOfRange(elements, 0, includedElements));
        int extraCount = elementCount - includedElements;
        snpListMultiMet.add(snp);
        return truncated + " ... (" + extraCount + " more)";
    }

    public ArrayList<String> getSnpListMultiMet() {
        return snpListMultiMet;
    }

    //delete entry in R
    public void handleSelect(HashMap<String, Object> map) {
        Object val = map.get("selected");
        boolean selected = false;

        if (val instanceof Boolean) {
            selected = (Boolean) val;
        } else if (val instanceof String) {
            selected = Boolean.parseBoolean((String) val);
        }
        System.out.println("=========.MgwasBean.handleSelect()" + selected);
        if (!selected) {
            int res = MgwasUtils.addEntry(sb.getRConnection(), (String) map.get("rowID"));
            if (res == 1) {
                sb.addMessage("Info", "Included " + map.get("rsid") + " from downstream analysis!");
            } else {
                sb.addMessage("Error", "Unable to include " + map.get("rsid") + " from downstream analysis!");
            }
        } else {
            int res = MgwasUtils.removeEntry(sb.getRConnection(), (String) map.get("rowID"));
            if (res == 1) {
                sb.addMessage("Info", "Excluded " + map.get("rsid") + " from downstream analysis!");
            } else {
                sb.addMessage("Error", "Unable to exclude " + map.get("rsid") + " from downstream analysis!");
            }
        }
    }

    public DefaultStreamedContent downloadCSVFile(String tableName) {
        MgwasUtils.saveMgwasResult(sb.getRConnection(), tableName);
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/" + tableName + ".csv");
    }

    public String getCurrentMRImageURL(String compoundName, String name) {
        name = compoundName + "_" + name;

        if (sb.getImgMap().keySet().contains(name)) {
            String nm_img = ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(name) + "dpi150.png";
            return nm_img;
        } else {
            return ab.getRootContext() + "/resources/images/temp.png";
        }
    }

    public void graphicsLnk_action_mr(String cmpdName, String code) {
        selectedCmpdName = cmpdName;
        System.out.println(cmpdName + "_" + code);
        sb.graphicsLnk_action(cmpdName + "_" + code);
    }

    private String selectedCmpdName = "";

    public String getSelectedCmpdName() {
        return selectedCmpdName;
    }

    public void setSelectedCmpdName(String selectedCmpdName) {
        this.selectedCmpdName = selectedCmpdName;
    }

    public boolean isPleiotropyOpt() {
        return pleiotropyOpt;
    }

    public void setPleiotropyOpt(boolean pleiotropyOpt) {
        this.pleiotropyOpt = pleiotropyOpt;
    }

    public void searchLiterature() {
        int res = MgwasUtils.performLiteratureSearch(sb.getRConnection(), getDisplayExposure(), getDisplayOutcome());

        res = setupLitEvidenceTable();
        if (res == 0) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "No hits found in the literarure evidence database."));

        }

    }

    //delete both entry in both Java and R
    public void deleteLitEntry(String rowID) {
        Iterator<HashMap> it = reslitTable.iterator();
        while (it.hasNext()) {
            if (it.next().get("rowID").equals(rowID)) {
                it.remove();
                break;
            }
        }
        RDataUtils.removeEntry(sb.getRConnection(), rowID);
    }

    private ArrayList<HashMap> reslitTable = null;

    public ArrayList<HashMap> getReslitTable() {
        return reslitTable;
    }

    public void setReslitTable(ArrayList<HashMap> reslitTable) {
        this.reslitTable = reslitTable;
    }

    private String pmIDs;

    public String getPmIDs() {
        return pmIDs;
    }

    public void setPmIDs(String pmIDs) {
        this.pmIDs = pmIDs;
    }

    private int edgeNum = 0;

    public int getEdgeNum() {
        return edgeNum;
    }

    public void setEdgeNum(int edgeNum) {
        this.edgeNum = edgeNum;
    }

    public int setupLitEvidenceTable() {
        RConnection RC = sb.getRConnection();

        ArrayList<HashMap> resTable = new ArrayList();
        String[] rowNms = MgwasUtils.getPathRowNames(RC);
        if (rowNms == null) {
            return 0;
        }
        //"Exposure","Exposure_Subject","Exposure_Predicate", "Exposure_Pval","Exposure_PMIDs",  "Overlap", "Outcome_Predicate", "Outcome_Object", "Outcome_Pval", "Outcome_PMIDs","Outcome"
        String[] col1 = MgwasUtils.getPathCol(RC, 1); // 
        String[] col2 = MgwasUtils.getPathCol(RC, 2); // 

        int rowNum = rowNms.length;
        if (rowNum == 0) {
            return 0;
        }

        setEdgeNum(rowNms.length);
        String rowID;
        HashMap mb;
        for (int i = 0; i < rowNum; i++) {
            rowID = rowNms[i];
            mb = new HashMap<>();
            mb.put("rowID", rowID);
            mb.put("col1", col1[i]);
            // col 5 for pmid
            String[] res = col2[i].split(",\\s*");
            //System.out.println("metaboanalyst.controllers.mgwas.MgwasBean.setupLitEvidenceTable()"+res);
            String url = "<a href=\"http://www.ncbi.nlm.nih.gov/pubmed/" + res[0] + "\" target=\"_blank\">" + res[0] + "</a>";
            for (int m = 1; m < res.length; m++) {
                url = url + ", " + "<a href=\"http://www.ncbi.nlm.nih.gov/pubmed/" + res[m] + "\" target=\"_blank\">" + res[m] + "</a>";
            }
            // System.out.println("metaboanalyst.controllers.mgwas.MgwasBean.setupLitEvidenceTable()"+url);
            mb.put("col2", url);

            resTable.add(mb);
        }
        setReslitTable(resTable);
        return 1;
    }

    private ArrayList<HashMap> sumTable = null;

    public ArrayList<HashMap> getSumTable() {
        return sumTable;
    }

    public void setSumTable(ArrayList<HashMap> sumTable) {
        this.sumTable = sumTable;
    }

    public void setupSumTable() {
        RConnection RC = sb.getRConnection();
        sumTable = new ArrayList();

        String[] rowNms = MgwasUtils.getSumCol(RC, "rw", "");
        String[] exps = MgwasUtils.getSumCol(RC, "exps", "");
        //"Metabolite", "SNP ID", "Chr", "Position", "A1", "A2", "Beta","SE", "P-value", "PMID"
        //String[] col1 = MgwasUtils.getResColByName(RC, netType, metNm); // metabolite name
        int rowNum = rowNms.length;
        //System.out.println("============.mgwas.MgwasBean.setupSumTable()"+rowNum);
        groupCounts = new HashMap<>();
        HashMap sumb;

        for (int i = 0; i < rowNum; i++) {
            sumb = new HashMap<>();
            String[] counts = MgwasUtils.getSumCol(RC, "num", exps[i]);
            sumb.put("rowID", rowNms[i]);
            sumb.put("col1", exps[i]);
            sumb.put("col2", counts[0]);
            sumTable.add(sumb);

        }
    }

    //only one exposure
    public ArrayList<HashMap> doSNPViewing() {
        setupTable("tableView");
        filteredResTable = resTable;
        return filteredResTable;
    }

    private String selectedGroupKey; // set by UI (e.g. dropdown or URL param)

    public String getSelectedGroupKey() {
        return selectedGroupKey;
    }

    public void setSelectedGroupKey(String selectedGroupKey) {
        this.selectedGroupKey = selectedGroupKey;
    }

    private ArrayList<HashMap> filteredResTable = null;

    public ArrayList<HashMap> getFilteredResTable() {
        return filteredResTable;
    }

    public void setFilteredResTable(ArrayList<HashMap> filteredResTable) {
        this.filteredResTable = filteredResTable;
    }

    public ArrayList<HashMap> filteredResTable() {
        ArrayList<HashMap> filtered = resTable.stream()
                .filter(row -> {
                    Object key = row.get("group");
                    String normRow = key == null ? "" : key.toString().trim();
                    String normSelected = selectedGroupKey == null ? "" : selectedGroupKey.trim();
                    boolean match = normSelected.equals(normRow);
                    if (!match) {
                        System.out.println("🚫 no match: [" + normSelected + "] vs [" + normRow + "]");
                    }
                    return match;
                })
                .map(row -> (HashMap) row)
                .collect(Collectors.toCollection(ArrayList::new)); // explicitly collect to ArrayList

        return filtered;
    }

    private SelectItem[] filterItem;

    public void setFilterItem(SelectItem[] filterItem) {
        this.filterItem = filterItem;
    }

    public SelectItem[] getFilterItem() {

        filterItem = new SelectItem[5];
        filterItem[0] = new SelectItem("pval", "P-value");
        filterItem[1] = new SelectItem("Biofluid", "Biofluid");
        filterItem[2] = new SelectItem("Population", "Population");
        filterItem[3] = new SelectItem("Study", "Study");

        return filterItem;
    }

    public void updateEntries() {
        System.out.println("metaboanalyst.controllers.mgwas.MgwasBean.updateEntries()=====================");
        RConnection RC = sb.getRConnection();

        if (filterOpt.equals("min")) {
            try {
                Double.valueOf(filterValue);
            } catch (NumberFormatException e) {
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Please enter a numeric!"));
                return;
            }
        }
        String[] rowIDs;

        rowIDs = MgwasUtils.updateSNPEntries(RC, filterCol, filterOpt, filterValue, filterActOpt, selectedGroupKey);

        if (rowIDs[0].equals("NA")) {
            if (filterOpt.equals("min")) {
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_WARN, "Warining", "No node was removed based on the cutoff"));
            } else {
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "No node names match your input."));
            }
        } else {
            setupTable("tableView");
            filteredResTable = resTable;
            setupSumTable();
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", "A total of " + rowIDs.length
                            + " SNP have been removed."));
        }
    }

    public void resetSNPTable() {
        RConnection RC = sb.getRConnection();
        int res = MgwasUtils.resetSNPEntries(RC, selectedGroupKey);
        if (res == 1) {
            setPerformClump(false);
            setPerformProxy(false);
            setHarmonizeOpt("1");
            setPleiotropyOpt(false);
            setUseSteiger(false);
            setupTable("tableView");
            filteredResTable = resTable;
            setupSumTable();
        } else {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", ""));
        }

    }

    public String checkSNP() {
        RConnection RC = sb.getRConnection();
        int res = MgwasUtils.checkSNPs(RC);

        if (res == 1) {
            return ("MR method");

        } else {

            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "No SNP has been selected for the analysis. Please check at least one SNP for furthur analysis."));

            return null;
        }

    }
}
