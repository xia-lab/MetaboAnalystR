/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.enrich;

import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.PrimeFaces;
import jakarta.inject.Inject;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("enrichLoader")
public class EnrichUploadBean implements Serializable {

    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;
    private static final Logger LOGGER = LogManager.getLogger(EnrichUploadBean.class);

    /*
     * ORA analysis
     */
    private String msetOraList;

    public String getMsetOraList() {
        return msetOraList;
    }

    public void setMsetOraList(String msetOraList) {
        this.msetOraList = msetOraList;
    }

    private String featType = "none";

    public String getFeatType() {
        return featType;
    }

    public void setFeatType(String featType) {
        this.featType = featType;
    }

    private String exampleType = "none";

    public String getExampleType() {
        return exampleType;
    }

    public void setExampleType(String exampleType) {
        this.exampleType = exampleType;
    }

    public String handleOraListUpload() {
        if (!sb.doLogin("conc", "msetora", false, false)) {
            sb.addMessage("error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }

        if (msetOraList == null || msetOraList.trim().length() == 0) {
            sb.addMessage("error", "Error: the input is empty!");
            return null;
        }

        if (featType.equals("none")) {
            sb.addMessage("error", "Please specify Feature Type!");
            return null;
        }
         sb.setUploadType("list");
        String[] qVec = DataUtils.getQueryNames(msetOraList, null);
        RDataUtils.setMapData(sb.getRConnection(), qVec);
        if (featType.equals("lipid")) {
            SearchUtils.crossReferenceExactLipid(sb.getRConnection(), sb.getCmpdIDType());
        } else {
            SearchUtils.crossReferenceExact(sb.getRConnection(), sb.getCmpdIDType());
        }

        sb.setDataUploaded();
        return "Name check";
    }

    public void updateOraArea() {
        switch (exampleType) {
            case "met" -> {
                sb.setCmpdIDType("name");
                featType = "met";
                msetOraList = "Acetoacetic acid\nBeta-Alanine\nCreatine\nDimethylglycine\nFumaric acid\nGlycine\nHomocysteine\nL-Cysteine\n"
                        + "L-Isolucine\nL-Phenylalanine\nL-Serine\nL-Threonine\nL-Tyrosine\nL-Valine\nPhenylpyruvic acid\nPropionic acid\nPyruvic acid\nSarcosine";
            }
            case "lipid" -> {
                sb.setCmpdIDType("name");
                featType = "lipid";
                msetOraList = "CerP(d18:1/26:1)\nDG(18:0/15:0)\nDG(18:2/19:0)\nLysoPC(10:0)\nLysoPC(17:0)\nLysoPE(22:2)\nPA(18:1/18:0)\nPA(18:1/21:0)\n"
                        + "PA(20:4/20:0)\nPA(22:2/24:0)\nPA(22:6/18:1)\nPC(20:5/18:2)\nPC(P-18:0/18:1)\nPE(18:1/22:1)\nPE(18:2/16:0)\nPE(18:2/21:0)\n"
                        + "PE(18:2/22:1)\nPE(20:2/18:2)\nPE(20:3/20:2)\nPE(20:3/22:0)\nPE(20:4/18:0)\nPE(20:4/20:0)\nPE(P-16:0/18:0)\nPE(P-18:0/13:0)\n"
                        + "PE(P-18:0/17:0)\nPE(P-18:0/20:4)\nPE(P-18:0/20:5)\nPE(P-18:0/22:1)\nPE(P-20:0/22:6)\nPG(18:0/16:0)\nPG(18:1/18:0)\nPG(22:6/20:1)\n"
                        + "PI(18:2/18:1)\nPI(22:2/16:0)\nPS(18:0/21:0)\nPS(18:1/20:3)\nPS(18:1/22:0)\nPS(18:1/24:1)\nPS(18:2/22:1)\nPS(20:1/18:0)\nPS(20:3/21:0)\n"
                        + "PS(22:6/17:2)\nPS(22:6/18:0)\nSQDG(18:0/12:0)";
            }
            default -> {
                featType = "none";
                msetOraList = "";
            }
        }
    }

    /*
     * SSP analysis
     */
    private String msetSspData = "";

    public String getMsetSspData() {
        return msetSspData;
    }

    public void setMsetSspData(String msetSspData) {
        this.msetSspData = msetSspData;
    }

    private boolean useMsetSspExample = false;

    public boolean isUseMsetSspExample() {
        return useMsetSspExample;
    }

    public void setUseMsetSspExample(boolean useMsetSspExample) {
        this.useMsetSspExample = useMsetSspExample;
    }

    private String biofluidType;

    public String getBiofluidType() {
        return biofluidType;
    }

    public void setBiofluidType(String biofluidType) {
        this.biofluidType = biofluidType;
    }

    public void updateSspArea() {
        if (useMsetSspExample) {
            featType = "met";
            msetSspData = "L-Isolecine	0.34\nFumaric acid	0.47\nAcetone	0.58\nSuccinic acid	9.4\n1-Methylhistidine	9.6\n"
                    + "L-Asparagine	19.62\n3-Methylhistidine	9.7\nL-Threonine	93.19\nCreatine	720\ncis-Aconitic acid	14.39\n"
                    + "L-Tryptophan	35.78\nL-Carnitine	16.01\nL-Serine	17.32\nL-Tyrosine	67.51\nL-Alanine	219.02\n"
                    + "L-Fucose	20.37\nD-Glucose	23.92\nPyroglutamic acid	26.38\nFormic acid	26.72\nIndoxyl sulfate	34.21\n"
                    + "Dimethylamine	38.28\nEthanolamine	39.29\nGlycolic acid	41.39\nL-Glutamine	52.99\nL-Histidine	55.95\n"
                    + "Trigonelline	57.4\n3-Aminoisobutanoic acid	89.76\nTaurine	116\nGlycine	123.52\nTrimethylamine N-oxide	128.04\n"
                    + "Citric acid	225.31\nHippuric acid	278.53";
        } else {
            msetSspData = "";
            featType = "none";
        }
    }

    public String handleSspDataUpload() {
        if (!sb.doLogin("conc", "msetssp", false, false)) {
            sb.addMessage("error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        if (msetSspData != null && msetSspData.trim().length() > 0) {
            if (!RDataUtils.setSspData(sb.getRConnection(), msetSspData, biofluidType, null)) {
                return null;
            }
            sb.setDataUploaded();
            sb.initNaviTree("enrich-ssp");
            SearchUtils.crossReferenceExact(sb.getRConnection(), sb.getCmpdIDType());
            return "Name check";
        }
        return null;
    }

    /*
     * QEA analysis
     */
    private UploadedFile csvFile;

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    private String qeaClsOpt = "disc";

    public String getQeaClsOpt() {
        return qeaClsOpt;
    }

    public void setQeaClsOpt(String qeaClsOpt) {
        this.qeaClsOpt = qeaClsOpt;
    }

    private String dataFormat;

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    private String qeaTestDataOpt = "msetDis";

    public String getQeaTestDataOpt() {
        return qeaTestDataOpt;
    }

    public void setQeaTestDataOpt(String qeaTestDataOpt) {
        this.qeaTestDataOpt = qeaTestDataOpt;
    }

    public String handleQeaDataUpload() {
        try {
            if (!sb.doLogin("conc", "msetqea", qeaClsOpt.equals("cont"), false)) {
                sb.addMessage("error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
                return null;
            }
            String fileName = DataUtils.uploadFile(csvFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            sb.setDataUploaded();
            //sb.setCmpdIDType(cmpdIDType);
            sb.initNaviTree("enrich-qea");
            sb.setFeatType(featType);
            return processMsetQeaData(fileName, qeaClsOpt, dataFormat);
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("handleQesDataUpload", e);
            return null;
        }
    }

    public String msetQeaTestBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        String lblType, clsType, fileName, fileType;
        String useCachexia = "FALSE";
        switch (qeaTestDataOpt) {
            case "msetDis" -> {
                lblType = "name";
                clsType = "disc";
                fileName = ab.getResourceByAPI("human_cachexia.csv");
                fileType = "rowu";
                useCachexia = "TRUE";
            }
            case "conReq" -> {
                lblType = "pubchem";
                clsType = "cont";
                fileName = ab.getResourceByAPI("cachexia_continuous.csv");
                fileType = "rowu";
                useCachexia = "TRUE";
            }
            default -> {
                lblType = "name";
                clsType = "disc";
                fileName = ab.getResourceByAPI("multiple_sclerosis_test.csv");
                featType = "lipid";
                fileType = "colu";
            }
        }

        if (!sb.doLogin("conc", "msetqea", clsType.equals("cont"), false)) {
            sb.addMessage("error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        RDataUtils.setCachexiaTestSet(sb.getRConnection(), useCachexia);
        sb.setDataUploaded();
        sb.setCmpdIDType(lblType);
        sb.setFeatType(featType);

        return processMsetQeaData(fileName, clsType, fileType);
    }

    public String processMsetQeaData(String fileName, String clsType, String dataFormat) {
        RConnection RC = sb.getRConnection();
        if (RDataUtils.readTextData(RC, fileName, dataFormat, clsType)) {
            //double check for multiple class, this will be issue for large sample
            if (RDataUtils.getGroupNumber(RC) > 2) {
                sb.addMessage("error", "Enrichment analysis for multiple-group data is not well-defined. Please subset your data to two groups to proceed!");
                return null;
            }
            sb.initNaviTree("enrich-qea");
            return "Data check";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("error", "Failed to read in the CSV file." + err);
            return null;
        }
    }

    /*
     * Handle Metabolomics Workbench datasets
     */
    private String nmdrStudyId = "ST001301";

    public String getNmdrStudyId() {
        return nmdrStudyId;
    }

    public void setNmdrStudyId(String nmdrStudyId) {
        this.nmdrStudyId = nmdrStudyId;
    }

    public String enrichQea_workbench() {
        if (sb.doLogin("conc", "msetqea", false, false)) {
            try {
                RConnection RC = sb.getRConnection();

                if (RDataUtils.getMetabolomicsWorkbenchData(RC, nmdrStudyId)) {
                    if (RDataUtils.readMetabolomicsWorkbenchData(RC, nmdrStudyId, "rowu", "disc")) {
                        sb.setDataUploaded();
                        sb.setCmpdIDType("name");

                        if (RDataUtils.getGroupNumber(RC) > 2) {
                            sb.addMessage("error", "Enrichment analysis for multiple-group data is not well-defined. Please subset your data to two groups to proceed!");
                            return null;
                        }
                        sb.initNaviTree("enrich-qea");
                        return "Data check";
                    } else {
                        String err = RDataUtils.getErrMsg(RC);
                        sb.addMessage("error", "Failed to read in the txt file." + err);
                        return null;
                    }
                } else {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.addMessage("error", "Failed to retrieve study from Metabolomics Workbench!" + err);
                    return null;
                }
            } catch (Exception e) {
                // e.printStackTrace();
                LOGGER.error("enrichQea_workbench", e);
            }
        }
        return null;
    }

    /*
     * Handle Metabolone XLSX datasheet
     */
    private UploadedFile metabolonFile;

    public UploadedFile getMetabolonFile() {
        return metabolonFile;
    }

    public void setMetabolonFile(UploadedFile metabolonFile) {
        this.metabolonFile = metabolonFile;
    }

    private SelectItem[] MetaboloneMetaOpts, MetaboloneChemIDOpts;
    private String defaultMetafactor, defaultchemid;

    public String handleMetabolonQEA() {

        if (metabolonFile.getSize() == 0) {
            sb.addMessage("error", "File is empty!");
            return null;
        }

        String dataType = "conc";
        if (sb.doLogin(dataType, "msetqea", false, false)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadXLSXFile(metabolonFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

            if (RDataUtils.validateMetabolon(RC, fileName)) {
                String[] metaFactors, compoundIDs;

                metaFactors = RDataUtils.getMetabolonMetaFactors(RC, fileName);
                compoundIDs = RDataUtils.getMetabolonCMPDIDs(RC, fileName);
                if (metaFactors.length == 0) {
                    sb.addMessage("error", "'Sample Meta Data' sheet does not contains metadata information more than 2 categories!");
                    return null;
                }

                if (compoundIDs.length == 0) {
                    sb.addMessage("warn", "'Chemical Annotation' sheet does not contain supportted ID! Will use default feature label.");
                }

                SelectItem[] metaFactorsSelect = new SelectItem[metaFactors.length];
                SelectItem[] CMPDIDsSelect = new SelectItem[compoundIDs.length - 1];
                for (int i = 0; i < metaFactors.length; i++) {
                    metaFactorsSelect[i] = new SelectItem(metaFactors[i], metaFactors[i]);
                }
                int ii = 0;
                for (String compoundID : compoundIDs) {
                    if (compoundID.equals("HMDB")) {
                        continue;
                    }
                    CMPDIDsSelect[ii] = new SelectItem(compoundID, compoundID);
                    ii++;
                }
                setMetaboloneMetaOpts(metaFactorsSelect);
                setMetaboloneChemIDOpts(CMPDIDsSelect);
                setDefaultMetafactor(metaFactors[0]);
                setDefaultchemid("HMDB");

                PrimeFaces.current().executeScript("PF('metabolondata').show();");
                return null;
            } else {
                sb.addMessage("error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }

        return null;
    }

    public SelectItem[] getMetaboloneMetaOpts() {
        return MetaboloneMetaOpts;
    }

    public void setMetaboloneMetaOpts(SelectItem[] MetaboloneMetaOpts) {
        this.MetaboloneMetaOpts = MetaboloneMetaOpts;
    }

    public SelectItem[] getMetaboloneChemIDOpts() {
        return MetaboloneChemIDOpts;
    }

    public void setMetaboloneChemIDOpts(SelectItem[] MetaboloneChemIDOpts) {
        this.MetaboloneChemIDOpts = MetaboloneChemIDOpts;
    }

    public String getDefaultMetafactor() {
        if (defaultMetafactor == null) {
            RConnection RC = sb.getRConnection();
            if (RC == null) {
                return "NA";
            }
            String[] metaFactors, compoundIDs;

            metaFactors = RDataUtils.getquickMetabolonMetaFactors(RC);
            compoundIDs = RDataUtils.getquickMetabolonCMPDIDs(RC);

            if (metaFactors.length == 0) {
                sb.addMessage("error", "'Sample Meta Data' sheet does not contains metadata information more than 2 categories!");
                return null;
            }

            if (compoundIDs.length == 0) {
                sb.addMessage("warn", "'Chemical Annotation' sheet does not contain supportted ID! Will use default feature label.");
            }

            SelectItem[] metaFactorsSelect = new SelectItem[metaFactors.length];
            SelectItem[] CMPDIDsSelect = new SelectItem[compoundIDs.length - 1];
            for (int i = 0; i < metaFactors.length; i++) {
                metaFactorsSelect[i] = new SelectItem(metaFactors[i], metaFactors[i]);
            }
            int ii = 0;
            for (String compoundID : compoundIDs) {
                if (compoundID.equals("HMDB")) {
                    continue;
                }
                CMPDIDsSelect[ii] = new SelectItem(compoundID, compoundID);
                ii++;
            }
            setMetaboloneMetaOpts(metaFactorsSelect);
            setMetaboloneChemIDOpts(CMPDIDsSelect);
            setDefaultMetafactor(metaFactors[0]);
            setDefaultchemid("HMDB");
        }
        return defaultMetafactor;
    }

    public void setDefaultMetafactor(String defaultMetafactor) {
        this.defaultMetafactor = defaultMetafactor;
    }

    public String getDefaultchemid() {
        if (defaultchemid == null) {
            defaultchemid = "HMDB";
        }
        return defaultchemid;
    }

    public void setDefaultchemid(String defaultchemid) {
        this.defaultchemid = defaultchemid;
    }

    public String processMetabolonData() {
        RConnection RC = sb.getRConnection();
        boolean res = RDataUtils.parseMetabolonData(RC, getDefaultMetafactor(), getDefaultchemid());
        if (!res) {
            sb.addMessage("error", "Fail to format your data. Please check your data format or post your data to OmicsForum!");
            return null;
        }
        // check meta factor is over 2 or not
        boolean resx = RDataUtils.ValidateMetaFactor2Level(RC, getDefaultMetafactor());
        if (!resx) {
            sb.addMessage("error", "You cannot select a meta factor with over 2 levels. Enrichment analysis is designed for two-groups analysis!");
            return null;
        }

        // check chemid type : name, hmdb, kegg, pubchem
        String lblType = null;
        if (null == getDefaultchemid()) {
            sb.addMessage("error", getDefaultchemid() + " is not supported for now! Please select another ID type for enrichment analysis.");
            return null;
        } else switch (getDefaultchemid()) {
            case "HMDB" -> lblType = "hmdb";
            case "KEGG" -> lblType = "kegg";
            case "PUBCHEM" -> lblType = "pubchem";
            case "CHEMICAL_NAME" -> lblType = "name";
            default -> {
                sb.addMessage("error", getDefaultchemid() + " is not supported for now! Please select another ID type for enrichment analysis.");
                return null;
            }
        }

        sb.setCmpdIDType(lblType);

        // set feature type
        featType = "met";
        sb.setFeatType("met");

        if (RDataUtils.readTextData(RC, "metaboanalyst_input.csv", "rowu", "disc")) {
            sb.setDataUploaded();
            return "Data check";
        } else {
            sb.addMessage("error:", RDataUtils.getErrMsg(RC));
            return null;
        }
    }
    
}
