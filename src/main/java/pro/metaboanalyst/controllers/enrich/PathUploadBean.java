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
@Named("pathLoader")
public class PathUploadBean implements Serializable {

    @Inject
    ApplicationBean1 ab;
    @Inject
    SessionBean1 sb;
    private static final Logger LOGGER = LogManager.getLogger(PathUploadBean.class);

    private boolean usePathListExample;

    public boolean isUsePathListExample() {
        return usePathListExample;
    }

    public void setUsePathListExample(boolean usePathListExample) {
        this.usePathListExample = usePathListExample;
    }

    private String pathOraList;

    public String getPathOraList() {
        return pathOraList;
    }

    public void setPathOraList(String pathOraList) {
        this.pathOraList = pathOraList;
    }

    public void updatePathListArea() {
        if (usePathListExample) {
            pathOraList = "Acetoacetic acid\nBeta-Alanine\nCreatine\nDimethylglycine\nFumaric acid\nGlycine\nHomocysteine\nL-Cysteine\n"
                    + "L-Isolucine\nL-Phenylalanine\nL-Serine\nL-Threonine\nL-Tyrosine\nL-Valine\nPhenylpyruvic acid\nPropionic acid\nPyruvic acid\nSarcosine";
            sb.setCmpdIDType("name");
        } else {
            pathOraList = "";
            sb.setCmpdIDType("na");
        }
    }

    public String handlePathListUpload() {

        if (!sb.doLogin("conc", "pathora", false, false)) {
            sb.addMessage("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        if (pathOraList == null || pathOraList.trim().length() == 0) {
            sb.addMessage("Error", "Empty input!");
            return null;
        } else {
            if (sb.getCmpdIDType().equals("na")) {
                sb.addMessage("Error", "Please specify the ID type for your data input!");
                return null;
            } else {
                sb.setUploadType("list");
                RConnection RC = sb.getRConnection();
                String[] qVec = DataUtils.getQueryNames(pathOraList, null);
                RDataUtils.setMapData(RC, qVec);
                SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());
                sb.setDataUploaded();

                return "Name check";
            }
        }
    }

    private String qeaTestDataOpt = "disc";

    public String getQeaTestDataOpt() {
        return qeaTestDataOpt;
    }

    public void setQeaTestDataOpt(String qeaTestDataOpt) {
        this.qeaTestDataOpt = qeaTestDataOpt;
    }

    private UploadedFile csvFile;

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    private String clsOpt = "disc";

    public String getClsOpt() {
        return clsOpt;
    }

    private String dataFormat;

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    public void setClsOpt(String clsOpt) {
        this.clsOpt = clsOpt;
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

    public String pathQea_workbench() {
        if (sb.doLogin("conc", "pathqea", false, false)) {
            try {
                RConnection RC = sb.getRConnection();

                if (RDataUtils.getMetabolomicsWorkbenchData(RC, nmdrStudyId)) {
                    if (RDataUtils.readMetabolomicsWorkbenchData(RC, nmdrStudyId, "rowu", "disc")) {
                        sb.setDataUploaded();
                        sb.setCmpdIDType("name");

                        if (RDataUtils.getGroupNumber(RC) > 2) {
                            sb.addMessage("Error", "Enrichment analysis for multiple-group data is not well-defined. Please subset your data to two groups to proceed!");
                            return null;
                        }
                        sb.initNaviTree("pathway-qea");
                        return "Data check";
                    } else {
                        String err = RDataUtils.getErrMsg(RC);
                        sb.addMessage("Error", "Failed to read in the txt file." + err);
                        return null;
                    }
                } else {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.addMessage("Error", "Failed to retrieve study from Metabolomics Workbench!" + err);
                    return null;
                }
            } catch (Exception e) {
                // e.printStackTrace();
                LOGGER.error("pathQea_workbench", e);
            }
        }
        return null;
    }

    public String pathQeaExampleBn_action() {
        if (!sb.doLogin("conc", "pathqea", false, false)) {
            sb.addMessage("Error", "Failed to log in!");
            return null;
        }
        sb.setDataUploaded();

        if (qeaTestDataOpt.equals("disc")) {
            sb.setCmpdIDType("name");
            return processPathQeaData(ab.getResourceByAPI("human_cachexia.csv"), "rowu", "disc");
        } else {
            sb.setCmpdIDType("pubchem");
            return processPathQeaData(ab.getResourceByAPI("cachexia_continuous.csv"), "rowu", "cont");
        }
    }

    public String pathQeaBn_action() {

        if (sb.getCmpdIDType().equalsIgnoreCase("na")) {
            sb.addMessage("Error", "Please specify the ID type for your data input!");
            return null;
        }

        if (!sb.doLogin("conc", "pathqea", clsOpt.equals("cont"), false)) {
            sb.addMessage("Error", "Failed to log in!");
            return null;
        }
        try {

            if (csvFile == null) {
                sb.addMessage("Error", "Please upload your file!");
                return null;
            }

            if (csvFile.getSize() == 0) {
                sb.addMessage("Error", "File is empty!");
                return null;
            }

            String fileName = DataUtils.getJustFileName(csvFile.getFileName());
            DataUtils.uploadFile(sb, csvFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());
            sb.setDataUploaded();
            //sb.setCmpdIDType(qeaCmpdIDType);
            return processPathQeaData(fileName, dataFormat, clsOpt);
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("pathQeaBn_action", e);
            return null;
        }

    }

    private String processPathQeaData(String fileName, String dataFormat, String lblType) {
        RConnection RC = sb.getRConnection();
        if (RDataUtils.readTextData(RC, fileName, dataFormat, lblType)) {
            if (RDataUtils.getGroupNumber(RC) > 2) {
                sb.addMessage("Error", "Enrichment analysis for multiple-group data is "
                        + "not well-defined. Please subset your data to two groups to proceed!");
                return null;
            }
            //SearchUtils.crossReferenceExact(sb.getRConnection(), cmpdType);
            sb.initNaviTree("pathway-qea");
            return "Data check";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", "Failed to read in the CSV file." + err);
            return null;
        }
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
            sb.addMessage("Error", "File is empty!");
            return null;
        }

        String dataType = "conc";
        if (sb.doLogin(dataType, "pathqea", false, false)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadXLSXFile(sb, metabolonFile, sb.getCurrentUser().getHomeDir(), null, ab.isOnProServer());

            if (RDataUtils.validateMetabolon(RC, fileName)) {
                String[] metaFactors, compoundIDs;

                metaFactors = RDataUtils.getMetabolonMetaFactors(RC, fileName);
                compoundIDs = RDataUtils.getMetabolonCMPDIDs(RC, fileName);
                if (metaFactors.length == 0) {
                    sb.addMessage("Error", "'Sample Meta Data' sheet does not contains metadata information more than 2 categories!");
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
                setDefaultchemid("NA");

                PrimeFaces.current().executeScript("PF('metabolondata').show();");
                return null;
            } else {
                sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
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
                sb.addMessage("Error", "'Sample Meta Data' sheet does not contains metadata information more than 2 categories!");
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
            sb.addMessage("Error", "Fail to format your data. Please check your data format or post your data to OmicsForum!");
            return null;
        }
        // check meta factor is over 2 or not
        boolean resx = RDataUtils.ValidateMetaFactor2Level(RC, getDefaultMetafactor());
        if (!resx) {
            sb.addMessage("Error", "You cannot select a meta factor with over 2 levels. Enrichment analysis is designed for two-groups analysis!");
            return null;
        }

        // check chemid type : name, hmdb, kegg, pubchem
        String lblType = null;
        if (null == getDefaultchemid()) {
            sb.addMessage("Error", getDefaultchemid() + " is not supported for now! Please select another ID type for enrichment analysis.");
            return null;
        } else {
            switch (getDefaultchemid()) {
                case "HMDB":
                    lblType = "hmdb";
                    break;
                case "KEGG":
                    lblType = "kegg";
                    break;
                case "PUBCHEM":
                    lblType = "pubchem";
                    break;
                case "CHEMICAL_NAME":
                    lblType = "name";
                    break;
                default:
                    sb.addMessage("Error", getDefaultchemid() + " is not supported for now! Please select another ID type for enrichment analysis.");
                    return null;
            }
        }

        sb.setCmpdIDType(lblType);

        if (RDataUtils.readTextData(RC, "metaboanalyst_input.csv", "rowu", "disc")) {
            sb.setDataUploaded();
            return "Data check";
        } else {
            sb.addMessage("Error:", RDataUtils.getErrMsg(RC));
            return null;
        }
    }

}
