/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Named;
import jakarta.faces.model.SelectItem;

/**
 *
 * @author xia
 */
@RequestScoped
@Named("optBean")
public class OptionBean {

    public SelectItem[] getDistMeasureOpts() {
        SelectItem[] distMeasureOpts = new SelectItem[6];
        distMeasureOpts[0] = new SelectItem("pearson", "Pearson r");
        distMeasureOpts[1] = new SelectItem("spearman", "Spearman rank correlation");
        distMeasureOpts[2] = new SelectItem("kendall", "Kendall rank correlation");
        distMeasureOpts[3] = new SelectItem("partial_pearson", "Pearson (partial)");
        distMeasureOpts[4] = new SelectItem("partial_spearman", "Spearman (partial)");
        distMeasureOpts[5] = new SelectItem("partial_kendall", "Kendall (partial)");
        return distMeasureOpts;
    }

    public SelectItem[] getTableTypeOpts() {
        SelectItem[] tableTypeOpts = new SelectItem[3];
        tableTypeOpts[0] = new SelectItem("conc", "Concentrations");
        tableTypeOpts[1] = new SelectItem("specbin", "Spectral bins");
        tableTypeOpts[2] = new SelectItem("pktable", "Peak intensities");
        return tableTypeOpts;
    }

    public SelectItem[] getMSMSDBOpts() {
        SelectItem[] msmsDBOpts = new SelectItem[12];
        msmsDBOpts[0] = new SelectItem("all", "All Database");
        msmsDBOpts[1] = new SelectItem("hmdb_exp", "HMDB Experimental");
        msmsDBOpts[2] = new SelectItem("hmdb_pre", "HMDB Predicted");
        msmsDBOpts[3] = new SelectItem("gnps", "GNPS");
        msmsDBOpts[4] = new SelectItem("mines", "MINEs");
        msmsDBOpts[5] = new SelectItem("lipidblast", "LIPIDBlast");
        msmsDBOpts[6] = new SelectItem("mona", "MoNA");
        msmsDBOpts[7] = new SelectItem("massbank", "MassBank");
        msmsDBOpts[8] = new SelectItem("riken", "RIKEN");
        msmsDBOpts[9] = new SelectItem("respect", "ReSpect");
        msmsDBOpts[10] = new SelectItem("msdial", "MS-DIAL");
        msmsDBOpts[11] = new SelectItem("bmdms", "BMDMS");
        return msmsDBOpts;
    }

    public SelectItem[] getIonsOpts() {
        SelectItem[] ionsOpts = new SelectItem[2];
        ionsOpts[0] = new SelectItem("positive", "Positive");
        ionsOpts[1] = new SelectItem("negative", "Negative");
        return ionsOpts;
    }

    public SelectItem[] getDoseOpts() {
        SelectItem[] clsOpts = new SelectItem[2];
        clsOpts[0] = new SelectItem("disc", "Repeated Dosing");
        clsOpts[1] = new SelectItem("cont", "Continuous Exposure");
        return clsOpts;
    }

    public SelectItem[] getUnitsOpts() {
        SelectItem[] unitsOpts = new SelectItem[2];
        unitsOpts[0] = new SelectItem("ppm", "PPM");
        unitsOpts[1] = new SelectItem("da", "Da");
        return unitsOpts;
    }

    public SelectItem[] getSimMethodOpts() {
        SelectItem[] simMethOpts = new SelectItem[2];
        simMethOpts[0] = new SelectItem(0, "Dot-product");
        simMethOpts[1] = new SelectItem(1, "Spectral entropy");
        return simMethOpts;
    }

    public SelectItem[] getDataSrcOpts() {
        SelectItem[] dataSrcOpts = new SelectItem[2];
        dataSrcOpts[0] = new SelectItem("mgf", "MGF");
        dataSrcOpts[1] = new SelectItem("msp", "MSP");
        //dataSrcOpts[1] = new SelectItem("mzmine", "mzMine format");
        return dataSrcOpts;
    }

    public SelectItem[] getCsvFormatOpts() {
        SelectItem[] csvFormatOpts = new SelectItem[4];
        csvFormatOpts[0] = new SelectItem("rowu", "Samples in rows (unpaired)");
        csvFormatOpts[1] = new SelectItem("colu", "Samples in columns (unpaired)");
        csvFormatOpts[2] = new SelectItem("rowp", "Samples in rows (paired)");
        csvFormatOpts[3] = new SelectItem("colp", "Samples in columns (paired)");
        return csvFormatOpts;
    }

    public SelectItem[] getTableFormatOpts() {
        SelectItem[] rocFormatOpts = new SelectItem[2];
        rocFormatOpts[0] = new SelectItem("rowu", "Samples in rows");
        rocFormatOpts[1] = new SelectItem("colu", "Samples in columns");
        return rocFormatOpts;
    }

    public SelectItem[] getClsOpts() {
        SelectItem[] clsOpts = new SelectItem[2];
        clsOpts[0] = new SelectItem("disc", "Categorical (Classification)");
        clsOpts[1] = new SelectItem("cont", "Continuous (Regression)");
        return clsOpts;
    }

    public SelectItem[] getClsShortenedOpts() {
        SelectItem[] clsOpts = new SelectItem[2];
        clsOpts[0] = new SelectItem("disc", "Categorical");
        clsOpts[1] = new SelectItem("cont", "Continuous");
        return clsOpts;
    }

    public SelectItem[] getGeneIDOpts() {
        SelectItem[] genericGeneIDOpts = new SelectItem[4];
        genericGeneIDOpts[0] = new SelectItem("NA", "--- Not Specified ---");
        genericGeneIDOpts[1] = new SelectItem("entrez", "Entrez ID");
        genericGeneIDOpts[2] = new SelectItem("symbol", "Official Gene Symbol");
        genericGeneIDOpts[3] = new SelectItem("uniprot", "Uniprot Protein ID");
        return genericGeneIDOpts;
    }

    //those with pathway annotations
    public SelectItem[] getPathCmpdIDOpts() {
        SelectItem[] cmpdIDOpts = new SelectItem[4];
        cmpdIDOpts[0] = new SelectItem("na", "--- Not Specified ---");
        cmpdIDOpts[1] = new SelectItem("name", "Compound Name");
        cmpdIDOpts[2] = new SelectItem("hmdb", "HMDB ID");
        cmpdIDOpts[3] = new SelectItem("kegg", "KEGG ID");
        return cmpdIDOpts;
    }

    public SelectItem[] getCmpdIDOpts() {
        SelectItem[] cmpdIDOpts = new SelectItem[7];
        cmpdIDOpts[0] = new SelectItem("name", "Compound names");
        cmpdIDOpts[1] = new SelectItem("hmdb", "HMDB ID");
        cmpdIDOpts[2] = new SelectItem("kegg", "KEGG ID");
        cmpdIDOpts[3] = new SelectItem("pubchem", "PubChem CID");
        cmpdIDOpts[4] = new SelectItem("chebi", "ChEBI ID");
        cmpdIDOpts[5] = new SelectItem("metlin", "METLIN");
        cmpdIDOpts[6] = new SelectItem("hmdb_kegg", "HMDB and KEGG ID");
        return cmpdIDOpts;
    }

    public SelectItem[] getNetIDOpts() {
        SelectItem[] netIDOpts = new SelectItem[5];
        netIDOpts[0] = new SelectItem("na", "--- Not Specified ---");
        netIDOpts[1] = new SelectItem("name", "Compound Name");
        netIDOpts[2] = new SelectItem("hmdb", "HMDB ID");
        netIDOpts[3] = new SelectItem("kegg", "KEGG ID");
        netIDOpts[4] = new SelectItem("pklist", "Peak List");
        return netIDOpts;
    }

    public SelectItem[] getPairAnalOpts() {
        SelectItem[] pairAnalOpts = new SelectItem[2];
        pairAnalOpts[0] = new SelectItem("FALSE", "Unpaired");
        pairAnalOpts[1] = new SelectItem("TRUE", "Paired");
        return pairAnalOpts;
    }

    public SelectItem[] getEqualVarOpts() {
        SelectItem[] equalVarOpts = new SelectItem[2];
        equalVarOpts[0] = new SelectItem("TRUE", "Equal");
        equalVarOpts[1] = new SelectItem("FALSE", "Unequal");
        return equalVarOpts;
    }

    public SelectItem[] getColorContrastOpts() {

        SelectItem[] colorContrastOpts = new SelectItem[9];
        colorContrastOpts[0] = new SelectItem("bwm", "Default");
        colorContrastOpts[1] = new SelectItem("viridis", "Viridis");
        colorContrastOpts[2] = new SelectItem("plasma", "Plasma");
        colorContrastOpts[3] = new SelectItem("npj", "NPJ");
        colorContrastOpts[4] = new SelectItem("aaas", "AAAS");
        colorContrastOpts[5] = new SelectItem("d3", "D3");
        colorContrastOpts[6] = new SelectItem("gbr", "Red / Green");
        colorContrastOpts[7] = new SelectItem("gray", "Gray Scale");
        colorContrastOpts[8] = new SelectItem("byr", "Red / Yellow / Blue");
        return colorContrastOpts;
    }

    public SelectItem[] getClustDistOpts() {
        SelectItem[] clustDistOpts = new SelectItem[3];
        clustDistOpts[0] = new SelectItem("euclidean", "Euclidean");
        clustDistOpts[1] = new SelectItem("spearman", "Spearman");
        clustDistOpts[2] = new SelectItem("pearson", "Pearson");
        return clustDistOpts;
    }

    public SelectItem[] getClustMethodOpts() {
        SelectItem[] clustMethodOpts = new SelectItem[4];
        clustMethodOpts[0] = new SelectItem("ward.D", "Ward");
        clustMethodOpts[1] = new SelectItem("average", "Average");
        clustMethodOpts[2] = new SelectItem("complete", "Complete");
        clustMethodOpts[3] = new SelectItem("single", "Single");
        return clustMethodOpts;
    }

    public SelectItem[] getMetaClustMethodOpts() {
        SelectItem[] metaClustMethodOpts = new SelectItem[3];
        metaClustMethodOpts[0] = new SelectItem("none", "None");
        metaClustMethodOpts[1] = new SelectItem("kmean", "K-means");
        metaClustMethodOpts[2] = new SelectItem("hierarchical", "Hierarchical");
        return metaClustMethodOpts;
    }

    public SelectItem[] getFeatOpts() {
        SelectItem[] featOpts = new SelectItem[3];
        featOpts[0] = new SelectItem("none", "--- Not Specified ---");
        featOpts[1] = new SelectItem("met", "Metabolites");
        featOpts[2] = new SelectItem("lipid", "Lipids");
        return featOpts;
    }
}
