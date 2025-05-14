/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.controllers.mgwas;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.MgwasUtils;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Inject;

/**
 *
 * @author zgy
 */

//for res tables
@SessionScoped
@Named("mgwasTableBean")
public class MgwasTableBean implements Serializable {

    @Inject
    SessionBean1 sb;
    private ArrayList<HashMap> resTable = null;
    private ArrayList<HashMap> resTableDis = null;
   
    private String selectedDisease = "";

    public String getSelectedDisease() {
        return selectedDisease;
    }

    public void setSelectedDisease(String selectedDisease) {
        this.selectedDisease = selectedDisease;
    }

    public ArrayList<HashMap> getResTableDis() {
        return resTableDis;
    }

    public void setResTableDis(ArrayList<HashMap> resTableDis) {
        this.resTableDis = resTableDis;
    }

    public void searchExposureItem(int mode) {

        //if (mode == 0 && sb.getCurrentPageID().equals("SNP mapping")) {
        //     return;
        // }
        int res = MgwasUtils.performExposureSearch(sb.getRConnection(), "");

        if (res == 0) {
            String msg = RDataUtils.getCurrentMsg(sb.getRConnection());
            sb.addMessage("error", msg);
            return;
        }

        //System.out.println(searchOptSelected);
        setupTable();

    }

    public void setupTable() {
        String netType = "exposure";
        RConnection RC = sb.getRConnection();
        resTable = new ArrayList();

        String[] rowNms = MgwasUtils.getResRowNames(RC, netType);
        //"Metabolite", "SNP ID", "Chr", "Position", "A1", "A2", "Beta","SE", "P-value", "PMID"
        String[] col1 = MgwasUtils.getResCol(RC, netType, 1); // metabolite name
        String[] col2 = MgwasUtils.getResCol(RC, netType, 2); // hmdb id
        String[] col4 = MgwasUtils.getResCol(RC, netType, 4); // snp
        String[] col5 = MgwasUtils.getResCol(RC, netType, 5); // chr
        String[] col6 = MgwasUtils.getResCol(RC, netType, 6); // pos
        String[] col10 = MgwasUtils.getResCol(RC, netType, 10); // beta
        String[] col11 = MgwasUtils.getResCol(RC, netType, 11); // p
        String[] col13 = MgwasUtils.getResCol(RC, netType, 13); // ea
        String[] col14 = MgwasUtils.getResCol(RC, netType, 14); // nea
        String[] col18 = MgwasUtils.getResCol(RC, netType, 18); // url
        String[] col19 = MgwasUtils.getResCol(RC, netType, 19); // se
        String[] pmid = MgwasUtils.getResCol(RC, netType, 15); // pmid

        int rowNum = rowNms.length;
        //sb.setEdgeNum(rowNms.length);
        String rowID;
        HashMap mb;
        for (int i = 0; i < rowNum; i++) {
            rowID = rowNms[i];
            mb = new HashMap<>();
            mb.put("rowID", rowID);
            // if HMDB ID is NA, then not link to HMDB; otherwise link to HMDB
            if ("NA".equals(col2[i])) {
                mb.put("col1", col1[i]);
            } else {
                mb.put("col1", "<a href=\"https://hmdb.ca/metabolites/" + col2[i] + "\" target=\"_blank\">" + col1[i] + "</a>");
            }
            mb.put("col2", "<a href=\"http://www.ncbi.nlm.nih.gov/snp/" + col4[i] + "\" target=\"_blank\">" + col4[i] + "</a>");
            mb.put("col3", col5[i]); // chr
            mb.put("col4", col6[i]); // pos
            mb.put("col5", col13[i]); // ea
            mb.put("col6", col14[i]); // nea
            mb.put("col7", col10[i]); // beta
            mb.put("col8", col19[i]); // se
            mb.put("col9", col11[i]); // p
            mb.put("col10", "<a style=\"font-weight:normal\" href=\"" + col18[i] + "\" target=\"_blank\">" + pmid[i] + "</a>");
            if (netType.equals("exposure")) {
                resTable.add(mb);
            } else {
                resTableDis.add(mb);
            }
        }
    }

    public void setupOutcomeTable() {
        String netType = "outcome";
        resTableDis = new ArrayList();

        RConnection RC = sb.getRConnection();
        String[] rowNms = MgwasUtils.getResRowNames(RC, netType);
        //"Metabolite", "SNP ID", "Chr", "Position", "A1", "A2", "Beta","SE", "P-value", "PMID"
        String[] col1 = MgwasUtils.getResCol(RC, netType, 1); // metabolite name
        String[] col2 = MgwasUtils.getResCol(RC, netType, 2); // hmdb id
        String[] col4 = MgwasUtils.getResCol(RC, netType, 4); // snp
        String[] col5 = MgwasUtils.getResCol(RC, netType, 5); // chr
        String[] col6 = MgwasUtils.getResCol(RC, netType, 6); // pos
        String[] col10 = MgwasUtils.getResCol(RC, netType, 10); // beta
        String[] col11 = MgwasUtils.getResCol(RC, netType, 11); // p
        String[] col13 = MgwasUtils.getResCol(RC, netType, 13); // ea
        String[] col14 = MgwasUtils.getResCol(RC, netType, 14); // nea
        String[] col18 = MgwasUtils.getResCol(RC, netType, 18); // url
        String[] col19 = MgwasUtils.getResCol(RC, netType, 19); // se
        String[] pmid = MgwasUtils.getResCol(RC, netType, 15); // pmid

        int rowNum = rowNms.length;
        //sb.setEdgeNum(rowNms.length);
        String rowID;
        HashMap mb;
        for (int i = 0; i < rowNum; i++) {
            rowID = rowNms[i];
            mb = new HashMap<>();
            mb.put("rowID", rowID);
            // if HMDB ID is NA, then not link to HMDB; otherwise link to HMDB
            if ("NA".equals(col2[i])) {
                mb.put("col1", col1[i]);
            } else {
                mb.put("col1", "<a href=\"https://hmdb.ca/metabolites/" + col2[i] + "\" target=\"_blank\">" + col1[i] + "</a>");
            }
            mb.put("col2", "<a href=\"http://www.ncbi.nlm.nih.gov/snp/" + col4[i] + "\" target=\"_blank\">" + col4[i] + "</a>");
            mb.put("col3", col5[i]); // chr
            mb.put("col4", col6[i]); // pos
            mb.put("col5", col13[i]); // ea
            mb.put("col6", col14[i]); // nea
            mb.put("col7", col10[i]); // beta
            mb.put("col8", col19[i]); // se
            mb.put("col9", col11[i]); // p
            mb.put("col10", "<a style=\"font-weight:normal\" href=\"" + col18[i] + "\" target=\"_blank\">" + pmid[i] + "</a>");
            resTable.add(mb);
        }
        setResTableDis(resTable);
    }

    private final String[] expHeaders = {"Metabolite", "SNP ID", "Chr", "Position", "A1", "A2", "Beta", "SE", "P-value", "PMID"};

    public String getColHeader(int inx) {
        return expHeaders[inx];
    }

    public ArrayList<HashMap> getResTable() {
        return resTable;
    }

    public void setResTable(ArrayList<HashMap> resTable) {
        this.resTable = resTable;
    }

    public DefaultStreamedContent getMetSnpTable() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/met_snp_restable.csv");
    }

    public DefaultStreamedContent getDisSnpTable() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/dis_snp_restable.csv");
    }

}
