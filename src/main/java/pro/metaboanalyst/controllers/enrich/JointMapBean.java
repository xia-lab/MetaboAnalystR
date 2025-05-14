/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.enrich;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.NameMapBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RIntegUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.inject.Inject;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("jointMapBean")
public class JointMapBean implements Serializable {

    @Inject
    SessionBean1 sb;

    //for genes 
    private List<NameMapBean> geneNameMaps;

    public List<NameMapBean> getGeneNameMaps() {
        if (geneNameMaps == null) {
            setupGeneNameMaps();
        }
        return geneNameMaps;
    }

    public void setGeneNameMaps(List<NameMapBean> geneNameMaps) {
        this.geneNameMaps = geneNameMaps;
    }

    private String geneDownloadMsg = "";

    public String getGeneDownloadMsg() {
        return geneDownloadMsg;
    }

    public void setGeneDownloadMsg(String geneDownloadMsg) {
        this.geneDownloadMsg = geneDownloadMsg;
    }

    public void setupGeneNameMaps() {
        RConnection RC = sb.getRConnection();
        // int res = SearchUtils.performApproxSearch(RC, current_hitInx)
        String[] mapRes = SearchUtils.getGeneNameMapTable(RC);

        if (mapRes == null || mapRes.length == 0) {
            return;
        }

        int row_count = SearchUtils.getGeneNameMapRowNumber(RC);

        geneNameMaps = new ArrayList();

        NameMapBean nameMap;
        for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
            nameMap = new NameMapBean();

            //walk through in order
            int count = 0;
            nameMap.setQuery(mapRes[i + count * row_count]);

            count++;
            nameMap.setHit(mapRes[i + count * row_count]);

            count++;
            nameMap.setSymbol(mapRes[i + count * row_count]);

            count++;
            nameMap.setName(mapRes[i + count * row_count]);

            count++;
            nameMap.setDetails(mapRes[i + count * row_count]);
            geneNameMaps.add(nameMap);
        }
        geneDownloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/gene_name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private String targetGene = "";

    public String getTargetGene() {
        return targetGene;
    }

    public void setCurrentGeneRowInx(int inx) {
        targetGene = geneNameMaps.get(inx).getQuery();
    }

    //#####################################
    //for compounds
    private List<NameMapBean> cmpdNameMaps;

    public List<NameMapBean> getCmpdNameMapResult() {
        if (cmpdNameMaps == null) {
            setupCmpdNameMaps();
        }
        return cmpdNameMaps;
    }

    private String cmpdDownloadMsg = "";

    public String getCmpdDownloadMsg() {
        return cmpdDownloadMsg;
    }

    public void setCmpdDownloadMsg(String downloadMsg) {
        this.cmpdDownloadMsg = downloadMsg;
    }

    public void setupCmpdNameMaps() {
        RConnection RC = sb.getRConnection();
        String[] mapRes = SearchUtils.getNameMapTable(RC);

        if (mapRes == null || mapRes.length == 0) {
            return;
        }

        int row_count = SearchUtils.getNameMapRowNumber(RC);

        cmpdNameMaps = new ArrayList();

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
            nameMap.setKegg_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setDetails(mapRes[i + count * row_count]);
            cmpdNameMaps.add(nameMap);
        }
        cmpdDownloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private NameMapBean[] cmpdCandidateMaps;
    private String preTarget = "";

    public NameMapBean[] getCmpdCandidateMaps() {
        if (!targetCmpd.equals(preTarget)) {
            RConnection RC = sb.getRConnection();
            SearchUtils.performDetailSearch(RC, targetCmpd);
            String[] canList = SearchUtils.getCandidateList(RC);
            int row_count = SearchUtils.getCanListRowNumber(RC);
            cmpdCandidateMaps = new NameMapBean[row_count];
            for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
                NameMapBean nameMap = new NameMapBean();
                //walk through in order
                nameMap.setQuery(targetCmpd);
                int count = 0;
                nameMap.setHit(canList[i + count * row_count]);
                count++;
                nameMap.setHmdb_id(canList[i + count * row_count]);
                count++;
                nameMap.setPubchem_id(canList[i + count * row_count]);
                count++;
                nameMap.setKegg_id(canList[i + count * row_count]);
                cmpdCandidateMaps[i] = nameMap;
            }
            preTarget = targetCmpd;
        }
        return cmpdCandidateMaps;
    }

    private String selectedHit;

    public String getSelectedHit() {
        return selectedHit;
    }

    public void setSelectedHit(String selectedHit) {
        this.selectedHit = selectedHit;
    }

    private String targetCmpd = "";

    public String getTargetCmpd() {
        return targetCmpd;
    }

    public void setCurrentCmpdRowInx(int inx) {
        targetCmpd = DataUtils.getStringHTMLTag(cmpdNameMaps.get(inx).getQuery());
    }
    
    public void setTargetCmpd(String cmpd) {
        targetCmpd = cmpd;
    }
    
    public void selectCmpdCandidate() {
        String selectedNm = "";
        NameMapBean nmb = null;
        for (NameMapBean cmpdCandidateMap : cmpdCandidateMaps) {
            if (cmpdCandidateMap.isSelected()) {
                NameMapBean nmbn = cmpdCandidateMap;
                selectedNm = nmbn.getHit();
                nmb = cmpdCandidateMap;
                break;
            }
        }
        RConnection RC = sb.getRConnection();
        int current_cmpd_hitInx = SearchUtils.setCandidate(RC, targetCmpd, selectedNm);
        int inx = current_cmpd_hitInx - 1;
        cmpdNameMaps.set(inx, nmb);
    }

    public void deleteCmpd(int inx) {
        cmpdNameMaps.remove(inx);
        RIntegUtils.removeCmpd(sb.getRConnection(), inx + 1);
    }

    public void deleteGene(int inx) {
        geneNameMaps.remove(inx);
        RIntegUtils.removeGene(sb.getRConnection(), inx + 1);
    }

    public String prepareData() {
        RConnection RC = sb.getRConnection();
        int res = RIntegUtils.prepareIntegData(RC);
        IntegProcessBean ip = (IntegProcessBean) DataUtils.findBean("integProcesser");
        String datatype = ip.getDatatype();
        if (res == 0) {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            return null;
        } else if (datatype.equals("peak")) {
            //ip.PerformMummiInitPrediction();
            return "IntegAnalPeak";
        } else {
            return "IntegAnal";
        }
    }
}
