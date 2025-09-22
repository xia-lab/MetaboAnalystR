/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.enrich;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.CmpdBean;
import pro.metaboanalyst.models.NameMapBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.REnrichUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.ProcessBean;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("mapBean")
public class MappingBean implements Serializable {

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
    private ProcessBean pcb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private String[] selectedCmpdList = null;
    private NameMapBean[] nameMaps = null;
    private int row_count;

    public NameMapBean[] getNameMapResult() {
        if (sb.getCurrentUser() == null) {
            return null;
        }

        if (nameMaps == null) {
            setupNameMaps();
        }
        pcb.setNameMaps(nameMaps);
        return nameMaps;
    }

    public void setSelectedCmpdList(String[] selectedCmpdList) {
        this.selectedCmpdList = selectedCmpdList;
    }

    private String downloadMsg = "";

    public String getDownloadMsg() {
        return downloadMsg;
    }

    public void setDownloadMsg(String downloadMsg) {
        this.downloadMsg = downloadMsg;
    }

    public int getRow_count() {
        return row_count;
    }

    public void setRow_count(int row_count) {
        this.row_count = row_count;
    }

    public void setupNameMaps() {

        /*
        if (sb.getCurrentUser() == null || sb.isShowTabView()) {
            return;
        }**/
        RConnection RC = sb.getRConnection();
        // int res = SearchUtils.performApproxSearch(RC, current_hitInx)
        String[] mapRes = SearchUtils.getNameMapTable(RC);
        row_count = SearchUtils.getNameMapRowNumber(RC);
        pcb.setRow_count(row_count);
        NameMapBean nameMap = new NameMapBean();
        if (mapRes == null || row_count == 0) {
            nameMaps = new NameMapBean[1];
            nameMaps[0] = nameMap;
        } else {
            nameMaps = new NameMapBean[row_count];
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
                nameMaps[i] = nameMap;
            }
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
                nameMap.setKegg_id(canList[i + count * row_count]);
                candidateMaps[i] = nameMap;
            }
            preTarget = target;
        }
        return candidateMaps;
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

    //note this cmpd contain html tags, need to parse out
    public void setCurrentCmpd(String cmpd) {
        target = DataUtils.getStringHTMLTag(cmpd);
    }

    public void selectCandidate() {
        for (NameMapBean candidateMap : candidateMaps) {
            if (candidateMap.isSelected() && !(candidateMap.getHit().equals("None of the above") || candidateMap.getHit().equals(""))) {
                int current_hitInx = SearchUtils.setCandidate(sb.getRConnection(), target, candidateMap.getHit());
                nameMaps[current_hitInx - 1] = candidateMap;
                return;
            }
        }
    }

    public String nextBn_action() {
        //only keep those real match and manually specified match
        return switch (sb.getAnalType()) {
            case "msetssp" ->
                "msetssp";
            case "msetora" ->
                "enrichparam";
            case "pathora" ->
                "pathparam";
            default -> {  //table input
                if (!sb.isMissingDisabled()) {
                    yield "Data filter";
                } else {
                    yield "Normalization";
                }
            }
        };
    }

    private CmpdBean[] cmpdBeans = null;

    public CmpdBean[] getCmpdBeans() {
        if (cmpdBeans == null) {
            RConnection RC = sb.getRConnection();
            REnrichUtils.doSSPTest(RC);
            String[] nms = REnrichUtils.getSSPNames(RC);
            String[] concs = REnrichUtils.getSSPConcs(RC);
            String[] hmdbs = REnrichUtils.getSSP_HMDB(RC);
            String[] refConcs = REnrichUtils.getSSPRefConcs(RC);
            String[] states = REnrichUtils.getSSPStates(RC);
            String[] details = REnrichUtils.getSSPdetails(RC);
            cmpdBeans = new CmpdBean[concs.length];

            for (int i = 0; i < concs.length; i++) {
                cmpdBeans[i] = new CmpdBean(nms[i], concs[i], hmdbs[i], refConcs[i], states[i], details[i]);
            }
        }
        return cmpdBeans;
    }

    public String getSspDetailTxt() {

        RConnection RC = sb.getRConnection();
        String[] concs = RDataUtils.getSSPConcs(RC, target);
        String[] refs = RDataUtils.getSSPReferences(RC, target);
        String[] pmids = RDataUtils.getSSP_Pmids(RC, target);
        String[] notes = RDataUtils.getSSP_Notes(RC, target);

        String str;
        if (concs.length == 1 && concs[0].equalsIgnoreCase("NA")) {
            str = "<font face=\"Arial, Helvetica, Geneva\" size =\"2\">Concentration information is unavailable</font>";
        } else {
            str = "<table width=\"550\" border=\"1\" cellpadding=\"2\" align=\"left\">";
            str = str + "<tr><th width=\"15%\">Study</th><th width=\"25%\">Concentration</th><th width=\"50%\">Reference</th><th width=\"10%\">Note</th></tr>";
            for (int i = 0; i < concs.length; i++) {
                str = str + "<tr>"
                        + "<td>" + "Study " + (i + 1) + "</td>"
                        + "<td>" + concs[i] + "</td>";
                if (pmids[i].equalsIgnoreCase("N/A")) {
                    str = str + "<td>" + refs[i] + "</td>";
                } else {
                    str = str + "<td>" + refs[i] + " (<a href = \"http://www.ncbi.nlm.nih.gov/pubmed/" + pmids[i] + "\">" + "Pubmed" + "</a>) " + "</td>";
                }

                if (notes[i].equalsIgnoreCase("NA")) {
                    str = str + "<td>" + " " + "</td>";
                } else {
                    str = str + "<td>" + notes[i] + "</td>";
                }
                str = str + "</tr>";
            }
            str = str + "</table>";
        }
        return str;
    }

    public String getSspImg() {
        String sspImgName = REnrichUtils.plotCmpdConcRange(sb.getRConnection(), target, "png", 150);
        String url = ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sspImgName;
        return url;
    }

    public void selectFirstFiveCmpds() {
        for (int i = 0; i < Math.min(5, cmpdBeans.length); i++) {
            cmpdBeans[i].setInclude(true);
        }
    }

    public String[] getSelectedCmpdList() {
        ArrayList<String> selectedCmpd = new ArrayList();
        for (CmpdBean cb : cmpdBeans) {
            if (cb.isInclude()) {
                selectedCmpd.add(cb.getNameOnly());
            }
        }
        selectedCmpdList = selectedCmpd.toArray(String[]::new);
        return selectedCmpdList;
    }

    public String sspNextBn_action() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_sspNextBn_action(this);
            return null;
        }
        String[] selectedCmpds = getSelectedCmpdList();
        if (selectedCmpds.length > 0) {
            jrd.record_sspNextBn_action(this);
            RDataUtils.updateMapData(sb, selectedCmpds);
            wb.getCalledWorkflows().add("SSP");
            return "enrichparam";
        } else {
            sb.addMessage("Error", "No compound was selected for enrichment analysis!");
            wb.getCalledWorkflowsError().add("SSP");
            return null;
        }
    }
}
