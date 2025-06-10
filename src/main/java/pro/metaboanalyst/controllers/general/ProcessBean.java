/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.List;
import pro.metaboanalyst.controllers.enrich.IntegProcessBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.SearchUtils;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.models.DataModel;
import pro.metaboanalyst.models.NameMapBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

@ViewScoped
@Named("procBean")
public class ProcessBean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private IntegProcessBean ipb;

    @JsonIgnore
    @Inject
    private DoseResponseBean drb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private static final Logger LOGGER = LogManager.getLogger(GenericControllers.class);
    private String msgText;

    private int filterCutoff = 5;

    public int getFilterCutoff() {
        return filterCutoff;
    }

    public void setFilterCutoff(int filterCutoff) {
        this.filterCutoff = filterCutoff;
    }

    private int intFilterCutoff = 0;

    public int getIntFilterCutoff() {
        return intFilterCutoff;
    }

    public void setIntFilterCutoff(int intFilterCutoff) {
        this.intFilterCutoff = intFilterCutoff;
    }

    public String getMsgText() {
        return msgText;
    }

    private boolean missingDisabled = true;

    public boolean isMissingDisabled() {
        return missingDisabled;
    }

    public void setMissingDisabled(boolean missingDisabled) {
        this.missingDisabled = missingDisabled;
    }

    private boolean editBnDisabled = false;

    public boolean isEditBnDisabled() {
        return editBnDisabled;
    }

    public void setEditBnDisabled(boolean editBnDisable) {
        this.editBnDisabled = editBnDisable;
    }

    private String selectedMetaData = "Meta-data";

    public String getSelectedMetaData() {
        return selectedMetaData;
    }

    public void setSelectedMetaData(String selectedMetaData) {
        this.selectedMetaData = selectedMetaData;
    }

    public void updateSmplGroup() {
        RDataUtils.setSampleGroups(sb.getRConnection(), sb.getSampleBeans(), selectedMetaData);
        performSanityCheck();
    }

    private boolean proceedBnDisabled = false;

    public boolean isProceedBnDisabled() {
        return proceedBnDisabled;
    }

    public void setProceedBnDisabled(boolean bnDisabled) {
        this.proceedBnDisabled = bnDisabled;
    }

    private boolean doQCFiltering = false;

    public boolean isDoQCFiltering() {
        return doQCFiltering;
    }

    public void setDoQCFiltering(boolean doQCFiltering) {
        this.doQCFiltering = doQCFiltering;
    }

    private int qcCutoff = 25;

    public int getQcCutoff() {
        return qcCutoff;
    }

    public void setQcCutoff(int qcCutoff) {
        this.qcCutoff = qcCutoff;
    }

    public void setMsgText(String msgText) {
        this.msgText = msgText;
    }

    //note this is viewscoped; need to init when visit this page
    private boolean sanityChecked = false;

    public void performSanityCheck() {

        //if (sb.isIntegChecked()) {
        //    return;
        //}
        if (sanityChecked) {
            return;
        }
        RConnection RC = sb.getRConnection();
        if (RC == null) {
            System.out.println("sanity RC is null");
            return;
        }
        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;
        metaDataSet = new ArrayList();
        dataSets = new ArrayList();
        try {
            System.out.println("==== sb.getAnalType()====> " + sb.getAnalType());
            switch (sb.getAnalType()) {

                case "mummichog" -> {
                    int sampleNum = 0;
                    if (RDataUtils.sanityCheckMummichogData(RC)) {

                        msgVec.add("Checking data content ...passed.");
                        msgArray = RDataUtils.getSanityCheckMessage(RC);
                        int featureNum = RDataUtils.getOrigFeatureNumber(RC);
                        sampleNum = RDataUtils.getSampleSize(RC);
                        sb.setupDataSize(featureNum, sampleNum);
                        proceedBnDisabled = false;

                        DataModel ds = new DataModel(sb, "upload");
                        ds.setSmplNum(sampleNum);
                        ds.setGeneNum(featureNum);
                        dataSets.add(ds);
                        RCenter.recordMessage(RC, "Data integrity check - <b>passed</b>");
                    } else {
                        msgVec.add("Checking data content ...failed.");
                        msgArray = RDataUtils.getErrorMsgs(RC);
                        proceedBnDisabled = true;
                        RCenter.recordMessage(RC, "Data integrity check - <b>failed</b>");
                    }
                    if (sb.getDataType().equals("mass_table")) {
                        missingDisabled = !RDataUtils.containMissing(RC);

                        String primInfo = RDataUtils.getPrimaryInfo(RC);
                        String factors = RDataUtils.getFactors(RC);

                        DataModel ms = new DataModel(sb, "meta");
                        ms.setSmplNum(sampleNum);
                        ms.setPrimaryMeta(primInfo);
                        ms.setGroupInfo(factors);

                        metaDataSet.add(ms);
                    } else {
                        editBnDisabled = true;
                    }

                }
                case "pathinteg" -> {
                    editBnDisabled = true;
                    if (!ipb.getDatatype().equals("peak")) {
                        return;
                    }
                    if (RDataUtils.sanityCheckMummichogData(RC)) {
                        msgVec.add("Checking data content ...passed.");
                        msgArray = RDataUtils.getSanityCheckMessage(RC);
                        proceedBnDisabled = false;
                        RCenter.recordMessage(RC, "Data integrity check - <b>passed</b>");
                    } else {
                        msgVec.add("Checking data content ...failed.");
                        msgArray = RDataUtils.getErrorMsgs(RC);
                        proceedBnDisabled = true;
                        RCenter.recordMessage(RC, "Data integrity check - <b>failed</b>");
                    }
                }
                case "raw" -> {
                    // doing nothing here to avoid error message
                    return;
                }
                default -> {
                    int res = RDataUtils.sanityCheckData(RC);

                    if (res == 1) {
                        msgVec.add("Checking data content ...passed.");
                        msgArray = RDataUtils.getSanityCheckMessage(RC);
                        int featureNum = RDataUtils.getOrigFeatureNumber(RC);
                        int sampleNum = RDataUtils.getSampleSize(RC);
                        String primInfo = RDataUtils.getPrimaryInfo(RC);
                        String factors = RDataUtils.getFactors(RC);

                        DataModel ds = new DataModel(sb, "upload");

                        ds.setSmplNum(sampleNum);
                        ds.setGeneNum(featureNum);
                        dataSets.add(ds);

                        DataModel ms = new DataModel(sb, "meta");

                        ms.setSmplNum(sampleNum);
                        ms.setPrimaryMeta(primInfo);
                        ms.setGroupInfo(factors);

                        metaDataSet.add(ms);

                        sb.setupDataSize(featureNum, sampleNum);
                        sb.settingRoc1Col(featureNum);
                        proceedBnDisabled = false;
                        missingDisabled = !RDataUtils.containMissing(RC);
                        RCenter.recordMessage(RC, "Data integrity check - <b>passed</b>");
                    } else {
                        msgVec.add("Checking data content ...failed.");
                        msgArray = RDataUtils.getErrorMsgs(RC);
                        editBnDisabled = (res == -2);
                        proceedBnDisabled = true;
                        RCenter.recordMessage(RC, "Data integrity check - <b>failed</b>");
                    }

                }
            }

        } catch (Exception e) {
            msgVec.add("Checking data content ...failed.");
            //e.printStackTrace();
            LOGGER.error("performSanityCheck", e);
            RCenter.recordMessage(RC, "Data integrity check - <b>failed</b>");
        }

        if (sb.getAnalType().equals("mf")) {
            editBnDisabled = true;
        }
        msgVec.addAll(Arrays.asList(msgArray));
        String msg;
        msg = "<table face=\"times\" size = \"3\">";
        if (!sb.getAnalType().equals("pathinteg")) {
            msg = msg + "<tr><th> Data processing information: " + "</th></tr>";
        }
        for (int i = 0; i < msgVec.size(); i++) {
            msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
        }
        msg = msg + "</table>";
        msgText = msg;
        sanityChecked = true;
        sb.setIntegChecked();
        //System.out.println(msgText + "======msgText");
    }

    public String imputeButton_action() {
        sb.setMultiGroup(RDataUtils.getGroupNumber(sb.getRConnection()) > 2);
        return "Missing value";
    }

    public String skipButton_action_default() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            jrd.record_skipButton_action_default(sb);
            return null;
        }

        if (sb.getDataType().equals("mass_all")) {
            return "mzlibview";
        }

        RConnection RC = sb.getRConnection();
        String analType = sb.getAnalType();

        RDataUtils.replaceMin(RC);
        if (!analType.equals("mf")) {
            sb.setMultiGroup(RDataUtils.getGroupNumber(RC) > 2);
        }
        //sb.setPageInit("sanity");
        sb.setIntegChecked();
        sb.setSmallSmplSize(RDataUtils.isSmallSampleSize(RC));
        //sb.setupDataOpts();
        jrd.record_skipButton_action_default(sb);

        if (analType.equals("mf")) {
            return "Metadata check";
        } else if (analType.equals("dose")) {
            String[] metaDataGroups = RDataUtils.getMetaDataGroups(RC);
            if (metaDataGroups.length > 1) {
                return "Metadata check";
            }
        }

        //for targeted metabolomics, need to perform name check
        if (analType.equals("msetqea") || analType.equals("pathqea")) {
            if (sb.getFeatType().equals("lipid")) {
                SearchUtils.crossReferenceExactLipid(sb, sb.getCmpdIDType());
            } else {
                SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());
            }
            return "Name check";
        }
        //dspc
        if (analType.equals("network")
                & (sb.getCmpdIDType().equals("name") || sb.getCmpdIDType().equals("hmdb") || sb.getCmpdIDType().equals("kegg"))) {
            SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());
            return "Name check";
        }

        return toFilterView();
    }

    private String varFilterOpt = "iqr";

    public String getVarFilterOpt() {
        return varFilterOpt;
    }

    public void setVarFilterOpt(String varFilterOpt) {
        this.varFilterOpt = varFilterOpt;
    }

    private String intFilterOpt = "mean";

    public String getIntFilterOpt() {
        return intFilterOpt;
    }

    public void setIntFilterOpt(String intFilterOpt) {
        this.intFilterOpt = intFilterOpt;
    }

    public String filterProceed_action() {
        RConnection RC = sb.getRConnection();
        String doQC = "F";
        if (!filtered) {
            RDataUtils.filterVariable(sb, doQC, qcCutoff, "none", -1, "mean", 0);
        }

        return "Normalization";
    }

    public void filterButton_action() {
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_filterButton_action(this);
            return;
        }
        RConnection RC = sb.getRConnection();
        String doQC = "F";
        String msg = "QC filtering: none";
        if (doQCFiltering) {
            doQC = "T";
            msg = "QC filtering: yes";
        }

        int res = RDataUtils.filterVariable(sb, doQC, qcCutoff, varFilterOpt, filterCutoff, intFilterOpt, intFilterCutoff);
        if (res == 0) {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            return;
        }

        msg = msg + "; " + RDataUtils.getCurrentMsg(RC);
        sb.addMessage("OK", msg);
        int featureNum = RDataUtils.getFiltFeatureNumber(RC);
        sb.updateFeatureNum(featureNum);
        filtered = true;

        if (sb.getAnalType().equals("dose")) {
            drb.setDePerformed(false);
        }
        wb.getCalledWorkflows().add("Filtering");
        jrd.record_filterButton_action(this);
    }

    private boolean filtered = false;

    public boolean isFiltered() {
        return filtered;
    }

    public void setFiltered(boolean filtered) {
        this.filtered = filtered;
    }

    private String nmrAlignText = "";

    public String getNmrAlignText() {
        return nmrAlignText;
    }

    public void setNmrAlignText(String nmrAlignText) {
        this.nmrAlignText = nmrAlignText;
    }

    public String performNMRPeakAlignment() {
        RConnection RC = sb.getRConnection();
        RDataUtils.processPeakList(RC, 0.03);
        setNMRpeakProcTable();
        return null;
    }

    private void setNMRpeakProcTable() {
        String[] msgArray = RDataUtils.getPeaklistProcMessage(sb.getRConnection());
        String msg = "<table face=\"times\" size = \"3\">"
                + "<tr><th> NMR peak processing information </th></tr>";
        for (String msgArray1 : msgArray) {
            msg = msg + "<tr><td align=\"left\">" + msgArray1 + "</td></tr>";
        }
        msg = msg + "</table>";
        nmrAlignText = msg;
    }

    public String nmrNextBn_action() {
        sb.setDataPreprocessed();
        return "Data check";
    }

    private double mzThresh = 0.025;
    private double rtThresh = 30;

    public double getMzThresh() {
        return mzThresh;
    }

    public void setMzThresh(double mzThresh) {
        this.mzThresh = mzThresh;
    }

    public double getRtThresh() {
        return rtThresh;
    }

    public void setRtThresh(double rtThresh) {
        this.rtThresh = rtThresh;
    }

    private String msPeakText = "";

    public String getMsPeakText() {
        return msPeakText;
    }

    public void setMsPeakText(String msPeakText) {
        this.msPeakText = msPeakText;
    }

    private void setMSpeakProcTable() {
        String[] msgArray = RDataUtils.getPeaklistProcMessage(sb.getRConnection());
        String msg = "<table face=\"times\" size = \"3\">"
                + "<tr><th> MS peak processing information </th></tr>";
        for (String msgArray1 : msgArray) {
            msg = msg + "<tr><td align=\"left\">" + msgArray1 + "</td></tr>";
        }
        msg = msg + "</table>";
        msPeakText = msg;
    }

    public String msPeakAlignBn_action() {
        RDataUtils.processPeakList(sb.getRConnection(), mzThresh, rtThresh);
        setMSpeakProcTable();
        sb.setMsPeakAligned(true);
        return null;
    }

    public String msPeakNextBn_action() {
        sb.setDataPreprocessed();
        return "Data check";
    }

    private boolean removeMissing = true;

    public boolean isRemoveMissing() {
        return removeMissing;
    }

    public void setRemoveMissing(boolean removeMissing) {
        this.removeMissing = removeMissing;
    }

    private int missingPercent = 50;

    public int getMissingPercent() {
        return missingPercent;
    }

    public void setMissingPercent(int missingPercent) {
        this.missingPercent = missingPercent;
    }

    private String missingImputeOpt = "min";

    public String getMissingImputeOpt() {
        return missingImputeOpt;
    }

    public void setMissingImputeOpt(String missingImputeOpt) {
        this.missingImputeOpt = missingImputeOpt;
    }

    private String replaceVarOpt;

    public String getReplaceVarOpt() {
        return replaceVarOpt;
    }

    public void setReplaceVarOpt(String replaceVarOpt) {
        this.replaceVarOpt = replaceVarOpt;
    }

    private String imputeAlgOpt;

    public String getImputeAlgOpt() {
        return imputeAlgOpt;
    }

    public void setImputeAlgOpt(String imputeAlgOpt) {
        this.imputeAlgOpt = imputeAlgOpt;
    }

    public String performMissingImpute() {
        RConnection RC = sb.getRConnection();

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_performMissingImpute(this);
            return null;
        }
        if (removeMissing) {
            double percent = missingPercent / 100.0;
            RDataUtils.removeVariableByPercent(sb, percent);
        }

        String method = missingImputeOpt;
        switch (missingImputeOpt) {
            case "replaceCol" ->
                method = replaceVarOpt;
            case "impute" ->
                method = imputeAlgOpt;
        }
        RDataUtils.imputeVariable(sb, method);
        sb.setDataPreprocessed();
        sb.setSmallSmplSize(RDataUtils.isSmallSampleSize(RC));

        String analType = sb.getAnalType();
        jrd.record_performMissingImpute(this);

        if (analType.equals("mf")) {
            return "Metadata check";
        } else if (analType.equals("dose")) {
            String[] metaDataGroups = RDataUtils.getMetaDataGroups(RC);
            if (metaDataGroups.length > 1) {
                return "Metadata check";
            }
        }

        if (analType.equals("msetqea") || analType.equals("pathqea")) {
            if (sb.getFeatType().equals("lipid")) {
                SearchUtils.crossReferenceExactLipid(sb, sb.getCmpdIDType());
            } else {
                SearchUtils.crossReferenceExact(sb, sb.getCmpdIDType());
            }
            return "Name check";
        }

        return toFilterView();
    }

    private String toFilterView() {
        //filter is recommended, even default is 0 for advanced users
        int featureNum = sb.getFeatureNumber();
        if (featureNum > 250) {
            return "Data filter";
        } else if (!sb.getDataType().equalsIgnoreCase("conc")) {
            return "Data filter";
        } else {
            return "Normalization";
        }
    }

    public void prepareFilterView() {

        int featureNum = sb.getFeatureNumber();
        int defaultFilterCutoff;

        if (featureNum < 250) {
            defaultFilterCutoff = 5;
        } else if (featureNum < 500) {
            defaultFilterCutoff = 10;
        } else if (featureNum < 1000) {
            defaultFilterCutoff = 25;
        } else {
            defaultFilterCutoff = 40;
        }

        sb.setDefaultFilterCutoff(defaultFilterCutoff);

        //control for large data on public server
        if (!sb.isPrivileged()) {
            if (featureNum > 2500 & sb.getAnalType().equals("power")) {
                sb.setFilterMin(defaultFilterCutoff);
            } else if (featureNum > 5000) { // mandatory
                sb.setFilterMin(defaultFilterCutoff);
            } else {
                sb.setFilterMin(0);
            }
        } else {
            sb.setFilterMin(0);
        }

    }

    private List<DataModel> dataSets;
    private List<DataModel> metaDataSet;

    public List<DataModel> getDataSets() {
        if (dataSets == null || dataSets.isEmpty()) {
            dataSets = new ArrayList();
        }
        return dataSets;
    }

    public void setDataSets(List<DataModel> dataSets) {
        this.dataSets = dataSets;
    }

    public List<DataModel> getMetaDataSet() {
        if (metaDataSet == null || metaDataSet.isEmpty()) {
            metaDataSet = new ArrayList();
        }

        return metaDataSet;
    }

    public void setMetaDataSet(List<DataModel> metaDataSet) {

        this.metaDataSet = metaDataSet;
    }

    public void prepViewData(String name) {
        if (sb.getAnalType().equals("metapaths") || sb.getAnalType().equals("metadata")) {
            RDataUtils.getViewData(sb.getRConnection(), name);
        }

    }

    private NameMapBean[] nameMaps = null;

    public NameMapBean[] getNameMaps() {
        return nameMaps;
    }

    public void setNameMaps(NameMapBean[] nameMaps) {
        this.nameMaps = nameMaps;
    }

    private int row_count;

    public int getRow_count() {
        return row_count;
    }

    public void setRow_count(int row_count) {
        this.row_count = row_count;
    }

}
