/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import jakarta.annotation.PostConstruct;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetaDataBean;
import pro.metaboanalyst.models.SampleBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.PrimeFaces;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@ViewScoped
@Named("mprocBean")
public class MetaProcBean implements Serializable {

    private static final Logger LOGGER = LogManager.getLogger(MetaProcBean.class);
    @JsonIgnore
    @Inject
    private SessionBean1 sb;
    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    private String msgTextMeta;

    public String getMsgTextMeta() {
        return msgTextMeta;
    }

    public void setMsgTextMeta(String msgTextMeta) {
        this.msgTextMeta = msgTextMeta;
    }

    @PostConstruct
    public void performSanityCheckMeta() {

        RConnection RC = sb.getRConnection();
        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;

        try {
            int res = RDataUtils.sanityCheckMeta(RC, 1);
            if (res == 1) {
                msgVec.add("Checking metadata content ..... passed.");
                msgArray = RDataUtils.getSanityCheckMetaMessage(RC);
            }

        } catch (Exception e) {
            msgVec.add("Checking data content .... failed.");
            //e.printStackTrace();
            LOGGER.error("performSanityCheckMeta", e);
        }

        msgVec.addAll(Arrays.asList(msgArray));
        String msg;
        msg = "<table face=\"times\" size = \"3\">";
        msg = msg + "<tr><th> Metadata processing information: " + "</th></tr>";

        for (int i = 0; i < msgVec.size(); i++) {
            msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
        }
        msg = msg + "</table>";
        msgTextMeta = msg;
    }

    public String metacheck_proceed() {
        String[] metaDataStatus = RDataUtils.getMetaDataStatus(sb.getRConnection());
        for (String metaDataStatu : metaDataStatus) {
            if (!metaDataStatu.equals("OK")) {
                PrimeFaces.current().executeScript("PF('confDialog').show();");
                return null;
            }
        }
        RConnection RC = sb.getRConnection();
        RDataUtils.setDataTypeOfMeta(RC);
        //WorkflowBean wb = CDI.current().select(WorkflowBean.class).get();
        wb.getCalledWorkflows().add("Metadata check");
        //return "Data filter";
        if (RDataUtils.getProcFeatureNumber(RC) > 250) {
            return "Data filter";
        } else if (!sb.getDataType().equalsIgnoreCase("conc")) {
            return "Data filter";
        } else {
            return "Normalization";
        }
    }

    public void metacheck_proceed4batch() {
        String[] metaDataStatus = RDataUtils.getMetaDataStatus(sb.getRConnection());
        for (String metaDataStatu : metaDataStatus) {
            if (!metaDataStatu.equals("OK")) {
                PrimeFaces.current().executeScript("PF('confDialog').show();");
            }
        }
        RConnection RC = sb.getRConnection();
        RDataUtils.setDataTypeOfMeta(RC);
    }

    public void updateMetaPerSample() {
        RDataUtils.setSampleGroups(sb.getRConnection(), sb.getSampleBeans(), mfb.getSelectedMetaData());
        mfb.initMetaData();
        sb.addMessage("Info", "Successfully updated the selected metadata!");

    }

    public void updateMetaPerFactor() {
        List<SampleBean> beans = mfb.getUniqueMetaNames();
        String[] metas = new String[beans.size()];
        for (int i = 0; i < beans.size(); i++) {
            metas[i] = beans.get(i).getGroup();
        }
        RDataUtils.updateMetaLevels(sb.getRConnection(), mfb.getSelectedMetaData(), metas);
        mfb.reinitVariables();
        mfb.initMetaData();
        sb.addMessage("Info", "The updated metadata is show <b style='color: orange'>as a new metadata at the bottom of the table</b>! You can click its name to to update it to a new name.");
    }

    public String metacheck_confirm_proceed() {
        //int res = RDataUtils.sanityCheckMeta(sb.getRConnection(), 0);
        Iterator<MetaDataBean> it = mfb.getMetaDataBeans().iterator();
        int countOk = 0;
        while (it.hasNext()) {
            MetaDataBean bean = it.next();
            if (bean.getStatus().contains("OK")) {
                countOk++;
            }
        }

        if (countOk < 2) {
            sb.addMessage("Error", "Less than two metadata classes pass sanity check!");
            //PrimeFaces.current().executeScript("PF('confDialog').hide();");
            return null;
        }

        Iterator<MetaDataBean> i = mfb.getMetaDataBeans().iterator();
        while (i.hasNext()) {
            MetaDataBean bean = i.next();
            String status = bean.getStatus();
            if (status.contains("Missing values") || status.contains("Too many low replicates") || status.contains("Not all numeric")) {
                if (sb.getTsDesign().equals("time")) {
                    sb.addMessage("Error", "The metadata factor needs to pass sanity check to proceed!");
                    return null;
                } else if (bean.isPrimary()) {
                    sb.addMessage("Error", "The primary metadata factor needs to pass sanity check to proceed!");
                    return null;
                }
                RDataUtils.removeSelectedMeta(sb.getRConnection(), bean.getName());
                i.remove();
            }
        }
        RDataUtils.setDataTypeOfMeta(sb.getRConnection());
        return "Data filter";
    }
}
