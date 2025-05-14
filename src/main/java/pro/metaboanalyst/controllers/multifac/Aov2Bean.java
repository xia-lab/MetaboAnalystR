/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.controllers.general.DetailsBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author xia
 */
@ViewScoped
@Named("aov2Bean")
public class Aov2Bean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @Inject
    private SessionBean1 sb;

    @Inject
    private MultifacBean tb;
    private final String pageID = "ANOVA2";

    private double pthresh = 0.05;

    public double getPthresh() {
        return pthresh;
    }

    public void setPthresh(double pthresh) {
        this.pthresh = pthresh;
    }

    private String pvalOpt = "fdr";

    public String getPvalOpt() {
        return pvalOpt;
    }

    public void setPvalOpt(String pvalOpt) {
        this.pvalOpt = pvalOpt;
    }

    private String phenOpt = "between";

    public String getPhenOpt() {
        return phenOpt;
    }

    public void setPhenOpt(String phenOpt) {
        this.phenOpt = phenOpt;
    }

    private String[] selectedMetasAnova = null;

    public String[] getSelectedMetasAnova() {
        return selectedMetasAnova;
    }

    public void setSelectedMetasAnova(String[] selectedMetasAnova) {
        this.selectedMetasAnova = selectedMetasAnova;
    }

    public void doDefaultAov2() {
        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/Anova2View.xhtml");
        }

        WorkflowBean fp = (WorkflowBean) DataUtils.findBean("workflowBean");
        if (fp.getFunctionInfos().get("Multifactor ANOVA") != null) {
            try {
                FunctionInvoker.invokeFunction(fp.getFunctionInfos().get("Multifactor ANOVA"));
            } catch (Exception ex) {
                Logger.getLogger(Aov2Bean.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        if (selectedMetasAnova == null) {
            selectedMetasAnova = tb.getDiscMetaOpts();
        }

    }

    public boolean aov2Bn_action() {
        Aov2Bean b = (Aov2Bean) DataUtils.findBean("aov2Bean");

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            JavaRecord.record_aov2Bn_action(b);
            return true;
        }
        System.out.println(selectedMetasAnova.length + "=====selectedMetasAnova");
        if (selectedMetasAnova.length != 2) {
            sb.addMessage("Error", "Please select exactly two meta-data classes!");
            return false;
        }
        JavaRecord.record_aov2Bn_action(b);

        int res = TimeSeries.initANOVA2(sb.getRConnection(), pthresh, pvalOpt, sb.getTsDesign(), phenOpt, selectedMetasAnova);
        switch (res) {
            case 0:
                sb.addMessage("Error", RDataUtils.getErrMsg(sb.getRConnection()));
                break;
            case -1:
                sb.addMessage("Error", "Selected metadata must be categorical!");
                break;
            default:
                DetailsBean db = (DetailsBean) DataUtils.findBean("detailsBean");
                db.update2CompModel("aov2");
                TimeSeries.plotAOV2(sb, sb.getNewImage("aov2"), "png", 72);
                tb.setAov2Performed(true);
                break;
        }
        return true;
    }
}
