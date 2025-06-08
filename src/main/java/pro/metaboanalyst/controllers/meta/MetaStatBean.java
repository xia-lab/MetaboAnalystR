/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.meta;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import pro.metaboanalyst.rwrappers.RMetaUtils;
import jakarta.inject.Named;
import jakarta.faces.view.ViewScoped;
import pro.metaboanalyst.workflows.JavaRecord;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("metaStatBean")
public class MetaStatBean implements Serializable {

    @Inject
    private SessionBean1 sb;
    @Inject
    private MetaLoadBean mb;
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;
    private boolean resOK = false;

    public boolean isResOK() {
        return resOK;
    }
    /**
     * p value combination
     *
     * @return
     */

    private double metpSigLvl = 0.05;

    public double getMetpSigLvl() {
        return metpSigLvl;
    }

    public void setMetpSigLvl(double metpSigLvl) {
        this.metpSigLvl = metpSigLvl;
    }
    private String metapMethod = "fisher";

    public String getMetapMethod() {
        return metapMethod;
    }

    public void setMetapMethod(String metapMethod) {
        this.metapMethod = metapMethod;
    }

    public String performPvalCombination() {
        mb.setAnalMethod("metap");
        jrd.record_performPvalCombination(this);

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            return null;
        }

        int res = RMetaUtils.performPvalCombination(sb, metapMethod, metpSigLvl);
        if (res > 0) {
            resOK = true;
            mb.setCurrentDeNum("metap", res);
            sb.addMessage("info", "A total of " + res + " significant features found. Click <b>Proceed</b> button to view the result. ");
            return "Result Table";
        } else {
            resOK = false;
            sb.addMessage("warn", "No significant features found. You may want to change the significance level threshold. ");

        }
        return null;
    }

    /**
     * Vote counts
     *
     * @return
     */
    private double vcSigLvl = 0.05;

    public double getVcSigLvl() {
        return vcSigLvl;
    }

    public void setVcSigLvl(double vcSigLvl) {
        this.vcSigLvl = vcSigLvl;
    }
    private int minVote;

    public int getMinVote() {
        //default half (+1) to ensure round to higher
        if (minVote == 0) {
            minVote = (mb.getDataSets().size() + 1) / 2;
        }
        return minVote;
    }

    public void setMinVote(int minVote) {
        this.minVote = minVote;
    }

    public String performVoteCounting() {
        // Record the function call
        jrd.record_performVoteCounting(this);

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            return null;
        }

        mb.setAnalMethod("votecount");
        int res = RMetaUtils.performVoteCounting(sb, vcSigLvl, minVote);
        if (res > 0) {
            resOK = true;
            mb.setCurrentDeNum("votecount", res);
            sb.addMessage("info", "A total of " + res + " significant features found. Click <b>Proceed</b> button to view the result. ");
        } else {
            resOK = false;
            sb.addMessage("warn", "No significant features found. You may want to change the significance level threshold. ");
        }
        return null;
    }

    /**
     * Direct merging
     */
    private double dmSigLvl = 0.05;

    public double getDmSigLvl() {
        return dmSigLvl;
    }

    public void setDmSigLvl(double dmSigLvl) {
        this.dmSigLvl = dmSigLvl;
    }

    public String performDirectMerging() {

        jrd.record_performDirectMerging(this);
        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");

            return null;
        }
        mb.setAnalMethod("merge");
        int res = RMetaUtils.performMetaMerge(sb, dmSigLvl);
        if (res > 0) {
            resOK = true;
            mb.setCurrentDeNum("merge", res);
            sb.addMessage("info", "A total of " + res + " significant features found. Click <b>Proceed</b> button to view the result.");
            return ("Result table");
        } else {
            resOK = false;
            sb.addMessage("warn", "No significant features found. You may want to change the significance level threshold.");
        }
        return null;
    }
}
