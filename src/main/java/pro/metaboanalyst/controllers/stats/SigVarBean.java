/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.SigVarSelect;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.utils.JavaRecord;

/**
 *
 * @author jianguox
 */
@RequestScoped
@Named("sigBean")
public class SigVarBean implements Serializable {

    @Inject
    SessionBean1 sb;
    @JsonIgnore
    @Inject
    private JavaRecord jrd;
    private boolean nonParSAM = false;

    public boolean isNonParSAM() {
        return nonParSAM;
    }

    public void setNonParSAM(boolean nonParSAM) {
        this.nonParSAM = nonParSAM;
    }

    private String pairedAnal = "FALSE";

    public String getPairedAnal() {
        return pairedAnal;
    }

    public void setPairedAnal(String pairedAnal) {
        this.pairedAnal = pairedAnal;
    }

    private String equalVar = "TRUE";

    public String getEqualVar() {
        return equalVar;
    }

    public void setEqualVar(String equalVar) {
        this.equalVar = equalVar;
    }

    private double delta = 0;
    private double deltaMin = 0;
    private double deltaMax = 0;
    private double step = 0;

    public double getDeltaMin() {
        return deltaMin;
    }

    public double getDeltaMax() {
        return deltaMax;
    }

    public double getStep() {
        return step;
    }

    public double getDelta() {
        return SigVarSelect.getSAMSuggestedDelta(sb);
    }

    public void setDelta(double delta) {
        this.delta = delta;
    }

    public void samBtn1_action() {
        String stat = "d.stat";
        if (!sb.isMultiGroup() && nonParSAM) {
            stat = "wilc.stat";
        }
        SigVarSelect.initSAM(sb, stat, pairedAnal, equalVar, delta, sb.getNewImage("sam_imp"));
        double myDelta = SigVarSelect.getSAMSuggestedDelta(sb);
        if (myDelta != delta) {
            sb.addMessage("Info", "The delta value has been updated.");
            delta = myDelta;
        }
        double[] deltas = SigVarSelect.getSAMDeltaRange(sb);
        deltaMin = deltas[0];
        deltaMax = deltas[1];
        step = deltas[2];

        SigVarSelect.plotSAM_FDR(sb, sb.getNewImage("sam_view"), "png", 150);
        //SigVarSelect.PlotSAM_Cmpd(sb, sb.getNewImage("sam_imp"), "png", 150);
        sb.addMessage("info", "The result is updated!");

        jrd.record_samBtn1_action(this);
    }

    private double alpha = -99;

    public double getAlpha() {
        return SigVarSelect.getEBAMSuggestedA0(sb);
    }

    public void setAlpha(double alpha) {
        this.alpha = alpha;
    }

    private boolean nonParEBAM = false;

    public boolean isNonParEBAM() {
        return nonParEBAM;
    }

    public void setNonParEBAM(boolean nonParEBAM) {
        this.nonParEBAM = nonParEBAM;
    }

    private double ebamDelta = 0.9;

    public double getEbamDelta() {
        return ebamDelta;
    }

    public void setEbamDelta(double ebamDelta) {
        this.ebamDelta = ebamDelta;
    }

    public void ebamBtn_action() {
        String nonPar = (nonParEBAM) ? "TRUE" : "FALSE";
        SigVarSelect.initEBAM(sb, pairedAnal, equalVar, nonPar, alpha, ebamDelta, sb.getNewImage("ebam_view"), sb.getNewImage("ebam_imp"));
        sb.addMessage("info", "The <b>Result view</b> is updated!");

        jrd.record_ebamBtn_action(this);
    }
}
