/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.inject.Named;
import jakarta.faces.view.ViewScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.models.SampleBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

@ViewScoped
@Named("normBean")
public class NormBean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private DoseResponseBean drb;

    @JsonIgnore
    @Inject
    private RocAnalBean rcb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private String rowNormOpt = "NULL";
    private String transNormOpt = "NULL";
    private String scaleNormOpt = "NULL";
    private String refSmpl = null;
    private String refGrp = null;
    private String refVar = null;


    public String getRefVar() {
        return refVar;
    }

    public void setRefVar(String refVar) {
        if (sb.isBigFeature()) {//in case user entered value, make sure the feature names is valid
            int res = RDataUtils.validateFeatureName(sb.getRConnection(), refVar);
            if (res == 0) {
                sb.addMessage("Error", "The feature name cannot be found in your data!");
                return;
            }
        }
        this.refVar = refVar;
        rowNormOpt = "CompNorm";
    }

    public String getRefSmpl() {
        return refSmpl;
    }

    public void setRefSmpl(String refSmpl) {
        this.refSmpl = refSmpl;
        rowNormOpt = "SamplePQN";
    }

    public String getRefGrp() {
        return refGrp;
    }

    public void setRefGrp(String refGrp) {
        this.refGrp = refGrp;
        rowNormOpt = "GroupPQN";
    }

    public String getRowNormOpt() {
        return rowNormOpt;
    }

    public void setRowNormOpt(String rowNormOpt) {
        this.rowNormOpt = rowNormOpt;
    }

    public String getTransNormOpt() {
        return transNormOpt;
    }

    public void setTransNormOpt(String transNormOpt) {
        this.transNormOpt = transNormOpt;
    }

    public String getScaleNormOpt() {
        return scaleNormOpt;
    }

    public void setScaleNormOpt(String scaleNormOpt) {
        this.scaleNormOpt = scaleNormOpt;
    }

    private boolean specNormSpecifed = false;

    public boolean isSpecNormSpecifed() {
        return specNormSpecifed;
    }

    public void setSpecNormSpecifed(boolean specNormSpecifed) {
        this.specNormSpecifed = specNormSpecifed;
    }

    public void setSmplSpecNorm() {
        rowNormOpt = "SpecNorm";
        RDataUtils.setSampleNormFactor(sb.getRConnection(), getSampleBeans());
        wb.setSampleBeans(sampleBeans);
        specNormSpecifed = true;
    }

    private List<SampleBean> sampleBeans = null;

    public List<SampleBean> getSampleBeans() {
        if (sampleBeans == null) {
            sampleBeans = createPrenormSampleBeans(sb.getRConnection(), false);
        }
        return sampleBeans;
    }

    private List<SampleBean> createPrenormSampleBeans(RConnection RC, boolean withNA) {
        String[] allSamples = RDataUtils.getPrenormSampleNames(RC);
        String[] allGrps = RDataUtils.getPrenormSmplGroupNames(RC);
        int samSize = allSamples.length;
        List<SampleBean> samNABeans = new ArrayList();
        if (withNA) {
            samNABeans.add(new SampleBean("<Not set>", "NA")); // the first one is NA
        }
        for (int i = 0; i < samSize; i++) {
            samNABeans.add(new SampleBean(allSamples[i], allGrps[i]));
        }
        return samNABeans;
    }

    private boolean normPerformed = false;

    public boolean isNormPerformed() {
        return normPerformed;
    }

    public void performDataNormalization() {
        //System.out.println(transNormOpt + "=======transNormOpt");
        //System.out.println(scaleNormOpt + "=======scaleNormOpt");
        //System.out.println(rowNormOpt + "=======rowNormOpt");
        //String specNorm = smplSpecNorm ? "T" : "NULL";
        if (rowNormOpt.equals("SpecNorm") && !specNormSpecifed) {
            sb.addMessage("Error", "You need to manually specify sample specific norm factors!");
            return;
        }
        if (rowNormOpt.equals("CompNorm") && refVar == null) {
            sb.addMessage("Error", "You need to manually specify a reference feature for normalization");
            return;
        }
        if (rowNormOpt.equals("GroupPQN") && refGrp == null) {
            sb.addMessage("Error", "You need to manually specify a reference group for normalization");
            return;
        }
        if (rowNormOpt.equals("SamplePQN") && refSmpl == null) {
            sb.addMessage("Error", "You need to manually specify a reference sample for normalization");
            return;
        }

        if (wb.isEditMode()) {
            sb.addMessage("Info", "Parameters have been updated!");
            jrd.record_PerformDataNormalization(this);
            return;
        }

        RConnection RC = sb.getRConnection();
        String ref;
        ref = switch (rowNormOpt) {
            case "CompNorm" ->
                refVar;
            case "GroupPQN" ->
                refGrp;
            default ->
                refSmpl;
        };

        if (sb.getAnalType().equals("roc")) {
            RDataUtils.setRatioOption(sb, ratioFilterOpt);
        }

        int res = RDataUtils.normalizeData(sb, rowNormOpt, transNormOpt, scaleNormOpt, ref, includeRatio, ratioNumOpt);
        jrd.record_PerformDataNormalization(this);

        if (res > 0) {

            if (sb.getAnalType().equals("dose")) {
                drb.setDePerformed(false);
            }

            //plot the new image
            RDataUtils.plotNormSummaryGraph(sb, sb.getNewImage("norm"), "png", 150);
            RDataUtils.plotSampleNormSummaryGraph(sb, sb.getNewImage("snorm"), "png", 150);
            //now reset all data analysis to default
            sb.setDataNormed();
            int featureNum = RDataUtils.getNormFeatureNumber(RC);
            sb.updateFeatureNum(featureNum);
            sb.resetAnalysis();
            if (sb.getAnalType().equals("roc")) {
                rcb.resetData();
            }
            sb.addMessage("OK", "Completed normalization. You can click <b>View Result</b> button to view the effect, or <b>Proceed</b> button to analysis page!");
            normPerformed = true;

            WorkflowBean wb = CDI.current().select(WorkflowBean.class).get();
            wb.getCalledWorkflows().add("Normalization");
        } else {
            sb.addMessage("Error", "Unknown error happened during data normalization process!");
        }
    }

    private boolean includeRatio = false;

    public boolean isIncludeRatio() {
        return includeRatio;
    }

    public void setIncludeRatio(boolean includeRatio) {
        this.includeRatio = includeRatio;
    }

    private int ratioNumOpt = 20;

    public int getRatioNumOpt() {
        return ratioNumOpt;
    }

    public void setRatioNumOpt(int ratioNumOpt) {
        this.ratioNumOpt = ratioNumOpt;
    }

    private String ratioFilterOpt = "sum";

    public String getRatioFilterOpt() {
        return ratioFilterOpt;
    }

    public void setRatioFilterOpt(String ratioFilterOpt) {
        this.ratioFilterOpt = ratioFilterOpt;
    }

    public void setGrpNmOpts(SelectItem[] grpNmOpts) {
        this.grpNmOpts = grpNmOpts;
    }

    private SelectItem[] varNmOpts = null;

    public SelectItem[] getVarNmOpts() {

        if (varNmOpts == null && !sb.isBigFeature()) {
            String[] varNms = RDataUtils.getPrenormFeatureNames(sb.getRConnection());
            int colLen = varNms.length;
            varNmOpts = new SelectItem[colLen];
            for (int i = 0; i < colLen; i++) {
                varNmOpts[i] = new SelectItem(varNms[i], varNms[i]);
            }
        }
        return varNmOpts;
    }

    private SelectItem[] smplNmOpts = null;

    public SelectItem[] getSmplNmOpts() {
        if (smplNmOpts == null) {
            setupSmplNmOpts();
        }
        return smplNmOpts;
    }

    private void setupSmplNmOpts() {
        String[] smplNms = RDataUtils.getPrenormSampleNames(sb.getRConnection());
        int rowLen = smplNms.length;
        smplNmOpts = new SelectItem[rowLen];
        for (int i = 0; i < rowLen; i++) {
            smplNmOpts[i] = new SelectItem(smplNms[i], smplNms[i]);
        }
    }

    public SelectItem[] getGrpNmOpts() {
        if (grpNmOpts == null) {
            setupGrpNmOpts();
        }
        return grpNmOpts;
    }

    private SelectItem[] grpNmOpts = null;

    private void setupGrpNmOpts() {
        if (!sb.isRegresion()) {
            String[] grpNms = RDataUtils.getPrenormGroupNames(sb.getRConnection());
            int grpLen = grpNms.length;
            grpNmOpts = new SelectItem[grpLen];
            for (int i = 0; i < grpLen; i++) {
                grpNmOpts[i] = new SelectItem(grpNms[i], grpNms[i]);
            }
        } else {
            grpNmOpts = new SelectItem[]{new SelectItem("NULL", "<Not set>")};
        }
    }
}
