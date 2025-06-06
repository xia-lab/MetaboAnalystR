/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.Serializable;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.TimeSeries;
import org.rosuda.REngine.Rserve.RConnection;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author xia
 */
@ViewScoped
@Named("ascaBean")
public class AscaBean implements Serializable {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @Inject
    private SessionBean1 sb;

    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private static final Logger LOGGER = LogManager.getLogger(AscaBean.class);

    private final String pageID = "ASCA";

    /**
     * ASCA
     */
    private int mdlANum = 1;
    private int mdlBNum = 1;
    private int mdlABNum = 2;
    private int mdlResNum = 2;

    public int getMdlANum() {
        return mdlANum;
    }

    public void setMdlANum(int mdlANum) {
        this.mdlANum = mdlANum;
    }

    public int getMdlBNum() {
        return mdlBNum;
    }

    public void setMdlBNum(int mdlBNum) {
        this.mdlBNum = mdlBNum;
    }

    public int getMdlABNum() {
        return mdlABNum;
    }

    public void setMdlABNum(int mdlABNum) {
        this.mdlABNum = mdlABNum;
    }

    public int getMdlResNum() {
        return mdlResNum;
    }

    public void setMdlResNum(int mdlRes) {
        this.mdlResNum = mdlRes;
    }

    private double lvlThresh = 0.90;
    private double alphaThresh = 0.05;

    public double getLvlThresh() {
        return lvlThresh;
    }

    public void setLvlThresh(double lvlThresh) {
        this.lvlThresh = lvlThresh;
    }

    public double getAlphaThresh() {
        return alphaThresh;
    }

    public void setAlphaThresh(double alphaThresh) {
        this.alphaThresh = alphaThresh;
    }

    private String[] selectedMetasAsca = null;

    public String[] getSelectedMetasAsca() {
        if (selectedMetasAsca == null) {
            selectedMetasAsca = mfb.getDiscMetaOpts();
        }
        return selectedMetasAsca;
    }

    public void setSelectedMetasAsca(String[] selectedMetasAsca) {
        this.selectedMetasAsca = selectedMetasAsca;
    }

    public void doDefaultAsca() {

        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/AscaView.xhtml");
        }

        if (wb.getFunctionInfos().get("ASCA") != null) {
            try {
                FunctionInvoker.invokeFunction(wb.getFunctionInfos().get("ASCA"));
            } catch (Exception ex) {

            }
        }

        if (selectedMetasAsca == null) {
            selectedMetasAsca = mfb.getDiscMetaOpts();
        }

    }

    private boolean useGreyCol = false;

    public boolean isUseGreyCol() {
        return useGreyCol;
    }

    public void setUseGreyCol(boolean useGreyCol) {
        this.useGreyCol = useGreyCol;
    }

    private int permNum = 20;

    public int getPermNum() {
        return permNum;
    }

    public void setPermNum(int perNum) {
        this.permNum = perNum;
    }

    public boolean mdlBtn_action() {

        try {
            jrd.record_mdlBtn_action(this);
            if (wb.isEditMode()) {
                return true;
            }
            if (getSelectedMetasAsca().length != 2) {
                sb.addMessage("Error", "Please select exactly two meta-data classes!");
                return false;
            }

            RConnection RC = sb.getRConnection();
            if (mdlANum < 1 | mdlBNum < 1) {
                sb.addMessage("Error", "The number of components for Model.a or Model.b cannot be zero!");
                return false;
            }
            int res = TimeSeries.performASCA(sb, mdlANum, mdlBNum, mdlABNum, mdlResNum, selectedMetasAsca);

            if (res == -1) {
                sb.addMessage("Error", "Selected metadata must be categorical!");
                return false;
            } else if (res == 0) {
                String err = RDataUtils.getErrMsg(RC);
                sb.addMessage("Error", "Please make sure parameters are set properly: " + err);
                return false;
            }
            String colorBW = useGreyCol ? "TRUE" : "FALSE";

            if (!mfb.isAscaInit()) {
                TimeSeries.plotASCAscree(sb, sb.getCurrentImage("asca_scree"), "png", 150);
                mfb.setAscaInit(true);
            }

            // TimeSeries.performASCAPermutation(RC, 20);
            // TimeSeries.plotASCAPermSummary(sb, sb.getCurrentImage("asca_perm"), "png", 150);
            TimeSeries.performASCAVarSelection(sb, 0.05, 0.90);
            TimeSeries.plotASCAImpVar(sb, sb.getCurrentImage("asca_impa"), "png", 150, "a");
            TimeSeries.plotASCAImpVar(sb, sb.getCurrentImage("asca_impb"), "png", 150, "b");
            TimeSeries.plotASCAImpVar(sb, sb.getCurrentImage("asca_impab"), "png", 150, "ab");
            TimeSeries.plotASCAModels(sb, sb.getCurrentImage("asca_fa"), "png", 150, "a", colorBW);
            TimeSeries.plotASCAModels(sb, sb.getCurrentImage("asca_fb"), "png", 150, "b", colorBW);
            TimeSeries.plotASCAInteraction(sb, sb.getCurrentImage("asca_fab"), "png", 150, colorBW);

        } catch (NumberFormatException e) {
            //e.printStackTrace();
            LOGGER.error("mdlBtn_action", e);
        }
        return true;

    }

    public void permBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        String status = "OK";
        String msg = "Permutation test was complete successfully.";
        if (sb.getFeatureNumber() > 2000) {
            if (permNum > 20) {
                permNum = 20;
                status = "Warning";
                msg += " Permutation number set to 20 due to data size.";
            }
        } else if (sb.getFeatureNumber() > 1000) {
            if (permNum > 100) {
                permNum = 100;
                status = "Warning";
                msg += " Permutation number set to 100 due to data size.";
            }
        }

        TimeSeries.performASCAPermutation(sb.getRConnection(), permNum);
        TimeSeries.plotASCAPermSummary(sb, sb.getNewImage("asca_perm"), "png", 150);
        sb.addMessage(status, msg);
    }

    public String sigBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {
            TimeSeries.performASCAVarSelection(sb, alphaThresh, lvlThresh);

            TimeSeries.plotASCAImpVar(sb, sb.getNewImage("asca_impa"), "png", 150, "a");
            TimeSeries.plotASCAImpVar(sb, sb.getNewImage("asca_impb"), "png", 150, "b");
            TimeSeries.plotASCAImpVar(sb, sb.getNewImage("asca_impab"), "png", 150, "ab");

        } catch (NumberFormatException e) {
            //e.printStackTrace();
            LOGGER.error("sigBn_action", e);
        }
        return null;
    }

}
