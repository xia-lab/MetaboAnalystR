/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.faces.model.ListDataModel;
import java.io.File;
import java.io.Serializable;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.Arrays;

import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.FeatureBean;
import pro.metaboanalyst.rwrappers.TimeSeries;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.workflows.FunctionInvoker;
import pro.metaboanalyst.workflows.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author xia
 */
@ViewScoped
@Named("mebaBean")
public class MebaBean implements Serializable {

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;
    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    private final String pageID = "MEBA";

    private String[] mebaMetas = null;
    private FeatureBean[] featureBeans = null;
    private String downloadTxt;
    private String mebaName = "Hotelling-T2";
    private ListDataModel<FeatureBean> listModel = null;

    public ListDataModel<FeatureBean> getFeatureListModel() {
        return listModel;
    }

    public String[] getMebaMetas() {
        return mebaMetas;
    }

    public void setMebaMetas(String[] mebaMetas) {
        this.mebaMetas = mebaMetas;
    }

    public FeatureBean[] getFeatureBeans() {
        return featureBeans;
    }

    public void setFeatureBeans(FeatureBean[] featureBeans) {
        this.featureBeans = featureBeans;
    }

    public String getDownloadTxt() {
        return downloadTxt;
    }

    public String getMEBAStatName() {
        return mebaName;
    }

    public void doDefaultMEBA() {
        if (!sb.isAnalInit(pageID)) {
            sb.addNaviTrack(pageID, "/Secure/multifac/TimeCourseView.xhtml");
            //if (mebaMetas == null) {
            //System.out.println(mebaMetas.length);
            //}
        }
        mebaMetas = mfb.getDiscMetaOpts();

    }

    public void mbButton_action() {

        if (!sb.isContainsTime()) {
            sb.addMessage("Error", "MEBA only work on time-series data.");
            return;
        }

        if (mebaMetas.length != 2) {
            sb.addMessage("Error", "Please select an experimental factor along with time-series meta-data.");
            return;
        }

        jrd.record_mbButton_action(this);
        RConnection RC = sb.getRConnection();
        int res = TimeSeries.performMB(sb, 10, mebaMetas);
        if (res == 0) {
            String msg = "Please make sure data are balanced for time-series analysis. In particular, "
                    + "for each time point, all experiments must exist and cannot be missing!";
            sb.addMessage("Error", msg);
            featureBeans = null;
            downloadTxt = "No results";
            return;
        }
        String[] rownames = TimeSeries.getMBSigRowNames(RC);
        String[] colnames = TimeSeries.getMBSigColNames(RC);
        mebaName = colnames[0];
        double[][] sigmat = TimeSeries.getMBSigMat(RC);

        //set up content
        if (rownames == null || rownames.length == 0) {
            featureBeans = null;
            listModel = null;
            downloadTxt = "No results";
            sb.addMessage("Error", "No result data was found!");
        } else {
            //now set up the feature beans
            featureBeans = new FeatureBean[rownames.length];
            FeatureBean fb;

            for (int i = 0; i < rownames.length; i++) {
                fb = new FeatureBean();
                fb.addName(rownames[i]);

                for (int m = 0; m < colnames.length; m++) {
                    fb.addValue(sigmat[i][m]);
                }
                featureBeans[i] = fb;
            }
            listModel = new ListDataModel(Arrays.asList(featureBeans));
            downloadTxt = "<b>You can download the table:</b> <a href = \"/MetaboAnalyst/resources/users/"
                    + sb.getCurrentUser().getName()
                    + File.separator + "meba_sig_features.csv\" target=\"_blank\"><b>" + "here" + "</b></a>";
        }
    }
}
