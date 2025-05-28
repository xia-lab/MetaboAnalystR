/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.stats;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.rwrappers.PowerUtils;
import pro.metaboanalyst.rwrappers.RDataUtils;
import software.xdev.chartjs.model.charts.ScatterChart;
import software.xdev.chartjs.model.data.ScatterData;
import software.xdev.chartjs.model.datapoint.ScatterDataPoint;
import software.xdev.chartjs.model.dataset.ScatterDataset;

/**
 *
 * @author jianguox
 */
@SessionScoped
@Named("powerAnalBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class PowerAnalBean implements Serializable {

    @JsonIgnore
    @Inject
    SessionBean1 sb;
    private String lineModel;
    private double fdr = 0.1;
    private int smplSize = 200;
    private String selectedContrast = "NA";
    private SelectItem[] grpContrasts;

    public String getLineModel() {
        return lineModel;
    }

    public void setLineModel(String lineModel) {
        this.lineModel = lineModel;
    }

    public double getFdr() {
        return fdr;
    }

    public void setFdr(double fdr) {
        this.fdr = fdr;
    }

    public int getSmplSize() {
        return smplSize;
    }

    public void setSmplSize(int smplSize) {
        if (smplSize < 60) {
            smplSize = 60;
        } else if (smplSize > 1000) {
            smplSize = 1000;
        }

        this.smplSize = smplSize;
    }

    public String getSelectedContrast() {
        return selectedContrast;
    }

    public void setSelectedContrast(String selectedContrast) {
        this.selectedContrast = selectedContrast;
    }

    public SelectItem[] getGroupContrastOpts() {
        if (grpContrasts == null) {
            String[] nms = RDataUtils.getGroupNames(sb.getRConnection(), "");
            int totNum = nms.length * (nms.length - 1) / 2;
            grpContrasts = new SelectItem[totNum];
            int pos = 0;
            for (int m = 0; m < nms.length - 1; m++) {
                String grpA = nms[m];
                for (int n = m + 1; n < nms.length; n++) {
                    String grpB = nms[n];
                    String target = grpA + " vs. " + grpB;
                    grpContrasts[pos] = new SelectItem(target, target);
                    pos++;
                }
            }
        }
        selectedContrast = grpContrasts[0].getValue().toString();
        return grpContrasts;
    }

    public void performPowerAnal() {
        PowerUtils.initPowerAnal(sb.getRConnection(), selectedContrast);
        PowerUtils.plotPowerStatDiagnostics(sb, sb.getNewImage("power_stat"), "png", 72);
    }

    public String profileBtn_action() {
        //PowerUtils.plotPowerProfile(sb, fdr, sb.getNewImage("power_profile"), "png", 72);
        fdr = 0.1;
        updateModel();
        return "powerview";
    }

    private void updateModel() {
        double fdrOld = fdr;
        fdr = PowerUtils.performPowerProfile(sb.getRConnection(), fdr, smplSize);
        if (fdrOld > fdr) {
            sb.addMessage("Warn", "FDR level has been re-adjusted in order to get meaningful result.");
        }
        
        double[] pwrs = PowerUtils.plotPowerProfile(sb, fdr, smplSize, sb.getNewImage("power_profile"), "png", 72);
        int[] smplNum = PowerUtils.getPowerValueX(sb.getRConnection()); //must be called after plotPowerProile

        List<ScatterDataPoint> myValues = new ArrayList<>();

        for (int i = 0; i < pwrs.length; i++) {
            myValues.add(new ScatterDataPoint(smplNum[i], pwrs[i]));
        }

        ScatterDataset scatterDataset = new ScatterDataset()
                .setData(myValues)
                .setBackgroundColor("rgba(153,102,255,0.2)")
                .setBorderColor("rgb(153, 102, 255)")
                .setShowLine(true);

        ScatterChart scatterChart = new ScatterChart();
        ScatterData scatterData = new ScatterData();
        scatterChart.setData(scatterData);
        
        scatterChart.getData().addDataset(scatterDataset);

        // Optionally, set other options or customize the chart as needed
        //scatterChart.setOptions(/* Configure Options here */);
        // Assuming rendering method or conversion to a JSON/string representation for further use
        lineModel = scatterChart.toJson();
    }

    /*
    public void updateModel() {
        double fdrOld = fdr;
        fdr = PowerUtils.performPowerProfile(sb.getRConnection(), fdr, smplSize);
        if (fdrOld > fdr) {
            sb.addMessage("Warn", "FDR level has been re-adjusted in order to get meaningful result.");
        }

        double[] pwrs = PowerUtils.plotPowerProfile(sb, fdr, smplSize, sb.getNewImage("power_profile"), "png", 72);
        int[] smplNum = PowerUtils.getPowerValueX(sb.getRConnection()); //must be called after plotPowerProile
        lineModel = new ScatterChartModel();
        ChartData data = new ChartData();
        LineChartDataSet myDataSet = new LineChartDataSet();
        List<Object> myValues = new ArrayList<>();

        for (int i = 0; i < pwrs.length; i++) {
            myValues.add(new NumericPoint(smplNum[i], pwrs[i]));
        }
        myDataSet.setData(myValues);
        myDataSet.setBackgroundColor("rgba(153, 102, 255, 0.2)");
        myDataSet.setBorderColor("rgb(153, 102, 255)");
        myDataSet.setShowLine(true);
        data.addChartDataSet(myDataSet);
        lineModel.setData(data);
        lineModel.setExtender("extender");
    }*/
}
