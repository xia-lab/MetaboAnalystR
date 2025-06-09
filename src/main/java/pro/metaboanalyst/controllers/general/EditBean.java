/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

@SessionScoped
@Named("editBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditBean implements Serializable {
    
    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    private DualListModel<String> sampleItems, featureItems, groupItems;

    public DualListModel<String> getGroupItems() {
        return groupItems;
    }

    public void setGroupItems(DualListModel<String> groupItems) {
        this.groupItems = groupItems;
    }

    public void setSampleItems(DualListModel<String> sampleItems) {
        this.sampleItems = sampleItems;
    }

    public void setFeatureItems(DualListModel<String> featureItems) {
        this.featureItems = featureItems;
    }

    public DualListModel<String> getSampleItems() {
        return sampleItems;
    }

    public DualListModel<String> getFeatureItems() {
        return featureItems;
    }

    private String bigFeatureList;

    public String getBigFeatureList() {
        return bigFeatureList;
    }

    public void setBigFeatureList(String bigFeatureList) {
        this.bigFeatureList = bigFeatureList;
    }

    public void prepareDataEditor() {
        if (!sb.isAnalInit("Data editor") || sampleItems == null) {
            RConnection RC = sb.getRConnection();
            String[] allSamples = RDataUtils.getPrenormSampleNames(RC);
            sampleItems = new DualListModel(Arrays.asList(allSamples), new ArrayList());
            //not work for multifactor
            if (!sb.getAnalType().equals("mf")) {
                String[] allGroups = RDataUtils.getGroupNames(RC, "");
                groupItems = new DualListModel(Arrays.asList(allGroups), new ArrayList());
            }
            //for features, this is only meaningful for small number of features (< 200) for manual inspection
            //more than that, users should directly enter the features on the other side to exclude
            if (!sb.isBigFeature()) {
                String[] names = RDataUtils.getPrenormFeatureNames(RC);
                featureItems = new DualListModel(Arrays.asList(names), new ArrayList());
            }
        }
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public String editData() {
        //edit feature
        String[] featVec;
        if (sb.isBigFeature()) {
            featVec = DataUtils.getQueryNames(bigFeatureList.trim(), null);
        } else {
            featVec = featureItems.getTarget().toArray(String[]::new);
        }
        if (featVec.length == 0) {
            featVec = new String[]{""};
        }

        String[] smplVec = sampleItems.getTarget().toArray(String[]::new);
        if (smplVec.length == 0) {
            smplVec = new String[]{""};
        }

        String[] useVec = new String[]{""};
        //note here we use source (rather than target) - it also contains order information
        if (!sb.getAnalType().equals("mf")) {
            useVec = groupItems.getSource().toArray(String[]::new);
            if (useVec.length < 2) {
                sb.addMessage("Error", "At least two groups are required.");
                return null;
            }
        }

        RConnection RC = sb.getRConnection();
        String ordGrp = sb.isKeepClsOrder()? "T" : "F";
        int res = RDataUtils.updateData(RC, featVec, smplVec, useVec, ordGrp);
        if (res > 0) {
            sb.addMessage("OK", RDataUtils.getCurrentMsg(RC));
            sb.setMultiGroup(res > 2);
            return "Normalization";
        } else {
            sb.addMessage("Error", RDataUtils.getErrMsg(RC));
            return null;
        }
    }
}
