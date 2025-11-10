/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.multifac;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.MetaDataBean;
import pro.metaboanalyst.models.SampleBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.PrimeFaces;
import org.primefaces.event.CellEditEvent;
import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author jianguox
 */
@SessionScoped
@Named("multifacBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class MultifacBean implements Serializable {

    //private final ClassificationBean cb = (ClassificationBean) DataUtils.findBean("classBean");
    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private LimmaBean lmb;

    private boolean disableMetaSelection = false;
    @JsonIgnore

    private List<MetaDataBean> metaDataBeans = null;
    @JsonIgnore
    private MetaDataBean selectedMetaDataBean;
    private String selectedMetaData;
    private String[] metaOpts;
    private String[] discMetaOpts;
    private String covJsonName;
    private double rawCovThresh = 0.05;
    private boolean ascaInit = false;
    private String mbImage = null;
    private int count = 0;
    @JsonIgnore
    private SelectItem[] discreteMetaOpts;
    private SelectItem[] boxMetaOpts;
    private SelectItem[] analysisMetaOpts;
    private String[] includedMetaData;
    private boolean covPerformed = false;
    private boolean corrPerformed = false;
    private boolean aov2Performed = false;
    private String boxMeta = "NA";
    private String boxMeta2 = "NA";
    private String boxId;
    private String defaultText = "The analysis has not been performed yet.";
    private SelectItem[] analysisMetaOptsAnova;
    private String covStyleOpt = "default";
    private List<SampleBean> uniqueMetaNames;
    private List<String> uniqueMetaStrings;
    private List<SelectItem> uniqueMetaList = null;
    private int boxMetaVersionNum = 0;
    private String compDesign = "cov";
    private String selectedCondition;
    private String selectedGrp1;
    private String selectedGrp2;
    private String selectedContrast1;
    private String selectedContrast2;
    private String nestCompOpt = "ref";
    @JsonIgnore
    private SelectItem[] grpContrasts = new SelectItem[]{new SelectItem("NA", "--- Not Available ---")};
    private SelectItem[] grps = new SelectItem[]{new SelectItem("NA", "--- Not Available ---")};
    private SelectItem[] conditions;
    private boolean interOnly = true;
    private boolean radioDisabled = false;
    private boolean disableInter = false;
    private boolean disableTwofac = false;

    public boolean isDisableMetaSelection() {
        return disableMetaSelection;
    }

    public void setDisableMetaSelection(boolean disableMetaSelection) {
        this.disableMetaSelection = disableMetaSelection;
    }

    @JsonIgnore
    public List<MetaDataBean> getMetaDataBeans() {
        if (metaDataBeans == null) {
            initMetaData();
        }
        return metaDataBeans;
    }

    public void initMetaData() {
        RConnection RC = sb.getRConnection();
        String[] metaDataGroups = RDataUtils.getMetaDataGroups(RC);
        String[] metaDataStatus = RDataUtils.getMetaDataStatus(RC);

        metaDataBeans = new ArrayList();
        String[] metatypes = RDataUtils.getMetaTypes(RC);
        if (metaDataGroups == null || metaDataStatus == null || metatypes == null) {
            sb.addMessage("Error", "Failed to parse the metadata information!");
            return;
        }
        for (int i = 0; i < metaDataGroups.length; i++) {
            String metatype = metatypes[i];
            String metastatus = metaDataStatus[i];
            //whether disabled or not in metadatacheck page
            //if tsdesign equals time or time0, disable metadata if it's name "time"

            if (i == 0) {
                if (metaDataGroups[i].toLowerCase().equals("time") && !sb.getTsDesign().equals("multi")) {
                    metaDataBeans.add(new MetaDataBean(sb, metaDataGroups[i], metatype, i, true, true, metastatus));
                } else {
                    metaDataBeans.add(new MetaDataBean(sb, metaDataGroups[i], metatype, i, true, false, metastatus));
                }
            } else if ((metaDataGroups[i].toLowerCase().equals("time") || metaDataGroups[i].toLowerCase().equals("subject")) && sb.getTsDesign().equals("time0")) {
                metaDataBeans.add(new MetaDataBean(sb, metaDataGroups[i], metatype, i, false, true, metastatus));
            } else {
                metaDataBeans.add(new MetaDataBean(sb, metaDataGroups[i], metatype, i, false, false, metastatus));
            }
        }

        selectedMetaDataBean = metaDataBeans.get(0);

    }

    public MetaDataBean getSelectedMetaDataBean() {
        return selectedMetaDataBean;
    }

    public void setSelectedMetaDataBean(MetaDataBean selectedMetaDataBean) {
        this.selectedMetaDataBean = selectedMetaDataBean;
    }

    public void removeMeta() {
        if (selectedMetaDataBean.isPrimary()) {
            sb.addMessage("Error", "Cannot remove the primary metadata!");
            return;
        }

        if (metaDataBeans.size() == 2) {
            sb.addMessage("Error", "Cannot remove the metadata, at least two is required to proceed!");
            return;
        }

        RDataUtils.removeSelectedMeta(sb.getRConnection(), selectedMetaDataBean.getName());
        metaDataBeans.remove(selectedMetaDataBean);
        reinitVariables();
    }

    public void resetState() {
        reinitVariables();
        discMetaOpts = null;
        discreteMetaOpts = null;
        uniqueMetaList = null;
        uniqueMetaStrings = null;
        uniqueMetaNames = null;
    }

    public void reinitVariables() {
        analysisMetaOpts = null;
        metaDataBeans = null;
        boxMetaOpts = null;
        includedMetaData = null;
    }

    public void editMeta(MetaDataBean meta) {
        selectedMetaDataBean = meta;
        selectedMetaData = meta.getName();
        sb.setSampleBeans(RDataUtils.createOrigSampleBeans(sb.getRConnection(), selectedMetaData, false));

        if (!selectedMetaDataBean.getParam().equals("cont")) {
            String[] allMetas = RDataUtils.getUniqueMetaNames(sb.getRConnection(), selectedMetaDataBean.getName());
            int samSize = allMetas.length;
            uniqueMetaList = new ArrayList();
            uniqueMetaStrings = new ArrayList();
            uniqueMetaNames = new ArrayList<>();
            for (int i = 0; i < samSize; i++) {
                uniqueMetaStrings.add(allMetas[i]);
                uniqueMetaList.add(new SelectItem(allMetas[i], allMetas[i]));
                uniqueMetaNames.add(new SampleBean(allMetas[i], allMetas[i]));
            }
        }
    }

    public String getSelectedMetaData() {
        return selectedMetaData;
    }

    public void setSelectedMetaData(String selectedMetaData) {
        this.selectedMetaData = selectedMetaData;
    }

    @JsonIgnore
    public String[] getMetaOpt() {
        if (metaOpts == null) {
            metaOpts = new String[2];
            for (int i = 0; i < 2; i++) {
                metaOpts[i] = getMetaDataBeans().get(i).getName();
            }
        }
        return metaOpts;
    }

    @JsonIgnore
    public String[] getDiscMetaOpts() {
        if (discMetaOpts == null) {
            discMetaOpts = new String[2];
            int discCount = 0;
            for (int i = 0; i < getMetaDataBeans().size(); i++) {
                if (!getMetaDataBeans().get(i).getParam().equals("cont")) {
                    discMetaOpts[discCount] = getMetaDataBeans().get(i).getName();
                    discCount++;
                    if (discCount == 2) {
                        break;
                    }
                }
            }
            if (discCount < 2) {
                sb.addMessage("Error", "This analysis can not be performed. At least two metadata of categorical type are required!");
                return null;
            }
        }
        return discMetaOpts;
    }

    public double getRawCovThresh() {
        return rawCovThresh;
    }

    public void setRawCovThresh(double rawCovThresh) {
        this.rawCovThresh = rawCovThresh;
    }

    @JsonIgnore
    public String getAov2Img() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("aov2") + "dpi150.png";
    }

    public String getCovJsonName() {
        return covJsonName;
    }

    public void setCovJsonName(String covJsonName) {
        this.covJsonName = covJsonName;
    }

    public boolean isAscaInit() {
        return ascaInit;
    }

    public void setAscaInit(boolean ascaInit) {
        this.ascaInit = ascaInit;
    }

    public void cmpdLnk_action() {
        mbImage = TimeSeries.plotMBTimeProfile(sb, sb.getCurrentCmpdName(), count, "png", 150 + "");
        count++;
    }

    @JsonIgnore
    public String getMEBACmpdImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + mbImage;
    }

    @JsonIgnore
    public SelectItem[] getAnalysisMetaOpts() {
        if (isMultiMeta()) {
            if (analysisMetaOpts == null) {
                List<MetaDataBean> beans = getMetaDataBeans();
                analysisMetaOpts = new SelectItem[beans.size()];
                for (int i = 0; i < beans.size(); i++) {
                    //System.out.println("======here 3");
                    analysisMetaOpts[i] = new SelectItem(beans.get(i).getName(), beans.get(i).getName());
                }
            }
        } else {
            analysisMetaOpts = new SelectItem[1];
            if (sb.getAnalType().equals("dose")) {
                //System.out.println("======here 00");
                analysisMetaOpts[0] = new SelectItem("NA", "Dose");
            } else {
                //System.out.println("======here 000");
                analysisMetaOpts[0] = new SelectItem("NA", "Class");
            }
        }

        return analysisMetaOpts;
    }

    @JsonIgnore

    public SelectItem[] getDiscreteMetaOpts() {

        if (discreteMetaOpts == null) {
            List<MetaDataBean> beans = getMetaDataBeans();
            int discCount = 0;

            for (MetaDataBean bean : beans) {
                if (!"cont".equals(bean.getParam())) {
                    discCount++;
                }
            }

            // +1 to include the "NA" option
            discreteMetaOpts = new SelectItem[discCount + 1];
            discreteMetaOpts[0] = new SelectItem("NA", "Not selected");

            int arrInx = 1; // start after the NA option
            for (MetaDataBean bean : beans) {
                if (!"cont".equals(bean.getParam())) {
                    discreteMetaOpts[arrInx] = new SelectItem(bean.getName(), bean.getName());
                    arrInx++;
                }
            }

            // fallback if somehow no options were added (excluding "NA")
            if (discCount == 0) {
                discreteMetaOpts = new SelectItem[]{new SelectItem("NA", "Not selected")};
            }
        }

        return discreteMetaOpts;
    }

    public void updateMetaData() {

        List<MetaDataBean> beans = getMetaDataBeans();
        String[] metas = new String[beans.size()];
        for (int i = 0; i < beans.size(); i++) {
            metas[i] = beans.get(i).getParam();
            if (metas[i].equals("NA")) {
                sb.addMessage("Error", "Please specify data type of meta-data classes.");
                return;
            }
        }
        RDataUtils.setMetaTypes(sb.getRConnection(), metas);

        if (getIncludedMetaData().length > 8) { // disable this requirement
            sb.addMessage("Error", "Please select at most eight different meta-data classes.");
            return;
        }
        sb.addMessage("Info", "Meta-data successfully updated!");
    }

    @JsonIgnore
    public SelectItem[] getBoxMetaOpts() {
        if (boxMetaOpts == null) {
            List<MetaDataBean> beans = getMetaDataBeans();
            boxMetaOpts = new SelectItem[beans.size()];
            for (int i = 0; i < beans.size(); i++) {
                //if (i > 0) {
                boxMetaOpts[i] = new SelectItem(beans.get(i).getName(), beans.get(i).getName());
                //}
                if (i == 0) {
                    boxMeta = beans.get(i).getName();
                    sb.setExpFac(boxMeta);
                } else if (i == 1) {
                    boxMeta2 = beans.get(i).getName();
                }
            }
        }
        return boxMetaOpts;
    }

    public void setBoxMetaOpts(SelectItem[] boxMetaOpts) {
        this.boxMetaOpts = boxMetaOpts;
    }

    @JsonIgnore
    public String[] getIncludedMetaData() {
        if (includedMetaData == null) {
            String[] metanms = RDataUtils.getMetaDataGroups(sb.getRConnection());
            includedMetaData = metanms;
        }
        return includedMetaData;
    }

    public void setIncludedMetaData(String[] includedMetaData) {
        this.includedMetaData = includedMetaData;
    }

    public boolean isCovPerformed() {
        return covPerformed;
    }

    public void setCovPerformed(boolean covPerformed) {
        this.covPerformed = covPerformed;
    }

    public boolean isCorrPerformed() {
        return corrPerformed;
    }

    public void setCorrPerformed(boolean corrPerformed) {
        this.corrPerformed = corrPerformed;
    }

    public boolean isAov2Performed() {
        return aov2Performed;
    }

    public void setAov2Performed(boolean aov2Performed) {
        this.aov2Performed = aov2Performed;
    }

    public String getBoxMeta() {
        return boxMeta;
    }

    public void setBoxMeta(String boxMeta) {
        this.boxMeta = boxMeta;
    }

    public void updateBoxplotMeta() {

        if (!boxMeta.equals("NA") || !boxMeta2.equals("NA")) {
            if (boxMeta.equals(boxMeta2)) {
                sb.addMessage("Error", "Primary factor can not be the same as secondary factor.");
                return;
            }
        }

        List<MetaDataBean> beans = getMetaDataBeans();
        for (int i = 0; i < beans.size(); i++) {
            MetaDataBean bean = beans.get(i);
            /*
            if (boxMeta.equals(bean.getName()) && bean.getParam().equals("cont")) {
                sb.addMessage("Error", "Primary factor can not be of continuous type.");
                return;
            }
             */
        }
        UniVarTests.setCmpdSummaryType(sb.getRConnection(), sb.getCmpdSummaryType());
        String cmpdName = UniVarTests.plotCmpdSummary(sb, boxId, boxMeta, boxMeta2, boxMetaVersionNum, "png", 150 + "");
        String imgUrl = "/MetaboAnalyst/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + cmpdName;
        System.out.println(imgUrl);
        sb.setBoxplotUrl(imgUrl);
        sb.setCurrentCmpdName(boxId);//important for high resolution image export
        boxMetaVersionNum = boxMetaVersionNum + 1;
        //sb.setLinMod(false);
        //PrimeFaces.current().executeScript("PF('FeatureView').show();");
    }

    public void updateMultiFacBoxplotMeta(String type) {

        String cmpdName;
        if (type.equals("default")) {
            cmpdName = UniVarTests.plotCmpdSummary(sb, boxId, lmb.getAnalysisMeta(), "NA", 0, "png", 150 + "");
        } else {
            cmpdName = UniVarTests.plotCmpdSummary(sb, boxId, boxMeta, boxMeta2, 0, "png", 150 + "");
        }
        UniVarTests.setCmpdSummaryType(sb.getRConnection(), sb.getCmpdSummaryType());
        String imgUrl = "/MetaboAnalyst/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + cmpdName;
        sb.setBoxplotUrl(imgUrl);
        //sb.setLinMod(true);
        sb.setCurrentCmpdName(boxId);//important for high resolution image export
        PrimeFaces.current().executeScript("PF('FeatureView').show();");
    }

    public void onCellEdit(CellEditEvent<String> event) {
        String oldValue = event.getOldValue();
        String newValue = event.getNewValue();

        if (newValue != null && !newValue.equals(oldValue)) {
            //need to update feature
            List<MetaDataBean> beans = getMetaDataBeans();
            int textcount = 0;
            for (int i = 0; i < beans.size(); i++) {

                if (beans.get(i).getName().equalsIgnoreCase(newValue)) {
                    textcount++;
                }
            }

            if (textcount > 1) {
                getMetaDataBeans().get(event.getRowIndex()).setName(oldValue);
                sb.addMessage("Error", "The new name must be unique among the other metadata names!");
                return;
            }

            RDataUtils.updateMetaColName(sb.getRConnection(), oldValue, newValue);
            sb.addMessage("info", "Updated from: " + oldValue + " to:" + newValue);
            reinitVariables();

            //need to recompute, set states to naive
            sb.resetAnalysis();
        }
    }

    public String getBoxId() {
        return boxId;
    }

    public void setBoxId(String boxId) {
        this.boxId = boxId;
    }

    public String getDefaultText() {
        return defaultText;
    }

    public void setDefaultText(String defaultText) {
        this.defaultText = defaultText;
    }

    @JsonIgnore
    public SelectItem[] getAnalysisMetaOptsAnova() {
        if (analysisMetaOptsAnova == null) {
            List<MetaDataBean> beans = getMetaDataBeans();
            analysisMetaOptsAnova = new SelectItem[beans.size()];
            for (int i = 0; i < beans.size(); i++) {
                String metaname = beans.get(i).getName();
                if (metaname.equals("Time")) {
                    analysisMetaOptsAnova[i] = new SelectItem(beans.get(i).getName(), beans.get(i).getName(), null, true);
                } else {
                    analysisMetaOptsAnova[i] = new SelectItem(beans.get(i).getName(), beans.get(i).getName(), null, false);
                }
            }
        }
        return analysisMetaOptsAnova;
    }

    public String getCovStyleOpt() {
        return covStyleOpt;
    }

    public void setCovStyleOpt(String covStyleOpt) {
        this.covStyleOpt = covStyleOpt;
    }

    public List<SampleBean> getUniqueMetaNames() {
        return uniqueMetaNames;
    }

    public void setUniqueMetaNames(List<SampleBean> uniqueMetaNames) {
        this.uniqueMetaNames = uniqueMetaNames;
    }

    @JsonIgnore
    public List<SelectItem> getUniqueMetaList() {
        if (uniqueMetaList == null) {
            prepUniqueMetaList("NA");
        }
        return uniqueMetaList;
    }

    public void setUniqueMetaList(List<SelectItem> uniqueMetaList) {
        this.uniqueMetaList = uniqueMetaList;
    }

    public List<String> getUniqueMetaStrings() {
        if (uniqueMetaStrings == null) {
            prepUniqueMetaList("NA");
        }
        return uniqueMetaStrings;
    }

    public void setUniqueMetaStrings(List<String> uniqueMetaStrings) {
        this.uniqueMetaStrings = uniqueMetaStrings;
    }

    public void updateMetaOrder() throws REngineException {
        String[] metaOrderArray = uniqueMetaList.toArray(String[]::new);
        RDataUtils.updateMetaOrder(sb.getRConnection(), selectedMetaData, metaOrderArray);
        sb.addMessage("Info", "Successfully updated the order of metadata groups!");
    }

    public String getBoxMeta2() {
        return boxMeta2;
    }

    public void setBoxMeta2(String boxMeta2) {
        this.boxMeta2 = boxMeta2;
    }

    public int getBoxMetaVersionNum() {
        return boxMetaVersionNum;
    }

    public void setBoxMetaVersionNum(int boxMetaVersionNum) {
        this.boxMetaVersionNum = boxMetaVersionNum;
    }

    public String getCompDesign() {
        return compDesign;
    }

    public void setCompDesign(String compDesign) {
        this.compDesign = compDesign;
    }

    public String getSelectedCondition() {
        return selectedCondition;
    }

    public void setSelectedCondition(String selectedCondition) {
        this.selectedCondition = selectedCondition;
    }

    public String getSelectedGrp1() {
        return selectedGrp1;
    }

    public void setSelectedGrp1(String selectedGrp1) {
        this.selectedGrp1 = selectedGrp1;
    }

    public String getSelectedGrp2() {
        return selectedGrp2;
    }

    public void setSelectedGrp2(String selectedGrp2) {
        this.selectedGrp2 = selectedGrp2;
    }

    public String getSelectedContrast1() {
        return selectedContrast1;
    }

    public void setSelectedContrast1(String selectedContrast1) {
        this.selectedContrast1 = selectedContrast1;
    }

    public String getSelectedContrast2() {
        return selectedContrast2;
    }

    public void setSelectedContrast2(String selectedContrast2) {
        this.selectedContrast2 = selectedContrast2;
    }

    public SelectItem[] getGrpContrasts() {
        return grpContrasts;
    }

    public void setGrpContrasts(String[] nms) {
        grps = new SelectItem[nms.length];
        for (int i = 0; i < nms.length; i++) {
            grps[i] = new SelectItem(nms[i], nms[i]);
        }
        //set default
        selectedGrp1 = nms[0];
        selectedGrp2 = nms[1];

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

    public SelectItem[] getGrps() {
        return grps;
    }

    public void setGrps(SelectItem[] grps) {
        this.grps = grps;
    }

    public SelectItem[] getConditions() {
        return conditions;
    }

    public void setConditions(SelectItem[] conditions) {
        this.conditions = conditions;
    }

    public boolean isInterOnly() {
        return interOnly;
    }

    public void setInterOnly(boolean interOnly) {
        this.interOnly = interOnly;
    }

    public String getNestCompOpt() {
        return nestCompOpt;
    }

    public void setNestCompOpt(String nestCompOpt) {
        this.nestCompOpt = nestCompOpt;
    }

    public boolean isRadioDisabled() {
        return radioDisabled;
    }

    public void setRadioDisabled(boolean radioDisabled) {
        this.radioDisabled = radioDisabled;
    }

    public boolean isDisableInter() {
        return disableInter;
    }

    public void setDisableInter(boolean disableInter) {
        this.disableInter = disableInter;
    }

    public boolean isDisableTwofac() {
        return disableTwofac;
    }

    public void setDisableTwofac(boolean disableTwofac) {
        this.disableTwofac = disableTwofac;
    }

    public void prepUniqueMetaList(String metacol) {
        String[] allMetas = RDataUtils.getMetaDataCol(sb.getRConnection(), metacol);
        uniqueMetaStrings = new ArrayList<>();
        uniqueMetaList = new ArrayList<>();
        for (String grp : allMetas) {
            uniqueMetaStrings.add(grp);
            uniqueMetaList.add(new SelectItem(grp, grp));
        }

    }

    @JsonIgnore
    public boolean isMultiMeta() {

        //String[] metaDataGroups = RDataUtils.getMetaDataGroups(sb.getRConnection());
        //return metaDataGroups.length > 1;
        return getMetaDataBeans().size() > 1;
    }

}
