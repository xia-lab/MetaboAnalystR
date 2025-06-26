/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.dose;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.DetailsBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.ColumnModel;
import pro.metaboanalyst.utils.DataUtils;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.SelectItem;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import org.primefaces.model.DefaultStreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.models.MetaDataBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RDoseUtils;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 *
 * @author soufanom
 */
@SessionScoped
@Named("doseResponseBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class DoseResponseBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;    //private final DoseResponseController drCtrl = (DoseResponseController) DataUtils.findBean("drCtrl");

    @JsonIgnore
    @Inject
    private MultifacBean tb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private DetailsBean dtb;

    private boolean exp2 = true;
    private boolean exp3 = true;
    private boolean exp4 = true;
    private boolean exp5 = false;
    private boolean lin = true;
    private boolean poly2 = true;
    private boolean poly3 = false;
    private boolean poly4 = false;
    private boolean hill = false;
    private boolean power = true;

    private boolean l4 = true;
    private boolean ll4 = true;
    private boolean ll24 = false;
    private boolean l5 = true;
    private boolean ll5 = true;
    private boolean ll25 = false;
    private boolean l3 = false;
    private boolean ll3 = false;
    private boolean ll23 = false;
    private boolean w14 = true;
    private boolean w24 = false;
    private boolean w13 = false;
    private boolean w23 = false;
    private boolean bc4 = false;
    private boolean bc5 = false;
    private boolean ar3 = false;
    private boolean mm3 = false;

    private boolean FDR = true;
    private boolean sigOK = false;
    private String[] adjustedMeta;

    public boolean isFDR() {
        return FDR;
    }

    public void setFDR(boolean FDR) {
        this.FDR = FDR;
    }
    // variables related to dose-response
    private String podJSONFileName;
    private String bmdenrichJSONFileName;
    private String units = "mg/kg";
    private String transDose = "natural";
    private String ctrlMode = "sampleMean";
    private String bmd = "sampleMean";

    // variables related to new nav tree
    private List<DoseResultBean> resBeans = new ArrayList<>();
    private List<ColumnModel> columns = new ArrayList<>();
    private SelectItem[] tableFields;

    private boolean robustTrend = false;
    private boolean aicModelSelection = true;
    private boolean pvalModelSelection = false;
    private double cutoffval = 0.1;
    private boolean modelFitPerformed = false;
    private String numsds = "1.0";
    private boolean bmdAnal = false;
    private String deMethod = "limma";
    private String selectedCondition = "";

    private double fcLevel = 0.0;
    private double sigLevel = 0.05;
    private double ceffLevel = 0.75;
    private boolean dePerformed = false;
    private String selectedMeta0;
    private String bmdOption = "all";
    //user for API calls; the tmp folder on microservice server
    private String guestId;
    private DoseResultBean selectedFeature;
    private String currentFeatureImg;
    private String[] ids = null;
    private int curvNum = 0, bmdNum = 0;

    private boolean contineousDoes = true;

    private String analysisMeta;
    private String primaryType;
    private String referenceGroupFromAnalysisMeta = "NA";

    public String getAnalysisMeta() {
        if (tb.isMultiMeta()) {
            if (analysisMeta == null) {
                List<MetaDataBean> beans = tb.getMetaDataBeans();
                analysisMeta = beans.get(0).getName();
                primaryType = RDataUtils.GetPrimaryType(sb.getRConnection(), analysisMeta);
                contineousDoes = !primaryType.equals("disc");
            }
        } else {
            analysisMeta = "NA";
        }
        return analysisMeta;
    }

    @JsonIgnore
    private SelectItem[] referenceGroupFromAnalysisMetaOpts;

    public String getPrimaryType() {
        return primaryType;
    }

    public void setPrimaryType(String primaryType) {
        this.primaryType = primaryType;
    }

    public String getReferenceGroupFromAnalysisMeta() {
        return referenceGroupFromAnalysisMeta;
    }

    public void setReferenceGroupFromAnalysisMeta(String referenceGroupFromAnalysisMeta) {
        this.referenceGroupFromAnalysisMeta = referenceGroupFromAnalysisMeta;
    }

    public void setAnalysisMeta(String analysisMeta) {
        this.analysisMeta = analysisMeta;
    }

    public String[] getAdjustedMeta() {
        return adjustedMeta;
    }

    public void setAdjustedMeta(String[] adjustedMeta) {
        this.adjustedMeta = adjustedMeta;
    }

    public boolean isContineousDoes() {
        return contineousDoes;
    }

    public void setContineousDoes(boolean contineousDoes) {
        this.contineousDoes = contineousDoes;
    }

    public boolean isSigOK() {
        return sigOK;
    }

    public void setSigOK(boolean sigOK) {
        this.sigOK = sigOK;
    }

    public double getFcLevel() {
        return fcLevel;
    }

    public void setFcLevel(double fcLevel) {
        this.fcLevel = fcLevel;
    }

    public double getSigLevel() {
        return sigLevel;
    }

    public void setSigLevel(double sigLevel) {
        this.sigLevel = sigLevel;
    }

    public double getCeffLevel() {
        return ceffLevel;
    }

    public void setCeffLevel(double ceffLevel) {
        this.ceffLevel = ceffLevel;
    }

    public String getSelectedCondition() {
        return selectedCondition;
    }

    public void setSelectedCondition(String selectedCondition) {
        this.selectedCondition = selectedCondition;
    }

    public String getSelectedMeta0() {
        return selectedMeta0;
    }

    public void setSelectedMeta0(String selectedMeta0) {
        this.selectedMeta0 = selectedMeta0;
    }

    public String getDeMethod() {
        return deMethod;
    }

    public void setDeMethod(String deMethod) {
        this.deMethod = deMethod;
    }

    public String getPodJSONFileName() {
        return podJSONFileName;
    }

    public void setPodJSONFileName(String podJSONFileName) {
        this.podJSONFileName = podJSONFileName;
    }

    public String getBmdenrichJSONFileName() {
        return bmdenrichJSONFileName;
    }

    public void setBmdenrichJSONFileName(String bmdenrichJSONFileName) {
        this.bmdenrichJSONFileName = bmdenrichJSONFileName;
    }

    public String getUnits() {
        return units;
    }

    public void setUnits(String units) {
        this.units = units;
    }

    public String getTransDose() {
        return transDose;
    }

    public void setTransDose(String transDose) {
        this.transDose = transDose;
    }

    public String getCtrlMode() {
        return ctrlMode;
    }

    public void setCtrlMode(String ctrlMode) {
        this.ctrlMode = ctrlMode;
    }

    public String[] getIds() {
        return ids;
    }

    public void setIds(String[] ids) {
        this.ids = ids;
    }

    public boolean isExp2() {
        return exp2;
    }

    public void setExp2(boolean exp2) {
        this.exp2 = exp2;
    }

    public boolean isExp3() {
        return exp3;
    }

    public void setExp3(boolean exp3) {
        this.exp3 = exp3;
    }

    public boolean isExp4() {
        return exp4;
    }

    public void setExp4(boolean exp4) {
        this.exp4 = exp4;
    }

    public boolean isExp5() {
        return exp5;
    }

    public void setExp5(boolean exp5) {
        this.exp5 = exp5;
    }

    public boolean isLin() {
        return lin;
    }

    public void setLin(boolean lin) {
        this.lin = lin;
    }

    public boolean isPoly2() {
        return poly2;
    }

    public void setPoly2(boolean poly2) {
        this.poly2 = poly2;
    }

    public boolean isPoly3() {
        return poly3;
    }

    public void setPoly3(boolean poly3) {
        this.poly3 = poly3;
    }

    public boolean isPoly4() {
        return poly4;
    }

    public void setPoly4(boolean poly4) {
        this.poly4 = poly4;
    }

    public boolean isHill() {
        return hill;
    }

    public void setHill(boolean hill) {
        this.hill = hill;
    }

    public boolean isL4() {
        return l4;
    }

    public void setL4(boolean l4) {
        this.l4 = l4;
    }

    public boolean isLl4() {
        return ll4;
    }

    public void setLl4(boolean ll4) {
        this.ll4 = ll4;
    }

    public boolean isLl24() {
        return ll24;
    }

    public void setLl24(boolean ll24) {
        this.ll24 = ll24;
    }

    public boolean isL5() {
        return l5;
    }

    public void setL5(boolean l5) {
        this.l5 = l5;
    }

    public boolean isLl5() {
        return ll5;
    }

    public void setLl5(boolean ll5) {
        this.ll5 = ll5;
    }

    public boolean isLl25() {
        return ll25;
    }

    public void setLl25(boolean ll25) {
        this.ll25 = ll25;
    }

    public boolean isL3() {
        return l3;
    }

    public void setL3(boolean l3) {
        this.l3 = l3;
    }

    public boolean isLl3() {
        return ll3;
    }

    public void setLl3(boolean ll3) {
        this.ll3 = ll3;
    }

    public boolean isLl23() {
        return ll23;
    }

    public void setLl23(boolean ll23) {
        this.ll23 = ll23;
    }

    public boolean isW14() {
        return w14;
    }

    public void setW14(boolean w14) {
        this.w14 = w14;
    }

    public boolean isW24() {
        return w24;
    }

    public void setW24(boolean w24) {
        this.w24 = w24;
    }

    public boolean isW13() {
        return w13;
    }

    public void setW13(boolean w13) {
        this.w13 = w13;
    }

    public boolean isW23() {
        return w23;
    }

    public void setW23(boolean w23) {
        this.w23 = w23;
    }

    public boolean isBc4() {
        return bc4;
    }

    public void setBc4(boolean bc4) {
        this.bc4 = bc4;
    }

    public boolean isBc5() {
        return bc5;
    }

    public void setBc5(boolean bc5) {
        this.bc5 = bc5;
    }

    public boolean isAr3() {
        return ar3;
    }

    public void setAr3(boolean ar3) {
        this.ar3 = ar3;
    }

    public boolean isMm3() {
        return mm3;
    }

    public void setMm3(boolean mm3) {
        this.mm3 = mm3;
    }

    public boolean isPower() {
        return power;
    }

    public void setPower(boolean power) {
        this.power = power;
    }

    public List<DoseResultBean> getResBeans() {
        return resBeans;
    }

    public void setResBeans(List<DoseResultBean> resBeans) {
        this.resBeans = resBeans;
    }

    public List<ColumnModel> getColumns() {
        return columns;
    }

    public void setColumns(List<ColumnModel> columns) {
        this.columns = columns;
    }

    public SelectItem[] getTableFields() {
        return tableFields;
    }

    public void setTableFields(SelectItem[] tableFields) {
        this.tableFields = tableFields;
    }

    public double getCutoffval() {
        return cutoffval;
    }

    public void setCutoffval(double cutoffval) {
        this.cutoffval = cutoffval;
    }

    public boolean isModelFitPerformed() {
        return modelFitPerformed;
    }

    public void setModelFitPerformed(boolean modelFitPerformed) {
        this.modelFitPerformed = modelFitPerformed;
    }

    public boolean isBmdAnal() {
        return bmdAnal;
    }

    public void setBmdAnal(boolean bmdAnal) {
        this.bmdAnal = bmdAnal;
    }

    public String getBmdOption() {
        return bmdOption;
    }

    public void setBmdOption(String bmdOption) {
        this.bmdOption = bmdOption;
    }

    public String getNumsds() {
        return numsds;
    }

    public void setNumsds(String numsds) {
        this.numsds = numsds;
    }

    /*Operational methods*/
    public int fitModel() {
        bmdAnal = false;

        List<String> mlist = new ArrayList<>();
        if (exp2) {
            mlist.add("Exp2");
        }
        if (exp3) {
            mlist.add("Exp3");
        }
        if (exp4) {
            mlist.add("Exp4");
        }
        if (exp5) {
            mlist.add("Exp5");
        }
        if (poly2) {
            mlist.add("Poly2");
        }
        if (poly3) {
            mlist.add("Poly3");
        }
        if (poly4) {
            mlist.add("Poly4");
        }
        if (lin) {
            mlist.add("Lin");
        }
        if (power) {
            mlist.add("Power");
        }
        if (hill) {
            mlist.add("Hill");
        }
        RConnection RC = sb.getRConnection();
        int numDoses = RDoseUtils.getNumDoses(RC);
        if (numDoses < 4 && (exp5 || hill)) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error!",
                            "You must have more than three non-zero dose groups for Exp5 and Hill models!"));
            return 0;
        }

        String[] models = mlist.toArray(String[]::new);

        int res;
        modelFitPerformed = false;
        RDoseUtils.prepareDataForDoseResponse(sb);
        int resPrep = RDoseUtils.prepareSignificantItems(sb, sigLevel, fcLevel, FDR, false, 0.05);
        if (resPrep == 0) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error!",
                            "No significantly responsive items were found (based on current specifications) for dose response curve analysis!"));
            return 0;
        }
        //this should run when the microservice is down!
        res = RDoseUtils.performModelFit(sb, models);

        //System.out.println("=========" + res);
        //one more try
        if (res != 1) {
            res = RDoseUtils.performModelFit(sb, models);
        }

        if (res != 1) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error!",
                            "Failed to perform dose response curve analysis!"));
            return 1;
        }
        //TODO: replace with API call or regular call if microservice is down
        //RDoseResponseUtils.performModelFit(sb.getRConnection(), models);
//        try {
//            //firs time, user id is null. So, createTmpfolder but other calls do not require creating tmp folder.
//            String cpus = "18"; //number of cores to be used for parallel processing
//            if (guestId == null) {
//                guestId = DoseResponseAPIUtils.createTmpSessionFolder();
//                //if guestId is still null after calling API, then, switch to local version
//                if (guestId == null) {
//                    RDoseUtils.performModelFit(sb.getRConnection(), models);
//                } else {
//                    drCtrl.performModelFit(sb.getRConnection(), models, guestId, cpus);
//                    //RDoseResponseUtils.performModelFit(sb.getRConnection(), models);
//                }
//            }else{
//                drCtrl.performModelFit(sb.getRConnection(), models, guestId, cpus);
//                //RDoseResponseUtils.performModelFit(sb.getRConnection(), models);
//            }         
//        } catch (Exception e) {
//            //this should run when the microservice is down!
//            RDoseUtils.performModelFit(sb.getRConnection(), models);
//        }

        modelFitPerformed = true;

        String modelSelOption;
        String signLevel;
        modelSelOption = "both";
        signLevel = Double.toString(cutoffval);

        RDoseUtils.performDrcResFilter(sb, modelSelOption, signLevel, models);

        return 1;
    }

    public int fitContModel() {
        bmdAnal = false;
        List<String> mlist = new ArrayList<>();
        if (l4) {
            mlist.add("l4");
        }
        if (ll4) {
            mlist.add("ll4");
        }
        if (ll24) {
            mlist.add("ll24");
        }
        if (l5) {
            mlist.add("l5");
        }
        if (ll5) {
            mlist.add("ll5");
        }
        if (ll25) {
            mlist.add("ll25");
        }
        if (l3) {
            mlist.add("l3");
        }
        if (ll3) {
            mlist.add("ll3");
        }
        if (ll23) {
            mlist.add("ll23");
        }
        if (w14) {
            mlist.add("w14");
        }
        if (w24) {
            mlist.add("w24");
        }
        if (w13) {
            mlist.add("w13");
        }
        if (w23) {
            mlist.add("w23");
        }
        if (bc4) {
            mlist.add("bc4");
        }
        if (bc5) {
            mlist.add("bc5");
        }
        if (ar3) {
            mlist.add("ar3");
        }
        if (mm3) {
            mlist.add("mm3");
        }

        RConnection RC = sb.getRConnection();
        String[] models = mlist.toArray(String[]::new);

        int res;
        modelFitPerformed = false;
        RDoseUtils.prepareDataForDoseResponse(sb);

        res = RDoseUtils.performContModelFit(RC, models);
        if (res != 1) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error!",
                            "Failed to perform dose response curve analysis!"));
            return 1;
        }

        modelFitPerformed = true;

        String modelSelOption;
        String signLevel;
        modelSelOption = "both";
        signLevel = Double.toString(cutoffval);

        RDoseUtils.performDrcResFilterCont(sb.getRConnection(), modelSelOption, signLevel, models);

        return 1;
    }

    public DoseResultBean getSelectedFeature() {
        return selectedFeature;
    }

    public boolean isAicModelSelection() {
        return aicModelSelection;
    }

    public void setAicModelSelection(boolean aicModelSelection) {
        this.aicModelSelection = aicModelSelection;
    }

    public boolean isPvalModelSelection() {
        return pvalModelSelection;
    }

    public void setPvalModelSelection(boolean pvalModelSelection) {
        this.pvalModelSelection = pvalModelSelection;
    }

    public void setSelectedFeature(DoseResultBean selectedData) {
        //System.out.println("selfeature1");

        this.selectedFeature = selectedData;
        currentFeatureImg = plotSelectedFeature(150, "png");
        //System.out.println("==========" +currentFeatureImg);
    }

    public String plotSelectedFeature(int dpi, String format) {
        //System.out.println("selfeature1");
        String geneID = selectedFeature.getID();
        String model = selectedFeature.getValue("Model name");
        double b = Double.parseDouble(selectedFeature.getValue("b"));
        double c = Double.parseDouble(selectedFeature.getValue("c"));
        double d = Double.parseDouble(selectedFeature.getValue("d"));
        double e = Double.parseDouble(selectedFeature.getValue("e"));
        double Bmdl = Double.parseDouble(selectedFeature.getValue("BMDl"));
        double Bmd = Double.parseDouble(selectedFeature.getValue("BMD"));
        double Bmdu = Double.parseDouble(selectedFeature.getValue("BMDu"));
        String geneName = selectedFeature.getName();
        //String transDose = getTransDose();
        //System.out.println("selfeature2");
        return RDoseUtils.plotSelectedFeatureDRCurve(sb, geneID, geneName, model, b, c, d, e, Bmdl, Bmd, Bmdu, getTransDose(), dpi, format);
        //currentFeatureImg = "Metabolite_" + geneID + "_" + model + "_dpi" + dpi + "." + format;
    }

    public void recordPlotSelectedFeature(int dpi, String format) {
        //System.out.println("selfeature1");
        String geneID = selectedFeature.getID();
        String model = selectedFeature.getValue("Model name");
        double b = Double.parseDouble(selectedFeature.getValue("b"));
        double c = Double.parseDouble(selectedFeature.getValue("c"));
        double d = Double.parseDouble(selectedFeature.getValue("d"));
        double e = Double.parseDouble(selectedFeature.getValue("e"));
        double Bmdl = Double.parseDouble(selectedFeature.getValue("BMDl"));
        double Bmd = Double.parseDouble(selectedFeature.getValue("BMD"));
        double Bmdu = Double.parseDouble(selectedFeature.getValue("BMDu"));
        String geneName = selectedFeature.getName();
        //String transDose = getTransDose();
        //System.out.println("selfeature2");
        RDoseUtils.recordPlotSelectedFeatureDRCurve(sb, geneID, geneName, model, b, c, d, e, Bmdl, Bmd, Bmdu, getTransDose(), dpi, format);
        //currentFeatureImg = "Metabolite_" + geneID + "_" + model + "_dpi" + dpi + "." + format;
    }

    public String getCurrentFeatureImg() {
        return currentFeatureImg;
    }

    public String getCurrentFeatureImgPath() {
        String imgNm = currentFeatureImg;
        if (imgNm == null) {
            return ab.getRootContext() + "/resources/images/background.png";
        }
        //imgNm = sb.getCurrentCmpdName().replaceAll("\\/", "_");
        //System.out.println("==========" + ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + imgNm);
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + imgNm;
    }

    public void populateDoseResBeans() {
        resBeans.clear();
        columns.clear();
        RConnection RC = sb.getRConnection();
        double[][] resMat = RDoseUtils.getDRFitResMatrix(RC);
        String[] modelNms = RDoseUtils.getDRFitResModelNmColumn(RC);
        String[] columnKeys = RDoseUtils.getDRFitResColNames(RC);

        columns.add(new ColumnModel("ID", "ID", "string")); //add entrez ID column
        for (String columnKey : columnKeys) {
            if (!columnKey.equals("b") && !columnKey.equals("c") && !columnKey.equals("d") && !columnKey.equals("e")) {
                columns.add(new ColumnModel(columnKey, columnKey, "double"));
            }
        }
        columns.add(new ColumnModel("Model name", "Model name", "string")); //add entrez ID column

        tableFields = new SelectItem[columns.size()];
        for (int k = 0; k < tableFields.length; k++) {
            ColumnModel cm = columns.get(k);
            tableFields[k] = new SelectItem(cm.getHeader(), cm.getHeader() + "  ");
        }

        ids = RDoseUtils.getDRFitResFeatureIDs(RC);
        String[] lnks = ids;
        String[] smbls = ids;
        //String featureIDType = "entrez";//RAnalUtils.getExpressGeneIDType(RC);
        //if (featureIDType.equalsIgnoreCase("entrez")) {
        //    lnks = RDoseUtils.getDRFitResGeneIDLinks(RC, sb.getOrg());
        //    smbls = RDoseUtils.getDRFitResGeneSymbols(RC, sb.getOrg());
        //}

        //set up object list
        // due to memory/performance issue, cannot display more than 5000 sig genes
        // otherwise java.lang.ArrayIndexOutOfBoundsException: 5000
        int lmt = ids.length;
        if (lmt > 500) {
            lmt = 500;
        }
        if (ids != null && ids.length > 0) {
            for (int i = 0; i < lmt; i++) {

                //we use gene symbol as ID for sorting purpose as actual 
                //gene symbol is hyper link
                DoseResultBean rb = new DoseResultBean(ids[i]);
                //System.out.println(smbls[i]);
                rb.setName(smbls[i]);
                rb.setValue("ID", lnks[i]);
                for (int m = 0; m < columnKeys.length; m++) {
                    rb.setValue(columnKeys[m], resMat[i][m] + "");
                }
                rb.setValue("Model name", modelNms[i]);
                resBeans.add(rb);
                if (i < 10) {
                    selectedFeature = rb;
                    recordPlotSelectedFeature(150, "png");
                }

            }
        }
    }

    public String getGuestId() {
        return guestId;
    }

    public void setGuestId(String guestId) {
        this.guestId = guestId;
    }

    public int getCurvNum() {
        return curvNum;
    }

    public void setCurvNum(int curvNum) {
        this.curvNum = curvNum;
    }

    public int getBmdNum() {
        return bmdNum;
    }

    public void setBmdNum(int bmdNum) {
        this.bmdNum = bmdNum;
    }

    public void prepareBMDRes() {
        int res = RDoseUtils.performBMDAnal(sb, bmdOption, numsds, getCtrlMode());

        switch (res) {
            case 1 -> {
                String[] sigRes = RDoseUtils.getDRRes(sb.getRConnection());
                setCurvNum(Integer.parseInt(sigRes[0]));
                setBmdNum(Integer.parseInt(sigRes[1]));
                bmdAnal = true;
                RDoseUtils.plotDRModelBar(sb, sb.getRConnection(), sb.getNewImage("dr_barplot"), 150, "png");
                RDoseUtils.plotDRHistogram(sb, sb.getRConnection(), sb.getNewImage("dr_histogram"), 150, "png", getUnits(), getTransDose());
                populateDoseResBeans();
            }
            case 2 ->
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_INFO, "No Results", "No BMDs passed the quality filters."));
            case 3 -> {
                String[] sigRes = RDoseUtils.getDRRes(sb.getRConnection());
                setCurvNum(Integer.parseInt(sigRes[0]));
                setBmdNum(Integer.parseInt(sigRes[1]));
                bmdAnal = true;
                populateDoseResBeans();
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_INFO, "Few Results", "Only one BMD passed the quality filters so plots could not be made."));
            }
            default ->
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error While Curve Fitting", "Please contact us via OmicsForum."));
        }
    }

    public void prepareContBMDRes() {
        int res = RDoseUtils.performContBMDAnal(sb.getRConnection(), bmdOption, numsds, getCtrlMode());

        switch (res) {
            case 1 -> {
                String[] sigRes = RDoseUtils.getDRRes(sb.getRConnection());
                setCurvNum(Integer.parseInt(sigRes[0]));
                setBmdNum(Integer.parseInt(sigRes[1]));
                bmdAnal = true;
                RDoseUtils.plotDRModelBar(sb, sb.getRConnection(), sb.getNewImage("dr_barplot"), 150, "png");
                RDoseUtils.plotDRHistogram(sb, sb.getRConnection(), sb.getNewImage("dr_histogram"), 150, "png", getUnits(), getTransDose());
                populateDoseResBeans();
            }
            case 2 ->
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_INFO, "No Results", "No BMDs passed the quality filters."));
            case 3 -> {
                String[] sigRes = RDoseUtils.getDRRes(sb.getRConnection());
                setCurvNum(Integer.parseInt(sigRes[0]));
                setBmdNum(Integer.parseInt(sigRes[1]));
                bmdAnal = true;
                populateDoseResBeans();
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_INFO, "Few Results", "Only one BMD passed the quality filters so plots could not be made."));
            }
            default ->
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error While Curve Fitting", "Please contact us via OmicsForum."));
        }
    }

    public boolean performCurveFitting() {
        int res = fitModel();

        if (res == 1) {
            prepareBMDRes();
        } else {
            wb.getCalledWorkflowsError().add("Curve Fitting");
            return false;

        }
        return true;
    }

    public void performContineousCurveFitting() {
        int res = fitContModel();

        if (res == 1) {
            prepareContBMDRes();
            setupBmd();
        }
    }

    public void setupBmd() {
        bmd = "<a target='_blank' style='color: white' href = \"/MetaboAnalyst/resources/users/"
                + sb.getCurrentUser().getName() + File.separator + "bmd.txt" + "\" download>" + "bmd.txt" + "</a>";
    }

    public String getBmd() {
        return bmd;
    }

    public void nullBmd() {
        bmd = null;
    }

    public void factorListenerDRPlots() {
        RDoseUtils.plotDRHistogram(sb, sb.getRConnection(), sb.getNewImage("dr_histogram"), 150, "png", getUnits(), getTransDose());
    }

    /*
    public int performModelFit(RConnection RC, String[] mdlTypes, String guestId, String cpus) {
        DoseResponseModel model = new DoseResponseModel();
        model.setData(RDoseUtils.getDRItemSelectData(RC));
        model.setDataMean(RDoseUtils.getDRItemSelectDataMean(RC));
        model.setDose(RDoseUtils.getDRItemSelectDose(RC));
        model.setItems(RDoseUtils.getDRItemSelectItems(RC));
        model.setDataMeanColNms(RDoseUtils.getDRItemSelectDataMeanColNms(RC));
        model.setModels(mdlTypes);
        model.setCpus(cpus);

        DoseResponseModel drMdl = RDoseUtils.performModelFit(RC);
        RDoseUtils.prepareDrcFitRObject(RC, drMdl);

        return 1;
    }

    private List<MetaDataBean> analysisMetaOpts = null;
        public SelectItem[] getAnalysisMetaOpts() {

        if (analysisMetaOpts == null) {
            List<MetaDataBean> beans = getMetaDataBeans();
            analysisMetaOpts = new SelectItem[beans.size()];
            for (int i = 0; i < beans.size(); i++) {
                analysisMetaOpts[i] = new SelectItem(beans.get(i).getName(), beans.get(i).getName());
            }
        }

        return analysisMetaOpts;
    }
        
            public void initMetaDataBean() {
        RConnection RC = sb.getRConnection();
        String[] metaDataGroups = RDataUtils.getMetaDataGroups(RC);
        String[] metaDataStatus = RDataUtils.getMetaDataStatus(RC);

        metaDataBean = new ArrayList();
        String[] metatypes = RDataUtils.getMetaTypes(RC);

        for (int i = 0; i < metaDataGroups.length; i++) {
            String metatype = metatypes[i];
            String metastatus = metaDataStatus[i];
            //whether disabled or not in metadatacheck page
            //if tsdesign equals time or time0, disable metadata if it's name "time"
            if (i == 0) {
                if (metaDataGroups[i].toLowerCase().equals("time") && !sb.getTsDesign().equals("multi")) {
                    metaDataBean.add(new MetaDataBean(metaDataGroups[i], metatype, i, true, true, metastatus));
                } else {
                    metaDataBean.add(new MetaDataBean(metaDataGroups[i], metatype, i, true, false, metastatus));
                }
            } else if ((metaDataGroups[i].toLowerCase().equals("time") || metaDataGroups[i].toLowerCase().equals("subject")) && !sb.getTsDesign().equals("multi")) {
                metaDataBean.add(new MetaDataBean(metaDataGroups[i], metatype, i, false, true, metastatus));
            } else {
                metaDataBean.add(new MetaDataBean(metaDataGroups[i], metatype, i, false, false, metastatus));
            }

        }
        if (selectedMetaDataBean == null) {
            selectedMetaDataBean = metaDataBean.get(0);
        }
    }*/
    public void updateDoseDEAnalysis() {
        //if (!dePerformed) {
        performDoseDEAnalysis(1);
        //} else {
        //    populateDeResBeans(sigLevel, fcLevel);
        //}

        dePerformed = true;
    }

    public void performDoseDEAnalysis(int mode) {
        dePerformed = false;
        if (mode == 0 && sb.getCurrentPageID().equals("Sig. analysis")) {
            return;
        }

        if (adjustedMeta != null) {
            for (String adjustedMeta1 : adjustedMeta) {
                if (adjustedMeta1.equals(analysisMeta)) {
                    sb.addMessage("Error", "Please make sure primary data is not also selected in covariate options.");
                    return;
                }
            }
        }

        if (contineousDoes) {
            if (ceffLevel < 0) {
                sb.addMessage("Error", "Please make sure that the threshold is positive.");
                return;
            }
            //RDoseUtils.setDoseType(sb.getRConnection(), "cont");
        } else {
            //RDoseUtils.setDoseType(sb.getRConnection(), "disc");
        }
        int res = RDoseUtils.performDoseDEAnal(sb.getRConnection(), analysisMeta, adjustedMeta);
        if (res == 1) {
            if (contineousDoes) {
                populateDeResBeans(sigLevel, ceffLevel);
            } else {
                populateDeResBeans(sigLevel, fcLevel);
            }
        } else {
            String msg = RDataUtils.getErrMsg(sb.getRConnection());
            sb.addMessage("Error", msg);

        }
    }

    public void populateDeResBeans(double pLvl, double cfLevel) {
        int res;
        if (contineousDoes) {
            res = RDoseUtils.computeContDoseLimma(sb.getRConnection(), pLvl, cfLevel, FDR);
        } else {
            res = RDoseUtils.computeDoseLimma(sb.getRConnection(), pLvl, cfLevel, FDR);
        }

        RDoseUtils.plotDoseVolcano(sb, sb.getRConnection(), sb.getNewImage("dose_volcano"), 150, "png");

        if (res == 0) { // no sig
            sb.addMessage("Error", "No significant features found with current thresholds. Please adjust threhsold to select features to continue.");
            sigOK = false;
        } else if (res > 1000) {
            sb.addMessage("Error", "Too many signficant features for dose response analysis (max 1000) - current: " + res + "!");
            sigOK = false;
        } else if (res > 0) {
            sigOK = true;
            dtb.update3CompModel("dose-de");
            sb.addMessage("OK", "A total of " + res + " are identified based on your parameters.");
        }
    }

    @JsonIgnore
    public DefaultStreamedContent getResTableFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/curvefit_detailed_table.csv");
    }

    public void analysisMetaChangeListener() {
        primaryType = RDataUtils.GetPrimaryType(sb.getRConnection(), analysisMeta);
        contineousDoes = !primaryType.equals("disc");
    }

    @JsonIgnore
    public SelectItem[] getReferenceGroupFromAnalysisMetaOpts() {
        String[] grpNames = RDataUtils.getUniqueMetaNames(sb.getRConnection(), analysisMeta);
        referenceGroupFromAnalysisMetaOpts = new SelectItem[grpNames.length];
        for (int i = 0; i < grpNames.length; i++) {
            referenceGroupFromAnalysisMetaOpts[i] = new SelectItem(grpNames[i], grpNames[i]);
        }
        return referenceGroupFromAnalysisMetaOpts;
    }

    public Object getSortValue(DoseResultBean resBean, String property) {
        Object value = resBean.getValue(property);

        if (value instanceof String valueStr) {

            // Check if the string is numeric
            if (isNumeric(valueStr)) {
                // Convert numeric string to Double for numeric sorting
                return Double.valueOf(valueStr);
            } else {
                // Non-numeric strings are returned as-is for string sorting
                return valueStr;
            }
        }

        // For non-string objects, return as-is (could handle other types if needed)
        return value;
    }

// Utility method to check if a string is numeric
    private boolean isNumeric(String str) {
        try {
            Double.valueOf(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    @JsonIgnore
    public boolean isMultiMeta() {
        String[] metaDataGroups = RDataUtils.getMetaDataGroups(sb.getRConnection());
        return metaDataGroups.length > 1;
    }

    public boolean isRobustTrend() {
        return robustTrend;
    }

    public void setRobustTrend(boolean robustTrend) {
        this.robustTrend = robustTrend;
    }

    public boolean isDePerformed() {
        return dePerformed;
    }

    public void setDePerformed(boolean dePerformed) {
        this.dePerformed = dePerformed;
    }

}
