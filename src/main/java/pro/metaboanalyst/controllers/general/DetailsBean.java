/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.general;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.gson.Gson;
import jakarta.annotation.PreDestroy;
import java.io.File;
import java.io.Serializable;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import jakarta.faces.view.ViewScoped;
import jakarta.faces.model.ListDataModel;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.models.ColumnBean;
import pro.metaboanalyst.models.FeatureBean;
import pro.metaboanalyst.models.MetaValueBean;
import pro.metaboanalyst.rwrappers.ChemoMetrics;
import pro.metaboanalyst.rwrappers.Classifying;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RocUtils;
import pro.metaboanalyst.rwrappers.SigVarSelect;
import pro.metaboanalyst.rwrappers.TimeSeries;
import pro.metaboanalyst.rwrappers.UniVarTests;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.event.CellEditEvent;
import org.primefaces.event.ItemSelectEvent;
import org.primefaces.model.StreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.models.MetSetBean;
import pro.metaboanalyst.rwrappers.RDoseUtils;
import software.xdev.chartjs.model.datapoint.ScatterDataPoint;
import software.xdev.chartjs.model.charts.ScatterChart;
import software.xdev.chartjs.model.dataset.ScatterDataset;
import software.xdev.chartjs.model.data.ScatterData;

/**
 *
 * @author jianguox
 */
@ViewScoped
@Named("detailsBean")
public class DetailsBean implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private MultifacBean mfb;

    private String[] colnames = null;
    private String[] rownames = null;
    private String[] stringCol = null;
    private String[] stringCol2 = null;

    private double[][] sigmat = null;

    private String fileName = "";
    private ListDataModel<FeatureBean> listModel = null;
    private ListDataModel<MetaValueBean> listMetaModel = null;
    private boolean init = false;
    private boolean initMeta = false;
    private boolean extraVis = false;

    //for detail view table
    private final int maxColNum = 11;
    private ColumnBean[] colVis = new ColumnBean[maxColNum];

    private String fileNameVolcano = "";
    private ColumnBean[] colVisVolcano = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelVolcano = null;

    private String fileNamePLS = "";
    private ColumnBean[] colVisPLS = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelPLS = null;

    private String fileNameOPLS = "";
    private ColumnBean[] colVisOPLS = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelOPLS = null;

    private String fileNamePtn = "";
    private ColumnBean[] colVisPtn = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelPtn = null;

    private String fileNameRF = "";
    private ColumnBean[] colVisRF = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelRF = null;

    private String fileNameRSVM = "";
    private ColumnBean[] colVisRSVM = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelRSVM = null;

    private String fileNameAnova = "";
    private ColumnBean[] colVisAnova = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelAnova = null;

    private String fileNameLM = "";
    private ColumnBean[] colVisLM = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelLM = null;

    private String fileNameAnova2 = "";
    private ColumnBean[] colVisAnova2 = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelAnova2 = null;

    private String fileNameMultiRF = "";
    private ColumnBean[] colVisMultiRF = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelMultiRF = null;

    private String fileNameDoseDe = "";
    private ColumnBean[] colVisDoseDe = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelDoseDe = null;

    private String fileNameEbam = "";
    private ColumnBean[] colVisEbam = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelEbam = null;

    private Map<String, ListDataModel<FeatureBean>> hashListModel = new HashMap<>();
    private Map<String, String> hashFileName = new HashMap<>();
    private Map<String, ColumnBean[]> hashColVis = new HashMap<>();

    @PreDestroy
    public void cleanup() {
        listModel = null;
        listModelVolcano = null;
        listModelPLS = null;
        listModelOPLS = null;
        listModelPtn = null;
        listModelRF = null;
        listModelRSVM = null;
        listModelAnova = null;
        listModelLM = null;
        listModelAnova2 = null;
        listModelMultiRF = null;
        listModelDoseDe = null;
        listModelEbam = null;

        if (hashListModel != null) {
            hashListModel.clear();
        }
        if (hashListModel2 != null) {
            hashListModel2.clear();
        }
        if (hashFileName != null) {
            hashFileName.clear();
        }
        if (hashColVis != null) {
            hashColVis.clear();
        }
        if (hashFileName2 != null) {
            hashFileName2.clear();
        }
        if (hashColVis2 != null) {
            hashColVis2.clear();
        }
    }

    public Map<String, ListDataModel<FeatureBean>> getHashListModel() {
        return hashListModel;
    }

    public void setHashListModel(Map<String, ListDataModel<FeatureBean>> hashListModel) {
        this.hashListModel = hashListModel;
    }

    private Map<String, ListDataModel<FeatureBean>> hashListModel2 = new HashMap<>();
    private Map<String, String> hashFileName2 = new HashMap<>();
    private Map<String, ColumnBean[]> hashColVis2 = new HashMap<>();

    public Map<String, ListDataModel<FeatureBean>> getHashListModel2() {
        return hashListModel2;
    }

    public void setHashListModel2(Map<String, ListDataModel<FeatureBean>> hashListModel2) {
        this.hashListModel2 = hashListModel2;
    }

    public Map<String, String> getHashFileName2() {
        return hashFileName2;
    }

    public void setHashFileName2(Map<String, String> hashFileName2) {
        this.hashFileName2 = hashFileName2;
    }

    public Map<String, ColumnBean[]> getHashColVis2() {
        return hashColVis2;
    }

    public void setHashColVis2(Map<String, ColumnBean[]> hashColVis2) {
        this.hashColVis2 = hashColVis2;
    }

    public String[] getStringCol2() {
        return stringCol2;
    }

    public void setStringCol2(String[] stringCol2) {
        this.stringCol2 = stringCol2;
    }

    public Map<String, String> getHashFileName() {
        return hashFileName;
    }

    public void setHashFileName(Map<String, String> hashFileName) {
        this.hashFileName = hashFileName;
    }

    public Map<String, ColumnBean[]> getHashColVis() {
        return hashColVis;
    }

    public void setHashColVis(Map<String, ColumnBean[]> hashColVis) {
        this.hashColVis = hashColVis;
    }

    public String[] getStringCol() {
        return stringCol;
    }

    public void setStringCol(String[] stringCol) {
        this.stringCol = stringCol;
    }

    public String[] getRownames() {
        return rownames;
    }

    public void setRownames(String[] rownames) {
        this.rownames = rownames;
    }

    public double[][] getSigmat() {
        return sigmat;
    }

    public void setSigmat(double[][] sigmat) {
        this.sigmat = sigmat;
    }

    public boolean isColVisibleEbam(int inx) {
        if (colVisEbam.length > inx && colVisEbam[inx] != null) {
            return colVisEbam[inx].isVisible();
        } else {
            return false;
        }
    }

    public String getColHeaderEbam(int inx) {
        if (colVisEbam[inx] != null) {
            return colVisEbam[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileEbam() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameEbam);
    }

    private String fileNameSam = "";
    private ColumnBean[] colVisSam = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelSam = null;

    public boolean isColVisibleSam(int inx) {
        if (colVisSam.length > inx && colVisSam[inx] != null) {
            return colVisSam[inx].isVisible();
        } else {
            return false;
        }
    }

    public String getColHeaderSam(int inx) {
        if (colVisSam[inx] != null) {
            return colVisSam[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileSam() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameSam);
    }

    //
    private String fileNameSPLSLoading = "";
    private ColumnBean[] colVisSPLSLoading = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelSPLSLoading = null;

    public boolean isColVisibleSPLSLoading(int inx) {
        if (colVisSPLSLoading.length > inx && colVisSPLSLoading[inx] != null) {
            return colVisSPLSLoading[inx].isVisible();
        } else {
            return false;
        }
    }

    public String getColHeaderSPLSLoading(int inx) {
        if (colVisSPLSLoading[inx] != null) {
            return colVisSPLSLoading[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileSPLSLoading() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameSPLSLoading);
    }
    //
//
    private String fileNamePCALoading = "";
    private ColumnBean[] colVisPCALoading = new ColumnBean[maxColNum];
    private ListDataModel<FeatureBean> listModelPCALoading = null;

    public boolean isColVisiblePCALoading(int inx) {
        if (colVisPCALoading.length > inx && colVisPCALoading[inx] != null) {
            return colVisPCALoading[inx].isVisible();
        } else {
            return false;
        }
    }

    public String getColHeaderPCALoading(int inx) {
        if (colVisPCALoading[inx] != null) {
            return colVisPCALoading[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFilePCALoading() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNamePCALoading);
    }
    //

    public String getFileNameSPLSLoading() {
        return fileNameSPLSLoading;
    }

    public void setFileNameSPLSLoading(String fileNameSPLSLoading) {
        this.fileNameSPLSLoading = fileNameSPLSLoading;
    }

    public ColumnBean[] getColVisSPLSLoading() {
        return colVisSPLSLoading;
    }

    public void setColVisSPLSLoading(ColumnBean[] colVisSPLSLoading) {
        this.colVisSPLSLoading = colVisSPLSLoading;
    }

    public ListDataModel<FeatureBean> getListModelSPLSLoading() {
        return listModelSPLSLoading;
    }

    public void setListModelSPLSLoading(ListDataModel<FeatureBean> listModelSPLSLoading) {
        this.listModelSPLSLoading = listModelSPLSLoading;
    }

    public String getFileNamePCALoading() {
        return fileNamePCALoading;
    }

    public void setFileNamePCALoading(String fileNamePCALoading) {
        this.fileNamePCALoading = fileNamePCALoading;
    }

    public ColumnBean[] getColVisPCALoading() {
        return colVisPCALoading;
    }

    public void setColVisPCALoading(ColumnBean[] colVisPCALoading) {
        this.colVisPCALoading = colVisPCALoading;
    }

    public ListDataModel<FeatureBean> getListModelPCALoading() {
        return listModelPCALoading;
    }

    public void setListModelPCALoading(ListDataModel<FeatureBean> listModelPCALoading) {
        this.listModelPCALoading = listModelPCALoading;
    }

    public String getFileNameSam() {
        return fileNameSam;
    }

    public void setFileNameSam(String fileNameSam) {
        this.fileNameSam = fileNameSam;
    }

    public ColumnBean[] getColVisSam() {
        return colVisSam;
    }

    public void setColVisSam(ColumnBean[] colVisSam) {
        this.colVisSam = colVisSam;
    }

    public ListDataModel<FeatureBean> getListModelSam() {
        return listModelSam;
    }

    public void setListModelSam(ListDataModel<FeatureBean> listModelSam) {
        this.listModelSam = listModelSam;
    }

    public boolean isInit() {
        return init;
    }

    public void setInit(boolean init) {
        this.init = init;
    }

    public String getFileNameLM() {
        return fileNameLM;
    }

    public void setFileNameLM(String fileNameLM) {
        this.fileNameLM = fileNameLM;
    }

    public ColumnBean[] getColVisLM() {
        return colVisLM;
    }

    public void setColVisLM(ColumnBean[] colVisLM) {
        this.colVisLM = colVisLM;
    }

    public String getFileNameEbam() {
        return fileNameEbam;
    }

    public void setFileNameEbam(String fileNameEbam) {
        this.fileNameEbam = fileNameEbam;
    }

    public ColumnBean[] getColVisEbam() {
        return colVisEbam;
    }

    public void setColVisEbam(ColumnBean[] colVisEbam) {
        this.colVisEbam = colVisEbam;
    }

    public ListDataModel<FeatureBean> getListModelEbam() {
        return listModelEbam;
    }

    public void setListModelEbam(ListDataModel<FeatureBean> listModelEbam) {
        this.listModelEbam = listModelEbam;
    }

    public String getFileNameDoseDe() {
        return fileNameDoseDe;
    }

    public void setFileNameDoseDe(String fileNameDoseDe) {
        this.fileNameDoseDe = fileNameDoseDe;
    }

    public ColumnBean[] getColVisDoseDe() {
        return colVisDoseDe;
    }

    public void setColVisDoseDe(ColumnBean[] colVisDoseDe) {
        this.colVisDoseDe = colVisDoseDe;
    }

    public ListDataModel<FeatureBean> getListModelDoseDe() {
        return listModelDoseDe;
    }

    public void setListModelDoseDe(ListDataModel<FeatureBean> listModelDoseDe) {
        this.listModelDoseDe = listModelDoseDe;
    }

    public String getFileNameMultiRF() {
        return fileNameMultiRF;
    }

    public void setFileNameMultiRF(String fileNameMultiRF) {
        this.fileNameMultiRF = fileNameMultiRF;
    }

    public ColumnBean[] getColVisMultiRF() {
        return colVisMultiRF;
    }

    public void setColVisMultiRF(ColumnBean[] colVisMultiRF) {
        this.colVisMultiRF = colVisMultiRF;
    }

    public ListDataModel<FeatureBean> getListModelMultiRF() {
        return listModelMultiRF;
    }

    public void setListModelMultiRF(ListDataModel<FeatureBean> listModelMultiRF) {
        this.listModelMultiRF = listModelMultiRF;
    }

    public String getFileNameAnova2() {
        return fileNameAnova2;
    }

    public void setFileNameAnova2(String fileNameAnova2) {
        this.fileNameAnova2 = fileNameAnova2;
    }

    public ColumnBean[] getColVisAnova2() {
        return colVisAnova2;
    }

    public void setColVisAnova2(ColumnBean[] colVisAnova2) {
        this.colVisAnova2 = colVisAnova2;
    }

    public ListDataModel<FeatureBean> getListModelAnova2() {
        return listModelAnova2;
    }

    public void setListModelAnova2(ListDataModel<FeatureBean> listModelAnova2) {
        this.listModelAnova2 = listModelAnova2;
    }

    public ListDataModel<FeatureBean> getListModelLM() {
        return listModelLM;
    }

    public void setListModelLM(ListDataModel<FeatureBean> listModelLM) {
        this.listModelLM = listModelLM;
    }

    public String getFileNameAnova() {
        return fileNameAnova;
    }

    public void setFileNameAnova(String fileNameAnova) {
        this.fileNameAnova = fileNameAnova;
    }

    public ColumnBean[] getColVisAnova() {
        return colVisAnova;
    }

    public void setColVisAnova(ColumnBean[] colVisAnova) {
        this.colVisAnova = colVisAnova;
    }

    public ListDataModel<FeatureBean> getListModelAnova() {
        return listModelAnova;
    }

    public void setListModelAnova(ListDataModel<FeatureBean> listModelAnova) {
        this.listModelAnova = listModelAnova;
    }

    public String getFileNameOPLS() {
        return fileNameOPLS;
    }

    public void setFileNameOPLS(String fileNameOPLS) {
        this.fileNameOPLS = fileNameOPLS;
    }

    public ColumnBean[] getColVisOPLS() {
        return colVisOPLS;
    }

    public void setColVisOPLS(ColumnBean[] colVisOPLS) {
        this.colVisOPLS = colVisOPLS;
    }

    public ListDataModel<FeatureBean> getListModelOPLS() {
        return listModelOPLS;
    }

    public void setListModelOPLS(ListDataModel<FeatureBean> listModelOPLS) {
        this.listModelOPLS = listModelOPLS;
    }

    public ColumnBean[] getColVisPLS() {
        return colVisPLS;
    }

    public void setColVisPLS(ColumnBean[] colVisPLS) {
        this.colVisPLS = colVisPLS;
    }

    public String getFileNameRSVM() {
        return fileNameRSVM;
    }

    public void setFileNameRSVM(String fileNameRSVM) {
        this.fileNameRSVM = fileNameRSVM;
    }

    public ColumnBean[] getColVisRSVM() {
        return colVisRSVM;
    }

    public void setColVisRSVM(ColumnBean[] colVisRSVM) {
        this.colVisRSVM = colVisRSVM;
    }

    public ListDataModel<FeatureBean> getListModelRSVM() {
        return listModelRSVM;
    }

    public void setListModelRSVM(ListDataModel<FeatureBean> listModelRSVM) {
        this.listModelRSVM = listModelRSVM;
    }

    public String getFileNameRF() {
        return fileNameRF;
    }

    public void setFileNameRF(String fileNameRF) {
        this.fileNameRF = fileNameRF;
    }

    public ColumnBean[] getColVisRF() {
        return colVisRF;
    }

    public void setColVisRF(ColumnBean[] colVisRF) {
        this.colVisRF = colVisRF;
    }

    public ListDataModel<FeatureBean> getListModelRF() {
        return listModelRF;
    }

    public void setListModelRF(ListDataModel<FeatureBean> listModelRF) {
        this.listModelRF = listModelRF;
    }

    public ColumnBean[] getColVisVolcano() {
        return colVisVolcano;
    }

    public void setColVisVolcano(ColumnBean[] colVisVolcano) {
        this.colVisVolcano = colVisVolcano;
    }

    public ListDataModel<FeatureBean> getListModelVolcano() {
        return listModelVolcano;
    }

    public void setListModelVolcano(ListDataModel<FeatureBean> listModelVolcano) {
        this.listModelVolcano = listModelVolcano;
    }

    public String getFileNameVolcano() {
        return fileNameVolcano;
    }

    public void setFileNameVolcano(String fileNameVolcano) {
        this.fileNameVolcano = fileNameVolcano;
    }

    public ListDataModel<FeatureBean> getListModel() {
        return listModel;
    }

    public void setListModel(ListDataModel<FeatureBean> listModel) {
        this.listModel = listModel;
    }

    public String getFileNamePLS() {
        return fileNamePLS;
    }

    public void setFileNamePLS(String fileNamePLS) {
        this.fileNamePLS = fileNamePLS;
    }

    public ListDataModel<FeatureBean> getListModelPLS() {
        return listModelPLS;
    }

    public void setListModelPLS(ListDataModel<FeatureBean> listModelPLS) {
        this.listModelPLS = listModelPLS;
    }

    public String getFileNamePtn() {
        return fileNamePtn;
    }

    public void setFileNamePtn(String fileNamePtn) {
        this.fileNamePtn = fileNamePtn;
    }

    public ColumnBean[] getColVisPtn() {
        return colVisPtn;
    }

    public void setColVisPtn(ColumnBean[] colVisPtn) {
        this.colVisPtn = colVisPtn;
    }

    public ListDataModel<FeatureBean> getListModelPtn() {
        return listModelPtn;
    }

    public void setListModelPtn(ListDataModel<FeatureBean> listModelPtn) {
        this.listModelPtn = listModelPtn;
    }

    public boolean isColVisible(int inx) {
        if (colVis.length > inx && colVis[inx] != null) {
            return colVis[inx].isVisible();
        } else {
            return false;
        }
    }

    public boolean isColVisibleLM(int inx) {
        if (colVisLM.length > inx && colVisLM[inx] != null) {
            return colVisLM[inx].isVisible();
        } else {
            return false;
        }
    }

    public String getColHeader(int inx) {
        return colVis[inx].getName();
    }

    public String getColVolcanoHeader(int inx) {
        if (colVisVolcano[inx] != null) {
            return colVisVolcano[inx].getName();
        } else {
            return "";
        }
    }

    public String getColHeaderPLS(int inx) {
        if (colVisPLS[inx] != null) {
            return colVisPLS[inx].getName();
        } else {
            return "";
        }
    }

    public String getColHeaderPtn(int inx) {
        if (colVisPtn[inx] != null) {
            return colVisPtn[inx].getName();
        } else {
            return "";
        }
    }

    public String getColHeaderRF(int inx) {
        if (colVisRF[inx] != null) {
            return colVisRF[inx].getName();
        } else {
            return "";
        }
    }

    public String getColHeaderRSVM(int inx) {
        if (colVisRSVM[inx] != null) {
            return colVisRSVM[inx].getName();
        } else {
            return "";
        }
    }

    public String getColHeaderOPLS(int inx) {
        if (colVisOPLS[inx] != null) {
            return colVisOPLS[inx].getName();
        } else {
            return "";
        }
    }

    public String getColHeaderDoseDe(int inx) {
        if (colVisDoseDe[inx] != null) {
            return colVisDoseDe[inx].getName();
        } else {
            return "";
        }
    }

    public String getFileName() {
        return fileName;
    }

    public ListDataModel<MetaValueBean> getListMetaModel() {
        setupMetaTable();
        return listMetaModel;
    }

    public ListDataModel<FeatureBean> getFeatureBeans() {
        return listModel;
    }

    public boolean isExtraColVisible() {
        return extraVis;
    }

    public void setupMetaTable() {
        if (!initMeta) {
            RConnection RC = sb.getRConnection();
            colnames = RDataUtils.getMetaDataGroups(RC);

            colVis = new ColumnBean[maxColNum];
            for (int i = 0; i < colVis.length; i++) {
                if (i < colnames.length) {
                    colVis[i] = new ColumnBean(true, colnames[i]);
                } else {
                    colVis[i] = new ColumnBean(false, "");
                }
            }

            ArrayList<MetaValueBean> metaBeans;
            MetaValueBean mb;
            rownames = RDataUtils.getSampleNames(RC);
            metaBeans = new ArrayList<>();
            for (int i = 0; i < rownames.length; i++) {
                mb = new MetaValueBean();
                mb.addName(rownames[i]);

                for (String colname : colnames) {
                    String[] vec = RDataUtils.getMetaByCol(RC, colname);
                    mb.addValue(vec[i]);
                }

                metaBeans.add(mb);
            }
            listMetaModel = new ListDataModel(metaBeans);

            if (mfb.getIncludedMetaData() == null) {
                mfb.setIncludedMetaData(colnames);
            }
        }
        initMeta = true;
        // PrimeFaces.current().executeScript("PF('editMetaDialog').show();");

    }

    public void setupDetailsTable() {
        setupDetailsTable(false, sb.getSigSource());
        init = true;
    }

    public void setupDetailsTable(boolean dashboardBool, String from) {
        // if (!from.equals("anova") & init) { //anova allow threshold change for posthos
        //     return; //important, somehow called even in ajax 
        // }
        if (init) { //anova allow threshold change for posthos
            return; //important, somehow called even in ajax 
        }

        if (dashboardBool) {
            boolean performed = RDataUtils.checkDetailsTablePerformed(sb.getRConnection(), from);
            if (!performed) {
                return;
            }
        }
        colnames = null;
        extraVis = false;
        rownames = null;
        sigmat = null;
        stringCol = null;
        stringCol2 = null;

        fileName = "";
        obtainTableVars(from);
        //set up the view
        if (colnames == null || colnames.length == 0) {
            if (!dashboardBool) {
                sb.addMessage("Error", "No results are available for: " + from);
            }
            return;
        } else if (rownames == null || rownames.length == 0) {
            if (!dashboardBool) {
                sb.addMessage("Error", "No significant features are detected using set parameters!");
            }
            return;
        }
        colVis = new ColumnBean[maxColNum];
        for (int i = 0; i < colVis.length; i++) {
            if (i < colnames.length) {
                colVis[i] = new ColumnBean(true, colnames[i]);
            } else {
                colVis[i] = new ColumnBean(false, "");
            }
        }

        ArrayList<FeatureBean> featureBeans;
        //set up content
        if (rownames.length > 0) {
            // errTxt.setText("No result data was found!");
            //sb.setFeatureBeans(null);
            //return null;
            featureBeans = new ArrayList<>();
            FeatureBean fb;

            //This will make value pretty, but not meaningful for column sorting
            DecimalFormat formatter = new DecimalFormat("0.#####E0");

            for (int i = 0; i < rownames.length; i++) {
                fb = new FeatureBean();
                fb.addName(rownames[i]);
                if (sigmat != null) {
                    for (int m = 0; m < colnames.length; m++) {
                        fb.addValue(Double.parseDouble(formatter.format(sigmat[i][m])));
                    }
                }
                if (stringCol != null) {
                    fb.addExtra(stringCol[i]);
                }
                if (stringCol2 != null) {
                    fb.addExtra2(stringCol2[i]);

                }
                featureBeans.add(fb);
            }
            listModel = new ListDataModel(featureBeans);

            if (dashboardBool) {
                setupTableVars(from);
            }
        }
    }

    private void obtainTableVars(String from) {
        RConnection RC = sb.getRConnection();

        if (from.equals("fc")) {
            rownames = UniVarTests.getFCSigRowNames(sb);
            colnames = UniVarTests.getFCSigColNames(sb);
            sigmat = UniVarTests.getFCSigMat(sb);
            fileName = "fold_change.csv";
        } else if (from.equals("tt")) {
            rownames = UniVarTests.getTTSigRowNames(sb);
            colnames = UniVarTests.getTTSigColNames(sb);
            sigmat = UniVarTests.getTTSigMat(sb);
            fileName = UniVarTests.getTtestSigFileName(sb);
        } else if (from.equals("volcano")) {
            rownames = UniVarTests.getVolcanoSigRowNames(sb);
            colnames = UniVarTests.getVolcanoSigColNames(sb);
            sigmat = UniVarTests.getVolcanoSigMat(sb);
            fileName = "volcano.csv";
        } else if (from.equals("volcano.all")) {
            rownames = UniVarTests.getVolcanoSigRowNames(sb);
            colnames = UniVarTests.getVolcanoSigColNames(sb);
            sigmat = UniVarTests.getVolcanoSigMat(sb);
            fileName = "volcano.csv";
        } else if (from.equals("anova")) {
            extraVis = true;
            rownames = UniVarTests.getAovSigRowNames(RC);
            colnames = UniVarTests.getAovSigColNames(RC);
            stringCol = UniVarTests.getAovPostHocSigNames(RC);
            sigmat = UniVarTests.getAovSigMat(RC);
            fileName = UniVarTests.getAnovaSigFileName(sb);
        } else if (from.equals("template")) {
            rownames = UniVarTests.getCorSigRowNames(sb);
            colnames = UniVarTests.getCorSigColNames(sb);
            sigmat = UniVarTests.getCorSigMat(sb);
            fileName = UniVarTests.getCorrSigFileName(sb);
        } else if (from.equals("ebam")) {
            sigmat = SigVarSelect.getEBAMSigMat(sb);
            rownames = SigVarSelect.getEBAMSigRowNames(sb);
            colnames = SigVarSelect.getEBAMSigColNames(sb);
            fileName = "ebam_sigfeatures.csv";
        } else if (from.equals("sam")) {
            sigmat = SigVarSelect.getSAMSigMat(sb);
            rownames = SigVarSelect.getSAMSigRowNames(sb);
            colnames = SigVarSelect.getSAMSigColNames(sb);
            fileName = "sam_sigfeatures.csv";
        } else if (from.equals("pca")) {
            sigmat = ChemoMetrics.getPCALoadings(sb);
            rownames = ChemoMetrics.getPCALoadingRowName(sb);
            colnames = ChemoMetrics.getPCALoadingColName(sb);
            fileName = "pca_loadings.csv";
        } else if (from.startsWith("pls")) {
            String spec = from.split("\\.")[1];
            sigmat = ChemoMetrics.getPLSSigMat(sb, spec);
            rownames = ChemoMetrics.getPLSSigRowNames(sb, spec);
            colnames = ChemoMetrics.getPLSSigColNames(sb, spec);
            fileName = "plsda_" + spec + ".csv";
        } else if (from.startsWith("opls")) {
            String spec = from.split("\\.")[1];
            sigmat = ChemoMetrics.getOPLSSigMat(RC, spec);
            rownames = ChemoMetrics.getOPLSSigCmpds(RC, spec);
            colnames = ChemoMetrics.getOPLSSigColNames(RC, spec);
            fileName = "oplsda_" + spec + ".csv";
        } else if (from.startsWith("spls")) {
            String spec = from.split("\\.")[1];
            sigmat = ChemoMetrics.getSPLSLoadMat(RC);
            rownames = ChemoMetrics.getSPLSLoadCmpds(RC);
            colnames = ChemoMetrics.getSPLSSigColNames(sb, spec);
            fileName = "splsda_" + spec + ".csv";
        } else if (from.equals("rf")) {
            sigmat = Classifying.getRFSigMat(sb);
            rownames = Classifying.getRFSigRowNames(sb);
            colnames = Classifying.getRFSigColNames(sb);
            fileName = "randomforests_sigfeatures.csv";
        } else if (from.equals("multirf")) {
            sigmat = Classifying.getMultiRFSigMat(sb);
            rownames = Classifying.getMultiRFSigRowNames(sb);
            colnames = Classifying.getMultiRFSigColNames(sb);
            fileName = "randomforests_sigfeatures.csv";
        } else if (from.equals("svm")) {
            sigmat = Classifying.getSVMSigMat(sb);
            rownames = Classifying.getSVMSigRowNames(sb);
            colnames = Classifying.getSVMSigColNames(sb);
            fileName = "svm_sigfeatures.csv";
        } else if (from.equals("anova2")) {
            rownames = TimeSeries.getAov2SigRowNames(sb.getRConnection());
            colnames = TimeSeries.getAov2SigColNames(sb.getRConnection());
            sigmat = TimeSeries.getAov2SigMat(sb.getRConnection());
            fileName = TimeSeries.getAov2SigFileName(sb);
        } else if (from.startsWith("asca")) {
            String spec = from.split("\\.")[1];
            rownames = TimeSeries.getAscaSigRowNames(sb, spec);
            colnames = TimeSeries.getAscaSigColNames(sb, spec);
            sigmat = TimeSeries.getAscaSigMat(sb, spec);
            fileName = TimeSeries.getAscaSigFileName(sb);
        } else if (from.startsWith("cov")) {
            rownames = TimeSeries.getCovSigRowNames(sb.getRConnection());
            colnames = TimeSeries.getCovSigColNames(sb.getRConnection());
            sigmat = TimeSeries.getCovSigMat(sb.getRConnection());
            fileName = TimeSeries.getCovSigFileName(sb);
        } else if (from.startsWith("roc.imp")) {
            rownames = RocUtils.getImpRowNames(sb.getRConnection());
            colnames = RocUtils.getImpColNames(sb.getRConnection());
            sigmat = RocUtils.getImpSigMat(sb.getRConnection());
            fileName = RocUtils.getRocSigFileName(sb);
        } else if (from.startsWith("dose-de")) {
            rownames = RDoseUtils.getDoseDERows(RC);
            colnames = RDoseUtils.getDoseDEColumns(RC);
            sigmat = RDoseUtils.getDoseDEMat(RC);
            fileName = "limma_restable.csv";
        } else if (from.endsWith("_enr")) {
            String type = from.replace("_enr", "");
            rownames = RDataUtils.getEnrResSetNames(RC, type);
            sigmat = RDataUtils.getEnrResMatrix(RC, type);
            colnames = RDataUtils.getEnrResColNames(RC, type);
            stringCol = RDataUtils.getEnrResSetIDs(RC, type);
            fileName = RDataUtils.getEnrResFileName(RC, type);
            if (from.equals("gba_enr")) {
                fileName = "gba_table.csv";
            }
        } else if (from.equals("match_integ")) {
            rownames = RDataUtils.getMatchIntegNames(RC);
            sigmat = null; //RDataUtils.getMatchIntegMatrix(RC);
            colnames = RDataUtils.getMatchIntegColNames(RC);
            stringCol = RDataUtils.getMatchIntegSetIDs(RC, 2);
            stringCol2 = RDataUtils.getMatchIntegSetIDs(RC, 3);

        } else {
            //do nothing
        }
    }

    private void setupTableVars(String from) {
        System.out.println(from + "========================from");
        System.out.println(fileName + "========================fromfilename");

        if (from.equals("volcano")) {
            listModelVolcano = listModel;
            colVisVolcano = colVis;
            fileNameVolcano = fileName;
        } else if (from.startsWith("pls")) {
            listModelPLS = listModel;
            colVisPLS = colVis;
            fileNamePLS = fileName;
        } else if (from.equals("template")) {
            listModelPtn = listModel;
            colVisPtn = colVis;
            fileNamePtn = fileName;
        } else if (from.equals("rf")) {
            listModelRF = listModel;
            colVisRF = colVis;
            fileNameRF = fileName;
        } else if (from.equals("opls.vip")) {
            listModelOPLS = listModel;
            colVisOPLS = colVis;
            fileNameOPLS = fileName;
        } else if (from.equals("svm")) {
            listModelRSVM = listModel;
            colVisRSVM = colVis;
            fileNameRSVM = fileName;
        } else if (from.equals("anova")) {
            listModelAnova = listModel;
            colVisAnova = colVis;
            fileNameAnova = fileName;
        } else if (from.equals("cov")) {
            listModelLM = listModel;
            colVisLM = colVis;
            fileNameLM = fileName;
        } else if (from.equals("anova2")) {
            listModelAnova2 = listModel;
            colVisAnova2 = colVis;
            fileNameAnova2 = fileName;
        } else if (from.equals("multirf")) {
            listModelMultiRF = listModel;
            colVisMultiRF = colVis;
            fileNameMultiRF = fileName;
        } else if (from.equals("dose-de")) {
            listModelDoseDe = listModel;
            colVisDoseDe = colVis;
            fileNameDoseDe = fileName;
        } else if (from.equals("sam")) {
            listModelSam = listModel;
            colVisSam = colVis;
            fileNameSam = fileName;
        } else if (from.equals("ebam")) {
            listModelEbam = listModel;
            colVisEbam = colVis;
            fileNameEbam = fileName;
        } else if (from.startsWith("spls")) {
            listModelSPLSLoading = listModel;
            colVisSPLSLoading = colVis;
            fileNameSPLSLoading = fileName;
        } else if (from.equals("pca")) {
            listModelPCALoading = listModel;
            colVisPCALoading = colVis;
            fileNamePCALoading = fileName;
        } else if (from.endsWith("_enr")) {
            hashListModel.put(from, listModel);
            hashFileName.put(from, fileName);
            hashColVis.put(from, colVis);
        } else if (from.equals("match_integ")) {
            hashListModel2.put(from, listModel);
            hashFileName2.put(from, fileName);
            hashColVis2.put(from, colVis);
        }

    }

    private String cmpdImg = null;

    public String getCmpdImg() {
        if (cmpdImg == null) {
            cmpdImg = ab.getRootContext() + "/resources/images/background.png";
        }
        return cmpdImg;
    }

    public void onCellEdit(CellEditEvent<String> event) {
        String oldValue = event.getOldValue();
        String newValue = event.getNewValue();

        if (newValue != null && !newValue.equals(oldValue)) {
            //need to update feature 
            RDataUtils.updateFeatureName(sb.getRConnection(), oldValue, newValue);
            sb.addMessage("info", "Updated from: " + oldValue + "to:" + newValue);
            //need to recompute, set states to naive
            sb.resetAnalysis();
        }
    }

    public StreamedContent getDetailFile() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileName);
    }

    public StreamedContent getDetailFileVolcano() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameVolcano);
    }

    public StreamedContent getDetailFilePLS() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNamePLS);
    }

    public StreamedContent getDetailFileOPLS() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameOPLS);
    }

    public StreamedContent getDetailFilePtn() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNamePtn);
    }

    public StreamedContent getDetailFileRF() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameRF);
    }

    public StreamedContent getDetailFileRSVM() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameRSVM);
    }

    public StreamedContent getDetailFileDoseDe() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameDoseDe);
    }

    //from user click a data point (dataInx, itemInx) to compound names
    private LinkedHashMap<String, String> pointMap;

    public void itemSelect(ItemSelectEvent event) {
        String cmpdID = pointMap.get(event.getDataSetIndex() + ":" + event.getItemIndex());
        if (!curType.equals("aov2")) {
            updateCmpdName(cmpdID);
        }
        //plotCmpd(cmpdID);
        sb.viewCmpdSummary(cmpdID);
    }

    public void itemSelectMeta(ItemSelectEvent event) {
        String cmpdID = pointMap.get(event.getDataSetIndex() + ":" + event.getItemIndex());
        //System.out.println(cmpdID);
        if (!curType.equals("aov2")) {
            updateCmpdName(cmpdID);
        }
        mfb.setBoxId(cmpdID);
        mfb.updateBoxplotMeta();
    }

    private void updateCmpdName(String cmpdID) {
        RConnection RC = sb.getRConnection();
        UniVarTests.updateLoadingCmpd(RC, cmpdID);
    }

    //Scatterplot models containing one, two or three datasets 
    private String model1, model2, model3;
    private String mdl1Type = "pca";
    private String curType;

    public String getModel1(String type) {
        if (model1 == null || !type.equals(mdl1Type)) {
            update1CompModel(type);
            mdl1Type = type;
        }
        curType = type;
        return model1;
    }

    public void update1CompModel(String type) {

        //model1 = new ScatterChartModel();
        pointMap = new LinkedHashMap<>();

        RConnection RC = sb.getRConnection();
        double[][] myMat;
        String[] myIDs;
        switch (type) {
            case "opls" -> {
                myMat = ChemoMetrics.getOPLSSigMat(RC, "splot");
                myIDs = ChemoMetrics.getOPLSSigCmpds(RC, "splot");
            }
            case "pls" -> {
                myMat = ChemoMetrics.getPLSLoadMat(RC);
                myIDs = ChemoMetrics.getPLSLoadCmpds(RC);
            }
            default -> {
                //pca
                myMat = ChemoMetrics.getPCALoadMat(RC);
                myIDs = ChemoMetrics.getPCALoadCmpds(RC);
            }
        }

        //ChartData data = new ChartData();
        //LineChartDataSet myDataSet = new LineChartDataSet();
        List<ScatterDataPoint> myValues = new ArrayList<>();
        int dataInx = 0;
        int itemInx = 0;
        for (double[] row : myMat) {
            myValues.add(new ScatterDataPoint(row[0], row[1]));
            pointMap.put(dataInx + ":" + itemInx, myIDs[itemInx]);
            itemInx += 1;
        }
        /**
         * myDataSet.setData(myValues); myDataSet.setBackgroundColor("rgba(153,
         * 102, 255, 0.2)"); myDataSet.setBorderColor("rgb(153, 102, 255)");
         * myDataSet.setShowLine(false);
         *
         * data.addChartDataSet(myDataSet); model1.setData(data);
         * model1.setExtender("extender");
         */

        ScatterDataset scatterDataset = new ScatterDataset()
                .setData(myValues)
                .setBackgroundColor("rgba(153,102,255,0.2)")
                .setBorderColor("rgb(153, 102, 255)")
                .setShowLine(false);

        // Create Scatter Chart
        ScatterChart scatterChart = new ScatterChart();
        ScatterData scatterData = new ScatterData();
        scatterChart.setData(scatterData);

        scatterChart.getData().addDataset(scatterDataset);

        model1 = scatterChart.toJson();
    }

    private String mdl2Type = "aov";

    public String getModel2(String type) {
        if (model2 == null || !type.equals(mdl2Type)) {
            update2CompModel(type);
            mdl2Type = type;
        }
        curType = type;
        return model2;
    }

    public void update2CompModel(String type) {
        RConnection RC = sb.getRConnection();
        double[][] upMat, dnMat;
        String[] upIDs, dnIDs;

        switch (type) {
            case "aov" -> {
                upMat = UniVarTests.getAnovaUpMat(RC);
                dnMat = UniVarTests.getAnovaDnMat(RC);
                upIDs = UniVarTests.getAovUpIDs(RC);
                dnIDs = UniVarTests.getAovDnIDs(RC);
            }
            case "aov2" -> {
                upMat = TimeSeries.getAnova2UpMat(RC);
                dnMat = TimeSeries.getAnova2DnMat(RC);
                upIDs = TimeSeries.getAnova2UpCmpds(RC);
                dnIDs = TimeSeries.getAnova2DnCmpds(RC);
            }
            case "cov" -> {
                upMat = TimeSeries.getCovUpMat(RC);
                dnMat = TimeSeries.getCovDnMat(RC);
                upIDs = TimeSeries.getCovUpIDs(RC);
                dnIDs = TimeSeries.getCovDnIDs(RC);
            }
            default -> {
                upMat = UniVarTests.getTtUpMat(RC);
                dnMat = UniVarTests.getTtDnMat(RC);
                upIDs = UniVarTests.getTtUpIDs(RC);
                dnIDs = UniVarTests.getTtDnIDs(RC);
            }
        }

        // Optionally, set other options or customize the chart as needed
        //scatterChart.setOptions(/* Configure Options here */);
        // Assuming rendering method or conversion to a JSON/string representation for further use
        //model2 = new ScatterChartModel();
        pointMap = new LinkedHashMap<>();
        // ChartData data = new ChartData();
        ScatterDataset upDataSet = new ScatterDataset();
        List<ScatterDataPoint> upValues = new ArrayList<>();
        ScatterDataset dnDataSet = new ScatterDataset();
        List<ScatterDataPoint> dnValues = new ArrayList<>();

        // Create Scatter Chart
        ScatterChart scatterChart = new ScatterChart();
        ScatterData scatterData = new ScatterData();
        scatterChart.setData(scatterData);

        int dataInx = 0;
        if (upMat[0][0] != -1) {
            int itemInx = 0;
            for (double[] row : upMat) {
                upValues.add(new ScatterDataPoint(row[0], row[1]));
                pointMap.put(dataInx + ":" + itemInx, upIDs[itemInx]);
                itemInx += 1;
            }
            upDataSet.setData(upValues);
            upDataSet.setBackgroundColor("rgba(153, 102, 255, 0.2)");
            upDataSet.setBorderColor("rgb(153, 102, 255)");
            upDataSet.setShowLine(false);
            upDataSet.setLabel("Significant" + " [" + upValues.size() + "]");
            //System.out.println("========called 2========" + upValues.size());
            scatterChart.getData().addDataset(upDataSet);
            dataInx += 1;
        }

        if (dnMat[0][0] != -1) {
            int itemInx = 0;
            for (double[] row : dnMat) {
                dnValues.add(new ScatterDataPoint(row[0], row[1]));
                pointMap.put(dataInx + ":" + itemInx, dnIDs[itemInx]);
                itemInx += 1;
            }
            dnDataSet.setData(dnValues);
            dnDataSet.setBackgroundColor("rgba(201, 203, 207, 0.2)");
            dnDataSet.setBorderColor("rgb(201, 203, 207)");
            dnDataSet.setLabel("Unsignificant" + " [" + dnValues.size() + "]");
            dnDataSet.setShowLine(false);
            //System.out.println("========called 3========" + dnValues.size());
            scatterChart.getData().addDataset(dnDataSet);
        }

        pointMapJson = new Gson().toJson(pointMap);
        model2 = scatterChart.toJson();
        //System.out.println("========called 4========" + model2);
    }

    private String mdl3Type = "fc";

    public String getModel3(String type) {
        if (model3 == null || !type.equals(mdl3Type)) {
            update3CompModel(type);
            mdl3Type = type;
        }
        curType = type;
        return model3;
    }

    public void toggleTheme(String type) {
        if (null != type) {
            switch (type) {
                case "fc", "volcano", "dose-de" ->
                    model3 = getModel3(type);
                case "tt", "aov", "aov2" ->
                    model2 = getModel2(type);
                case "pca", "pls", "opls" ->
                    model1 = getModel1(type);
                default -> {
                }
            }
        }
    }

    public void update3CompModel(String type) {
        RConnection RC = sb.getRConnection();

        pointMap = new LinkedHashMap<>();
        ScatterChart scatterChart = new ScatterChart();
        ScatterData scatterData = new ScatterData();
        scatterChart.setData(scatterData);

        ScatterDataset sigLftDataSet = new ScatterDataset();
        List<ScatterDataPoint> sigLftValues = new ArrayList<>();
        ScatterDataset sigRgtDataSet = new ScatterDataset();
        List<ScatterDataPoint> sigRgtValues = new ArrayList<>();
        ScatterDataset unsigDataSet = new ScatterDataset();
        List<ScatterDataPoint> unsigValues = new ArrayList<>();

        double[][] sigDnMat, sigUpMat, unsigMat;
        String[] sigDnIDs, sigUpIDs, unsigIDs;

        switch (type) {
            case "volcano" -> {
                sigDnMat = UniVarTests.getVolcanoSigLftMat(RC);
                sigUpMat = UniVarTests.getVolcanoSigRgtMat(RC);
                unsigMat = UniVarTests.getVolcanoUnsigMat(RC);
                sigDnIDs = UniVarTests.getVolcanoSigLftIDs(RC);
                sigUpIDs = UniVarTests.getVolcanoSigRgtIDs(RC);
                unsigIDs = UniVarTests.getVolcanoUnsigIDs(RC);
            }
            case "dose-de" -> {
                sigDnMat = RDoseUtils.getDoseDnMat(RC);
                if (sigDnMat == null) {
                    return;
                }
                sigUpMat = RDoseUtils.getDoseUpMat(RC);
                unsigMat = RDoseUtils.getDoseUnsigMat(RC);
                sigDnIDs = RDoseUtils.getDoseDnIDs(RC);
                sigUpIDs = RDoseUtils.getDoseUpIDs(RC);
                unsigIDs = RDoseUtils.getDoseUnsigIDs(RC);
            }
            default -> {
                sigDnMat = UniVarTests.getFcSigDnMat(RC);
                sigUpMat = UniVarTests.getFcSigUpMat(RC);
                unsigMat = UniVarTests.getFcUnsigMat(RC);
                sigDnIDs = UniVarTests.getFcSigDnIDs(RC);
                sigUpIDs = UniVarTests.getFcSigUpIDs(RC);
                unsigIDs = UniVarTests.getFcUnsigIDs(RC);
            }
        }

        int dataInx = 0;
        if (sigDnMat != null && sigDnMat[0][0] != -1) {
            int itemInx = 0;
            for (double[] row : sigDnMat) {
                sigLftValues.add(new ScatterDataPoint(row[0], row[1]));
                pointMap.put(dataInx + ":" + itemInx, sigDnIDs[itemInx]);
                itemInx += 1;
            }
            sigLftDataSet.setData(sigLftValues);
            sigLftDataSet.setBackgroundColor("rgba(23, 107, 239, 0.2)");
            sigLftDataSet.setBorderColor("rgb(23, 107, 239)");
            sigLftDataSet.setShowLine(false);
            sigLftDataSet.setLabel("Sig.Down" + " [" + sigLftValues.size() + "]");

            scatterChart.getData().addDataset(sigLftDataSet);
            dataInx += 1;
        }

        if (sigUpMat != null && sigUpMat[0][0] != -1) {
            int itemInx = 0;
            for (double[] row : sigUpMat) {
                sigRgtValues.add(new ScatterDataPoint(row[0], row[1]));
                pointMap.put(dataInx + ":" + itemInx, sigUpIDs[itemInx]);
                itemInx += 1;
            }
            sigRgtDataSet.setData(sigRgtValues);
            sigRgtDataSet.setBackgroundColor("rgba(255, 62, 48, 0.2)");
            sigRgtDataSet.setBorderColor("rgb(255, 62, 48)");
            sigRgtDataSet.setShowLine(false);
            sigRgtDataSet.setLabel("Sig.Up" + " [" + sigRgtValues.size() + "]");

            scatterChart.getData().addDataset(sigRgtDataSet);
            dataInx += 1;
        }

        if (unsigMat != null && unsigMat[0][0] != -1) {
            int itemInx = 0;
            for (double[] row : unsigMat) {
                unsigValues.add(new ScatterDataPoint(row[0], row[1]));
                pointMap.put(dataInx + ":" + itemInx, unsigIDs[itemInx]);
                itemInx += 1;
            }
            unsigDataSet.setData(unsigValues);
            unsigDataSet.setBackgroundColor("rgba(201, 203, 207, 0.2)");
            unsigDataSet.setBorderColor("rgb(201, 203, 207)");
            unsigDataSet.setShowLine(false);
            unsigDataSet.setPointHoverRadius(List.of(0));

            unsigDataSet.setLabel("Unsig." + " [" + unsigValues.size() + "]");

            scatterChart.getData().addDataset(unsigDataSet);
        }

        pointMapJson = new Gson().toJson(pointMap);
        model3 = scatterChart.toJson();
    }

    public String getDoseExtender() {
        if (curType.equals("dose-de")) {
            return "extender";
        } else {
            return "extender2";
        }
    }
    private String pointMapJson = new Gson().toJson("");

    public String getPointMapJson() {
        return pointMapJson;
    }

    public LinkedHashMap<String, String> getPointMap() {
        return pointMap;
    }

    public void setPointMap(LinkedHashMap<String, String> pointMap) {
        this.pointMap = pointMap;
    }

    public String getColHeaderAnova(int inx) {
        if (colVisAnova[inx] != null) {
            return colVisAnova[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileAnova() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameAnova);
    }

    public String getColHeaderLM(int inx) {
        if (colVisLM[inx] != null) {
            return colVisLM[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileLM() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameLM);
    }

    public String getColHeaderAnova2(int inx) {
        if (colVisAnova2[inx] != null) {
            return colVisAnova2[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileAnova2() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameAnova2);
    }

    public String getColHeaderMultiRF(int inx) {
        if (colVisMultiRF[inx] != null) {
            return colVisMultiRF[inx].getName();
        } else {
            return "";
        }
    }

    public StreamedContent getDetailFileMultiRF() {
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fileNameMultiRF);
    }

    public Set<Map.Entry<String, ListDataModel<FeatureBean>>> getListModelEntries() {
        return hashListModel.entrySet();
    }

    public String getEnrQueryType(String key) {
        key = key.replace("_enr", "");
        String res = RDataUtils.getEnrQueryType(sb.getRConnection(), key);
        //System.out.println(res);
        return res;
    }

    public static String getEnrTableName(String key) {
        return switch (key) {

            case "gene_metabolites_enr" ->
                "Gene-Metabolite Network";
            case "metabo_phenotypes_enr" ->
                "Metabolite-Disease Network";
            case "metabo_metabolites_enr" ->
                "Metabolite-Metabolite Network";
            case "global_enr", "keggGlobal_enr" ->
                "Global Network";
            case "dspc_enr" ->
                "DSPC Network";

            default -> {
                yield key;
            }
        };
    }

    public String getEnrFunType(String key) {
        key = key.replace("_enr", "");
        String res = RDataUtils.getEnrFunType(sb.getRConnection(), key);
        return switch (res) {

            case "integ" ->
                "Metabolic pathways (integrated)";
            case "all" ->
                "All pathways (integrated)";
            case "metab" ->
                "Metabolic pathways (metabolite only)";
            case "genetic" ->
                "All pathways (gene only)";
            case "kegg" ->
                "KEGG";
            case "reactome" ->
                "Reactome";
            case "bp" ->
                "GO:BP";
            case "mf" ->
                "GO:MF";
            case "cc" ->
                "GO:CC";
            case "motif" ->
                "Motif";

            default -> {
                yield res;
            }
        };

    }

    public StreamedContent getFileByKey(String key, String type) {
        try {
            String fullPath;
            if (type.equals("enr")) {
                fullPath = hashFileName.get(key);
            } else {
                fullPath = hashFileName2.get(key);
            }

            if (fullPath == null || fullPath.isEmpty()) {
                sb.addMessage("Error", "No file found for key: " + key);
                return null;
            }
            return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + File.separator + fullPath);
        } catch (Exception e) {
            sb.addMessage("Error", "Failed to retrieve file for: " + key);
            e.printStackTrace();
            return null;
        }
    }

    public String getColHeaderByEntry(int inx, String entry) {
        return hashColVis.get(entry)[inx].getName();
    }

    private String currentSetName = "";

    public MetSetBean[] getCurrentPathSet() {
        ArrayList<MetSetBean> libVec = new ArrayList();
        String[] details = RDataUtils.getEnrHTMLPathSet(sb.getRConnection(), currentSetName);
        libVec.add(new MetSetBean(details[0], details[1], ""));
        return libVec.toArray(MetSetBean[]::new);
    }

    public void setSelectedPath(MetSetBean selectedData) {
        String setName = selectedData.getName();
        currentSetName = setName;
        getCurrentPathSet();
    }

    public boolean checkHashListModel2Key(String key) {
        if (!hashListModel2.containsKey(key)) {
            return false;
        }
        ListDataModel<FeatureBean> model = hashListModel2.get(key);
        return model != null && model.getRowCount() > 0;
    }

    public Set<Map.Entry<String, ListDataModel<FeatureBean>>> getListModelEntries2() {
        return hashListModel2.entrySet();
    }

}
