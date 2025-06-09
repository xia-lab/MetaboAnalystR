/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonIgnore;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.util.Arrays;
import java.util.List;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.controllers.general.NormBean;
import pro.metaboanalyst.controllers.general.ProcessBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.dose.DoseResponseBean;
import pro.metaboanalyst.controllers.enrich.MappingBean;
import pro.metaboanalyst.controllers.enrich.MsetBean;
import pro.metaboanalyst.controllers.enrich.PathBean;
import pro.metaboanalyst.controllers.meta.MetaLoadBean;
import pro.metaboanalyst.controllers.meta.MetaStatBean;
import pro.metaboanalyst.controllers.metapath.MetaPathLoadBean;
import pro.metaboanalyst.controllers.metapath.MetaPathStatBean;
import pro.metaboanalyst.controllers.multifac.Aov2Bean;
import pro.metaboanalyst.controllers.multifac.AscaBean;
import pro.metaboanalyst.controllers.multifac.HeatMap2Bean;
import pro.metaboanalyst.controllers.multifac.LimmaBean;
import pro.metaboanalyst.controllers.multifac.LivePCABean;
import pro.metaboanalyst.controllers.multifac.MebaBean;
import pro.metaboanalyst.controllers.multifac.MetaHeatmapBean;
import pro.metaboanalyst.controllers.multifac.MultiCorrBean;
import pro.metaboanalyst.controllers.multifac.MultiRfBean;
import pro.metaboanalyst.controllers.mummichog.MummiAnalBean;
import pro.metaboanalyst.controllers.mummichog.PeakCustomBean;
import pro.metaboanalyst.controllers.stats.ClassificationBean;
import pro.metaboanalyst.controllers.stats.ClusterBean;
import pro.metaboanalyst.controllers.stats.OPLSDABean;
import pro.metaboanalyst.controllers.stats.PCABean;
import pro.metaboanalyst.controllers.stats.RocAnalBean;
import pro.metaboanalyst.controllers.stats.SPLSDABean;
import pro.metaboanalyst.controllers.stats.SigVarBean;
import pro.metaboanalyst.controllers.stats.UnivBean;
import pro.metaboanalyst.lts.FunctionInfo;
import static pro.metaboanalyst.rwrappers.RCenter.cleanRCmd;
import pro.metaboanalyst.spectra.SpectraParamBean;
import pro.metaboanalyst.spectra.SpectraUploadBean;
import pro.metaboanalyst.spectra.SpectraProcessBean;

/**
 *
 * @author zgy
 */
@RequestScoped
@Named("javaRecord")
public class JavaRecord {

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    public void record_skipButton_action_default(SessionBean1 sb) {
        FunctionInfo functionInfo = new FunctionInfo("Sanity Check", "ProcessBean.skipButton_action_default", "Sanity check function");
        functionInfo.addParameter("sessionBean1.dataType", sb.getDataType());
        functionInfo.addParameter("sessionBean1.analType", sb.getAnalType());
        functionInfo.addParameter("sessionBean1.featType", sb.getFeatType());
        functionInfo.addParameter("sessionBean1.cmpdIDType", sb.getCmpdIDType());
        functionInfo.addParameter("sessionBean1.multiGroup", sb.isMultiGroup());
        functionInfo.addParameter("sessionBean1.integChecked", sb.isIntegChecked());
        functionInfo.addParameter("sessionBean1.smallSmplSize", sb.isSmallSmplSize());

        wb.addFunctionInfo("Sanity Check", functionInfo);
        wb.getCalledWorkflows().add("Sanity Check");
        wb.getCalledWorkflows().add("Data Processing");
        if (sb.getAnalType().equals("mummichog")) {
            if (sb.getDataType().equals("mass_all")) {
                wb.getCalledWorkflows().add("Sanity Check Peak");
            } else {
                wb.getCalledWorkflows().add("Sanity Check Intensity");
            }
        }
    }

    public void record_PerformDataNormalization(NormBean nb) {
        // Initialize FunctionInfo with the name and description of the function being logged
        FunctionInfo functionInfo = new FunctionInfo("Normalization", "normBean.performDataNormalization", "Perform data normalization.");

        // Add parameters from SessionBean and the current normalization settings
        functionInfo.addParameter("normBean.rowNormOpt", nb.getRowNormOpt());
        functionInfo.addParameter("normBean.transNormOpt", nb.getTransNormOpt());
        functionInfo.addParameter("normBean.scaleNormOpt", nb.getScaleNormOpt());
        functionInfo.addParameter("normBean.includeRatio", nb.isIncludeRatio());
        functionInfo.addParameter("normBean.ratioNumOpt", nb.getRatioNumOpt());
        functionInfo.addParameter("normBean.specNormSpecifed", nb.isSpecNormSpecifed());
        functionInfo.addParameter("normBean.refVar", nb.getRefVar());
        functionInfo.addParameter("normBean.refGrp", nb.getRefGrp());
        functionInfo.addParameter("normBean.refSmpl", nb.getRefSmpl());
        //System.out.println("record_PerformDataNormalization");

        wb.addFunctionInfo("Normalization", functionInfo);

    }

    public void record_filterButton_action(ProcessBean pb) {
        FunctionInfo functionInfo = new FunctionInfo("Filtering", "procBean.filterButton_action", "Executes QC filtering and variable filtering based on user settings.");

        // Extract information directly from ProcessBean
        functionInfo.addParameter("procBean.doQCFiltering", pb.isDoQCFiltering() ? "Yes" : "No");
        functionInfo.addParameter("procBean.qcCutoff", pb.getQcCutoff());
        functionInfo.addParameter("procBean.varFilterOpt", pb.getVarFilterOpt());
        functionInfo.addParameter("procBean.filterCutoff", pb.getFilterCutoff());
        functionInfo.addParameter("procBean.intFilterOpt", pb.getIntFilterOpt());
        functionInfo.addParameter("procBean.IntFilterCutoff", pb.getIntFilterCutoff());
        System.out.println("record_filterButton_action");

        wb.addFunctionInfo("Filtering", functionInfo);

    }

    public void record_vcButton_action(UnivBean vb) {
        FunctionInfo functionInfo = new FunctionInfo("Volcano", "univBean.vcButton_action", "Performs volcano plot analysis based on user settings.");

        // Extract information directly from VolcanoBean
        functionInfo.addParameter("univBean.nonParVcTt", vb.isNonParVcTt() ? "true" : "false");
        functionInfo.addParameter("univBean.vcFcThresh", vb.getVcFcThresh());
        functionInfo.addParameter("univBean.cmpType", vb.getCmpType());
        functionInfo.addParameter("univBean.nonpar", vb.isNonParVcTt() ? "true" : "false");
        functionInfo.addParameter("univBean.vcPThresh", vb.getVcPThresh());
        functionInfo.addParameter("univBean.equalVar", vb.getEqualVar());
        functionInfo.addParameter("univBean.vcPvalType", vb.getVcPvalType());
        functionInfo.addParameter("univBean.labelOpt", vb.getLabelOpt());
        System.out.println("record_vcButton_action");

        wb.getCalledWorkflows().add("Univariate");
        wb.getCalledWorkflows().add("Volcano");
        wb.addFunctionInfo("Volcano", functionInfo);
    }

    public void record_aovButton_action(UnivBean vb) {
        FunctionInfo functionInfo = new FunctionInfo("ANOVA", "univBean.aovButton_action", "Performs ANOVA based on user settings.");

        // Extract information directly from VolcanoBean
        functionInfo.addParameter("univBean.aovPThresh", vb.getAovPThresh());
        functionInfo.addParameter("univBean.nonParam", vb.isNonParam() ? "True" : "False");
        System.out.println("record_vcButton_action");

        wb.getCalledWorkflows().add("Univariate");
        wb.getCalledWorkflows().add("ANOVA");
        wb.addFunctionInfo("ANOVA", functionInfo);
    }

    public void record_corrBtn_action(UnivBean vb) {
        FunctionInfo functionInfo = new FunctionInfo("Correlation Heatmap", "univBean.corrBtn_action", "Performs feature correlation heatmap analysis.");

        //PrimeFaces.current().scrollTo("form1:corrPane");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("univBean.corDirection", vb.getCorDirection());
        functionInfo.addParameter("univBean.hmDistMeasure", vb.getHmDistMeasure());
        functionInfo.addParameter("univBean.colContrast", vb.getColContrast());
        functionInfo.addParameter("univBean.fixRange", vb.isFixRange() ? "true" : "false");
        functionInfo.addParameter("univBean.noClust", vb.isNoClust() ? "true" : "false");
        functionInfo.addParameter("univBean.fontSize", vb.getFontSize());
        functionInfo.addParameter("univBean.unit", vb.getUnit());
        functionInfo.addParameter("univBean.corrThresh", vb.getCorrThresh());
        System.out.println("record_corrBtn_action");

        wb.getCalledWorkflows().add("Advanced Significance");
        wb.getCalledWorkflows().add("Correlation Heatmap");
        wb.addFunctionInfo("Correlation Heatmap", functionInfo);
    }

    public void record_ebamBtn_action(SigVarBean sv) {
        FunctionInfo functionInfo = new FunctionInfo("EBAM", "sigBean.ebamBtn_action", "Performs Empirical Bayesian Analysis");

        functionInfo.addParameter("sigBean.nonParEBAM", sv.isNonParEBAM() ? "true" : "false");
        functionInfo.addParameter("sigBean.pairedAnal", sv.getPairedAnal());
        functionInfo.addParameter("sigBean.equalVar", sv.getEqualVar());
        functionInfo.addParameter("sigBean.alpha", sv.getAlpha());
        functionInfo.addParameter("sigBean.ebamDelta", sv.getEbamDelta());
        System.out.println("record_ebamBtn_action");

        wb.getCalledWorkflows().add("Advanced Significance");
        wb.getCalledWorkflows().add("EBAM");

        wb.addFunctionInfo("EBAM", functionInfo);
    }

    public void record_fcButton_action(UnivBean vb) {
        FunctionInfo functionInfo = new FunctionInfo("Fold Change", "univBean.fcButton_action", "Performs fold change analysis.");

        functionInfo.addParameter("univBean.pairedFcAnal", vb.getPairedFcAnal());
        functionInfo.addParameter("univBean.fcThresh", vb.getFcThresh());
        functionInfo.addParameter("univBean.cmpType", vb.getCmpType());

        System.out.println("record_fcButton_action");

        wb.addFunctionInfo("Fold Change", functionInfo);
    }

    public void record_kmButton_action(ClusterBean b) {
        FunctionInfo functionInfo = new FunctionInfo("K-means", "clusterBean.kmButton_action", "Performs fold change analysis.");

        functionInfo.addParameter("clusterBean.kmClustNm", b.getKmClustNm());
        functionInfo.addParameter("clusterBean.kmColPal", b.getKmColPal());

        System.out.println("record_kmButton_action");

        wb.addFunctionInfo("K-means", functionInfo);
    }

    public void record_ttButton_action(UnivBean b) {
        FunctionInfo functionInfo = new FunctionInfo("T-test", "univBean.ttButton_action", "Performs fold change analysis.");

        functionInfo.addParameter("univBean.nonParTt", b.isNonParTt());
        functionInfo.addParameter("univBean.ttPThresh", b.getTtPThresh());
        functionInfo.addParameter("univBean.pairedTtAnal", b.getPairedTtAnal());
        functionInfo.addParameter("univBean.equalVar", b.getEqualVar());
        functionInfo.addParameter("univBean.ttPvalType", b.getTtPvalType());

        System.out.println("record_ttButton_action");
        wb.getCalledWorkflows().add("Univariate");

        wb.addFunctionInfo("T-test", functionInfo);
    }

    public void recordOplsdaAction(OPLSDABean bean) {
        FunctionInfo functionInfo = new FunctionInfo("OPLSDA", "oplsBean.perform_oplsda", "Performs OPLS-DA analysis.");

        functionInfo.addParameter("oplsBean.displayConfs", String.valueOf(bean.isDisplayConfs()));
        functionInfo.addParameter("oplsBean.displayNames", String.valueOf(bean.isDisplayNames()));
        functionInfo.addParameter("oplsBean.displayFeatNames", String.valueOf(bean.isDisplayFeatNames()));
        functionInfo.addParameter("oplsBean.loadOpt", bean.getLoadOpt());
        functionInfo.addParameter("oplsBean.grayScale", String.valueOf(bean.isGrayScale()));
        functionInfo.addParameter("oplsBean.permStat", bean.getPermStat());
        functionInfo.addParameter("oplsBean.permNum", String.valueOf(bean.getPermNum()));
        functionInfo.addParameter("oplsBean.cexOpt", bean.getCexOpt());

        wb.addFunctionInfo("OPLSDA", functionInfo);
        wb.getCalledWorkflows().add("OrthPLSDA");
        wb.getCalledWorkflows().add("Chemometrics");

    }

    public void record_pca(PCABean pcaBean) {
        FunctionInfo functionInfo = new FunctionInfo("PCA", "pcaBean.performPCA", "Performs Principal Component Analysis.");

        functionInfo.addParameter("pcaBean.displayConfs", String.valueOf(pcaBean.isDisplayConfs()));
        functionInfo.addParameter("pcaBean.displayNames", String.valueOf(pcaBean.isDisplayNames()));
        functionInfo.addParameter("pcaBean.displayFeatNames", String.valueOf(pcaBean.isDisplayFeatNames()));
        functionInfo.addParameter("pcaBean.loadOpt", pcaBean.getLoadOpt());
        functionInfo.addParameter("pcaBean.greyScale", String.valueOf(pcaBean.isGreyScale()));
        functionInfo.addParameter("pcaBean.diffShapes", String.valueOf(pcaBean.isDiffShapes()));
        functionInfo.addParameter("pcaBean.pcaPairNum", String.valueOf(pcaBean.getPcaPairNum()));
        functionInfo.addParameter("pcaBean.pcaScreeNum", String.valueOf(pcaBean.getPcaScreeNum()));
        functionInfo.addParameter("pcaBean.pcaScoreX", String.valueOf(pcaBean.getPcaScoreX()));
        functionInfo.addParameter("pcaBean.pcaScoreY", String.valueOf(pcaBean.getPcaScoreY()));
        functionInfo.addParameter("pcaBean.pcaScore3dX", String.valueOf(pcaBean.getPcaScore3dX()));
        functionInfo.addParameter("pcaBean.pcaScore3dY", String.valueOf(pcaBean.getPcaScore3dY()));
        functionInfo.addParameter("pcaBean.pcaScore3dZ", String.valueOf(pcaBean.getPcaScore3dZ()));
        functionInfo.addParameter("pcaBean.pcaLoadX", String.valueOf(pcaBean.getPcaLoadX()));
        functionInfo.addParameter("pcaBean.pcaLoadY", String.valueOf(pcaBean.getPcaLoadY()));
        functionInfo.addParameter("pcaBean.pcaBiplotX", String.valueOf(pcaBean.getPcaBiplotX()));
        functionInfo.addParameter("pcaBean.pcaBiplotY", String.valueOf(pcaBean.getPcaBiplotY()));
        functionInfo.addParameter("pcaBean.rotationAngle", String.valueOf(pcaBean.getRotationAngle()));
        functionInfo.addParameter("pcaBean.cexOpt", pcaBean.getCexOpt());
        functionInfo.addParameter("pcaBean.flipOpt", pcaBean.getFlipOpt());

        // Assuming FunctionProfiler has a way to log or store the function info
        wb.addFunctionInfo("PCA", functionInfo);
        wb.getCalledWorkflows().add("Chemometrics");

    }

    public void record_rfBn_action(ClassificationBean b) {
        FunctionInfo functionInfo = new FunctionInfo("Random Forest", "classBean.rfBn_action", "Performs Random Forest");

        functionInfo.addParameter("classBean.treeNum", b.getTreeNum());
        functionInfo.addParameter("classBean.tryNum", b.getTryNum());
        functionInfo.addParameter("classBean.rfRandom", b.getRfRandom());

        System.out.println("record_rfBn_action");

        wb.addFunctionInfo("Random Forest", functionInfo);
        wb.getCalledWorkflows().add("Classification");
    }

    public void record_svmBn_action(ClassificationBean b) {
        FunctionInfo functionInfo = new FunctionInfo("Support Vector Machine", "classBean.svmBn_action", "Performs support vector machine");

        functionInfo.addParameter("classBean.validationOpt", b.getValidationOpt());

        System.out.println("record_svmBn_action");

        wb.addFunctionInfo("SVM", functionInfo);
        wb.getCalledWorkflows().add("SVM");
        wb.getCalledWorkflows().add("Classification");

    }

    public void record_samBtn1_action(SigVarBean b) {
        FunctionInfo functionInfo = new FunctionInfo("Random Forest", "classBean.samBtn1_action", "Performs Significance Analysis of Metabolomics (SAM)");

        functionInfo.addParameter("sigBean.nonParSam", b.isNonParSAM());
        functionInfo.addParameter("sigBean.pairedAnal", b.getPairedAnal());
        functionInfo.addParameter("sigBean.equalVar", b.getEqualVar());

        functionInfo.addParameter("sigBean.delta", b.getDelta());
        functionInfo.addParameter("sigBean.deltaMin", b.getDeltaMin());
        functionInfo.addParameter("sigBean.deltaMax", b.getDeltaMax());
        functionInfo.addParameter("sigBean.step", b.getStep());

        System.out.println("record_samBtn1_action");

        wb.addFunctionInfo("SAM", functionInfo);
        wb.getCalledWorkflows().add("SAM");
        wb.getCalledWorkflows().add("Advanced Significance");

    }

    public void record_somButton_action(ClusterBean b) {
        FunctionInfo functionInfo = new FunctionInfo("SOM", "clusterBean.somButton_action", "Performs Self-organizing Map (SOM).");

        functionInfo.addParameter("clusterBean.somXdim", b.getSomXdim());
        functionInfo.addParameter("clusterBean.somYdim", b.getSomYdim());
        functionInfo.addParameter("clusterBean.somInitOpt", b.getSomInitOpt());

        functionInfo.addParameter("clusterBean.somNbOpt", b.getSomNbOpt());
        functionInfo.addParameter("clusterBean.somLabel", b.isSomLabel());
        functionInfo.addParameter("clusterBean.somColPal", b.getSomColPal());

        System.out.println("record_somButton_action");

        wb.addFunctionInfo("SOM", functionInfo);
        wb.getCalledWorkflows().add("SOM");
        wb.getCalledWorkflows().add("Clustering");

    }

    public void record_updateSPLSDA(SPLSDABean s) {
        FunctionInfo functionInfo = new FunctionInfo("sPLSDA", "splsBean.updateSPLSDA", "Performs sPLSDA");

        functionInfo.addParameter("splsBean.compNum", String.valueOf(s.getCompNum()));
        functionInfo.addParameter("splsBean.varNum", String.valueOf(s.getVarNum()));
        functionInfo.addParameter("splsBean.compVarOpt", s.getCompVarOpt());
        functionInfo.addParameter("splsBean.splsPairNum", String.valueOf(s.getSplsPairNum()));
        functionInfo.addParameter("splsBean.splsScore2dX", String.valueOf(s.getSplsScore2dX()));
        functionInfo.addParameter("splsBean.splsScore2dY", String.valueOf(s.getSplsScore2dY()));
        functionInfo.addParameter("splsBean.displayConfs", String.valueOf(s.isDisplayConfs()));
        functionInfo.addParameter("splsBean.displayNames", String.valueOf(s.isDisplayNames()));
        functionInfo.addParameter("splsBean.displayFeatNames", String.valueOf(s.isDisplayFeatNames()));
        functionInfo.addParameter("splsBean.cvOpt", s.getCvOpt());
        functionInfo.addParameter("splsBean.foldNum", String.valueOf(s.getFoldNum()));
        functionInfo.addParameter("splsBean.splsScore3dX", String.valueOf(s.getSplsScore3dX()));
        functionInfo.addParameter("splsBean.splsScore3dY", String.valueOf(s.getSplsScore3dY()));
        functionInfo.addParameter("splsBean.splsScore3dZ", String.valueOf(s.getSplsScore3dZ()));
        functionInfo.addParameter("splsBean.rotationAngle", String.valueOf(s.getRotationAngle()));
        functionInfo.addParameter("splsBean.splsLoadX", String.valueOf(s.getSplsLoadX()));
        functionInfo.addParameter("splsBean.splsLoadY", String.valueOf(s.getSplsLoadY()));
        functionInfo.addParameter("splsBean.viewOpt", s.getViewOpt());
        functionInfo.addParameter("splsBean.grayScale", String.valueOf(s.isGrayScale()));
        functionInfo.addParameter("splsBean.greyScale", String.valueOf(s.isGreyScale()));
        functionInfo.addParameter("splsBean.cexOpt", s.getCexOpt());

        System.out.println("record_updateSPLSDA");

        wb.addFunctionInfo("sPLSDA", functionInfo);
        wb.getCalledWorkflows().add("sPLSDA");

    }

    public void record_treeButton_action(ClusterBean b) {
        FunctionInfo functionInfo = new FunctionInfo("Dendrogram", "clusterBean.treeButton_action", "Performs Hierarchical Clustering Dendrogram");
        functionInfo.addParameter("clusterBean.clustDistOpt", b.getClustDistOpt());
        functionInfo.addParameter("clusterBean.clustMethodOpt", b.getClustMethodOpt());
        System.out.println("record_treeButton_action");
        wb.addFunctionInfo("Dendrogram", functionInfo);
        wb.getCalledWorkflows().add("Dendrogram");

    }

    public void record_ptnBtn_action(UnivBean vb) {
        FunctionInfo functionInfo = new FunctionInfo("Pattern Search", "univBean.ptnBtn_action", "Performs Pattern Search, also known as Pattern Hunter analysis");

        // Extract information directly from VolcanoBean
        functionInfo.addParameter("univBean.ptnDistMeasure", vb.getPtnDistMeasure());
        functionInfo.addParameter("univBean.ptnFeature", vb.getPtnFeature());
        functionInfo.addParameter("univBean.ptnTemplate", vb.getPtnTemplate());
        functionInfo.addParameter("univBean.usrPtn", vb.getUsrPtn());

        System.out.println("record_ptnBtn_action");

        wb.addFunctionInfo("Pattern Search", functionInfo);
        wb.getCalledWorkflows().add("Pattern Search");

    }

    public void record_initPCA3D(LivePCABean b) {
        FunctionInfo functionInfo = new FunctionInfo("PCA 3D", "livePcaBean.initPCA3D", "Performs PCA (Principal Component Analysis) visualization in 3D");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("livePcaBean.pcaPairNum", b.getPcaPairNum());
        functionInfo.addParameter("livePcaBean.colOpt", b.getColOpt());
        functionInfo.addParameter("livePcaBean.shapeOpt", b.getShapeOpt());

        System.out.println("record_initPCA3D");

        wb.addFunctionInfo("PCA 3D", functionInfo);
        wb.getCalledWorkflows().add("Data Overview");

    }

    public void record_aov2Bn_action(Aov2Bean b) {
        FunctionInfo functionInfo = new FunctionInfo("Multifactor anova", "aov2Bean.initPCA3D", "Performs PCA (Principal Component Analysis) visualization in 3D");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("aov2Bean.pthresh", b.getPthresh());
        functionInfo.addParameter("aov2Bean.pvalOpt", b.getPvalOpt());
        functionInfo.addParameter("aov2Bean.phenOpt", b.getPhenOpt());
        functionInfo.addParameter("aov2Bean.selectedMetasAnova", b.getSelectedMetasAnova());

        System.out.println("record_aov2Bn_action");

        wb.addFunctionInfo("Multifactor anova", functionInfo);
        wb.getCalledWorkflows().add("Multifactor anova");

    }

    public void record_mdlBtn_action(AscaBean b) {
        FunctionInfo functionInfo = new FunctionInfo("ASCA", "ascaBean.mdlBtn_action", "Performs ASCA");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("ascaBean.mdlANum", b.getMdlANum());
        functionInfo.addParameter("ascaBean.mdlBNum", b.getMdlBNum());
        functionInfo.addParameter("ascaBean.mdlABNum", b.getMdlABNum());
        functionInfo.addParameter("ascaBean.mdlResNum", b.getMdlResNum());

        functionInfo.addParameter("ascaBean.useGreyCol", b.isUseGreyCol());
        functionInfo.addParameter("ascaBean.permNum", b.getPermNum());
        functionInfo.addParameter("ascaBean.alphaThresh", b.getAlphaThresh());
        functionInfo.addParameter("ascaBean.lvlThresh", b.getLvlThresh());

        System.out.println("mdlBtn_action");

        wb.addFunctionInfo("ASCA", functionInfo);
        wb.getCalledWorkflows().add("ASCA");
        wb.getCalledWorkflows().add("Multivariate Analysis");

    }

    public void record_hm2b_action(HeatMap2Bean b) {
        FunctionInfo functionInfo = new FunctionInfo("Heatmap2", "hm2Bean.hm2b_action", "Performs Heatmap visualization of multifactor data");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("hm2Bean.smplSortOpt", b.getSmplSortOpt());
        functionInfo.addParameter("hm2Bean.useTopFeature", b.isUseTopFeature());
        functionInfo.addParameter("hm2Bean.selectedMetas", b.getSelectedMetas());
        functionInfo.addParameter("hm2Bean.maxFeatureNum", b.getMaxFeatureNum());

        functionInfo.addParameter("hm2Bean.topThresh", b.getTopThresh());
        functionInfo.addParameter("hm2Bean.fontSizeCol", b.getFontSizeCol());
        functionInfo.addParameter("hm2Bean.fontSizeRow", b.getFontSizeRow());
        functionInfo.addParameter("hm2Bean.smplSortOptList", b.getSmplSortOptList());

        System.out.println("hm2b_action");

        wb.addFunctionInfo("Heatmap2", functionInfo);
        wb.getCalledWorkflows().add("Clustering heatmap");
        wb.getCalledWorkflows().add("Data Overview");

    }

    public void record_covScatterButton_action(LimmaBean b) {
        FunctionInfo functionInfo = new FunctionInfo("Linear Models", "lmBean.covScatterButton_action", "Performs linear modeling with covariate adjustment");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("lmBean.covPThresh", b.getCovPThresh());
        functionInfo.addParameter("lmBean.analysisMeta", b.getAnalysisMeta());
        functionInfo.addParameter("lmBean.blockFac", b.getBlockFac());
        functionInfo.addParameter("lmBean.referenceGroupFromAnalysisMeta", b.getReferenceGroupFromAnalysisMeta());

        functionInfo.addParameter("lmBean.contrastFromAnalysisMeta", b.getContrastFromAnalysisMeta());
        functionInfo.addParameter("lmBean.adjustedMeta", b.getAdjustedMeta());
        functionInfo.addParameter("lmBean.analysisMeta", b.getAnalysisMeta());
        functionInfo.addParameter("lmBean.covStyleOpt", b.getCovStyleOpt());

        System.out.println("covScatterButton_action");

        wb.addFunctionInfo("Linear Models", functionInfo);
        wb.getCalledWorkflows().add("Linear Models");
        wb.getCalledWorkflows().add("Univariate Analysis");

    }

    public void record_mbButton_action(MebaBean b) {
        FunctionInfo functionInfo = new FunctionInfo("MEBA", "mebaBean.mbButton_action", "Performs Time Course Profiling (MEBA)");
        // Extract information directly from VolcanoBean
        functionInfo.addParameter("mebaBean.mebaMetas", b.getMebaMetas());

        System.out.println("record_mbButton_action");

        wb.addFunctionInfo("MEBA", functionInfo);
        wb.getCalledWorkflows().add("MEBA");
        wb.getCalledWorkflows().add("Multivariate Analysis");

    }

    public void record_metaOverviewBn_action(MetaHeatmapBean b) {
        FunctionInfo functionInfo = new FunctionInfo("Metadata Heatmap", "mhmBean.metaOverviewBn_action", "Performs meta-data heatmap clustering.");

        functionInfo.addParameter("mhmBean.metaDistOpt", b.getMetaDistOpt());
        functionInfo.addParameter("mhmBean.metaViewOpt", b.getMetaViewOpt());
        functionInfo.addParameter("mhmBean.metaClusterOpt", b.getMetaClusterOpt());
        functionInfo.addParameter("mhmBean.metaClusterSelOpt", b.getMetaClusterSelOpt());
        functionInfo.addParameter("mhmBean.metaColorOpt", b.getMetaColorOpt());
        functionInfo.addParameter("mhmBean.includeRowNamesMeta", String.valueOf(b.isIncludeRowNamesMeta()));
        functionInfo.addParameter("mhmBean.drawBordersMeta", String.valueOf(b.isDrawBordersMeta()));
        functionInfo.addParameter("mhmBean.corOpt", b.getCorOpt());

        System.out.println("record_metaOverviewBn_action");

        wb.addFunctionInfo("Metadata Heatmap", functionInfo);
        wb.getCalledWorkflows().add("Data Overview");
        wb.getCalledWorkflows().add("Metadata Heatmap");
    }

    public void record_corBtn_action(MultiCorrBean bean) {
        FunctionInfo functionInfo = new FunctionInfo(
                "corBtn_action",
                "mCorrBean.corBtn_action",
                "Performs correlation analysis based on the provided metadata and features."
        );

        functionInfo.addParameter("mCorrBean.covFeatures", bean.getCovFeatures());
        functionInfo.addParameter("mCorrBean.ptnMetaCov", bean.getPtnMetaCov());
        functionInfo.addParameter("mCorrBean.ptnFeature", bean.getPtnFeature());
        functionInfo.addParameter("mCorrBean.tgtType", bean.getTgtType());
        functionInfo.addParameter("mCorrBean.covType", bean.getCovType());
        functionInfo.addParameter("mCorrBean.ptnMeta", bean.getPtnMeta());
        functionInfo.addParameter("mCorrBean.ptnDistMeasure", bean.getPtnDistMeasure());
        functionInfo.addParameter("mCorrBean.covMetas", Arrays.toString(bean.getCovMetas()));

        // Log or store the functionInfo somewhere, e.g., console or a file
        System.out.println("Recorded correlation button action: " + functionInfo);
        wb.addFunctionInfo("corBtn_action", functionInfo);
        wb.getCalledWorkflows().add("Correlation Analysis");
        wb.getCalledWorkflows().add("Data Overview");

    }

    public void record_rfBn_action_time(MultiRfBean bean) {
        FunctionInfo functionInfo = new FunctionInfo("Random Forest2", "mrfBean.rfBn_action_time", "Executes and times a Random Forest analysis.");

        functionInfo.addParameter("mrfBean.treeNum", String.valueOf(bean.getTreeNum()));
        functionInfo.addParameter("mrfBean.tryNum", String.valueOf(bean.getTryNum()));
        functionInfo.addParameter("mrfBean.rfRandom", String.valueOf(bean.getRfRandom()));
        functionInfo.addParameter("mrfBean.rfMeta", bean.getRfMeta() == null ? "null" : bean.getRfMeta());
        functionInfo.addParameter("mrfBean.predictedMeta", bean.getPredictedMeta() == null ? "null" : Arrays.toString(bean.getPredictedMeta()));

        // Here, fp is assumed to be a context or application-wide accessible object
        wb.addFunctionInfo("Random Forest2", functionInfo);
        wb.getCalledWorkflows().add("Random Forest2");
        wb.getCalledWorkflows().add("Supervise Classification");

    }

    public void record_performExploreAnalysis(RocAnalBean bean) {
        FunctionInfo functionInfo = new FunctionInfo("Multivariate ROC", "rocAnalBean.performExploreAnalysis", "Perform Multivariate ROC based exploratory analysis.");

        functionInfo.addParameter("rocAnalBean.analMode", String.valueOf(bean.getAnalMode()));
        functionInfo.addParameter("rocAnalBean.clsMethodOpt", String.valueOf(bean.getClsMethodOpt()));
        functionInfo.addParameter("rocAnalBean.featRankOpt", String.valueOf(bean.getFeatRankOpt()));
        functionInfo.addParameter("rocAnalBean.lvNum", bean.getLvNum());

        // Here, fp is assumed to be a context or application-wide accessible object
        wb.addFunctionInfo("Multivariate ROC", functionInfo);
        wb.getCalledWorkflows().add("Multivariate ROC");

    }

    public void record_updateDoseDEAnalysis(DoseResponseBean bean) {
        FunctionInfo functionInfo = new FunctionInfo("Dose Differential Expression", "doseResponseBean.updateDoseDEAnalysis", "Perform Differential Expression analysis in the dose response analysis workflow.");

        functionInfo.addParameter("doseResponseBean.sigLevel", bean.getSigLevel());
        functionInfo.addParameter("doseResponseBean.fcLevel", bean.getFcLevel());

        // Here, fp is assumed to be a context or application-wide accessible object
        wb.addFunctionInfo("Dose Differential Expression", functionInfo);
        wb.getCalledWorkflows().add("DE Analysis");
    }

    public void record_performCurveFitting(DoseResponseBean bean) {
        FunctionInfo functionInfo = new FunctionInfo("Curve Fitting", "doseResponseBean.performCurveFitting", "Perform curve fitting analysis in the dose response analysis.");

        // Additional parameters
        functionInfo.addParameter("doseResponseBean.exp2", String.valueOf(bean.isExp2()));
        functionInfo.addParameter("doseResponseBean.exp3", String.valueOf(bean.isExp3()));
        functionInfo.addParameter("doseResponseBean.exp4", String.valueOf(bean.isExp4()));
        functionInfo.addParameter("doseResponseBean.exp5", String.valueOf(bean.isExp5()));
        functionInfo.addParameter("doseResponseBean.lin", String.valueOf(bean.isLin()));
        functionInfo.addParameter("doseResponseBean.poly2", String.valueOf(bean.isPoly2()));
        functionInfo.addParameter("doseResponseBean.poly3", String.valueOf(bean.isPoly3()));
        functionInfo.addParameter("doseResponseBean.poly4", String.valueOf(bean.isPoly4()));
        functionInfo.addParameter("doseResponseBean.hill", String.valueOf(bean.isHill()));
        functionInfo.addParameter("doseResponseBean.power", String.valueOf(bean.isPower()));
        functionInfo.addParameter("doseResponseBean.FDR", String.valueOf(bean.isFDR()));
        functionInfo.addParameter("doseResponseBean.sigOK", String.valueOf(bean.isSigOK()));
        functionInfo.addParameter("doseResponseBean.units", bean.getUnits());
        functionInfo.addParameter("doseResponseBean.transDose", bean.getTransDose());
        functionInfo.addParameter("doseResponseBean.ctrlMode", bean.getCtrlMode());
        functionInfo.addParameter("doseResponseBean.bmd", bean.getBmd());
        functionInfo.addParameter("doseResponseBean.sigLevel", bean.getSigLevel());
        functionInfo.addParameter("doseResponseBean.fcLevel", bean.getFcLevel());
        functionInfo.addParameter("doseResponseBean.cutoffval", bean.getCutoffval());
        functionInfo.addParameter("doseResponseBean.bmdOption", bean.getBmdOption());
        functionInfo.addParameter("doseResponseBean.numsds", bean.getNumsds());
        // Here, fp is assumed to be a context or application-wide accessible object
        wb.addFunctionInfo("Curve Fitting", functionInfo);
        wb.getCalledWorkflows().add("Curve Fitting");

    }

    public void record_submitBtn_action(MsetBean bean) {
        // Create an instance of FunctionInfo for logging purpose
        FunctionInfo functionInfo = new FunctionInfo(
                "Enrichment",
                "msetBean.submitBtn_action",
                "Perform enrichment analysis ORA or QEA"
        );

        // Log each parameter by fetching through getter methods
        functionInfo.addParameter("msetBean.doMsetFilter", String.valueOf(bean.isDoMsetFilter()));
        functionInfo.addParameter("msetBean.minMsetNum", String.valueOf(bean.getMinMsetNum()));
        functionInfo.addParameter("msetBean.libOpt", bean.getLibOpt());
        functionInfo.addParameter("msetBean.msetOpt", bean.getMsetOpt());

        // Assuming there's a method in 'fp' to add function information
        wb.addFunctionInfo("Enrichment", functionInfo);
    }

    public void record_sspNextBn_action(MappingBean bean) {
        FunctionInfo functionInfo = new FunctionInfo(
                "SSP",
                "mapBean.sspNextBn_action",
                "Handles Single Sample Profiling (SSP) in enrichment analysis."
        );

        // Record parameters that define the state at the time of the action
        functionInfo.addParameter("mapBean.selectedCmpdList", bean.getSelectedCmpdList());

        // Assuming fp is a system-wide accessible object for handling function information
        wb.addFunctionInfo("SSP", functionInfo);
    }

    public void record_customButton_action(PeakCustomBean b) {
        FunctionInfo functionInfo = new FunctionInfo(
                "InitLibrary",
                "peakcBean.customButton_action",
                "Init Functional Annotation Library"
        );

        // Assuming fp is a system-wide accessible object for handling function information
        wb.addFunctionInfo("InitLibrary", functionInfo);
    }

    public void record_performPeaks2Fun(MummiAnalBean bean) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performPeaks2Fun",
                "mummiAnalBean.performPeaks2Fun",
                "Handles the execution of peak to function analysis and decides the analytical path based on selected options."
        );

        // Log each parameter and condition that could influence the method execution
        functionInfo.addParameter("mummiAnalBean.mumVersion", bean.getMumVersion());
        functionInfo.addParameter("mummiAnalBean.instrumentOpt", bean.getInstrumentOpt());
        functionInfo.addParameter("mummiAnalBean.analOption", bean.getAnalOption());
        functionInfo.addParameter("mummiAnalBean.pathDBOpt", bean.getPathDBOpt());
        functionInfo.addParameter("mummiAnalBean.libVersion", bean.getLibVersion());
        functionInfo.addParameter("mummiAnalBean.minMsetNum", String.valueOf(bean.getMinMsetNum()));
        functionInfo.addParameter("mummiAnalBean.filterOpt", bean.getFilterOpt());
        functionInfo.addParameter("mummiAnalBean.pvalCutoff", String.valueOf(bean.getPvalCutoff()));

        // Here, fp is assumed to be a context or application-wide accessible object
        wb.addFunctionInfo("performPeaks2Fun", functionInfo);
    }

    public void record_paBn_proceed(PathBean bean) {
        FunctionInfo functionInfo = new FunctionInfo(
                "paBn_proceed",
                "pathBean.paBn_proceed",
                "Determines the processing flow based on analysis options for pathway analysis."
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("pathBean.analOption", bean.getAnalOption());
        functionInfo.addParameter("pathBean.libOpt", bean.getLibOpt());
        functionInfo.addParameter("pathBean.libVersion", bean.getLibVersion());
        functionInfo.addParameter("pathBean.refLibOpt", bean.getRefLibOpt());
        functionInfo.addParameter("pathBean.showGrid", String.valueOf(bean.isShowGrid()));
        functionInfo.addParameter("pathBean.topoCode", bean.getTopoCode());
        functionInfo.addParameter("pathBean.qeaStatCode", bean.getQeaStatCode());
        functionInfo.addParameter("pathBean.oraStatCode", bean.getOraStatCode());

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("paBn_proceed", functionInfo);
    }

    public void record_doMnetworkAnalysis(String visMode) {
        FunctionInfo functionInfo = new FunctionInfo(
                "doMnetworkAnalysis",
                "mnetResBean.doMnetworkAnalysis",
                "Perform network mapping to build metabolic network"
        );
        functionInfo.addParameter("sessionBean1.visMode", visMode);
        wb.addFunctionInfo("doMnetworkAnalysis", functionInfo);
    }

    public void record_prepareNetworks() {
        FunctionInfo functionInfo = new FunctionInfo(
                "prepareNetworks",
                "mnetResBean.prepareNetworks",
                "Prepare network for viewing"
        );

        wb.addFunctionInfo("prepareNetworks", functionInfo);
    }

    public void record_computeDspcNet() {
        FunctionInfo functionInfo = new FunctionInfo(
                "computeDspcNet",
                "sessionBean1.computeDspcNet",
                "Compute DSPC Network"
        );

        wb.addFunctionInfo("computeDspcNet", functionInfo);
        wb.getCalledWorkflows().add("computeDspcNet");

    }

    public void record_performPvalCombination(MetaStatBean mb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performPvalCombination",
                "metaStatBean.performPvalCombination",
                "Performs statistical meta-analysis using p-value combination"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("metapMethod", mb.getMetapMethod());
        functionInfo.addParameter("metpSigLvl", String.valueOf(mb.getMetpSigLvl()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("performPvalCombination", functionInfo);
        wb.getCalledWorkflows().add("Combine P-values");
    }

    public void record_performVoteCounting(MetaStatBean mb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performVoteCounting",
                "metaStatBean.performVoteCounting",
                "Performs vote counting for statistical meta-analysis"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("vcSigLvl", String.valueOf(mb.getVcSigLvl()));
        functionInfo.addParameter("minVote", String.valueOf(mb.getMinVote()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("performVoteCounting", functionInfo);
        wb.getCalledWorkflows().add("Vote Counting");

    }

    public void record_performDirectMerging(MetaStatBean mb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performDirectMerging",
                "metaStatBean.performDirectMerging",
                "Performs direct merging for statistical meta-analysis"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("dmSigLvl", String.valueOf(mb.getDmSigLvl()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("performDirectMerging", functionInfo);
        wb.getCalledWorkflows().add("Direct Merging");

    }

    public void record_prepareUpsetView(MetaLoadBean sb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "prepareUpsetView",
                "metaLoadBean.prepareUpsetView",
                "Prepares the data for Upset diagram visualization"
        );

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("prepareUpsetView", functionInfo);
        wb.getCalledWorkflows().add("Upset Diagram");

    }

    public void record_performMetaPathAnalysis(MetaPathStatBean mb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performMetaPathAnalysis",
                "metaPathStatBean.performMetaPathAnalysis",
                "Performs pathway-level integration analysis for statistical meta pathway analysis"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("pathAlgOpt", mb.getPathAlgOpt());
        functionInfo.addParameter("pathAlgVersion", mb.getPathAlgVersion());
        functionInfo.addParameter("lib", mb.getLib());
        functionInfo.addParameter("libVersion", mb.getLibVersion());
        functionInfo.addParameter("minMsetNum", String.valueOf(mb.getMinMsetNum()));
        functionInfo.addParameter("permuNUm", String.valueOf(mb.getPermuNUm()));
        functionInfo.addParameter("combinelevel", mb.getCombinelevel());
        functionInfo.addParameter("pvalmethod", mb.getPvalmethod());
        functionInfo.addParameter("esmethod", mb.getEsmethod());
        functionInfo.addParameter("rankmetric", mb.getRankmetric());
        functionInfo.addParameter("pvalCutoff", String.valueOf(mb.getPvalCutoff()));
        functionInfo.addParameter("plotType", mb.getPlotType());
        functionInfo.addParameter("overlap", String.valueOf(mb.getOverlap()));
        functionInfo.addParameter("maxPaths", String.valueOf(mb.getMaxPaths()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("performMetaPathAnalysis", functionInfo);
        wb.getCalledWorkflows().add("Pathway-level integration");

    }

    public void record_performMetaPoolAnalysis(MetaPathStatBean mb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performMetaPoolAnalysis",
                "metaPathStatBean.performMetaPoolAnalysis",
                "Performs pooling peaks analysis for statistical meta pathway analysis"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("poolAlgOpt", mb.getPoolAlgOpt());
        functionInfo.addParameter("poolAlgVersion", mb.getPoolAlgVersion());
        functionInfo.addParameter("lib", mb.getLib());
        functionInfo.addParameter("libVersion", mb.getLibVersion());
        functionInfo.addParameter("minMsetNum", String.valueOf(mb.getMinMsetNum()));
        functionInfo.addParameter("permuNUm", String.valueOf(mb.getPermuNUm()));
        functionInfo.addParameter("pvalmethod", mb.getPvalmethod());
        functionInfo.addParameter("esmethod", mb.getEsmethod());
        functionInfo.addParameter("rankmetric", mb.getRankmetric());
        functionInfo.addParameter("pvalCutoff", String.valueOf(mb.getPvalCutoff()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("performMetaPoolAnalysis", functionInfo);
        wb.getCalledWorkflows().add("Pooling peaks");

    }

    public void record_prepareMetaPathUpsetView(MetaPathLoadBean sb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "prepareMetaPathUpsetView",
                "metaPathLoadBean.prepareMetaPathUpsetView",
                "Prepares the data for Upset diagram visualization of meta-pathways"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("selectedDataNms", sb.getSelectedDataNms().toString());

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("prepareMetaPathUpsetView", functionInfo);
        wb.getCalledWorkflows().add("metapaths Upset Diagram path");
    }

    public void record_goToProcessing(SpectraUploadBean sb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "goToProcessing",
                "spectraUploadBean.goToProcessing",
                "Checks if the necessary conditions are met and proceeds to data processing"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("uploadedFileNames", sb.getUploadedFileNames().toString());
        functionInfo.addParameter("containsMeta", String.valueOf(sb.isContainsMeta()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("goToProcessing", functionInfo);
    }

    public void record_prepareSpecProc(SpectraProcessBean spb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "prepareSpecProc",
                "spectraProcessBean.prepareSpecProc",
                "Prepares the data for spectral processing"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("ms2DataOpt", spb.getMs2DataOpt());

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("prepareSpecProc", functionInfo);
        wb.getCalledWorkflows().add("Spectra Check");

    }

    public void record_prepareDIASpec(SpectraProcessBean spb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "prepareDIASpec",
                "spectraProcessBean.prepareDIASpec",
                "Prepares the DIA spectral processing and checks SWATH design"
        );

        // Log relevant parameters and the current state
        functionInfo.addParameter("fromGoogleDrive", String.valueOf(spb.isFromGoogleDrive()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("prepareDIASpec", functionInfo);
        wb.getCalledWorkflows().add("Spectra Check");

    }

    public void record_spectraParams(SpectraParamBean spb) {
        FunctionInfo functionInfo = new FunctionInfo(
                "spectraParamer",
                "NA",
                "Set peak parameters"
        );

        // Log relevant parameters from SpectraParamBean
        functionInfo.addParameter("peakmeth", spb.getPeakmeth());
        functionInfo.addParameter("rtmeth", spb.getRtmeth());
        functionInfo.addParameter("polarity", spb.getPolarity());
        functionInfo.addParameter("ppm", String.valueOf(spb.getPpm()));
        functionInfo.addParameter("min_peakwidth", String.valueOf(spb.getMin_peakwidth()));
        functionInfo.addParameter("max_peakwidth", String.valueOf(spb.getMax_peakwidth()));
        functionInfo.addParameter("mzdiff", String.valueOf(spb.getMzdiff()));
        functionInfo.addParameter("snthresh", String.valueOf(spb.getSnthresh()));
        functionInfo.addParameter("noise", String.valueOf(spb.getNoise()));
        functionInfo.addParameter("prefilter", String.valueOf(spb.getPrefilter()));
        functionInfo.addParameter("value_of_prefilter", String.valueOf(spb.getValue_of_prefilter()));
        functionInfo.addParameter("bw", String.valueOf(spb.getBw()));
        functionInfo.addParameter("minFraction", String.valueOf(spb.getMinFraction()));
        functionInfo.addParameter("minSamples", String.valueOf(spb.getMinSamples()));
        functionInfo.addParameter("maxFeatures", String.valueOf(spb.getMaxFeatures()));
        functionInfo.addParameter("integrate", String.valueOf(spb.getIntegrate()));
        functionInfo.addParameter("extra", String.valueOf(spb.getExtra()));
        functionInfo.addParameter("span", String.valueOf(spb.getSpan()));
        functionInfo.addParameter("profStep", String.valueOf(spb.getProfStep()));
        functionInfo.addParameter("fwhm", String.valueOf(spb.getFwhm()));
        functionInfo.addParameter("sigma", String.valueOf(spb.getSigma()));
        functionInfo.addParameter("steps", String.valueOf(spb.getSteps()));
        functionInfo.addParameter("max", String.valueOf(spb.getMax()));
        functionInfo.addParameter("fwhmThresh", String.valueOf(spb.getFwhmThresh()));
        functionInfo.addParameter("mzmabsmiso", String.valueOf(spb.getMzmabsmiso()));
        functionInfo.addParameter("max_charge", String.valueOf(spb.getMax_charge()));
        functionInfo.addParameter("max_iso", String.valueOf(spb.getMax_iso()));
        functionInfo.addParameter("corr_eic_th", String.valueOf(spb.getCorr_eic_th()));
        functionInfo.addParameter("mz_abs_add", String.valueOf(spb.getMz_abs_add()));
        functionInfo.addParameter("adducts", spb.getAdducts());
        functionInfo.addParameter("rmConts", String.valueOf(spb.isRmConts()));
        functionInfo.addParameter("blksub", String.valueOf(spb.isBlksub()));

        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("spectraParams", functionInfo);
    }

    public void record_performDefaultUnivAnalysis_internal(RocAnalBean b) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performDefaultUnivAnalysis",
                "rocAnalBean.performDefaultUnivAnalysis",
                "Perform default univariate ROC analysis"
        );

        // Log relevant parameters and the current state
        // Here, fp is assumed to be a system-wide accessible object for handling function information
        wb.addFunctionInfo("performDefaultUnivAnalysis", functionInfo);
        wb.getCalledWorkflows().add("Univariate ROC");

    }

    public void record_performMissingImpute(ProcessBean processBean) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performMissingImpute",
                "processBean.performMissingImpute",
                "Performs missing value imputation and metadata handling"
        );

        // Log relevant parameters and the current state using getter functions
        functionInfo.addParameter("processBean.removeMissing", processBean.isRemoveMissing());
        functionInfo.addParameter("processBean.missingPercent", processBean.getMissingPercent());
        functionInfo.addParameter("processBean.missingImputeOpt", processBean.getMissingImputeOpt());
        functionInfo.addParameter("processBean.replaceVarOpt", processBean.getReplaceVarOpt());
        functionInfo.addParameter("processBean.imputeAlgOpt", processBean.getImputeAlgOpt());

        // Log the function call
        wb.addFunctionInfo("performMissingImpute", functionInfo);
    }

    public void recordRCommandFunctionInfo(RConnection RC, String rCmd, String functionName) {
        try {
            String rCommand = cleanRCmd(rCmd);
            //for local MetaboAnalystR package
            if (rCommand.contains("(NA")) {
                rCommand = rCommand.replace("(NA", "(mSet");
                if (!rCommand.contains("<-")) {
                    rCommand = "mSet<-" + rCommand;
                }
            }
            if (rCommand.contains("InitDataObjects")) {
                if (!rCommand.contains("<-")) {
                    rCommand = "mSet<-" + rCommand;
                }
            }
            FunctionInfo functionInfo = wb.getFunctionInfo(functionName);
            functionInfo.addRCommand(rCmd);
            //System.out.println("RecordRCommand(NA" + ", \"" + rCommand + "\")");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public FunctionInfo getFunctionInfoByFunction(List<FunctionInfo> list, String function) {
        for (FunctionInfo info : list) {
            if (function.equals(info.getFunction())) { // Check if function matches
                return info; // Return the matched object
            }
        }
        return null; // Return null if no match is found
    }

    public void record_performBatchCorrection(MetaLoadBean metaLoadBean) {
        FunctionInfo functionInfo = new FunctionInfo(
                "performBatchCorrection",
                "metaLoadBean.performBatchCorrection",
                "Performs ComBat batch correction on meta-analysis datasets"
        );

        functionInfo.addParameter("metaLoadBean.adjustBatch", metaLoadBean.isAdjustBatch());
        wb.addFunctionInfo("performBatchCorrection", functionInfo);
    }
}
