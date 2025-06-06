/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.controllers.mnet;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.models.NetBean;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.rwrappers.RNetworkUtils;
import pro.metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import org.rosuda.REngine.Rserve.RConnection;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.application.FacesMessage;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import pro.metaboanalyst.utils.JavaRecord;
import pro.metaboanalyst.workflows.WorkflowBean;

/**
 * @author jianguox
 */
@SessionScoped
@Named("mnetResBean")
@JsonIgnoreProperties(ignoreUnknown = true)
public class MnetResBean implements Serializable {

    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;
    /**
     * Network methods
     */
    private int nodeCount = 0;
    private int edgeCount = 0;
    private double confThresh = 0.5;
    @JsonIgnore
    private FacesMessage checkMsg;
    private int queryCount = 0;
    private int seedCount = 0;
    private boolean netOK = false;
    private int imgCount = 0;
    private List<NetBean> netsModel;
    private int jsonCount = 0;
    private double minCor = 0.01;
    private double negCoeff1 = -1;
    private double negCoeff2 = 0;
    private double posCoeff1 = 0;
    private double posCoeff2 = 1;
    private String corFilteringOpt = "pval";
    private double minDgr = 2.0;
    private double minBtw = 1.0;

    public int getNodeCount() {
        return nodeCount;
    }

    public int getEdgeCount() {
        return edgeCount;
    }

    public double getConfThresh() {
        return confThresh;
    }

    public void setConfThresh(double confThresh) {
        this.confThresh = confThresh;
    }

    private void updateCheckMsg() {
        if (nodeCount > 2000) {
            sb.addMessage("warn",
                    "The network size is over <b style='color: orange'>2000</b> nodes! We recommend switch from current network to "
                    + "<b>Zero-order network</b> or <b>Minimum Network</b> function to reduce the size for better performance and experience.");
        } else if (nodeCount > 1200) {
            sb.addMessage("info", "Click the <b>Proceed</b> button to perform network analysis and visualization. "
                    + "You may also consider <b>Zero-order network</b> or <b>Minimum Network</b> to reduce the network size "
                    + "for better performance and experience.");
        } else {
            sb.addMessage("info",
                    "Please click the <b>Proceed</b> button to perform network analysis and visualization.");
        }
    }

    public int getQueryCount() {
        return queryCount;
    }

    public int getSeedCount() {
        return seedCount;
    }

    public boolean isNetOK() {
        return netOK;
    }

    public void setNetOK(boolean netOK) {
        this.netOK = netOK;
    }

    public String preparePhenoNetwork() {
        String dbType = "pheno";
        String visMode = sb.getVisMode();
        netOK = false;

        int[] res = RNetworkUtils.searchNetDB(sb, dbType, visMode, confThresh);
        nodeCount = res[0];
        edgeCount = res[1];

        if (nodeCount == 0) {
            sb.addMessage("error", "No matches found for the given seed genes/proteins.");
            return null;
        }

        int[] counts = RNetworkUtils.createIGraph(sb.getRConnection());
        if (counts[0] > 0) {
            queryCount = counts[0];
            seedCount = counts[1];
            nodeCount = counts[2];
            edgeCount = counts[3];
            populateNetBeans();
            populateCorVal();
            updateCheckMsg();

            //ppiOrder = 1;
            //default load the the first network
            String netNm = "subnetwork1";
            RNetworkUtils.prepareNetwork(sb.getRConnection(), netNm, "networkanalyst_" + visMode + ".json");
            //System.out.println(visMode + "========");
            netOK = true;
            return "MnetStats";

        } else {
            sb.addMessage("error", "Failed  to process your request: no interaction partners identified.");
            return null;
        }
    }

    //type is globaltest or DE-hypergeo test
    public String prepareKEGGNetwork() {
        RNetworkUtils.prepareKeggQueryJson(sb);
        String nm = "network_enrichment_pathway_0";
        RNetworkUtils.doEnrichmentTest_KO01100(sb, "pathway", nm);
        return "MnetView";
    }

    public String prepareDspcNetwork() {
        int[] res = RNetworkUtils.performDSPC(sb);
        if (res.length == 1 & res[0] == 0) {
            sb.addMessage("error", "DSPC failed - possible reason: some variables are highly collinear or sample size is too small.");
            return null;
        }
        return (setupDspcNetwork(res));
    }

    public String setupDspcNetwork(int[] res) {
        netOK = false;
        nodeCount = res[0];
        edgeCount = res[1];

        if (nodeCount == 0) {
            sb.addMessage("error", "No matches found for the given seed nodes.");
            return null;
        }

        int[] counts = RNetworkUtils.createIGraph(sb.getRConnection());
        if (counts[0] > 0) {
            queryCount = counts[0];
            seedCount = counts[1];
            nodeCount = counts[2];
            edgeCount = counts[3];
            populateNetBeans();
            populateCorVal();
            updateCheckMsg();
            //ppiOrder = 1;

            //default load the the first network
            String netNm = "subnetwork1";
            RNetworkUtils.prepareNetwork(sb.getRConnection(), netNm, "networkanalyst_dspc.json");
            netOK = true;
            return "MnetStats";

        } else {
            sb.addMessage("error", "Failed  to process your request: no interaction partners identified.");
            return null;
        }
    }

    public String doMnetworkAnalysis(String visMode) {
        sb.setVisMode(visMode);
        if (wb.isEditMode()) {
            sb.addMessage("info", "Parameters have been updated!");

            jrd.record_doMnetworkAnalysis(visMode);
            return null;
        }

        String networkpage;
        jrd.record_doMnetworkAnalysis(visMode);

        //WorkflowBean wb = CDI.current().select(WorkflowBean.class).get();
        switch (visMode) {
            case "static" -> {
                networkpage = prepareKEGGNetwork();
                wb.getCalledWorkflows().add("Network Builder_static");
            }
            case "dspc" -> {
                networkpage = prepareDspcNetwork();
                wb.getCalledWorkflows().add("Network Builder_dspc");
            }
            default -> {
                networkpage = preparePhenoNetwork();
                wb.getCalledWorkflows().add("doMnetworkAnalysis");
            }
        }
        return networkpage;
    }

    public String exportNetwork(String format) {
        String fileName = "network_" + imgCount;
        if (format.equalsIgnoreCase("graphml")) {
            fileName = fileName + ".graphml";
        } else if (format.equalsIgnoreCase("ndtb")) {//node property table, already there
            fileName = "node_table.csv";
            return fileName;
        } else if (format.equalsIgnoreCase("comtb")) {
            fileName = "module_table.csv";
            return fileName;
        } else if (format.equalsIgnoreCase("funtb")) {
            fileName = "metaboanalyst_enrichment.csv";
            return fileName;
        } else {
            System.out.println("===========Not supported format: " + format);
            return "NA";
        }
        RNetworkUtils.exportNetwork(sb.getRConnection(), fileName);
        imgCount++;
        return fileName;
    }

    public String extractModule(String nodeIDs) {
        String res = RNetworkUtils.extractModule(sb.getRConnection(), nodeIDs);
        populateNetBeans(); //remember to update the net stats
        populateCorVal();
        return (res);
    }

    public List<NetBean> getNetsDataModel() {
        return netsModel;
    }

    private void populateNetBeans() {
        netsModel = new ArrayList();
        RConnection RC = sb.getRConnection();

        String[] nms = RNetworkUtils.getNetsNames(RC);
        int[] nds = RNetworkUtils.getNetsNodeNum(RC);
        int[] egs = RNetworkUtils.getNetsEdgeNum(RC);
        int[] qs = RNetworkUtils.getNetsQueryNum(RC);
        for (int i = 0; i < nms.length; i++) {
            netsModel.add(new NetBean(nms[i], nds[i], egs[i], qs[i]));
        }
    }

    private void populateCorVal() {
        String visMode = sb.getVisMode();
        if (visMode.equals("dspc")) {
            RConnection RC = sb.getRConnection();
            double p_val = RNetworkUtils.getMaxRawPVal(RC);
            double neg_coeff1 = RNetworkUtils.getMinNegCoeff(RC);
            double neg_coeff2 = RNetworkUtils.getMaxNegCoeff(RC);
            double pos_coeff1 = RNetworkUtils.getMinPosCoeff(RC);
            double pos_coeff2 = RNetworkUtils.getMaxPosCoeff(RC);
            setMinCor(p_val);
            setNegCoeff1(neg_coeff1);
            setNegCoeff2(neg_coeff2);
            setPosCoeff1(pos_coeff1);
            setPosCoeff2(pos_coeff2);
        }
    }

    public String updateNetworkLayout(String algo) {
        jsonCount++;
        String jsonName = "metaboanalyst_" + jsonCount + ".json";
        String res = RNetworkUtils.updateNetworkLayout(sb.getRConnection(), algo, jsonName);
        return res;
    }

    public String performNodesFilter(String nodeIDs) {
        jsonCount++;
        String jsonName = "metaboanalyst_node_" + jsonCount + ".json";
        String res = RNetworkUtils.excludeNodes(sb.getRConnection(), nodeIDs, jsonName);
        populateNetBeans();
        populateCorVal();
        return res;
    }

    //ajax call to switch network
    public String loadNetwork(String netName) {
        jsonCount++;
        String jsonName = "metaboanalyst_" + jsonCount + ".json";
        int res = RNetworkUtils.prepareNetwork(sb.getRConnection(), netName, jsonName);
        if (res == 1) {
            return jsonName;
        } else {
            return "NA";
        }
    }

    public DefaultStreamedContent getSifFile(String name) {
        RNetworkUtils.prepareSubnetDownload(sb.getRConnection(), name);
        return DataUtils.getDownloadFile(sb.getCurrentUser().getHomeDir() + "/" + name + "_sif.zip");
    }

    public int doIntegPathwayAnalysis(String qlist, String nm) {

        String visMode = sb.getVisMode();
        String pathDBOpt = switch (visMode) {
            case "gene_metabolites" ->
                "integ";
            case "metabo_metabolites", "metabo_phenotypes", "dspc" ->
                "metab";
            default ->
                "genetic";
        };
        //use default hypergeometric tests
        String enrichOpt = "hyper";
        String topoOpt = "dc";
        return (RNetworkUtils.updateIntegPathwayAnalysis(sb.getRConnection(), qlist, nm, topoOpt, enrichOpt, pathDBOpt, visMode));
    }

    public void resetNetwork() {
        int[] counts = RNetworkUtils.createIGraph(sb.getRConnection());
        updateNetworkInfo(counts);
    }

    private void updateNetworkInfo(int[] counts) {
        //System.out.println(Arrays.toString(counts));
        if (counts[0] > 0) {
            queryCount = counts[0];
            seedCount = counts[1];
            nodeCount = counts[2];
            edgeCount = counts[3];
            populateNetBeans();
            populateCorVal();
            updateCheckMsg();
        } else {
            sb.addMessage("error", "Failed to process your request. " + RDataUtils.getErrMsg(sb.getRConnection()));
        }
    }

    public void shrinkNetwork() {
        int[] counts = RNetworkUtils.buildMinConnectedNet(sb.getRConnection());
        populateNetBeans();
        populateCorVal();
        updateNetworkInfo(counts);
    }

    public double getMinCor() {
        return minCor;
    }

    public void setMinCor(double minCor) {
        this.minCor = minCor;
    }

    public double getNegCoeff1() {
        return negCoeff1;
    }

    public void setNegCoeff1(double negCoeff1) {
        this.negCoeff1 = negCoeff1;
    }

    public double getNegCoeff2() {
        return negCoeff2;
    }

    public void setNegCoeff2(double negCoeff2) {
        this.negCoeff2 = negCoeff2;
    }

    public double getPosCoeff1() {
        return posCoeff1;
    }

    public void setPosCoeff1(double posCoeff1) {
        this.posCoeff1 = posCoeff1;
    }

    public double getPosCoeff2() {
        return posCoeff2;
    }

    public void setPosCoeff2(double posCoeff2) {
        this.posCoeff2 = posCoeff2;
    }

    public String getCorFilteringOpt() {
        return corFilteringOpt;
    }

    public void setCorFilteringOpt(String corFilteringOpt) {
        this.corFilteringOpt = corFilteringOpt;
    }

    public void filterNetByCor() {
        if ("pval".equals(corFilteringOpt)) {
            int[] counts = RNetworkUtils.filterNetByCor(sb.getRConnection(), getMinCor(), 0, -1, 0, 0, 1);
            updateNetworkInfo(counts);
        } else if ("qval".equals(corFilteringOpt)) {
            int[] counts = RNetworkUtils.filterNetByCor(sb.getRConnection(), 0, getMinCor(), -1, 0, 0, 1);
            updateNetworkInfo(counts);
        }
        populateNetBeans();
    }

    public void filterNetByCorBoth() {
        if ("pval".equals(corFilteringOpt)) {
            int[] counts = RNetworkUtils.filterNetByCor(sb.getRConnection(), getMinCor(), 0, getNegCoeff1(), getNegCoeff2(), getPosCoeff1(), getPosCoeff2());
            updateNetworkInfo(counts);
        } else if ("qval".equals(corFilteringOpt)) {
            int[] counts = RNetworkUtils.filterNetByCor(sb.getRConnection(), 0, getMinCor(), getNegCoeff1(), getNegCoeff2(), getPosCoeff1(), getPosCoeff2());
            updateNetworkInfo(counts);
        }
        populateNetBeans();
    }

    public void filterNetByCoeff() {
        int[] counts = RNetworkUtils.filterNetByCor(sb.getRConnection(), 0, 0, getNegCoeff1(), getNegCoeff2(), getPosCoeff1(), getPosCoeff2());
        updateNetworkInfo(counts);
        populateNetBeans();
    }

    public double getMinDgr() {
        return minDgr;
    }

    public void setMinDgr(double minDgr) {
        this.minDgr = minDgr;
    }

    public double getMinBtw() {
        return minBtw;
    }

    public void setMinBtw(double minBtw) {
        this.minBtw = minBtw;
    }

    public void filterPpiNetByDgr() {
        int[] counts = RNetworkUtils.filterBipartiNet(sb.getRConnection(), "all", getMinDgr(), 0);
        populateNetBeans();
        populateCorVal();
        updateNetworkInfo(counts);
    }

    public void filterPpiNetByBtw() {
        int[] counts = RNetworkUtils.filterBipartiNet(sb.getRConnection(), "all", 0, getMinBtw());
        populateNetBeans();
        populateCorVal();
        updateNetworkInfo(counts);
    }

    public String proceedToViewer() {
        if (netOK) {
            return "Network viewer";
        } else {
            sb.addMessage("error", "Make sure to select at least one network to proceed!");
            return null;
        }
    }

    public void prepareNetworks() {
        netOK = false;
        int res = RNetworkUtils.prepareNetwork(sb.getRConnection(), "subnetwork1", "networkanalyst_" + sb.getVisMode() + ".json");
        if (res == 1) {
            netOK = true;
        }
    }
}
