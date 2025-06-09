/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import org.primefaces.model.DefaultTreeNode;
import org.primefaces.model.TreeNode;

/**
 *
 * @author xia
 */
public class NaviUtils {

    private static final List<String> parentNodes = Arrays.asList(new String[]{
        "Processing", "Statistics", "Enrichment", "Pathway", "Multi-factors", "ID Conversion",
        "Batch Effect", "ROC Analysis", "Integrative Analysis", "Power Analysis", "Dose Response", 
        "Multivariate", "Univariate", "Tester", "View result", "Merging Replicates"
    });


    public static TreeNode createNaviTree(String type) {

        TreeNode naviTree = new DefaultTreeNode("Root", null);
        if (type.equalsIgnoreCase("utils")) {
            addUtilNodes(naviTree);
        } else {
            TreeNode upNode = new DefaultTreeNode("Upload", naviTree);
            //upNode.setSelectable(false);

            if (type.startsWith("mummichog")) {
                addMummichogNodes(naviTree, type);
            } else if (type.equalsIgnoreCase("mgwas")) {
                addMgwasNodes(naviTree);
            } else if (type.equalsIgnoreCase("spec")) {
                addSpecNodes(naviTree);
            } else if (type.equalsIgnoreCase("specManager")) {
                addAdminNodes(naviTree);
            } else if (type.equalsIgnoreCase("network")) {
                addMNetworkNodes(naviTree);
            } else if (type.equalsIgnoreCase("metadata")) {
                addMetaAnalNodes(naviTree);
            } else if (type.equalsIgnoreCase("metaPath")) {
                addMetaPathNodes(naviTree);
            //} else if (type.equalsIgnoreCase("pathora") || type.startsWith("pathway-ora")) {
            //    addPathNodes(naviTree);
            } else if (type.equalsIgnoreCase("pathinteg")) {
                addPathIntegNodes(naviTree);
            } else if (type.equalsIgnoreCase("tandemms")) {
                addMS2Nodes(naviTree);
            } else {
                addProcNodes(naviTree, type);
                if (type.startsWith("stat")) {
                    addStatNodes(naviTree);
                } else if (type.startsWith("dspc")) {
                    addDspcNodes(naviTree);
                } else if (type.startsWith("enrich")) {
                    addEnrichNodes(naviTree);
                } else if (type.startsWith("path")) {
                    addPathNodes(naviTree);
                } else if (type.equalsIgnoreCase("time") || type.equalsIgnoreCase("mf")) {
                    addTimeNodes(naviTree);
                } else if (type.equalsIgnoreCase("power")) {
                    addPowerNodes(naviTree);
                } else if (type.equalsIgnoreCase("roc")) {
                    addRocNodes(naviTree);
                } else if (type.equalsIgnoreCase("dose")) {
                    addDoseNodes(naviTree);
                } else {
                    System.out.println("You need to define the navigation tree for this analysis type: " + type);
                }
            }

            TreeNode dn_node = new DefaultTreeNode("Download", naviTree);
        }
        //TreeNode exitNd = new DefaultTreeNode("Exit", naviTree);
        return naviTree;
    }

    private static void addProcNodes(TreeNode parent, String mode) {
        TreeNode processNode = new DefaultTreeNode("Processing", parent);
        processNode.setSelectable(false);
        boolean listInput = false;
        switch (mode) {
            case "pathora", "pathway", "enrich" -> //pathway-ora
            {
                //enrich-ora
                listInput = true;
                TreeNode nodep3 = new DefaultTreeNode("Name check", processNode);
            }
            case "enrich-ssp" -> {
                listInput = true;
                TreeNode nodep3 = new DefaultTreeNode("Name check", processNode);
                TreeNode nodep4 = new DefaultTreeNode("Conc. check", processNode);
            }
            case "dspc", "enrich-qea", "pathway-qea" -> {
                TreeNode nodep2 = new DefaultTreeNode("Data check", processNode);
                TreeNode nodep3 = new DefaultTreeNode("Name check", processNode);
            }
            case "time" -> {
                listInput = true;
                TreeNode nodep211 = new DefaultTreeNode("Data check", processNode);
                TreeNode nodep5 = new DefaultTreeNode("Missing value", processNode);
                TreeNode nodep212 = new DefaultTreeNode("Metadata check", processNode);
                TreeNode nodep6 = new DefaultTreeNode("Data filter", processNode);
                TreeNode nodep7 = new DefaultTreeNode("Data editor", processNode);
                TreeNode normNode = new DefaultTreeNode("Normalization", parent);
            }
            case "stat-peak" -> {
                TreeNode nodep3 = new DefaultTreeNode("Pre-process", processNode);
                TreeNode nodep2 = new DefaultTreeNode("Data check", processNode);
            }
            default -> {
                TreeNode nodep21 = new DefaultTreeNode("Data check", processNode);
            }
        }
        //pathway-ora
        if (!listInput) {
            TreeNode nodep5 = new DefaultTreeNode("Missing value", processNode);
            TreeNode nodep6 = new DefaultTreeNode("Data filter", processNode);
            TreeNode nodep7 = new DefaultTreeNode("Data editor", processNode);
            TreeNode normNode = new DefaultTreeNode("Normalization", parent);
        }
    }

    private static void addStatNodes(TreeNode parent) {
        TreeNode analNode = new DefaultTreeNode("Statistics", parent);
        TreeNode nodea1 = new DefaultTreeNode("Fold change", analNode);
        TreeNode nodea2 = new DefaultTreeNode("T-test", analNode);
        TreeNode nodea3 = new DefaultTreeNode("Volcano plot", analNode);
        TreeNode nodea4 = new DefaultTreeNode("ANOVA", analNode);
        TreeNode nodea5 = new DefaultTreeNode("Correlations", analNode);
        TreeNode nodea51 = new DefaultTreeNode("DSPC network", analNode);
        TreeNode nodea6 = new DefaultTreeNode("PatternHunter", analNode);
        TreeNode nodea9 = new DefaultTreeNode("SAM", analNode);
        TreeNode nodea10 = new DefaultTreeNode("EBAM", analNode);
        TreeNode nodea7 = new DefaultTreeNode("PCA", analNode);
        TreeNode nodea8 = new DefaultTreeNode("PLSDA", analNode);
        TreeNode nodea801 = new DefaultTreeNode("sPLSDA", analNode);
        TreeNode nodea81 = new DefaultTreeNode("OrthoPLSDA", analNode);
        TreeNode nodea11 = new DefaultTreeNode("Dendrogram", analNode);
        TreeNode nodea12 = new DefaultTreeNode("Heatmap", analNode);
        TreeNode nodea14 = new DefaultTreeNode("K-means", analNode);
        TreeNode nodea13 = new DefaultTreeNode("SOM", analNode);
        TreeNode nodea15 = new DefaultTreeNode("RandomForest", analNode);
        TreeNode nodea16 = new DefaultTreeNode("SVM", analNode);
    }

    private static void addEnrichNodes(TreeNode parent) {
        TreeNode enrichNode = new DefaultTreeNode("Enrichment", parent);
        enrichNode.setSelectable(false);
        TreeNode nodee1 = new DefaultTreeNode("Set parameter", enrichNode);
        TreeNode nodee2 = new DefaultTreeNode("View result", enrichNode);
    }

    private static void addPathNodes(TreeNode parent) {
        TreeNode pathNode = new DefaultTreeNode("Pathway", parent);
        pathNode.setSelectable(false);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", pathNode);
        TreeNode node_p2 = new DefaultTreeNode("View result", pathNode);
    }

    private static void addDspcNodes(TreeNode parent) {
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p2 = new DefaultTreeNode("Network stats", parent);
        TreeNode node_p3 = new DefaultTreeNode("Network viewer", parent);
    }

    private static void addTimeNodes(TreeNode parent) {
        TreeNode tsNode = new DefaultTreeNode("Multi-factors", parent);
        //tsNode.setSelectable(false);
        TreeNode tsNode9 = new DefaultTreeNode("Metadata", tsNode);
        TreeNode tsNode1 = new DefaultTreeNode("iPCA", tsNode);
        TreeNode tsNode2 = new DefaultTreeNode("Heatmap2", tsNode);
        TreeNode tsNode6 = new DefaultTreeNode("Linear Model", tsNode);
        TreeNode tsNode8 = new DefaultTreeNode("Correlations", tsNode);
        TreeNode tsNode3 = new DefaultTreeNode("ANOVA2", tsNode);
        TreeNode tsNode4 = new DefaultTreeNode("ASCA", tsNode);
        TreeNode tsNode5 = new DefaultTreeNode("MEBA", tsNode);
        TreeNode tsNode7 = new DefaultTreeNode("RandomForest", tsNode);
    }

    private static void addPowerNodes(TreeNode parent) {
        TreeNode pathNode = new DefaultTreeNode("Power Analysis", parent);
        pathNode.setSelectable(false);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", pathNode);
        TreeNode node_p2 = new DefaultTreeNode("View result", pathNode);
    }

    private static void addDoseNodes(TreeNode parent) {
        TreeNode doseNode = new DefaultTreeNode("Dose Response", parent);
        TreeNode node_p0 = new DefaultTreeNode("Sig. analysis", doseNode);
        TreeNode node_p2 = new DefaultTreeNode("Set parameters", doseNode);
        TreeNode node_p3 = new DefaultTreeNode("View result", doseNode);
    }

    private static void addMS2Nodes(TreeNode parent) {
        TreeNode node_p0 = new DefaultTreeNode("Spectra Check", parent);
        TreeNode node_p1 = new DefaultTreeNode("MS2 Result", parent);
    }

    private static void addMgwasNodes(TreeNode parent) {
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p2 = new DefaultTreeNode("MR method", parent);
        TreeNode node_p3 = new DefaultTreeNode("View result", parent);
    }

    private static void addPathIntegNodes(TreeNode parent) {
        TreeNode pathNode = new DefaultTreeNode("Integrative Analysis", parent);
        pathNode.setSelectable(false);
        TreeNode node_p1s = new DefaultTreeNode("Path library", pathNode);
        TreeNode node_p0 = new DefaultTreeNode("ID map", pathNode);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", pathNode);
        // TreeNode node_p2 = new DefaultTreeNode("Overview", pathNode);
        TreeNode node_p3 = new DefaultTreeNode("View result", pathNode);
    }

    private static void addMNetworkNodes(TreeNode parent) {
        TreeNode nodep4 = new DefaultTreeNode("Name check", parent);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p2 = new DefaultTreeNode("Network stats", parent);
        TreeNode node_p3 = new DefaultTreeNode("Network viewer", parent);
    }

    private static void addSpecNodes(TreeNode parent) {
        TreeNode node_p0 = new DefaultTreeNode("Spectra check", parent);
        TreeNode node_p1 = new DefaultTreeNode("Spectra processing", parent);
        TreeNode node_p4 = new DefaultTreeNode("MS2 spectra", parent);
        TreeNode node_p2 = new DefaultTreeNode("Job status", parent);
        TreeNode node_p3 = new DefaultTreeNode("Spectra result", parent);
    }

    private static void addAdminNodes(TreeNode parent) {
        TreeNode node_p0 = new DefaultTreeNode("Job manager", parent);
        TreeNode node_p1 = new DefaultTreeNode("Spectra check", parent);
        TreeNode node_p2 = new DefaultTreeNode("Spectra processing", parent);
        TreeNode node_p3 = new DefaultTreeNode("Job status", parent);
        TreeNode node_p4 = new DefaultTreeNode("Spectra result", parent);
    }

    private static void addMetaAnalNodes(TreeNode parent) {
        TreeNode nodep4 = new DefaultTreeNode("Meta analysis", parent);
        TreeNode node_p1 = new DefaultTreeNode("Result table", parent);
        TreeNode node_p2 = new DefaultTreeNode("Upset diagram", parent);
    }

    private static void addMetaPathNodes(TreeNode parent) {
        TreeNode node_p2 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p3 = new DefaultTreeNode("View result", parent);
        node_p3.setSelectable(false);
        node_p3.setExpanded(true);
        TreeNode node_p42 = new DefaultTreeNode("Meta paths", node_p3);
        TreeNode node_p41 = new DefaultTreeNode("Pooling peaks", node_p3);
        TreeNode node_p5 = new DefaultTreeNode("Upset diagram", parent);
        TreeNode node_p6 = new DefaultTreeNode("Network viewer", parent);
    }

    private static void addMummichogNodes(TreeNode parent, String type) {
        TreeNode procNode = new DefaultTreeNode("Processing", parent);
        TreeNode nodep = new DefaultTreeNode("Data check", procNode);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p2 = new DefaultTreeNode("View result", parent);
        TreeNode node_p3 = new DefaultTreeNode("Mummi. result", node_p2);
        TreeNode node_p4 = new DefaultTreeNode("GSEA result", node_p2);
        TreeNode node_p5 = new DefaultTreeNode("Integ. result", node_p2);
        TreeNode node_p6 = new DefaultTreeNode("Metabolic network", node_p2);
        if (type.endsWith("table")) {
            TreeNode node_p11 = new DefaultTreeNode("Missing value", procNode);
            TreeNode node_p12 = new DefaultTreeNode("Data filter", procNode);
            TreeNode node_p13 = new DefaultTreeNode("Data editor", procNode);
            TreeNode node_p14 = new DefaultTreeNode("Normalization", parent);
            TreeNode node_p15 = new DefaultTreeNode("Heatmap view", node_p2);
        }
    }

    private static void addRocNodes(TreeNode parent) {
        TreeNode rocNode = new DefaultTreeNode("ROC Analysis", parent);
        //rocNode.setSelectable(false);
        //TreeNode node_p1 = new DefaultTreeNode("Univariate", rocNode);
        //TreeNode node_p11 = new DefaultTreeNode("ROC detail", node_p1);
        TreeNode node_p1 = new DefaultTreeNode("Univariate", rocNode);
        node_p1.setSelectable(false);
        TreeNode node_p10 = new DefaultTreeNode("Classical ROC", node_p1);
        TreeNode node_p11 = new DefaultTreeNode("ROC detail", node_p1);
        
        TreeNode node_p2 = new DefaultTreeNode("Multivariate", rocNode);
        node_p2.setSelectable(false);
        TreeNode node_p3 = new DefaultTreeNode("Set parameter", node_p2);
        TreeNode node_p4 = new DefaultTreeNode("Explorer", node_p2);

        TreeNode node_p5 = new DefaultTreeNode("Tester", rocNode);
        node_p5.setSelectable(false);
        TreeNode node_p6 = new DefaultTreeNode("Builder", node_p5);
        TreeNode node_p7 = new DefaultTreeNode("Evaluator", node_p5);
    }

    private static void addUtilNodes(TreeNode parent) {
        //TreeNode uNode = new DefaultTreeNode("Utilities", parent);
        //uNode.setSelectable(false);

        TreeNode convertNode = new DefaultTreeNode("ID Conversion", parent);
        convertNode.setSelectable(false);
        TreeNode idInputNode = new DefaultTreeNode("ID Upload", convertNode);
        TreeNode resNode = new DefaultTreeNode("Map Result", convertNode);

        TreeNode batchNode = new DefaultTreeNode("Batch Effect", parent);
        batchNode.setSelectable(false);
        TreeNode inputNode = new DefaultTreeNode("Batch Upload", batchNode);
        TreeNode viewNode = new DefaultTreeNode("Batch View", batchNode);

        TreeNode dpInputNode = new DefaultTreeNode("Duplicates Upload", parent);

        TreeNode powerNode = new DefaultTreeNode("Lipidomics Upload", parent);
    }

    //find node based on the ID from tree, note, only search for three layers
    public static TreeNode getSelectedNode(TreeNode naviTree, String pageID) {
        TreeNode myNode = null;
        Iterator<TreeNode> i = naviTree.getChildren().iterator();
        while (i.hasNext()) {
            TreeNode nd = i.next();
            if (nd.getData().toString().equals(pageID)) {
                myNode = nd;
            }
            if (!nd.isLeaf()) {
                Iterator<TreeNode> i2 = nd.getChildren().iterator();
                while (i2.hasNext()) {
                    TreeNode nd2 = i2.next();
                    if (nd2.getData().toString().equals(pageID)) {
                        myNode = nd2;
                    }
                    if (!nd2.isLeaf()) {
                        Iterator<TreeNode> i3 = nd2.getChildren().iterator();
                        while (i3.hasNext()) {
                            TreeNode nd3 = i3.next();
                            if (nd3.getData().toString().equals(pageID)) {
                                myNode = nd3;
                            }
                        }
                    }
                }
            }
        }
        if (myNode != null) {
            selectNode(myNode);
        }
        return null;
    }


    public static void selectNode(TreeNode myNode) {
        myNode.setSelected(true);
        
        String nm = myNode.getData().toString();
        if (parentNodes.contains(nm)) {
            myNode.setExpanded(true);
        }
        
        String parentNm = myNode.getParent().getData().toString();
        if (parentNodes.contains(parentNm)) {
            myNode.getParent().setExpanded(true);
        }
    }
}
