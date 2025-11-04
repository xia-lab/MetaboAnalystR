/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.primefaces.PrimeFaces;
import org.primefaces.event.diagram.ConnectEvent;
import org.primefaces.event.diagram.ConnectionChangeEvent;
import org.primefaces.event.diagram.DisconnectEvent;
import org.primefaces.model.diagram.Connection;
import org.primefaces.model.diagram.DefaultDiagramModel;
import org.primefaces.model.diagram.DiagramModel;
import org.primefaces.model.diagram.Element;
import org.primefaces.model.diagram.connector.StraightConnector;
import org.primefaces.model.diagram.endpoint.EndPoint;
import org.primefaces.model.diagram.endpoint.EndPointAnchor;
import org.primefaces.model.diagram.overlay.ArrowOverlay;
import org.rosuda.REngine.Rserve.RConnection;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.DownloadBean;
import pro.metaboanalyst.controllers.general.NormBean;
import pro.metaboanalyst.controllers.general.ProcessBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.mnet.MnetLoadBean;
import pro.metaboanalyst.datacenter.DatasetController;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import pro.metaboanalyst.lts.FireUserBean;
import static pro.metaboanalyst.lts.FireBaseController.saveJsonStringToFile;
import pro.metaboanalyst.lts.FireProjectBean;
import pro.metaboanalyst.lts.FunctionInfo;
import pro.metaboanalyst.lts.HistoryBean;
import pro.metaboanalyst.lts.JobScheduler;
import pro.metaboanalyst.lts.MailService;
import pro.metaboanalyst.project.ProjectModel;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.models.JobInfo;
import pro.metaboanalyst.models.StickyDTO;

/**
 *
 * @author zgy
 */
@SessionScoped
@Named("diagramView")
@JsonIgnoreProperties(ignoreUnknown = true)
public class DiagramView implements Serializable {

    @JsonIgnore
    @Inject
    private DatasetController dc;

    @JsonIgnore
    @Inject
    private ProcessBean pcb;

    @JsonIgnore
    @Inject
    private NormBean nb;

    @JsonIgnore
    @Inject
    private DatabaseClient db;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private WorkflowBean wb;

    @JsonIgnore
    @Inject
    private WorkflowView wfv;

    @JsonIgnore
    @Inject
    private JobScheduler js;

    @JsonIgnore
    @Inject
    private FireBase fbb;

    @JsonIgnore
    @Inject
    private FireBaseController fcb;

    @JsonIgnore
    @Inject
    private FireProjectBean fpb;

    @JsonIgnore
    @Inject
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private HistoryBean hb;

    @JsonIgnore
    @Inject
    private JobExecution jeb;

    @JsonIgnore
    @Inject
    private MnetLoadBean mnb;

    @JsonIgnore
    @Inject
    private DownloadBean dldb;

    @JsonIgnore
    @Inject
    private MailService ms;

    @JsonIgnore
    @Inject
    private JavaRecord jrd;

    @JsonIgnore
    @Inject
    private JobTimerService jsv;

    @JsonIgnore
    @Inject
    pro.metaboanalyst.controllers.mummichog.PeakUploadBean pu;

    @JsonIgnore
    private DefaultDiagramModel model;
    private boolean workflowFinished = false;
    private boolean suspendEvent;
    private String selectedFirstLevelNode; // Track the selected first-level node
    @JsonIgnore
    private Map<String, List<String>> dependencyMap; // Define dependencies for second-level nodes
    @JsonIgnore
    private Map<String, List<String>> procDependencyMap; // Define dependencies for second-level nodes
    private Map<String, String> errorMessages = new HashMap<>(); // Define dependencies for second-level nodes
    private String input = "NA";
    private Map<String, Boolean> selectionMap = new HashMap<>(); // Map to track selection state
    private Map<String, Boolean> successExecutionMap = new HashMap<>(); // Map to track selection state
    private Map<String, Boolean> executionMap = new HashMap<>(); // Map to track selection state
    private String status = "NA";
    private String rawJobId = "NA";

    public void setRawJobId(String rawJobId) {
        this.rawJobId = rawJobId;
    }

    public String getRawJobId() {
        return rawJobId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public void setNormalizationResults(List<String> normalizationResults) {
        this.normalizationResults = normalizationResults;
    }

    public void setSelectionMap(Map<String, Boolean> selectionMap) {
        this.selectionMap = selectionMap;
    }

    public Map<String, Boolean> getSelectionMap() {
        return selectionMap;
    }

    private boolean isRawWorkflowRequested() {
        if (selectionMap.getOrDefault("LC-MS Spectra", false)) {
            return true;
        }
        String dataType = sb.getDataType();
        return dataType != null && dataType.equalsIgnoreCase("spec");
    }

    private void ensureRawModuleSelected() {
        if (!isRawWorkflowRequested()) {
            return;
        }
        selectionMap.put("Spectra Parameters Settings", true);
        selectionMap.put("Spectra Processing", true);

    }

    public boolean isWorkflowFinished() {
        return workflowFinished;
    }

    public void setWorkflowFinished(boolean workflowFinished) {
        this.workflowFinished = workflowFinished;
    }

    public Map<String, Boolean> getExecutionMap() {
        return executionMap;
    }

    @JsonIgnore
    public Map<String, List<String>> getDependencyMap() {
        return dependencyMap;
    }

    public void setDependencyMap(Map<String, List<String>> dependencyMap) {
        this.dependencyMap = dependencyMap;
    }

    @JsonIgnore
    public Map<String, List<String>> getProcDependencyMap() {
        return procDependencyMap;
    }

    public void setProcDependencyMap(Map<String, List<String>> procDependencyMap) {
        this.procDependencyMap = procDependencyMap;
    }

    public Map<String, Boolean> getSuccessExecutionMap() {
        return successExecutionMap;
    }

    public void setSuccessExecutionMap(Map<String, Boolean> successExecutionMap) {
        this.successExecutionMap = successExecutionMap;
    }

    public List<String> getFirstLevelNodes() {
        return firstLevelNodes;
    }

    public void setFirstLevelNodes(List<String> firstLevelNodes) {
        this.firstLevelNodes = firstLevelNodes;
    }

    public List<String> getSecondLevelNodes() {
        return secondLevelNodes;
    }

    public void setSecondLevelNodes(List<String> secondLevelNodes) {
        this.secondLevelNodes = secondLevelNodes;
    }

    public List<String> getThirdLevelNodes() {
        return thirdLevelNodes;
    }

    public void setThirdLevelNodes(List<String> thirdLevelNodes) {
        this.thirdLevelNodes = thirdLevelNodes;
    }

    @JsonIgnore
    public List<Element> getAllElements() {
        return allElements;
    }

    public void setAllElements(List<Element> allElements) {
        this.allElements = allElements;
    }

    public Map<String, Boolean> getNodeVisibility() {
        return nodeVisibility;
    }

    public void setNodeVisibility(Map<String, Boolean> nodeVisibility) {
        this.nodeVisibility = nodeVisibility;
    }

    public void setExecutionMap(Map<String, Boolean> executionMap) {
        this.executionMap = executionMap;
    }

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }

    @JsonIgnore
    public String getDependencyMapJson() {
        Gson gson = new Gson();
        JsonObject json = new JsonObject();

        for (Map.Entry<String, List<String>> entry : dependencyMap.entrySet()) {
            json.add(entry.getKey(), gson.toJsonTree(entry.getValue()));
        }

        return json.toString();
    }

    public String getSelectedFirstLevelNode() {
        return selectedFirstLevelNode;
    }

    public void setSelectedFirstLevelNode(String selectedFirstLevelNode) {
        this.selectedFirstLevelNode = selectedFirstLevelNode;
    }

    private void initDependencyMap() {

        // Define dependencies (first-level -> second-level)
        dependencyMap = new HashMap<>();
        dependencyMap.put("LC-MS Spectra", List.of(
                "Spectra Processing",
                "Statistics [one factor]",
                "Biomarker Analysis",
                "Dose Response Analysis"
        ));
        dependencyMap.put("Compound Table", List.of(
                "Statistics [one factor]",
                "Statistics [metadata table]",
                "Biomarker Analysis",
                "Enrichment Analysis",
                "Pathway Analysis",
                "Dose Response Analysis",
                "Network Analysis"
        ));
        dependencyMap.put("Metabolite List", List.of(
                "Enrichment Analysis",
                "Pathway Analysis",
                "Causal Analysis",
                "Network Analysis"
        ));
        dependencyMap.put("Gene List", List.of(
                "Network Analysis"
        ));

        dependencyMap.put("Generic Table", List.of(
                "Statistics [one factor]",
                "Statistics [metadata table]",
                "Biomarker Analysis",
                "Dose Response Analysis"
        ));
        dependencyMap.put("Peak Table", List.of(
                "Statistics [one factor]",
                "Statistics [metadata table]",
                "Biomarker Analysis",
                "Dose Response Analysis",
                "Functional Analysis"
        ));
        dependencyMap.put("Peak List", List.of(
                "Functional Analysis"
        //"Peak Annotation"
        ));
        dependencyMap.put("Generic Tables", List.of(
                "Statistical Meta-analysis"
        ));
        dependencyMap.put("Peak Tables", List.of(
                "Functional Meta-analysis"
        ));
        procDependencyMap = new HashMap<>();
        procDependencyMap.put("LC-MS Spectra", List.of(
                "Spectra Processing",
                "Data Filtering",
                "Missing Value",
                "Data Normalization"
        ));
        procDependencyMap.put("Compound Table", List.of(
                "Name Mapping",
                "Data Filtering",
                "Missing Value",
                "Data Normalization"
        ));
        procDependencyMap.put("Metabolite List", List.of(
                "Name Mapping"
        ));
        procDependencyMap.put("Gene List", List.of(
                "Name Mapping"
        ));
        procDependencyMap.put("Generic Table", List.of(
                "Data Filtering",
                "Missing Value",
                "Data Normalization"
        ));
        procDependencyMap.put("Peak Table", List.of(
                "Data Filtering",
                "Missing Value",
                "Data Normalization"
        ));
        procDependencyMap.put("Peak List", List.of());
        procDependencyMap.put("Generic Tables", List.of(
                "Data Harmonization"
        ));
        procDependencyMap.put("Peak Tables", List.of(
                "Data Harmonization"
        ));

    }
    /*
    private List<String> firstLevelNodes = new ArrayList<>(List.of(
            "LC-MS Spectra",
            "Generic Table",
            "Peak Table",
            "Compound Table",
            "Peak List",
            "Metabolite List"
    ));
     */

    private List<String> firstLevelNodes = new ArrayList<>(List.of(
            "LC-MS Spectra",
            "Feature List",
            "Data Table",
            "Multiple Tables"
    ));

    private List<String> secondLevelNodes = new ArrayList<>(List.of(
            "Spectra Processing",
            "Data Filtering",
            "Missing Value",
            "Data Normalization",
            "Name Mapping",
            "Data Harmonization"
    ));

    private List<String> thirdLevelNodes = new ArrayList<>(List.of(
            //"Peak Annotation",
            "Spectra Profiling",
            "Functional Analysis",
            "Functional Meta-analysis",
            "Statistics [one factor]",
            "Statistics [metadata table]",
            "Biomarker Analysis",
            "Statistical Meta-analysis",
            "Dose Response Analysis",
            "Enrichment Analysis",
            "Pathway Analysis",
            "Network Analysis"));

    @JsonIgnore
    private List<Element> allElements = new ArrayList<>();

    private static String toSafeId(String label) {
        // collapse non-alphanumerics into underscores, trim edges, lower-case
        String id = label.toLowerCase().replaceAll("[^a-z0-9]+", "_");
        // ids can’t start with a digit per some CSS selector quirks; prefix if needed
        if (id.isEmpty() || Character.isDigit(id.charAt(0))) {
            id = "n_" + id;
        }
        return label;
    }

    @PostConstruct
    public void init() {
        FacesContext.getCurrentInstance().getPartialViewContext().getRenderIds().add("designForm");
        // Check if the model already contains elements; if so, skip initialization
        if (model != null) {
            return; // Model already initialized
        }

        initDependencyMap();
        nodeVisibility = new HashMap<>();
        model = new DefaultDiagramModel();
        model.setMaxConnections(-1);
        model.setContainment(false);

        model.getDefaultConnectionOverlays().add(new ArrowOverlay(20, 15, 1, 1));
        StraightConnector connector = new StraightConnector();
        connector.setPaintStyle("{stroke:'tomato', strokeWidth:2}");
        connector.setHoverPaintStyle("{stroke:'darkorange'}");
        model.setDefaultConnector(connector);

        int firstLevelXPosition = 15; // Starting X position for second-level nodes
        for (String node : firstLevelNodes) {
            String id = toSafeId(node);

            Element element = new Element(new NetworkElement(node, node, obtainInputImageUrl(node), "input", false), firstLevelXPosition + "em", "4em");
            element.setId(id);
            element.setStyleClass("inputcls");
            element.setTitle(node);
            element.setDraggable(false);
            //element.addEndPoint(createSquareEndPoint(EndPointAnchor.BOTTOM));
            model.addElement(element);
            allElements.add(element);
            selectionMap.put(element.getId(), false);

            nodeVisibility.put(node, true);

            firstLevelXPosition += 20; // Adjust horizontal spacing for second-level nodes
        }

        int secondLevelXPosition = 10; // Starting X position for second-level nodes
        for (String node : secondLevelNodes) {
            String id = toSafeId(node);

            Element secondLevelElement = new Element(new NetworkElement(node, node, "icon.png", "proc", false), secondLevelXPosition + "em", "19.5em");
            secondLevelElement.setId(id);

            secondLevelElement.setStyleClass("proccls");
            secondLevelElement.setTitle(node);
            secondLevelElement.setDraggable(false);
            //secondLevelElement.addEndPoint(createCircleEndPoint(EndPointAnchor.TOP));
            //secondLevelElement.addEndPoint(createCircleEndPoint(EndPointAnchor.LEFT));
            //secondLevelElement.addEndPoint(createCircleEndPoint(EndPointAnchor.RIGHT));
            //secondLevelElement.addEndPoint(createCircleEndPoint(EndPointAnchor.BOTTOM));
            model.addElement(secondLevelElement);
            allElements.add(secondLevelElement);

            nodeVisibility.put(node, true);
            secondLevelXPosition += 15; // Adjust horizontal spacing for second-level nodes
        }

        int thirdLevelXPosition = 2; // Starting X position for third-level nodes
        for (String node : thirdLevelNodes) {
            String id = toSafeId(node);

            Element thirdLevelElement = new Element(new NetworkElement(node, node, "icon.png", "anal", false), thirdLevelXPosition + "em", "35em");
            thirdLevelElement.setId(id);

            thirdLevelElement.setStyleClass("nodecls");
            thirdLevelElement.setTitle(node);
            //thirdLevelElement.addEndPoint(createCircleEndPoint(EndPointAnchor.TOP));
            //thirdLevelElement.addEndPoint(new BlankEndPoint(EndPointAnchor.BOTTOM));

            model.addElement(thirdLevelElement);
            allElements.add(thirdLevelElement);
            nodeVisibility.put(node, true);
            thirdLevelXPosition += 12; // Adjust horizontal spacing for third-level nodes
        }

        for (Element element : model.getElements()) {
            selectionMap.put(element.getId(), false);
            executionMap.put(element.getId(), false);
        }

    }

    public void resetDiagram() {
        //disconnectAllConnections();
        for (Element element : model.getElements()) {
            selectionMap.put(element.getId(), false);
            executionMap.put(element.getId(), false);
            successExecutionMap.clear();
        }
        for (Element element : allElements) {
            nodeVisibility.put(element.getId(), true);
        }
        updateDiagramModel();
    }

    public void saveWorkflow() {

    }

    private String workflowName;

    public String getWorkflowName() {
        return workflowName;
    }

    public void setWorkflowName(String workflowName) {
        this.workflowName = workflowName;
    }

    public void disableAnalNodes(String input) {
        List<String> enabledNodes = dependencyMap.get(input);
        List<String> enabledNodes2 = procDependencyMap.get(input);
        for (Element element : model.getElements()) {
            String nodeTooltip = element.getTitle();
            if (element.getStyleClass().contains("nodecls")) {
                if (enabledNodes != null && enabledNodes.contains(nodeTooltip)) {
                    toggleNodeVisibility(element.getTitle(), true);
                } else {
                    toggleNodeVisibility(element.getTitle(), false);
                }
            } else if (element.getStyleClass().contains("proccls")) {
                if (enabledNodes2 != null && enabledNodes2.contains(nodeTooltip)) {
                    toggleNodeVisibility(element.getTitle(), true);
                } else {
                    toggleNodeVisibility(element.getTitle(), false);
                }
            }
        }

        //special cases
        if (input.equals("Generic Table") || input.equals("Peak Table") || input.equals("Compound Table") || input.equals("LC-MS Spectra")) {
            if (wb.getMetaName().equals("")) {
                toggleNodeVisibility("Statistics [metadata table]", false);
            }

            if (!wb.getTableAnalType().equals("dose")) {
                toggleNodeVisibility("Dose Response Analysis", false);
            }

        }
        updateDiagramModel();

    }

    public void connectInputToProc(String input) {
        if (model == null || model.getElements().isEmpty()) {
            init();
        }

        switch (input) {
            case "LC-MS Spectra" -> {
                // Spectra Processing → Data Filtering → Missing Value → Data Normalization
                connectNodes("Spectra Processing", EndPointAnchor.RIGHT,
                        "Data Filtering", EndPointAnchor.LEFT);
                connectNodes("Data Filtering", EndPointAnchor.RIGHT,
                        "Missing Value", EndPointAnchor.LEFT);
                connectNodes("Missing Value", EndPointAnchor.RIGHT,
                        "Data Normalization", EndPointAnchor.LEFT);
                toggleNodeVisibility("Name Mapping", false);
            }

            case "Compound Table" -> {
                // Compound Table → Data Filtering → Missing Value → Data Normalization → Name Mapping
                connectNodes("Compound Table", EndPointAnchor.BOTTOM,
                        "Data Filtering", EndPointAnchor.TOP);
                connectNodes("Data Filtering", EndPointAnchor.RIGHT,
                        "Missing Value", EndPointAnchor.LEFT);
                connectNodes("Missing Value", EndPointAnchor.RIGHT,
                        "Data Normalization", EndPointAnchor.LEFT);
                connectNodes("Data Normalization", EndPointAnchor.RIGHT,
                        "Name Mapping", EndPointAnchor.LEFT);
                toggleNodeVisibility("Spectra Processing", false);
            }

            case "Metabolite List" -> {
                connectNodes("Metabolite List", EndPointAnchor.BOTTOM,
                        "Name Mapping", EndPointAnchor.TOP);
                toggleNodeVisibility("Data Filtering", false);
                toggleNodeVisibility("Data Normalization", false);
                toggleNodeVisibility("Spectra Processing", false);
                toggleNodeVisibility("Missing Value", false);
            }

            case "Generic Table" -> {
                // Generic Table → Data Filtering → Missing Value → Data Normalization
                connectNodes("Generic Table", EndPointAnchor.BOTTOM,
                        "Data Filtering", EndPointAnchor.TOP);
                connectNodes("Data Filtering", EndPointAnchor.RIGHT,
                        "Missing Value", EndPointAnchor.LEFT);
                connectNodes("Missing Value", EndPointAnchor.RIGHT,
                        "Data Normalization", EndPointAnchor.LEFT);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
            }

            case "Peak Table" -> {
                // Peak Table → Data Filtering → Missing Value → Data Normalization
                connectNodes("Peak Table", EndPointAnchor.BOTTOM,
                        "Data Filtering", EndPointAnchor.TOP);
                connectNodes("Data Filtering", EndPointAnchor.RIGHT,
                        "Missing Value", EndPointAnchor.LEFT);
                connectNodes("Missing Value", EndPointAnchor.RIGHT,
                        "Data Normalization", EndPointAnchor.LEFT);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
            }

            case "Peak List" -> {
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
                toggleNodeVisibility("Data Filtering", false);
                toggleNodeVisibility("Data Normalization", false);
                toggleNodeVisibility("Missing Value", false);
            }
            default -> {
            }
        }
        updateDiagramModel();
    }

    public void selectInputNode(String input) {
        if (model == null || model.getElements().isEmpty()) {
            init();
        }
        this.input = input;

        workflowFinished = false;
        switch (input) {
            case "LC-MS Spectra" -> {
                selectNode("LC-MS Spectra", true);
                selectNode("Spectra Processing", true);
                selectNode("Data Filtering", true);

                selectNode("Missing Value", true);
                selectNode("Data Normalization", true);
                toggleNodeVisibility("Name Mapping", false);
            }
            case "Compound Table" -> {
                selectNode("Data Table", true);
                selectNode("Compound Table", true);
                selectNode("Data Filtering", true);

                selectNode("Missing Value", true);
                selectNode("Data Normalization", true);
                selectNode("Name Mapping", true);
                markNodeExecuted("Name Mapping", true);
                toggleNodeVisibility("Spectra Processing", false);
            }
            case "Metabolite List", "Gene List" -> {
                selectNode("Feature List", true);
                selectNode(input, true);
                selectNode("Name Mapping", true);
                markNodeExecuted("Name Mapping", true);
                toggleNodeVisibility("Data Filtering", false);
                toggleNodeVisibility("Data Normalization", false);
                toggleNodeVisibility("Spectra Processing", false);
                toggleNodeVisibility("Missing Value", false);
            }

            case "Generic Table" -> {
                selectNode("Data Table", true);
                selectNode("Generic Table", true);
                selectNode("Data Filtering", true);
                selectNode("Missing Value", true);

                selectNode("Data Normalization", true);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
            }
            case "Peak Table" -> {
                selectNode("Data Table", true);
                selectNode("Peak Table", true);
                selectNode("Data Filtering", true);
                selectNode("Missing Value", true);

                selectNode("Data Normalization", true);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
            }
            case "Peak List" -> {
                selectNode("Feature List", true);
                selectNode("Peak List", true);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
                toggleNodeVisibility("Data Filtering", false);
                toggleNodeVisibility("Data Normalization", false);
                toggleNodeVisibility("Missing Value", false);
            }
            case "Generic Tables" -> {
                selectNode("Multiple Tables", true);
                selectNode("Generic Tables", true);
                selectNode("Data Harmonization", true);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
                toggleNodeVisibility("Data Filtering", false);
                toggleNodeVisibility("Data Normalization", false);
                toggleNodeVisibility("Missing Value", false);
            }
            case "Peak Tables" -> {
                selectNode("Multiple Tables", true);
                selectNode("Peak Tables", true);
                selectNode("Data Harmonization", true);
                toggleNodeVisibility("Name Mapping", false);
                toggleNodeVisibility("Spectra Processing", false);
                toggleNodeVisibility("Data Filtering", false);
                toggleNodeVisibility("Data Normalization", false);
                toggleNodeVisibility("Missing Value", false);
            }
            default -> {
                System.out.println("Unrecognized input: " + input);
            }
        }
        updateDiagramModel();
    }

    private void connectNodes(String sourceTitle, EndPointAnchor sourceAnchor,
            String targetTitle, EndPointAnchor targetAnchor) {
        // Find the source and target elements by their titles
        Element sourceElement = findElementById(sourceTitle);
        Element targetElement = findElementById(targetTitle);

        if (sourceElement != null && targetElement != null) {
            // Find the specific endpoints based on the anchor
            EndPoint sourceEndPoint = findEndPointByAnchor(sourceElement, sourceAnchor);
            EndPoint targetEndPoint = findEndPointByAnchor(targetElement, targetAnchor);

            if (sourceEndPoint != null && targetEndPoint != null) {
                // Connect the endpoints
                Connection connection = new Connection(sourceEndPoint, targetEndPoint);

                // Optionally add overlays or styling to the connection
                //connection.getOverlays().add(new ArrowOverlay(20, 20, 1, 1));
                // Add the connection to the model
                model.connect(connection);
            } else {
                System.err.println("Cannot connect: Endpoint not found for source or target.");
            }
        } else {
            System.err.println("Cannot connect: Source or target element not found.");
        }
    }

// Helper function to find an endpoint by anchor
    private EndPoint findEndPointByAnchor(Element element, EndPointAnchor anchor) {
        for (EndPoint endPoint : element.getEndPoints()) {
            if (endPoint.getAnchor().equals(anchor)) {
                return endPoint;
            }
        }
        return null; // Return null if no endpoint with the specified anchor is found
    }

    public void prepareFilteredWorkflow(String input) {
        List<String> enabledModules = new ArrayList<>();
        for (Map.Entry<String, Boolean> entry : selectionMap.entrySet()) {
            String nodeName = entry.getKey();
            boolean isSelected = entry.getValue();

            if (isSelected) {
                String module = convertToModuleCode(nodeName);

                if (!module.equals("")) {
                    enabledModules.add(module);
                }
            }
        }
        ArrayList<HashMap<String, Object>> res = wb.filterWorkflowsByPropertyList(wb.getDefaultWorkflowList(), "module", enabledModules);
        wb.setFilteredWorkflowList(res);
    }

    private Element findElementById(String title) {
        for (Element element : model.getElements()) {
            if (title.equals(element.getId())) {
                return element;
            }
        }
        return null; // Return null if no element with the specified title is found
    }

    @JsonIgnore
    public DiagramModel getModel() {
        return model;
    }

    private List<String> sourceNodes = new ArrayList<>(); // To store current source nodes
    private List<String> targetNodes = new ArrayList<>(); // To store current target nodes

    public void onConnect(ConnectEvent event) {
        if (!suspendEvent) {
            String sourceNode = event.getSourceElement().getData().toString();
            String targetNode = event.getTargetElement().getData().toString();

            // Add source and target nodes to their respective lists if not already present
            if (!sourceNodes.contains(sourceNode)) {
                sourceNodes.add(sourceNode);
            }
            if (!targetNodes.contains(targetNode)) {
                targetNodes.add(targetNode);
            }

            sb.addMessage("info", "Connected From " + sourceNode + " To " + targetNode);

            PrimeFaces.current().ajax().update("form:msgs");
        } else {
            suspendEvent = false;
        }
    }

    public void onDisconnect(DisconnectEvent event) {
        String sourceNode = event.getSourceElement().getData().toString();
        String targetNode = event.getTargetElement().getData().toString();

        // Remove source and target nodes from their respective lists
        targetNodes.remove(targetNode);

        // Remove the source node only if no connections remain with it
        boolean hasOtherConnections = false;
        for (String target : targetNodes) {
            if (sourceNodes.contains(sourceNode)) {
                hasOtherConnections = true;
                break;
            }
        }
        if (!hasOtherConnections) {
            sourceNodes.remove(sourceNode);
        }

        sb.addMessage("info", "Disconnected From " + sourceNode + " To " + targetNode);

        PrimeFaces.current().ajax().update("form:msgs");
    }

    public void onConnectionChange(ConnectionChangeEvent event) {
        String originalSource = event.getOriginalSourceElement().getData().toString();
        String newSource = event.getNewSourceElement().getData().toString();
        String originalTarget = event.getOriginalTargetElement().getData().toString();
        String newTarget = event.getNewTargetElement().getData().toString();

        // Update sourceNodes list
        sourceNodes.remove(originalSource);
        if (!sourceNodes.contains(newSource)) {
            sourceNodes.add(newSource);
        }

        // Update targetNodes list
        targetNodes.remove(originalTarget);
        if (!targetNodes.contains(newTarget)) {
            targetNodes.add(newTarget);
        }

        sb.addMessage("info", "Connection Changed - Original Source: " + originalSource + ", New Source: " + newSource
                + ", Original Target: " + originalTarget + ", New Target: " + newTarget);

        PrimeFaces.current().ajax().update("form:msgs");
        suspendEvent = true;
    }

    private void disconnectAllConnections() {
        if (model != null && model.getConnections() != null) {
            // Clear all connections in the model
            model.getConnections().clear();
            for (String node : thirdLevelNodes) {
                connectNodes(node, EndPointAnchor.BOTTOM,
                        node + "_report", EndPointAnchor.TOP);
            }
            //System.out.println("All connections have been disconnected.");
        } else {
            System.err.println("Model or connections are null, unable to disconnect.");
        }
    }

    // Getters and Setters for sourceNodes and targetNodes
    public List<String> getSourceNodes() {
        return sourceNodes;
    }

    public void setSourceNodes(List<String> sourceNodes) {
        this.sourceNodes = sourceNodes;
    }

    public List<String> getTargetNodes() {
        return targetNodes;
    }

    public void setTargetNodes(List<String> targetNodes) {
        this.targetNodes = targetNodes;
    }

    public class NetworkElement implements Serializable {

        private String name;
        private String image;
        private String type;
        private String id;
        private boolean selected;

        public NetworkElement() {
        }

        public NetworkElement(String id, String name, String image, String type, boolean selected) {
            this.id = id;
            this.name = name;
            this.image = image;
            this.type = type;
            this.selected = selected;

        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getImage() {
            return image;
        }

        public void setImage(String image) {
            this.image = image;
        }

        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public boolean isSelected() {
            return selected;
        }

        public void setSelected(boolean selected) {
            this.selected = selected;
        }

        @Override
        public String toString() {
            return name;
        }
    }
    private String selectedElement;   // Store the identifier of the clicked element

    public void onElementClick() {
        String clickedElement = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap().get("clickedElement");

        if (clickedElement != null) {
            boolean isSelected = !selectionMap.getOrDefault(clickedElement, false);
            System.out.println(isSelected + "===isSelected");
            if (isSelected) {
                sb.addMessage("info", clickedElement + " has been selected.");
            } else {
                sb.addMessage("info", clickedElement + " has been unselected.");
            }
            if (firstLevelNodes.contains(clickedElement)) {
                openInputDialog(clickedElement);
            } else {
                selectNode(clickedElement, isSelected);
            }
            //}
        }
    }

    public void onElementDoubleClick() {
        // Retrieve the clicked element's title sent from JavaScript
        String clickedElement = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap().get("clickedElement");
        if (clickedElement != null) {
            //handleElementSelect(clickedElement, true);
            Element currentElement = model.findElement(toSafeId(clickedElement));
            //System.out.println("dbl");
            if (currentElement != null) {
                if (currentElement.getStyleClass().contains("proccls")) {
                    navToPage(clickedElement);
                } else if (currentElement.getStyleClass().contains("inputcls")) {
                    handleElementSelect(clickedElement, false);
                }
            }
        }
    }

    public void handleElementSelect(String clickedElement, boolean addEdges) {
        this.selectedElement = clickedElement;

        Element currentElement = null;
        // Find and modify the corresponding element in the model
        for (Element element : model.getElements()) {
            if (element.getStyleClass().contains("inputcls")) {
                if (clickedElement.equals(element.getId())) {
                    element.setStyleClass("inputcls selected");
                    currentElement = element;
                } else {
                    element.setStyleClass("inputcls");
                }
            }
        }

        if (currentElement != null) {
            if (currentElement.getStyleClass().contains("proccls")) {
                navToPage(clickedElement);
            } else if (currentElement.getStyleClass().contains("inputcls")) {
                for (Element element : allElements) {
                    nodeVisibility.put(element.getId(), true);
                }

                List<String> enabledNodes = dependencyMap.get(clickedElement);
                List<String> enabledNodes2 = procDependencyMap.get(clickedElement);
                for (Element element : model.getElements()) {
                    String nodeTooltip = element.getTitle();
                    if (element.getStyleClass().contains("nodecls")) {
                        if (enabledNodes != null && enabledNodes.contains(nodeTooltip)) {
                            toggleNodeVisibility(element.getTitle(), true);
                        } else {
                            toggleNodeVisibility(element.getTitle(), false);
                        }
                    } else if (element.getStyleClass().contains("proccls")) {
                        if (enabledNodes2 != null && enabledNodes2.contains(nodeTooltip)) {
                            toggleNodeVisibility(element.getTitle(), true);
                        } else {
                            toggleNodeVisibility(element.getTitle(), false);
                        }
                    } else if (element.getStyleClass().contains("repcls")) {
                        String nodeId = element.getId();
                        System.out.println(nodeId.replace("_report", ""));
                        if (enabledNodes != null && enabledNodes.contains(nodeId.replace("_report", ""))) {
                            toggleNodeVisibility(element.getId(), true);
                        } else {
                            toggleNodeVisibility(element.getId(), false);
                        }
                    }
                }
                // Add edges only if addEdges is true
                if (addEdges) {
                    disconnectAllConnections();
                    //connectInputToProc(clickedElement);
                    selectInputNode(clickedElement);
                } else {
                    updateDiagramModel();
                }
            }
        }

        String naviType = settingNaviType(clickedElement);
        if (!naviType.equals("")) {
            DataUtils.doRedirectWithGrowl(sb,
                    "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml",
                    "info",
                    "Please click on Data menu to try our example or upload your own data!"
            );
        }
    }

    public String getSelectedElement() {
        return selectedElement;
    }

    public void setSelectedElement(String selectedElement) {
        this.selectedElement = selectedElement;
    }

    public String settingNaviType(String name) {
        String naviType = "";
        switch (name) {
            case "Spectra Processing" ->
                naviType = "raw";

            case "Peak Annotation" ->
                naviType = "tandemMS";

            case "Functional Analysis" -> {
                if (selectionMap.getOrDefault("Peak List", false)) {
                    naviType = "mass_all";
                } else if (selectionMap.getOrDefault("Peak Table", false)) {
                    naviType = "mass_table";
                }
            }

            case "Functional Meta-analysis" -> {
            }

            case "Statistics [one factor]" ->
                naviType = "stat";

            case "Statistics [metadata table]" ->
                naviType = "mf";

            case "Biomarker Analysis" ->
                naviType = "roc";

            case "Statistical Meta-analysis" -> {
            }

            case "Dose Response Analysis" ->
                naviType = "dose";

            case "Enrichment Analysis" -> {
                if (selectionMap.getOrDefault("Metabolite List", false)) {
                    naviType = "msetora";
                } else if (selectionMap.getOrDefault("Compound Table", false)) {
                    naviType = "msetqea";
                }
            }

            case "Pathway Analysis" -> {
                if (selectionMap.getOrDefault("Metabolite List", false)) {
                    naviType = "pathora";
                } else if (selectionMap.getOrDefault("Compound Table", false)) {
                    naviType = "pathqea";
                }
            }

            case "Network Analysis" ->
                naviType = "network";

            case "Causal Analysis" ->
                naviType = "mgwas";

            default -> {
            }
        }
        // Logic for Functional Meta-analysis
        // Default case if no match is found
        System.out.println("settinganaltype=====naviType");
        sb.setNaviType(naviType);

        return naviType;

    }

    public String convertToModuleCode(String name) {
        String naviType = "";
        switch (name) {
            case "Spectra Processing" ->
                naviType = "raw";

            case "Peak Annotation" ->
                naviType = "tandemMS";

            case "Functional Analysis" -> {
                naviType = "mummichog";
            }
            case "Functional Meta-analysis" ->
                naviType = "metapaths";

            case "Statistics [one factor]" ->
                naviType = "stat";

            case "Statistics [metadata table]" ->
                naviType = "mf";

            case "Biomarker Analysis" ->
                naviType = "roc";

            case "Statistical Meta-analysis" ->
                naviType = "metadata";

            case "Dose Response Analysis" ->
                naviType = "dose";

            case "Enrichment Analysis" -> {
                naviType = "mset";
            }

            case "Pathway Analysis" -> {
                naviType = "path";

            }

            case "Network Analysis" ->
                naviType = "network";

            case "Causal Analysis" ->
                naviType = "mgwas";

            default -> {
            }
        }
        // Default case if no match is found
        return naviType;

    }

    public String settingAnalType(String name) {
        String naviType = "";
        switch (name) {
            case "Spectra Processing" ->
                naviType = "raw";

            case "Peak Annotation" ->
                naviType = "tandemMS";

            case "Functional Analysis" -> {
                System.out.println(selectionMap.getOrDefault("Peak Table", false) + "sourcenodecontains peak table");
                if (selectionMap.getOrDefault("Peak List", false)) {
                    naviType = "mass_all";
                } else {
                    naviType = "mass_table";
                }
            }

            case "Functional Meta-analysis" ->
                naviType = "metapaths";

            case "Statistics [one factor]" ->
                naviType = "stat";

            case "Statistics [metadata table]" ->
                naviType = "mf";

            case "Biomarker Analysis" ->
                naviType = "roc";

            case "Statistical Meta-analysis" ->
                naviType = "metadata";

            case "Dose Response Analysis" ->
                naviType = "dose";

            case "Enrichment Analysis" -> {
                if (selectionMap.getOrDefault("Metabolite List", false)) {
                    naviType = "msetora";
                } else if (selectionMap.getOrDefault("Compound Table", false)) {
                    naviType = "msetqea";
                }
            }

            case "Pathway Analysis" -> {
                if (selectionMap.getOrDefault("Metabolite List", false)) {
                    naviType = "pathora";
                } else if (selectionMap.getOrDefault("Compound Table", false)) {
                    naviType = "pathqea";
                } else if (selectionMap.getOrDefault("Gene List", false)) {
                    naviType = "pathinteg";
                }
            }

            case "Network Analysis" ->
                naviType = "network";

            case "Causal Analysis" ->
                naviType = "mgwas";

            default -> {
            }
        }
        // Default case if no match is found
        sb.setNaviType(naviType);
        return naviType;

    }

    public void removeConnection() {
        // Retrieve source and target IDs from request parameters
        Map<String, String> params = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap();

        String sourceId = params.get("sourceId");
        String targetId = params.get("targetId");

        if (sourceId != null && targetId != null) {
            // Remove the connection from the model
            Connection toRemove = null;
            for (Connection connection : model.getConnections()) {

                if (connection.getSource().getId().equals(sourceId) && connection.getTarget().getId().equals(targetId)) {
                    toRemove = connection;
                    break;
                }
            }

            if (toRemove != null) {
                Element targetElement = findElementByEndPointId(targetId);

                targetNodes.remove(targetElement.getId());
                model.disconnect(toRemove);
                System.out.println("Connection removed: Source = " + sourceId + ", Target = " + targetId);
            }
        }
    }

    public void navToInput() {
        FacesContext context = FacesContext.getCurrentInstance();
        Map<String, String> params = context.getExternalContext().getRequestParameterMap();
        String targetPage = params.get("targetPage");
        navToPage(targetPage);
    }

    public void navToPage(String name) {
        String url = "";
        //String input = "";
        wb.setReturnType("diagram");
        switch (name) {
            case "LC-MS Spectra" ->
                url = "/Secure/workflow/upload/SpecGoogleUploadView.xhtml";
            case "Compound Table" ->
                url = "/Secure/workflow/upload/AnotTableUploadView.xhtml";
            //   input = "table_anot";
            case "Metabolite List" ->
                url = "/Secure/workflow/upload/ListUploadView.xhtml";
            case "Gene List" ->
                url = "/Secure/workflow/upload/NetUploadView.xhtml";
            case "Generic Table" ->
                url = "/Secure/workflow/upload/TableUploadView.xhtml";
            case "Peak Table" ->
                url = "/Secure/workflow/upload/PeakTableUploadView.xhtml";
            case "Peak List" ->
                url = "/Secure/workflow/upload/PeakUploadView.xhtml";
            case "Generic Tables" ->
                url = "/Secure/workflow/upload/MetaLoadView.xhtml";
            case "Peak Tables" ->
                url = "/Secure/workflow/upload/MetaPathLoadView.xhtml";
            default ->
                wb.setReturnType("individual");
        }

        if (!url.equals("")) {
            wb.setDataPreparationUrl("/MetaboAnalyst/" + url);
            if (url.equals("/Secure/workflow/upload/SpecGoogleUploadView.xhtml") & ab.isOnProServer() & !ab.isOnVipServer()) {
                String new_url = fcb.goToSpectraWorkflowUpload();
                //DataUtils.doRedirectWithGrowl(sb, "/" + new_url, "info", "Please start preparing your data!");
            } else {
                DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + url, "info", "Please start preparing your data!");
            }
            return;
        }

        if (sb.getCurrentUser() == null) {
            sb.addMessage("warn", "Please prepare your data before modifying parameters!");
            return;
        }

        if (name.contains("Data Filtering")) {
            url = "/Secure/process/FilterView.xhtml";
        } else if (name.contains("Data Normalization")) {
            url = "/Secure/process/NormalizationView.xhtml";
        } else if (name.contains("Spectra Processing")) {
            url = "/Secure/spectra/SpectraCheck.xhtml";
        } else if (name.contains("Name Mapping")) {
            url = "/Secure/process/NameMapView.xhtml";
        } else if (name.contains("Missing Value")) {
            url = "/Secure/process/ProcMissing.xhtml";
        }
        if (!url.equals("")) {
            wb.setEditModeReturn("overview");
            wb.setEditMode(true);
            wb.setReloadingWorkflow(false);
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + url, "info", "Please prepare your data!");
        } else {
            sb.addMessage("warn", "It is not supported yet!");
        }
    }

    public boolean startWorkflowOld() {
        if (!sb.isLoggedIn()) {
            sb.addMessage("warn", "Please prepare your data first by clicking on the upload icon!");
            return false;
        }

        lastExecutedSteps = new LinkedHashSet<>();
        List<String> naviTypes = new ArrayList<>();
        List<String> origNaviTypes = new ArrayList<>();
        List<String> procs = new ArrayList<>();

        for (Map.Entry<String, Boolean> entry : selectionMap.entrySet()) {
            String nodeName = entry.getKey();
            boolean isSelected = entry.getValue();

            if (isSelected) {
                // Map nodeName to naviType
                String naviType = settingAnalType(nodeName);
                if (!naviType.isEmpty()) {
                    // Add only valid types

                    naviTypes.add(naviType);
                    origNaviTypes.add(nodeName);
                } else {
                    // If not a valid naviType, check if it's a second-level node
                    if (secondLevelNodes.contains(nodeName)) {
                        procs.add(nodeName);
                    }
                }
            }
        }

        //adjustParams(procs);
        wb.setModuleNames(naviTypes);
        List<String> moduleNms = wb.getModuleNames();

        if (moduleNms.size() > 1) {
            wb.setReloadingWorkflow(false);
        }

        if (moduleNms.contains("raw")) {
            jeb.setStopStatusCheck(false);
            executeModule("raw", "Spectra Processing");
            return false;
        }

        if (moduleNms.isEmpty()) {
            sb.addMessage("warn", "Please select at least an analysis module before proceeding1!");
            return false;
        }

        if (!wb.getWorkflowOptions().isEmpty() && (input.equals("Generic Table") || input.equals("Peak Table") || input.equals("Compound Table") || input.equals("LC-MS Spectra"))) {
            if (wb.getWorkflowOptions().size() > 1) {
                wb.setResultPageDisplay("multi");
            }
            int workflowIndex = 0;

            // Loop through the unique sets of WorkflowParameters
            for (WorkflowParameters params : wb.getWorkflowOptions()) {
                int moduleIndex = 0;

                // If this is not the first workflow, clear previous files and images
                if (workflowIndex > 0) {
                    List<String> extensionsToRemove = List.of(".json", ".png");
                    DataUtils.removeFilesByExtensions(sb.getCurrentUser().getHomeDir(), extensionsToRemove);
                    sb.getImgMap().clear();
                }

                // Update NormBean with the current WorkflowParameters
                updatingParams(params);
                // Record the normalization operation
                // Iterate through all modules and execute the workflows
                for (String moduleName : moduleNms) {
                    boolean result = executeModule(moduleName, origNaviTypes.get(moduleIndex));

                    if (result) {
                        System.out.println("Execution succeeded for: " + origNaviTypes.get(moduleIndex));
                        successExecutionMap.put(origNaviTypes.get(moduleIndex), true);
                    } else {
                        System.out.println("Execution failed for: " + origNaviTypes.get(moduleIndex));
                        successExecutionMap.put(origNaviTypes.get(moduleIndex), false);
                    }

                    // Reset memoized R functions after each module execution
                    //RDataUtils.resetMemoise(sb.getRConnection());
                    moduleIndex++;
                }

                // Perform PCA and Volcano plots if "stat" module is not part of the workflow
                if (!Arrays.asList(moduleNms).contains("stat")) {
                    RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), "stat");
                    try {
                        wfv.executeWorkflow("PCA");
                        wfv.executeWorkflow("Volcano");
                    } catch (Exception ex) {
                        Logger.getLogger(DiagramView.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }

                // Save results and organize directories for each set of parameters
                RCenter.saveRLoadImg(sb.getRConnection());
                fcb.saveJavaHistory();
                String jh = hb.getJavaHistoryString();
                jh = jh.replace(":\"[{\"", ":[{\\\"");
                String folderName = params.getFolderName();
                DataUtils.createAndCopyFolder(sb.getCurrentUser().getHomeDir(), sb.getCurrentUser().getHomeDir() + "/" + folderName);
                saveJsonStringToFile(jh, sb.getCurrentUser().getHomeDir() + File.separator + folderName + File.separator + "java_history.json");

                workflowIndex++;

            }

            // Show normalization summary dialog
        } else {
            int i = 0;

            for (String name : moduleNms) {
                boolean res = executeModule(name, origNaviTypes.get(i));
                if (res) {
                    successExecutionMap.put(origNaviTypes.get(i), true);
                } else {
                    successExecutionMap.put(origNaviTypes.get(i), false);
                }
                i++;
            }
        }

        for (Map.Entry<String, Boolean> entry : selectionMap.entrySet()) {
            if (entry.getValue() != null && entry.getValue()) {
                if (!successExecutionMap.getOrDefault(entry.getKey(), true)) {
                    executionMap.put(entry.getKey(), false);

                } else {
                    executionMap.put(entry.getKey(), true);
                }
            }
        }
        //prepareFilteredWorkflow(input);
        //DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=2", "info", "Execution detail is displayed in the diagram below, you can select the module on the top left panel.");
        for (Boolean value : successExecutionMap.values()) {
            if (Boolean.TRUE.equals(value)) {
                workflowFinished = true;
            }
        }

        List<String> pagesToVisit = new ArrayList<>();
        for (String nm : lastExecutedSteps) {
            pagesToVisit = obtainPagesToVisit(pagesToVisit, nm);
            String demoImage = obtainDemoImage(nm);
            if (!demoImage.equals("")) {
                DataUtils.copyFileToFolder(demoImage, sb.getCurrentUser().getHomeDir() + "/");
            }
        }
//ab.isOnZgyPc() || 
        if (!(ab.isOnProServer() || ab.isOnQiangPc())) {
            sb.getNotice().add(DataUtils.obtainTimestampText());
            sb.getNotice().add("Your workflow processing job has become <b>COMPLETED</b>.");
            setShowNotif(true);
        }

        if (!pagesToVisit.isEmpty()) {
            Gson gson = new Gson();
            String pagesJson = gson.toJson(pagesToVisit);
            //PrimeFaces.current().executeScript("startNavigation(" + pagesJson + ");");
            return true;

        } else {
            sb.addMessage("info", "Execution Finished! Click 'Dashboard' button to view results.");
            return true;
        }

    }

    public void checkPagesToVisit() {
        List<String> pagesToVisit = new ArrayList<>();
        for (String nm : lastExecutedSteps) {
            pagesToVisit = obtainPagesToVisit(pagesToVisit, nm);

        }

        if (!pagesToVisit.isEmpty()) {
            Gson gson = new Gson();
            String pagesJson = gson.toJson(pagesToVisit);
            PrimeFaces.current().executeScript("startNavigation(" + pagesJson + ");");
        }
    }

    private LinkedHashSet<String> lastExecutedSteps = new LinkedHashSet<>();

    public LinkedHashSet<String> getLastExecutedSteps() {
        return lastExecutedSteps;
    }

    public void setLastExecutedSteps(LinkedHashSet<String> lastExecutedSteps) {
        this.lastExecutedSteps = lastExecutedSteps;
    }

    public boolean executeModule(String name, String origName) {
        boolean okBool = true;
        ArrayList<String> steps = new ArrayList<>();
        RDataUtils.initDataObjects(sb.getRConnection(), sb.getDataType(), name, sb.isPaired());
        RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), name);
        String errMsg = "";
        System.out.print("executemodule===========" + name);
        switch (name) {
            case "mf" -> {

                sb.setAnalType("mf");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                if (RDataUtils.readTextDataTs(sb.getRConnection(), wb.getDataName(), sb.getDataFormat())) {
                    if (RDataUtils.readMetaData(sb.getRConnection(), wb.getMetaName())) {
                        sb.setDataUploaded();
                        steps = new ArrayList<>(Arrays.asList(
                                "Sanity Check",
                                "Metadata Check",
                                "Filtering",
                                "Normalization",
                                "Visual Analytics",
                                "Supervise Classification",
                                "Random Forest2",
                                "Multivariate Analysis",
                                "MEBA",
                                "ASCA",
                                "Univariate Analysis",
                                "Multifactor anova",
                                "Correlation Analysis",
                                "Linear Models",
                                "Data Overview",
                                "Clustering heatmap",
                                "iPCA",
                                "Metadata Heatmap"
                        ));
                    } else {
                        okBool = false;
                        errMsg = "Metadata table is required.";
                    }
                } else {
                    okBool = false;
                }
            }
            case "dose" -> {
                sb.setDataType("disc");
                sb.setAnalType("dose");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                if (RDataUtils.readTextDataDose(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                    if (!wb.getMetaName().equals("")) {
                        RDataUtils.readMetaData(sb.getRConnection(), wb.getMetaName());
                    }
                    sb.setDataUploaded();
                    steps = new ArrayList<>(Arrays.asList(
                            "Sanity Check",
                            "Filtering",
                            "Normalization",
                            "DE Analysis"
                    ));
                } else {
                    okBool = false;
                }
            }
            case "network" -> {
                if (input.equals("Compound Table")) {

                    sb.setDataType("disc");
                    sb.setAnalType("network");
                    RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                    sb.setPaired(false);
                    sb.setRegression(false);
                    if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                        sb.setDataUploaded();
                        steps = new ArrayList<>(Arrays.asList(
                                "Table",
                                "Sanity Check",
                                "Annotation_Table",
                                "Filtering",
                                "Normalization",
                                "Network Builder_dspc",
                                "Network Viewer_dspc"
                        ));
                    } else {
                        okBool = false;
                    }
                } else {
                    if (!sb.getFeatType().equals("lipid")) {

                        sb.setDataType("disc");
                        sb.setAnalType("network");
                        RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                        sb.setPaired(false);
                        sb.setRegression(false);

                        //if (uploadListOpt.equals("genemetabo") || uploadListOpt.equals("metabo")) {
                        mnb.setCmpdList(wb.getOraList());
                        mnb.handleCmpdListUpload();

                        //if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                        sb.setDataUploaded();
                        steps = new ArrayList<>(Arrays.asList(
                                "List",
                                "Annotation_List_network",
                                "Network Selection",
                                "KEGG Network",
                                "doMnetworkAnalysis_metabo_phenotypes"
                        ));
                    } else {
                        okBool = false;
                        errMsg = "Lipid is not supported.";
                    }
                }
            }
            case "roc" -> {
                sb.setDataType("disc");
                sb.setAnalType("roc");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                //System.out.println("homedir============" + sb.getCurrentUser().getHomeDir());
                if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                    if (!wb.getMetaName().equals("")) {
                        RDataUtils.readMetaData(sb.getRConnection(), wb.getMetaName());
                    }
                    sb.setDataUploaded();
                    steps = new ArrayList<>(Arrays.asList(
                            "Sanity Check",
                            "Filtering",
                            "Normalization",
                            "ROC Analysis",
                            "Univariate ROC",
                            "Multivariate ROC"
                    ));
                } else {
                    okBool = false;
                }
            }
            case "pathqea" -> {
                sb.setDataType("disc");
                sb.setAnalType("pathqea");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(sb.getDataClsOpt().equals("cont"));
                if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), sb.getDataClsOpt())) {
                    sb.setDataUploaded();
                    steps = new ArrayList<>(Arrays.asList(
                            "Sanity Check_Table",
                            "Annotation_Table",
                            "Filtering",
                            "Normalization",
                            "paBn_action",
                            "paBn_heatmap",
                            "Results_Table"
                    ));
                } else {
                    okBool = false;
                }
            }
            case "msetqea" -> {
                sb.setDataType("disc");
                sb.setAnalType("msetqea");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(sb.getDataClsOpt().equals("cont"));
                if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), sb.getDataClsOpt())) {
                    sb.setDataUploaded();
                    steps = new ArrayList<>(Arrays.asList(
                            "Annotation_Table",
                            "Sanity Check_Table",
                            "Filtering_Table",
                            "Normalization_Table",
                            "QEA"
                    ));
                } else {
                    okBool = false;
                }
            }
            case "stat" -> {
                sb.setDataType("disc");
                sb.setAnalType("stat");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                System.out.println(sb.getDataFormat() + "======sb.getDataFormat()");
                if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                    if (!wb.getMetaName().equals("")) {
                        RDataUtils.readMetaData(sb.getRConnection(), wb.getMetaName());
                    }
                    sb.setDataUploaded();
                    steps = new ArrayList<>(Arrays.asList(
                            "Sanity Check",
                            "Filtering",
                            "Normalization",
                            "Visual Analytics", "Univariate Analysis",
                            "Volcano",
                            "T-test",
                            "Fold Change",
                            "Correlation Heatmap",
                            "Classification and Feature Selection",
                            "Random Forest",
                            "Cluster Analysis",
                            "SVM",
                            "SOM",
                            "K-means",
                            "Heatmap",
                            "Dendrogram",
                            "Chemometrics Analysis",
                            "OrthoPLSDA",
                            "sPLSDA",
                            "PLSDA",
                            "PCA",
                            "Advanced Significance Analysis",
                            "EBAM",
                            "SAM",
                            "Univariate Analysis",
                            "Correlation Networks (DSPC)",
                            "Pattern Search",
                            "ANOVA"
                    ));
                } else {
                    okBool = false;
                }
            }
            case "msetora" -> {
                sb.setDataType("disc");
                sb.setAnalType("msetora");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                sb.setDataUploaded();
                steps = new ArrayList<>(Arrays.asList(
                        "List",
                        "Annotation_List",
                        "Enrichment"
                ));
            }

            case "pathora" -> {
                sb.setDataType("disc");
                sb.setAnalType("pathora");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());
                sb.setPaired(false);
                sb.setRegression(false);
                //if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                sb.setDataUploaded();
                steps = new ArrayList<>(Arrays.asList(
                        "Annotated Features",
                        "Pathway Analysis",
                        "List",
                        "Annotation_List",
                        "Name check_List",
                        "paBn_action",
                        "Results_List"
                ));
            }

            case "mass_all" -> {
                sb.setDataType("mass_all");
                sb.setAnalType("mummichog");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                pu.setPeakListParams();
                RDataUtils.readPeakListData(sb.getRConnection(), wb.getDataName());
                //if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                sb.setDataUploaded();
                steps = new ArrayList<>(Arrays.asList(
                        "Sanity Check Peak",
                        "Functional Annotation",
                        "performPeaks2Fun_mum",
                        "performPeaks2Fun_gsea",
                        "performPeaks2Fun_integ",
                        "Heatmap_mum",
                        "Network"
                ));
            }
            case "mass_table" -> {
                sb.setDataType("mass_table");
                sb.setAnalType("mummichog");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                    sb.setDataUploaded();
                    steps = new ArrayList<>(Arrays.asList(
                            "Sanity Check Intensity",
                            "Filtering Intensity",
                            "Normalization Intensity",
                            "Functional Annotation",
                            "Scatter",
                            "Network",
                            "Heatmap_mum"
                    ));
                } else {
                    okBool = false;
                }
            }
            case "raw" -> {
                sb.setDataUploaded();
                steps = new ArrayList<>(Arrays.asList(
                        "Save Project",
                        "Spectra Processing",
                        "Spectra View Result"
                ));

            }
            case "metadata" -> {
                sb.setDataType("disc");
                sb.setAnalType("metadata");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                //if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                sb.setDataUploaded();
                steps = new ArrayList<>(Arrays.asList(
                        "Data_Processing",
                        "Method Selection",
                        "Combine P-values",
                        "Upset Diagram"
                ));

            }
            case "metapaths" -> {
                sb.setDataType("disc");
                sb.setAnalType("metapaths");
                RDataUtils.setAnalType(sb.getRConnection(), sb.getAnalType());

                sb.setPaired(false);
                sb.setRegression(false);
                //if (RDataUtils.readTextData(sb.getRConnection(), wb.getDataName(), sb.getDataFormat(), "disc")) {
                sb.setDataUploaded();
                steps = new ArrayList<>(Arrays.asList(
                        "Data_Processing",
                        "Method Selection",
                        "Pathway-level integration",
                        "metapaths Network Explorer path",
                        "metapaths Upset Diagram path"
                ));

            }
        }
        if (okBool) {
            for (String nm : steps) {
                try {
                    int res = wfv.executeWorkflow(nm);
                    if (res == 0) {
                        sb.addMessage("error", "Error occured at this step: " + nm);
                    } else if (res == 1) {
                        this.lastExecutedSteps.add(nm);
                    }
                } catch (Exception ex) {
                    Logger.getLogger(DiagramView.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        } else {
            if (errMsg.equals("")) {
                String msg = RDataUtils.getErrMsg(sb.getRConnection());
                errMsg = msg;
            }
            errorMessages.put(origName, errMsg);
            sb.addMessage("Error", "The data is not appropriate for " + origName + " module: " + errMsg);
            return false;

        }
        return true;
    }

    public void onInputClick() {
        String clickedElement = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap().get("nodeId");
        if (clickedElement != null) {
            openInputDialog(clickedElement);
        }
    }

    public void onProcClick() {
        String clickedElement = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap().get("nodeId");
        if (clickedElement != null) {
            navToPage(clickedElement);
        }
    }

    public void openInputDialog(String name) {
        switch (name) {
            case "LC-MS Spectra" -> {
                navToPage(name);
            }
            case "Feature List" -> {
                PrimeFaces.current().executeScript("PF('listInputDlg').show()");
            }
            case "Data Table" -> {
                PrimeFaces.current().executeScript("PF('tableInputDlg').show()");
            }
            case "Multiple Tables" -> {
                PrimeFaces.current().executeScript("PF('tablesInputDlg').show()");
            }
        }
    }

    public String obtainInputImageUrl(String name) {
        String imgUrl = "";
        if (ab.isOnLocalServer() || ab.isInDocker()) {
            switch (name) {
                case "LC-MS Spectra" -> {
                    imgUrl = "/resources/images/raw_spectra.png";
                }
                case "Feature List" -> {
                    imgUrl = "/resources/images/list.png";
                }
                case "Data Table" -> {
                    imgUrl = "/resources/images/single_table.png";
                }
                case "Multiple Tables" -> {
                    imgUrl = "/resources/images/tables.png";
                }
            }
        } else {
            switch (name) {
                case "LC-MS Spectra" -> {
                    imgUrl = ab.getAppName() + "/resources/images/raw_spectra.png";
                }
                case "Feature List" -> {
                    imgUrl = ab.getAppName() + "/resources/images/list.png";
                }
                case "Data Table" -> {
                    imgUrl = ab.getAppName() + "/resources/images/single_table.png";
                }
                case "Multiple Tables" -> {
                    imgUrl = ab.getAppName() + "/resources/images/tables.png";
                }
            }
        }
        return imgUrl;
    }

    public void onReportClick() {
        String clickedElement = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap().get("nodeId");
        if (clickedElement != null) {
            boolean res = dldb.generateReportByModule(settingAnalType(clickedElement.replace("_report", "")), "html");
            if (res) {
                DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/ReportView.xhtml", "info", "Please click on Data Preparation to try our example or upload your own data!");
            } else {
                PrimeFaces.current().executeScript("PF('statusDialog').hide()");
            }
        }
    }

    private Map<String, Boolean> nodeVisibility = new HashMap<>();

    private void toggleNodeVisibility(String nodeTitle, boolean show) {
        nodeVisibility.put(nodeTitle, show);
    }

    private void updateDiagramModel() {
        if (nodeVisibility == null) {
            return;
        }
        for (Element element : model.getElements()) {

            boolean isNodeVisible = Optional.ofNullable(nodeVisibility.get(element.getId())).orElse(false);
            // Update the node's style and its endpoints' states
            if (isNodeVisible) {
                element.setStyleClass(element.getStyleClass().replace(" disabled", "").trim());
                enableEndpoints(element); // Enable the endpoints
            } else {
                if (!element.getStyleClass().contains("disabled")) {
                    element.setStyleClass(element.getStyleClass() + " disabled");
                }
                disableEndpoints(element); // Disable the endpoints
            }
        }
    }

    private void enableEndpoints(Element element) {
        List<EndPoint> endPoints = element.getEndPoints();

        if (endPoints != null) {
            for (EndPoint endPoint : endPoints) {
                String currentStyleClass = endPoint.getStyleClass();
                if (currentStyleClass == null) {
                    endPoint.setSource(true);
                    endPoint.setTarget(true);
                    endPoint.setStyleClass("");
                } else if (currentStyleClass.contains("disabled")) {
                    endPoint.setStyleClass(currentStyleClass.replace("disabled", "").trim());
                    endPoint.setSource(true);
                    endPoint.setTarget(true);
                }
            }
        }
    }

    private void disableEndpoints(Element element) {
        List<EndPoint> endPoints = element.getEndPoints();

        if (endPoints != null) {
            for (EndPoint endPoint : endPoints) {
                String currentStyleClass = endPoint.getStyleClass();

                if (currentStyleClass == null) {
                    // Initialize with 'disabled' if null
                    endPoint.setStyleClass("disabled");
                    endPoint.setSource(false);
                    endPoint.setTarget(false);
                } else if (!currentStyleClass.contains("disabled")) {
                    // Add 'disabled' class if not already present
                    endPoint.setStyleClass(currentStyleClass + " disabled");
                    endPoint.setSource(false);
                    endPoint.setTarget(false);
                }
            }
        }
    }

    public boolean resumeRawProject(String folderName, String jobId, String email) {

        Map<String, Object> obj = db.obtainFolderNameProject(folderName);
        Optional<Map.Entry<String, Object>> matchingEntry = obj.entrySet().stream()
                .filter(entry -> entry.getKey().equals("partialtoken") || entry.getKey().contains("partialtoken"))
                .findFirst();

        if (matchingEntry.isPresent()) {
            Object token2 = matchingEntry.get().getValue();
            try {
                boolean res1 = fcb.loadProject(token2 + "", "workflow");
            } catch (Exception ex) {
                Logger.getLogger(FireBaseController.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        sb.doLogin("conc", "roc", false, false, folderName, true);

        String fileName = "metaboanalyst_input.csv";
        String inPath = "/data/glassfish/projects/metaboanalyst/" + folderName + "/";
        String currentPath = sb.getCurrentUser().getHomeDir() + "/";
        DataUtils.createAndCopyFolder(inPath, currentPath);

        wb.setDataName(fileName);
        wb.setDataFormat("colu");
        wb.setDataType("disc");
        sb.setDataFormat("colu");
        sb.setDataType("disc");
        sb.setDataUploaded();

        RDataUtils.updateRawJobStatusByFolder(sb.getRConnection(), folderName, "RAW_FINISHED");
        boolean res = resumeWorkflow();
        if (ab.isOnProServer() || ab.isOnVipServer() || ab.isOnVipServer2() || ab.isInDocker()) {
            RCenter.setWd(sb.getRConnection(), "/data/glassfish/projects/metaboanalyst/" + folderName + "/");
        }

        RCenter.saveRLoadImg(sb.getRConnection());

        RCenter.setWd(sb.getRConnection(), sb.getCurrentUser().getHomeDir());
        sb.setCurrentNaviUrl("/Secure/xialabpro/DashboardView.xhtml");
        sb.setAnalType("roc");
        //System.out.println("resumeRawProject=====FINISHED");
        return true;
    }

    public boolean sendRawResume(String to, String jobId, String shareLink) throws JsonProcessingException, IOException, InterruptedException {
        //System.out.println("sendingrawresume=================================");
        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<body style=\"font-family: Arial; font-size: 12px;\">\n"
                + "<div>\n"
                + "    <p>\n"
                + "        Your workflow processing job (ID: " + jobId + ") status has become <b>COMPLETED</b>.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        You can access the following link to resume your project: " + shareLink + ".\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        Please ignore this email if you did not submit any jobs to MetaboAnalyst.\n"
                + "    </p>\n"
                + "\n"
                + "\n Do NOT reply this email."
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        boolean res = ms.sendEmail(to, ab.getAppName() + " - Workflow Completed", "text/html", htmlMsg);
        return res;
    }

    private List<ConnectionState> connectionStates = new ArrayList<>();

    // Save the diagram state
    public void saveDiagramState(String filePath) {
        connectionStates.clear();

        // Save connections
        for (Connection connection : model.getConnections()) {
            // Retrieve source and target endpoint IDs
            String sourceId = connection.getSource().getId();
            String targetId = connection.getTarget().getId();

            // Find source and target elements based on their endpoint IDs
            Element sourceElement = findElementByEndPointId(sourceId);
            Element targetElement = findElementByEndPointId(targetId);

            if (sourceElement != null && targetElement != null) {
                // Retrieve anchor details
                String sourceAnchor = connection.getSource().getAnchor().toString();
                String targetAnchor = connection.getTarget().getAnchor().toString();

                // Get titles of source and target elements
                String sourceTitle = sourceElement.getTitle();
                String targetTitle = targetElement.getTitle();

                // Save the connection state with titles
                if (!sourceElement.getTitle().equals("") && targetElement.getTitle() != null) {
                    connectionStates.add(new ConnectionState(sourceAnchor, targetAnchor, sourceTitle, targetTitle));
                }
            } else {
                System.err.println("Connection skipped: Missing source or target element for IDs " + sourceId + " -> " + targetId);
            }
        }

        // Save node visibility
        nodeVisibility.clear();
        for (Element element : model.getElements()) {
            nodeVisibility.put(element.getId(), !element.getStyleClass().contains("disabled"));
        }

        DiagramState diagramState = new DiagramState(
                connectionStates,
                input,
                targetNodes
        );
        DiagramUtils.saveDiagramState(diagramState, filePath);

    }

    // Restore the diagram state
    public void restoreDiagramState(String path) {

        DiagramState diagramState = DiagramUtils.loadDiagramState(path);
        System.out.println(diagramState.toString());
        if (diagramState == null) {
            return;
        }
        if (diagramState.getInput().equals("NA")) {
            return;
        }
        //connectionStates = diagramState.getConnectionStates();
        input = diagramState.getInput();
        targetNodes = diagramState.getTargetNodes();
        // Re-add connections   
        /*
        model.getConnections().clear();
        for (ConnectionState state : connectionStates) {
            // Restore connection using titles and anchors
            connectNodes(state.getSourceTitle(),
                    EndPointAnchor.valueOf(state.getSourceAnchor().toUpperCase()),
                    state.getTargetTitle(),
                    EndPointAnchor.valueOf(state.getTargetAnchor().toUpperCase()));
        }
         */
    }

    private Element findElementByEndPointId(String endPointId) {
        return model.getElements().stream()
                .filter(element -> element.getEndPoints().stream()
                .anyMatch(endpoint -> endpoint.getId().equals(endPointId)))
                .findFirst()
                .orElse(null);
    }

    private List<String> normalizationResults = new ArrayList<>();

    public List<String> getNormalizationResults() {
        return normalizationResults;
    }

    public void selectNode(String elementName, boolean isSelected) {
        System.out.println("selectNode=====" + elementName);
        // Clean up the element name (remove checkmark and trim spaces)

        // Find the element in the diagram model
        Element currentElement = model.findElement(toSafeId(elementName));
        if (currentElement != null) {
            // Retrieve current properties of the element
            String currentStyleClass = currentElement.getStyleClass();
            String currentTitle = currentElement.getTitle();

            // Extract the type from the current element's data
            NetworkElement netEle = (NetworkElement) currentElement.getData();
            String type = netEle.getType();

            if (isSelected) {
                /*
                if (!currentStyleClass.contains("selected")) {
                    currentElement.setStyleClass(currentStyleClass + " selected");
                }
                currentElement.setData(new NetworkElement(currentTitle, currentTitle, obtainInputImageUrl(elementName), type, true));
                 */
                selectionMap.put(elementName, true);
            } else {
                /*
                currentElement.setStyleClass(currentStyleClass.replace("selected", "").trim());
                currentElement.setData(new NetworkElement(currentTitle, currentTitle, obtainInputImageUrl(elementName), type, false));
                 */
                selectionMap.put(elementName, false);
            }
        } else {
            if (selectionMap.containsKey(elementName)) {
                selectionMap.put(elementName, true);
                System.out.println("Element with name '" + elementName + "' selected.");

            } else {
                selectionMap.put(elementName, true);
                System.out.println("Element with name '" + elementName + "' not found.");
            }
        }
    }

    @JsonIgnore
    public boolean isNodeSelected(String nodeId) {
        return selectionMap.getOrDefault(nodeId, false);
    }

    @JsonIgnore
    public boolean isNodeExecuted(String nodeId) {
        return executionMap.getOrDefault(nodeId, false);
    }

    @JsonIgnore
    public boolean isNodeFailed(String nodeId) {
        return successExecutionMap.getOrDefault(nodeId, true);
    }

    public void markNodeExecuted(String nodeId, boolean bool) {
        executionMap.put(nodeId, bool);
    }

    public String goToResultPage() {
        if (!workflowFinished) {
            sb.addMessage("warn", "Please execute your workflow first.");
            return null;
        } else {
            return "DashboardView";
        }
    }

    public List<String> obtainPagesToVisit(List<String> pagesToVisit, String option) {

        switch (option) {
            case "DSPC Network", "Network Viewer_dspc", "Correlation Networks (DSPC)" -> {
                pagesToVisit.add("/" + ab.getAppName() + "/Secure/network/MphenoNetView.xhtml");
            }
            case "KEGG Network" -> {
                pagesToVisit.add("/" + ab.getAppName() + "/Secure/network/MetaboNetView.xhtml");
            }
            case "Upset Diagram", "metapaths Upset Diagram path" -> {
                pagesToVisit.add("/" + ab.getAppName() + "/Secure/metastat/UpsetDiagramView.xhtml");
            }
            case "Heatmap_mum", "paBn_heatmap" -> {
                pagesToVisit.add("/" + ab.getAppName() + "/Secure/viewer/HeatmapView.xhtml");
            }
            case "ORA" -> {
                pagesToVisit.add("/" + ab.getAppName() + "/Secure/enrichment/OraView.xhtml");
            }
            // ... handle other cases ...
            default -> {

            }
        }

        return pagesToVisit;
    }

    public String obtainDemoImage(String option) {
        String url = "";
        switch (option) {
            case "Enrichment Network" -> {
                url = ab.getRealPath() + "/images/default_figures/network_demo.png";
            }
            /*
            case "Ridgeline Diagram" -> {
                pagesToVisit.add("/" + ab.getAppName() + "/Secure/vis/RidgelineView.xhtml");
            }*/
            case "3D PCA" -> {
                url = ab.getRealPath() + "/images/default_figures/scatter_demo.png";
            }
            case "Heatmap_mum", "paBn_heatmap" -> {
                url = ab.getRealPath() + "/images/default_figures/heatmap_demo.png";
            }
            case "Upset Diagram", "metapaths Upset Diagram path" -> {
                url = ab.getRealPath() + "/images/default_figures/upset_demo.png";
            }

            default -> {

            }
        }
        return url;
    }

    public String resetWorkflow() {
        resetDiagram();
        doLogoutKeepDataset();

        return "WorkflowView";

    }

    public void doLogoutKeepDataset() {
        boolean loggedIn = sb.isLoggedIn();
        RConnection RC = sb.getRConnection();
        if (loggedIn) {
            if (RC != null) {
                if (ab.isOnLocalServer()) {
                    RCenter.showMemoryUsage(RC);
                }
                RC.close();
            }
            sb.setLoggedIn(false);

            StickyDTO sticky = dc.exportSticky();

            // reset other state you want cleared (optional)
            sb.reset2DefaultState();

            // real logout + handoff sticky to new session
            logoutAndPreserve(sticky);

            FacesContext.getCurrentInstance().getExternalContext()
                    .getSessionMap().put("MA6_PRO_user", true);

            sb.keepUserInfo();
        }
    }

    //workflow
    private String statusMsg = "Job not started yet.";

    public String getStatusMsg() {
        return statusMsg;
    }

    public void setStatusMsg(String statusMsg) {
        this.statusMsg = statusMsg;
    }

    public void submitWorkflowJob() {
//ab.isOnZgyPc() ||
        ensureRawModuleSelected();
        boolean res = false;
        if (ab.isOnProServer() || ab.isOnQiangPc() || ab.isOnZgyPc()) {
            if (selectionMap.getOrDefault("Spectra Processing", false)) {
                res = startWorkflow();
            } else {
                res = submitWorkflowOther();
            }
        } else {
            jeb.setStopStatusCheck(true);
            res = startWorkflow();
        }

        WorkflowRunModel activeRun = wb.getSelectedWorkflowRun();
        if (activeRun != null) {
            activeRun.setStatus("running");
        }

    }

    public boolean submitWorkflowOther() {

        // 0) Collect selected modules
        List<String> naviTypes = new ArrayList<>();
        for (Map.Entry<String, Boolean> e : selectionMap.entrySet()) {
            if (Boolean.TRUE.equals(e.getValue())) {
                String nt = settingAnalType(e.getKey());
                if (nt != null && !nt.isEmpty()) {
                    naviTypes.add(nt);
                }
            }
        }
        if (naviTypes.isEmpty()) {
            sb.addMessage("warn", "Please select at least an analysis module before proceeding2!");
            return false;
        }

        // 1) Basic session checks
        if (!sb.isLoggedIn() || sb.getCurrentUser() == null) {
            sb.addMessage("warn", "Please prepare your data first by clicking on the upload icon!");
            return false;
        }

        // 2) Ensure project is saved
        boolean saveSuccess = false;
        try {
            saveSuccess = fcb.saveProject("share");
        } catch (Exception ex) {
            Logger.getLogger(DiagramView.class.getName()).log(Level.SEVERE, "Error saving project", ex);
        }
        if (!saveSuccess) {
            sb.addMessage("warn", "Failed to save project!");
            return false;
        }

        final String userId = sb.getCurrentUser().getName();
        final String jobId = userId;
        final String token = sb.getShareToken();                 // used for redirect later
        final String email = fub.getEmail();
        final String appName = ab.getAppName();
        final String node = ab.getToolLocation();
        final String type = selectionMap.getOrDefault("LC-MS Spectra", false) ? "raw" : "other";
        final String folder = sb.getCurrentUser().getHomeDir();
        final String baseUrl = ab.getBaseUrlDyn();
        final Integer workflowRunId = (wb.getSelectedWorkflowRun() != null)
                ? wb.getSelectedWorkflowRun().getId()
                : null;

        JobTimerService.Status existing = jsv.getStatus(jobId);
        if (existing == JobTimerService.Status.IN_PROGRESS) {
            sb.addMessage("warn", "Please wait for your last workflow to finish before submitting again!");
            return false;
        }

        // 6) Prime UI status
        jeb.setStopStatusCheck(false);
        jeb.setStatusMsg("Job is running...");
        jeb.setCurrentJobId(jobId);
        jeb.setCurrentStartTime(DataUtils.obtainTimestampText());
        updateNoticeStartWorkflow();
        JobInfo info = new JobInfo(jobId, token, email, appName, node, type, folder, baseUrl, workflowRunId);
        jsv.schedule(info);

        // 9) Show progress dialog & notify
        //PrimeFaces.current().executeScript("PF('workflowInfoDialog').show();");
        sb.addMessage("info", "Workflow has started processing... You will receive an email once it finishes.", false);
        return true;
    }

    private void updatingParams(WorkflowParameters params) {

        pcb.setRemoveMissing(params.isRemoveMissing());
        pcb.setMissingImputeOpt(params.getMissingImputeOpt());
        pcb.setReplaceVarOpt(params.getReplaceVarOpt());
        pcb.setImputeAlgOpt(params.getImputeAlgOpt());

        pcb.setDoQCFiltering(params.isDoQCFiltering());
        pcb.setQcCutoff(params.getQcCutoff());
        pcb.setVarFilterOpt(params.getVarFilterOpt());
        pcb.setFilterCutoff(params.getFilterCutoff());
        pcb.setIntFilterOpt(params.getIntFilterOpt());
        pcb.setIntFilterCutoff(params.getIntFilterCutoff());

        nb.setRowNormOpt(params.getRowNormOpt());
        nb.setTransNormOpt(params.getTransNormOpt());
        nb.setScaleNormOpt(params.getScaleNormOpt());

        switch (nb.getRowNormOpt()) {
            case "SpecNorm" ->
                nb.setSpecNormSpecifed(params.isSpecNormSpecifed());
            case "SamplePQN" ->
                nb.setRefSmpl(params.getRefSmpl());
            case "GroupPQN" ->
                nb.setRefGrp(params.getRefGrp());
            case "CompNorm" ->
                nb.setRefVar(params.getRefVar());
            default -> {
            }
        }

        //System.out.println(params.getDetailText());
        jrd.record_performMissingImpute(pcb);
        jrd.record_filterButton_action(pcb);
        jrd.record_PerformDataNormalization(nb);

    }
    private int showNotifNum = 0;

    private boolean showNotif = false;

    public boolean isShowNotif() {
        return showNotif;
    }

    public void setShowNotif(boolean showNotif) {
        if (showNotif) {
            showNotifNum = showNotifNum + 1;
        } else {
            showNotifNum = 0;
        }
        this.showNotif = showNotif;
    }

    public int getShowNotifNum() {
        return showNotifNum;
    }

    public void setShowNotifNum(int showNotifNum) {
        this.showNotifNum = showNotifNum;
    }

    public void updateNoticeStartWorkflow() {

        String startTime = jeb.getCurrentStartTime();
        String jobId = jeb.getCurrentJobId();
        String statusMsg = jeb.getStatusMsg();  // or any other status

        StringBuilder sbd = new StringBuilder();
        sbd.append(startTime).append("\n")
                .append("Job ID: ").append(jobId).append("\n")
                .append("Status: ");

        sbd.append("\n\n")
                .append("Job submitted successfully. An email notice will be sent to you when your job is completed.\n\n");
        // Check status to display a custom message if the job is running
        if ("Job is running...".equals(statusMsg)) {
            sbd.append("Job is running...");
        } else {
            sbd.append(statusMsg);
        }
        // Split the complete message into individual lines
        String[] lines = sbd.toString().split("\\n");

        // Add each line as an element to the ArrayList
        sb.getNotice().addAll(Arrays.asList(lines));

        // For demonstration, print out each line
        for (String line : lines) {
            System.out.println(line);
        }

        showNotif = true;
    }

    public boolean resumeWorkflow() {

        if (!sb.isLoggedIn()) {
            sb.addMessage("warn", "Please prepare your data first by clicking on the upload icon!");
            return false;
        }
        lastExecutedSteps = new LinkedHashSet<>();

        //adjustParams(procs);
        List<String> moduleNms = wb.getModuleNames();

        if (moduleNms.isEmpty()) {
            sb.addMessage("warn", "Please select at least an analysis module before proceeding3!");
            return false;
        }

        int i = 0;
        RCenter.recordMessage(sb.getRConnection(), "Workflow ------ <b>Started!</b>");
        //System.out.println("moduleNmsSize========" + moduleNms.size());

        for (String name : moduleNms) {
            if (!name.equals("raw")) {
                boolean res = executeModule(name, moduleNms.get(i));
                if (!res) {
                    successExecutionMap.put(moduleNms.get(i), false);
                } else {
                    successExecutionMap.put(moduleNms.get(i), true);
                }
            }
            i++;
        }

        RCenter.recordMessage(sb.getRConnection(), "Workflow ------ <b>Finished!</b>");
        for (String nm : lastExecutedSteps) {
            String demoImage = obtainDemoImage(nm);
            if (!demoImage.equals("")) {
                DataUtils.copyFileToFolder(demoImage, sb.getCurrentUser().getHomeDir() + "/");
            }
        }
        updatingDiagramAfterFinish();
        sb.addMessage("info", "Execution Finished! Click 'Dashboard' button to view results.");
        return true;
    }

    private void updatingDiagramAfterFinish() {
        for (Map.Entry<String, Boolean> entry : selectionMap.entrySet()) {
            if (entry.getValue() != null && entry.getValue()) {
                if (!successExecutionMap.getOrDefault(entry.getKey(), true)) {
                    executionMap.put(entry.getKey(), false);
                } else {
                    executionMap.put(entry.getKey(), true);
                }
            }
        }

    }
    private String jobId = "";
    private String finishLink = "";

    public String getJobId() {
        return jobId;
    }

    public void setJobId(String jobId) {
        this.jobId = jobId;
    }

    public String getFinishLink() {
        return finishLink;
    }

    public void setFinishLink(String finishLink) {
        this.finishLink = finishLink;
    }

    public void initWorkflowBasedOnProject() {
        String url = "";

        ProjectModel selectedProject = fpb.getSelectedProject();
        String workflowType = "";
        switch (fpb.getSelectedProject().getType()) {
            case "raw" -> {
                url = "/Secure/workflow/upload/SpecGoogleUploadView.xhtml";
                workflowType = "LC-MS Spectra";
            }
            case "network" -> {
                url = "/Secure/workflow/upload/NetUploadView.xhtml";
                workflowType = "Gene List";

            }
            case "stat" -> {
                url = "/Secure/workflow/upload/TableUploadView.xhtml";
                workflowType = "Generic Table";
            }
            case "mass_all" -> {
                url = "/Secure/workflow/upload/PeakTableUploadView.xhtml";
                workflowType = "Peak Table";

            }
            case "mass_table" -> {
                url = "/Secure/workflow/upload/PeakUploadView.xhtml";
                workflowType = "Peak List";

            }
            case "metadata" -> {
                url = "/Secure/workflow/upload/MetaLoad.xhtml";
                workflowType = "Generic Tables";

            }
            case "metapaths" -> {
                url = "/Secure/workflow/upload/MetaPathLoad.xhtml";
                workflowType = "Peak Tables";

            }
            case "mf" -> {
                url = "/Secure/workflow/upload/TableUploadView.xhtml";
                workflowType = "Generic Table";

            }
            case "dose" -> {
                url = "/Secure/workflow/upload/TableUploadView.xhtml";
                workflowType = "Generic Table";

            }
            case "roc" -> {
                url = "/Secure/workflow/upload/TableUploadView.xhtml";
                workflowType = "Generic Table";

            }
            case "pathqea" -> {
                url = "/Secure/workflow/upload/AnotTableUploadView.xhtml";
                workflowType = "Compound Table";

            }
            case "msetqea" -> {
                url = "/Secure/workflow/upload/AnotTableUploadView.xhtml";
                workflowType = "Compound Table";

            }
            case "msetora" -> {
                url = "/Secure/workflow/upload/ListUploadView.xhtml";
                workflowType = "Metabolite List";

            }
            case "pathora" -> {
                url = "/Secure/workflow/upload/AnotListUploadView.xhtml";
                workflowType = "Metabolite List";

            }
            default ->
                wb.setReturnType("individual");
        }
        wb.setReloadingWorkflowType("Generic Table");

        boolean res = sb.doLogin(fpb.getSelectedProject().getDataType(), fpb.getSelectedProject().getType(), false, false);
        if (!res) {
            sb.addMessage("error", "Unable to create user folder!");
            return;
        }

        if (sb.getCurrentUser() == null) {
            sb.addMessage("Warn", "Please start an analysis session first!");
            return;
        }

        String destDirPath = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";
        String bucketObjectName = "user_folders/" + fub.getEmail() + "/" + selectedProject.getFolderName() + ".zip";
        String localFilePath = fbb.getProjectPath() + bucketObjectName;
        File f = new File(localFilePath);
        if (f.exists()) {
            DataUtils.extractFileFromZip(localFilePath, "workflow.json", destDirPath + "workflow.json");
        } else {

            fcb.downloadObject(selectedProject.getHostname(), fub.getEmail(), selectedProject.getFolderName(), destDirPath + selectedProject.getFolderName() + ".zip");
            DataUtils.extractFileFromZip(destDirPath + selectedProject.getFolderName() + ".zip", "workflow.json", destDirPath + "workflow.json");
        }

        Map<String, FunctionInfo> functionInfos = FunctionInvoker.loadFunctionInfosFromFile(destDirPath + "workflow.json");
        wb.setFunctionInfos(functionInfos);
        wb.setReloadingWorkflow(true);
        wb.setActiveIndex(0);
        DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + url, "info", "Workflow loaded successfully! Please upload your dataset now.");
    }

    public void selectBlock(String moduleType) {
        String name = convertToBlockName(moduleType);
        selectNode(name, true);

    }

    public String convertToBlockName(String code) {
        String moduleName;
        //System.out.println("converttoblockname====" + code);
        switch (code) {
            case "raw" ->
                moduleName = "Spectra Processing";
            case "tandemMS" ->
                moduleName = "Peak Annotation";
            case "mummichog" ->
                moduleName = "Functional Analysis";
            case "metapaths" ->
                moduleName = "Functional Meta-analysis";
            case "stat" ->
                moduleName = "Statistics [one factor]";
            case "mf" ->
                moduleName = "Statistics [metadata table]";
            case "roc" ->
                moduleName = "Biomarker Analysis";
            case "metadata" ->
                moduleName = "Statistical Meta-analysis";
            case "dose" ->
                moduleName = "Dose Response Analysis";
            case "msetora", "msetqea", "msetssp", "mset" ->
                moduleName = "Enrichment Analysis";
            case "pathora", "pathqea" ->
                moduleName = "Pathway Analysis";
            case "network" ->
                moduleName = "Network Analysis";
            case "mgwas" ->
                moduleName = "Causal Analysis";
            default ->
                moduleName = "";
        }
        return moduleName;
    }

    private static final Logger LOGGER = Logger.getLogger(DiagramView.class.getName());

    // === Convert Set<WorkflowParameters> to a deterministic List ===
    // Deterministic order for Set<WorkflowParameters>
    private static List<WorkflowParameters> orderedWorkflowOptions(Set<WorkflowParameters> set) {
        if (set == null || set.isEmpty()) {
            return Collections.emptyList();
        }
        List<WorkflowParameters> list = new ArrayList<>(set);
        list.sort(Comparator.comparing(wp -> Optional.ofNullable(wp.getFolderName()).orElse("~zzz_null~")));
        return list;
    }

    /**
     * Build RunPlans (Cartesian product of ordered WF options x module list).
     * If there are no WF options, builds one plan per module with params ==
     * null.
     */
    private List<RunPlan> buildRunPlans(List<String> moduleNms, List<String> origNaviTypes, List<WorkflowParameters> wfOptsOrdered) {
        List<RunPlan> plans = new ArrayList<>();
        boolean hasWF = wfOptsOrdered != null && !wfOptsOrdered.isEmpty();

        if (hasWF) {
            for (WorkflowParameters p : wfOptsOrdered) {
                for (int i = 0; i < moduleNms.size(); i++) {
                    plans.add(new RunPlan(p, moduleNms.get(i), origNaviTypes.get(i)));
                }
            }
        } else {
            for (int i = 0; i < moduleNms.size(); i++) {
                plans.add(new RunPlan(null, moduleNms.get(i), origNaviTypes.get(i)));
            }
        }
        return plans;
    }

    /* ---------- Multi-combo mode (creates subfolders) ---------- */
    private Map<String, Boolean> runAllPlansMulti(List<RunPlan> runPlans, List<String> moduleNms) {
        Map<String, Boolean> comboSuccessMap = new LinkedHashMap<>();
        String userHome = sb.getCurrentUser().getHomeDir();

        int idx = 0;
        for (RunPlan rp : runPlans) {
            // Isolation between combos
            if (idx > 0) {
                List<String> exts = List.of(".json", ".png");
                DataUtils.removeFilesByExtensions(userHome, exts);
                sb.getImgMap().clear();
            }

            if (rp.getParams() != null) {
                updatingParams(rp.getParams());
            }

            boolean result;
            try {
                result = executeModule(rp.getModuleName(), rp.getOrigNaviType());
            } catch (Exception ex) {
                LOGGER.log(Level.SEVERE, "executeModule failed: " + rp.comboKey(), ex);
                result = false;
            }

            comboSuccessMap.put(rp.comboKey(), result);
            successExecutionMap.put(rp.getOrigNaviType(), result);

            // If "stat" not selected anywhere, run PCA/Volcano for this combo
            if (!moduleNms.contains("stat")) {
                try {
                    RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), "stat");
                    wb.setReloadingWorkflow(false);
                    wfv.executeWorkflow("PCA");
                    wfv.executeWorkflow("Volcano");
                } catch (Exception ex) {
                    LOGGER.log(Level.SEVERE, "PCA/Volcano failed: " + rp.comboKey(), ex);
                }
            }

            // Save R images & history into combo subfolder
            try {
                RCenter.saveRLoadImg(sb.getRConnection());
            } catch (Exception ex) {
                LOGGER.log(Level.WARNING, "saveRLoadImg failed: " + rp.comboKey(), ex);
            }

            fcb.saveJavaHistory();
            String jh = hb.getJavaHistoryString().replace(":\"[{\"", ":[{\\\"");

            String comboFolder = rp.folderName();
            DataUtils.createAndCopyFolder(userHome, userHome + "/" + comboFolder);
            try {
                saveJsonStringToFile(jh, userHome + File.separator + comboFolder + File.separator + "java_history.json");
            } catch (Exception ex) {
                LOGGER.log(Level.WARNING, "saveJsonStringToFile failed: " + rp.comboKey(), ex);
            }

            idx++;
        }
        wb.setRunPlans(runPlans);
        return comboSuccessMap;
    }

    /* ---------- Legacy single-run mode (no subfolder) ---------- */
    private boolean runSinglePlanLegacy(RunPlan rp, List<String> moduleNms) {
        // No cleanup, no per-combo subfolder; mimic old single-run behavior
        if (rp.getParams() != null) {
            updatingParams(rp.getParams());
        }

        boolean ok;
        try {
            ok = executeModule(rp.getModuleName(), rp.getOrigNaviType());
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, "executeModule failed (single plan): " + rp.getModuleName(), ex);
            ok = false;
        }

        successExecutionMap.put(rp.getOrigNaviType(), ok);

        // If "stat" not selected anywhere, run PCA/Volcano once
        if (!moduleNms.contains("stat")) {
            try {
                RDataUtils.loadRscriptsOnDemand(sb.getRConnection(), "stat");
                wfv.executeWorkflow("PCA");
                wfv.executeWorkflow("Volcano");
            } catch (Exception ex) {
                LOGGER.log(Level.SEVERE, "PCA/Volcano failed (single plan)", ex);
            }
        }

        // Save R images & history into current working directory (no subfolder)
        try {
            RCenter.saveRLoadImg(sb.getRConnection());
        } catch (Exception ex) {
            LOGGER.log(Level.WARNING, "saveRLoadImg failed (single plan)", ex);
        }

        fcb.saveJavaHistory();
        String jh = hb.getJavaHistoryString().replace(":\"[{\"", ":[{\\\"");
        try {
            // Keep same location as legacy code (e.g., userHome/java_history.json or wherever you used)
            String userHome = sb.getCurrentUser().getHomeDir();
            saveJsonStringToFile(jh, userHome + File.separator + "java_history.json");
        } catch (Exception ex) {
            LOGGER.log(Level.WARNING, "saveJsonStringToFile failed (single plan)", ex);
        }

        return ok;
    }

    /* ---------- MAIN ENTRY ---------- */
    public boolean startWorkflow() {
        if (!sb.isLoggedIn()) {
            sb.addMessage("warn", "Please prepare your data first by clicking on the upload icon!");
            return false;
        }

        ensureRawModuleSelected();

        lastExecutedSteps = new LinkedHashSet<>();
        List<String> naviTypes = new ArrayList<>();
        List<String> origNaviTypes = new ArrayList<>();
        List<String> procs = new ArrayList<>();

        // Gather selected nodes
        System.out.println("startworkflowanaltype======" + sb.getAnalType());

        for (Map.Entry<String, Boolean> entry : selectionMap.entrySet()) {
            String nodeName = entry.getKey();
            boolean isSelected = Boolean.TRUE.equals(entry.getValue());
            if (!isSelected) {
                continue;
            }

            String naviType = settingAnalType(nodeName);

            if (!naviType.isEmpty()) {
                System.out.println("naviType======" + naviType);
                naviTypes.add(naviType);
                origNaviTypes.add(nodeName);
            } else if (secondLevelNodes.contains(nodeName)) {
                procs.add(nodeName);
            }
        }

        // Persist modules and deduplicate (preserve order)
        wb.setModuleNames(naviTypes);

        // Preserve first occurrence mapping module -> UI node
        Map<String, String> moduleToOrig = new LinkedHashMap<>();
        for (int i = 0; i < naviTypes.size(); i++) {
            String module = naviTypes.get(i);
            if (!moduleToOrig.containsKey(module) && i < origNaviTypes.size()) {
                moduleToOrig.put(module, origNaviTypes.get(i));
            }
        }

        List<String> moduleSequence = new ArrayList<>(new LinkedHashSet<>(wb.getModuleNames()));
        boolean rawPresent = moduleSequence.remove("raw");
        if (rawPresent) {
            moduleSequence.add(0, "raw");
        }

        List<String> origOrdered = moduleSequence.stream()
                .map(m -> moduleToOrig.getOrDefault(m, convertToBlockName(m)))
                .collect(Collectors.toCollection(ArrayList::new));

        wb.setModuleNames(new ArrayList<>(moduleSequence));

        if (moduleSequence.size() > 1) {
            wb.setReloadingWorkflow(false);
        }

        if (rawPresent) {
            jeb.setStopStatusCheck(false);
            boolean rawOk = executeModule("raw", origOrdered.isEmpty() ? "Spectra Processing" : origOrdered.get(0));
            if (!rawOk) {
                successExecutionMap.put("Spectra Processing", false);
                return false;
            }
            successExecutionMap.put("Spectra Processing", true);
            executionMap.put("Spectra Processing", true);

            if (ab.shouldUseScheduler()) {
                return false;
            }

            // Continue immediately with downstream modules (local execution)
            if (!moduleSequence.isEmpty()) {
                moduleSequence.remove(0);
            }
            if (!origOrdered.isEmpty()) {
                origOrdered.remove(0);
            }
            Path processedTable = Paths.get(sb.getCurrentUser().getHomeDir(), "metaboanalyst_input.csv");
            if (Files.exists(processedTable)) {
                wb.setDataName("metaboanalyst_input.csv");
                wb.setDataFormat("colu");
                wb.setDataType("disc");
                sb.setDataFormat("colu");
                sb.setDataType("disc");
            } else {
                sb.addMessage("warn", "Spectra processing output (metaboanalyst_input.csv) was not found; downstream modules may fail.");
            }
            wb.setModuleNames(new ArrayList<>(moduleSequence));
        }

        if (moduleSequence.isEmpty()) {
            if (rawPresent) {
                workflowFinished = true;
                updatingDiagramAfterFinish();
                sb.addMessage("info", "Spectra processing has completed.");
                return true;
            }
            sb.addMessage("warn", "Please select at least an analysis module before proceeding4!");
            return false;
        }

        List<String> moduleNms = moduleSequence;
        origNaviTypes = origOrdered;

        // Build run plans
        Set<WorkflowParameters> wfOptsSet = wb.getWorkflowOptions(); // Set<>
        List<WorkflowParameters> wfOptsOrdered = orderedWorkflowOptions(wfOptsSet);
        List<RunPlan> runPlans = buildRunPlans(moduleNms, origNaviTypes, wfOptsOrdered);
        if (runPlans.isEmpty()) {
            sb.addMessage("warn", "Nothing to run—please select options or modules.");
            return false;
        }
        wb.setRunPlans(runPlans); // if you want to store them

        boolean anySuccess;
        if (runPlans.size() == 1) {
            // === Legacy single-run behavior (NO subfolders) ===
            anySuccess = runSinglePlanLegacy(runPlans.get(0), moduleNms);
        } else {
            // === Multi-combo behavior (subfolders per combo) ===
            Map<String, Boolean> comboSuccessMap = runAllPlansMulti(runPlans, moduleNms);
            anySuccess = comboSuccessMap.values().stream().anyMatch(Boolean::booleanValue);
            wb.setResultPageDisplay("multi");

        }

        workflowFinished = anySuccess;

        // Reconcile selection -> execution map for UI
        for (Map.Entry<String, Boolean> entry : selectionMap.entrySet()) {
            if (Boolean.TRUE.equals(entry.getValue())) {
                boolean ok = successExecutionMap.getOrDefault(entry.getKey(), true);
                executionMap.put(entry.getKey(), ok);
            }
        }

        // Pages & demo images
        List<String> pagesToVisit = new ArrayList<>();
        for (String nm : lastExecutedSteps) {
            pagesToVisit = obtainPagesToVisit(pagesToVisit, nm);
            String demoImage = obtainDemoImage(nm);
            if (!demoImage.isEmpty()) {
                DataUtils.copyFileToFolder(demoImage, sb.getCurrentUser().getHomeDir() + "/");
            }
        }

        // Notifications (skip on known servers)
        if (!(ab.isOnProServer() || ab.isOnQiangPc())) {
            sb.getNotice().add(DataUtils.obtainTimestampText());
            sb.getNotice().add("Your workflow processing job has become <b>COMPLETED</b>.");
            setShowNotif(true);
        }

        if (!pagesToVisit.isEmpty()) {
            String pagesJson = new Gson().toJson(pagesToVisit);
            // PrimeFaces.current().executeScript("startNavigation(" + pagesJson + ");");
            return true;
        } else {
            sb.addMessage("info", "Execution Finished! Click 'Dashboard' button to view results.");
            return true;
        }
    }

    public static void logoutAndPreserve(StickyDTO dto) {
        var fc = FacesContext.getCurrentInstance();
        var ext = fc.getExternalContext();
        var req = (jakarta.servlet.http.HttpServletRequest) ext.getRequest();

        // 1) kill old session
        var old = req.getSession(false);
        if (old != null) {
            old.invalidate();
        }

        // 2) new clean session
        var fresh = req.getSession(true);

        // 3) hand off sticky to the new session for @PostConstruct to pick up
        fresh.setAttribute("STICKY_AFTER_LOGOUT", dto);
    }
}
