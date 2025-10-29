package pro.metaboanalyst.workflows;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.SelectItem;
import jakarta.faces.model.SelectItemGroup;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import java.util.HashMap;

import java.util.LinkedHashSet;

import java.util.logging.Level;
import java.util.logging.Logger;

import java.util.Collection;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Locale;
import java.util.stream.Collectors;
import java.util.Objects;
import java.util.UUID;
import java.util.regex.Pattern;
import org.postgresql.util.PGobject;
import org.primefaces.PrimeFaces;
import org.primefaces.component.api.UIColumn;
import org.primefaces.event.CellEditEvent;

import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.StreamedContent;
import pro.metaboanalyst.api.DatabaseClient;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.NormBean;
import pro.metaboanalyst.controllers.general.ProcessBean;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.multifac.MultifacBean;
import pro.metaboanalyst.datalts.DatasetController;
import pro.metaboanalyst.datalts.DatasetRow;
import pro.metaboanalyst.lts.FireBase;
import pro.metaboanalyst.lts.FireBaseController;
import static pro.metaboanalyst.lts.FireBaseController.safeIntFromDoubleLike;
import pro.metaboanalyst.lts.FireProjectBean;
import pro.metaboanalyst.lts.FireUserBean;
import pro.metaboanalyst.lts.FunctionInfo;
import pro.metaboanalyst.models.SampleBean;
import pro.metaboanalyst.rwrappers.RCenter;
import pro.metaboanalyst.rwrappers.RDataUtils;
import pro.metaboanalyst.utils.DataUtils;

@Named("workflowBean")
@SessionScoped
@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkflowBean implements Serializable {

    @JsonIgnore
    @Inject
    private MultifacBean mf;

    @JsonIgnore
    @Inject
    private DiagramView dv;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private FireUserBean fub;

    @JsonIgnore
    @Inject
    private FireBase fbb;
    @JsonIgnore
    @Inject
    private FireBaseController fbc;

    @JsonIgnore
    @Inject
    private DatabaseClient dbc;

    @JsonIgnore
    @Inject
    private ProcessBean procBean;

    @JsonIgnore
    @Inject
    private NormBean normBean;

    @JsonIgnore
    @Inject
    private WorkflowView wf;

    @JsonIgnore
    @Inject
    private DatasetController ds;

    @JsonIgnore
    private ArrayList<HashMap<String, Object>> workflowList = new ArrayList();
    @JsonIgnore
    private ArrayList<HashMap<String, Object>> defaultWorkflowList = new ArrayList();
    @JsonIgnore
    private HashMap<String, Object> selectedWorkflow = new HashMap<>();

    @JsonIgnore
    private ArrayList<HashMap<String, Object>> filteredWorkflowList;
    private String currentWorkflow = "default";

    private String name = "Saved workflow";
    private String description = "None";
    private String currentStep = "";
    private String inputType = "";
    private String editModeReturn = "overview";
    private boolean editMode = false;
    private List<String> moduleNames = new ArrayList<>();
    private String reportModule = "";
    private String returnType = "diagram"; //diagram or individual
    private List<String> selectedRCommands = new ArrayList<>();

    private int activeIndex = 0;
    private String dataPreparationUrl = null;
    private String currentSubFolder = "NA";
    private List<SampleBean> sampleBeans = null;
    private String tableAnalType = "default";
    private boolean reloadingWorkflow = false;
    private String reloadingParameters = "saved";

    private String reloadingWorkflowType = "";

    public String getReloadingParameters() {
        return reloadingParameters;
    }

    public void setReloadingParameters(String reloadingParameters) {
        this.reloadingParameters = reloadingParameters;
    }

    public boolean isReloadingWorkflow() {
        return reloadingWorkflow;
    }

    public void setReloadingWorkflow(boolean reloadingWorkflow) {
        this.reloadingWorkflow = reloadingWorkflow;
    }

    public String getReloadingWorkflowType() {
        return reloadingWorkflowType;
    }

    public void setReloadingWorkflowType(String reloadingWorkflowType) {
        this.reloadingWorkflowType = reloadingWorkflowType;
    }

    public String getTableAnalType() {
        return tableAnalType;
    }

    public void setTableAnalType(String tableAnalType) {
        this.tableAnalType = tableAnalType;
    }

    public List<SampleBean> getSampleBeans() {
        return sampleBeans;
    }

    public void setSampleBeans(List<SampleBean> sampleBeans) {
        this.sampleBeans = sampleBeans;
    }

    public String getCurrentSubFolder() {
        if (sb.isWorkflowMode()) {
            return currentSubFolder;
        } else {
            return "NA";
        }
    }

    public void setCurrentSubFolder(String currentSubFolder) {
        this.currentSubFolder = currentSubFolder;
    }

    @JsonIgnore
    public boolean isProcButtonsRendered() {
        return !(sb.getAnalType().equals("metadata") || sb.getAnalType().equals("metapaths"));
    }

    public String getDataPreparationUrl() {
        return dataPreparationUrl;
    }

    public void setDataPreparationUrl(String dataPreparationUrl) {
        this.dataPreparationUrl = dataPreparationUrl;
    }

    private String resultPageDisplay = "default";

    //if reload saved workflow
    public void settingActiveIndex(int activeIndex) {
        if (activeIndex == 1 || activeIndex == 2) {
            if (reloadingWorkflow) {
                finishMultiPreparation(reloadingWorkflowType);
                dv.selectBlock(sb.getAnalType());
            }
        }
        this.activeIndex = activeIndex;
    }

    public int getActiveIndex() {
        return activeIndex;
    }

    public void setActiveIndex(int activeIndex) {
        this.activeIndex = activeIndex;
    }

    public String getResultPageDisplay() {
        return resultPageDisplay;
    }

    public void setResultPageDisplay(String resultPageDisplay) {
        this.resultPageDisplay = resultPageDisplay;
    }

    public List<String> getSelectedRCommands() {
        return selectedRCommands;
    }

    public void setSelectedRCommands(List<String> selectedRCommands) {
        this.selectedRCommands = selectedRCommands;
    }

    public ArrayList<HashMap<String, Object>> getFilteredWorkflowList() {
        return filteredWorkflowList;
    }

    public void setFilteredWorkflowList(ArrayList<HashMap<String, Object>> filteredWorkflowList) {
        this.filteredWorkflowList = filteredWorkflowList;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public String getEditModeReturn() {
        return editModeReturn;
    }

    public void setEditModeReturn(String editModeReturn) {
        this.editModeReturn = editModeReturn;
    }

    public String getReportModule() {
        return reportModule;
    }

    public void setReportModule(String reportModule) {
        this.reportModule = reportModule;
    }

    public String getInputType() {
        return inputType;
    }

    public void setInputType(String inputType) {
        this.inputType = inputType;
    }

    public List<String> getModuleNames() {
        return moduleNames;
    }

    public void setModuleNames(List<String> moduleNames) {
        this.moduleNames = moduleNames;
    }

    public String getCurrentWorkflow() {
        return currentWorkflow;
    }

    public void setCurrentWorkflow(String currentWorkflow) {
        this.currentWorkflow = currentWorkflow;
    }

    public ArrayList<HashMap<String, Object>> getWorkflowList() {
        return workflowList;
    }

    public void setWorkflowList(ArrayList<HashMap<String, Object>> workflowList) {
        this.workflowList = workflowList;
    }

    public ArrayList<HashMap<String, Object>> getDefaultWorkflowList() {
        return defaultWorkflowList;
    }

    public void setDefaultWorkflowList(ArrayList<HashMap<String, Object>> defaultWorkflowList) {
        this.defaultWorkflowList = defaultWorkflowList;
    }

    public HashMap<String, Object> getSelectedWorkflow() {
        return selectedWorkflow;
    }

    public void setSelectedWorkflow(HashMap<String, Object> selectedWorkflow) {
        this.selectedWorkflow = selectedWorkflow;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getCurrentStep() {
        return currentStep;
    }

    public void setCurrentStep(String currentStep) {
        this.currentStep = currentStep;
    }

    public boolean isEditMode() {
        return editMode;
    }

    public void setEditModeWorkflow(boolean editMode) {
        this.editModeReturn = "module";
        this.editMode = editMode;

    }

    public void setEditMode(boolean editMode) {
        this.editMode = editMode;
    }

    @JsonIgnore
    private Map<String, FunctionInfo> functionInfos = new LinkedHashMap<>();

    @JsonIgnore
    private Set<String> calledWorkflowsError = new LinkedHashSet();

    public Set<String> getCalledWorkflowsError() {
        return calledWorkflowsError;
    }

    public void setCalledWorkflowsError(Set<String> calledWorkflowsError) {
        this.calledWorkflowsError = calledWorkflowsError;
    }

    @JsonIgnore
    private Set<String> calledWorkflows = new LinkedHashSet();

    public Set<String> getCalledWorkflows() {
        return calledWorkflows;
    }

    public void setCalledWorkflows(Set<String> calledWorkflows) {
        this.calledWorkflows = calledWorkflows;
    }

    private String workflow;

    @JsonIgnore
    private List<SelectItem> workflowsGroup;

    public String getWorkflow() {
        return workflow;
    }

    public void setWorkflow(String workflow) {
        this.workflow = workflow;
    }

    public List<SelectItem> getWorkflowsGroup() {
        return workflowsGroup;
    }

    public void setWorkflowsGroup(List<SelectItem> workflowsGroup) {
        this.workflowsGroup = workflowsGroup;
    }

    @PostConstruct
    public void init() {
        initFuncArgumentMap();
        workflowsGroup = new ArrayList<>();

        SelectItemGroup europeCountries = new SelectItemGroup("Generic Format");
        europeCountries.setSelectItems(new SelectItem[]{
            new SelectItem("stat", "Statistical Analysis"),
            new SelectItem("mf", "Multifactor Statistical Analysis"),
            new SelectItem("metadata", "Statistical Meta-analysis"),
            new SelectItem("roc", "Biomarker Analysis"),
            new SelectItem("dose", "Dose Response Analysis"),});

        SelectItemGroup americaCountries = new SelectItemGroup("Annotated Features");
        americaCountries.setSelectItems(new SelectItem[]{
            new SelectItem("network", "Network Analysis"),
            new SelectItem("path", "Pathway Analysis"),
            new SelectItem("mset", "Enrichment Analysis")
        });

        SelectItemGroup mspeaks = new SelectItemGroup("MS Peaks");
        mspeaks.setSelectItems(new SelectItem[]{
            new SelectItem("metapaths", "Functional Meta-Analysis"),
            new SelectItem("mummichog", "Functional Analysis"),
            new SelectItem("tandemMS", "Peak Annotation")
        });

        SelectItemGroup raw = new SelectItemGroup("LC-MS Spectra");
        raw.setSelectItems(new SelectItem[]{
            new SelectItem("raw", "Spectra Processing")
        });

        SelectItemGroup mgwas = new SelectItemGroup("Link to Genomics & Phenotype");
        mgwas.setSelectItems(new SelectItem[]{
            new SelectItem("mgwas", "Causal Analysis")
        });

        //setup default value;
        workflowsGroup.add(europeCountries);
        workflowsGroup.add(americaCountries);
        workflowsGroup.add(mspeaks);
        workflowsGroup.add(raw);
        workflowsGroup.add(mgwas);

        /*
        root = new DefaultTreeNode("Files", null);
        TreeNode node0 = new DefaultTreeNode("Workflows", root);
        TreeNode node03 = new DefaultTreeNode("All Analyses", node0);
        TreeNode node00 = new DefaultTreeNode("Differential Expression", node0);
        TreeNode node01 = new DefaultTreeNode("Clustering analysis", node0);
        TreeNode node02 = new DefaultTreeNode("Custom workflow", node0);

        node0.setExpanded(true);
         */
    }

    public void addFunctionInfo(String functionName, FunctionInfo info) {
        functionInfos.put(functionName, info);
    }

    public FunctionInfo getFunctionInfo(String functionName) {
        return functionInfos.computeIfAbsent(functionName,
                key -> new FunctionInfo(key, key, "No description available"));
    }

    public Map<String, FunctionInfo> getFunctionInfos() {
        return functionInfos;
    }

    private String summaryText;

    public void setSummaryText(String summaryText) {
        this.summaryText = summaryText;
    }

    public void populateSummaryText() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        this.summaryText = facesContext.getExternalContext().getRequestParameterMap().get("summaryText");
        System.out.println(summaryText + "populateSummaryText"); // Just for debugging

    }

    public String getSummaryText() {
        return summaryText;
    }

    /*
    public void loadAllWorkflows() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }

        workflowList = dbc.getAllWorkflows(ab.getAppName(), fub.getEmail());

        for (HashMap<String, Object> myworkflow : workflowList) {
            myworkflow.put("type", "Custom"); // Set the "type" key to "Custom"
        }

        defaultWorkflowList = loadDefaultWorkflows("/" + ab.getRealPath() + "/pro/default_workflows.json");

        // Add all elements from defaultWorkflowList to the beginning of workList
        if (workflowList == null) {
            workflowList = new ArrayList<>();
        }

        //defaultWorkflowList = filterWorkflowsByProperty(defaultWorkflowList, "module", sb.getNaviType());
        if (!defaultWorkflowList.isEmpty()) {
            workflowList.addAll(0, defaultWorkflowList); // Insert at the beginning
        }

        if (selectedWorkflow == null || selectedWorkflow.isEmpty()) {
            if (!workflowList.isEmpty()) {
                selectedWorkflow = workflowList.get(0);
            }
        }
        System.out.println(workflowList.size() + "===workflowList");
    }
     */
    public void loadDefaultWorkflows() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }
        //FireUserBean fu = (FireUserBean) DataUtils.findBean("fireUserBean");

        System.out.println("loadDefaultWorkflows");

        //workflowList = db.getAllWorkflows(ab.getAppName(), fu.getEmail());
        defaultWorkflowList = loadDefaultWorkflows("/" + ab.getRealPath() + "/pro/default_workflows.json");

        // Add all elements from defaultWorkflowList to the beginning of workList
        //if (workflowList == null) {
        workflowList = new ArrayList<>();
        //}

        //defaultWorkflowList = filterWorkflowsByProperty(defaultWorkflowList, "module", sb.getNaviType());
        if (!defaultWorkflowList.isEmpty()) {
            workflowList.addAll(0, defaultWorkflowList); // Insert at the beginning
        }

        if (selectedWorkflow == null || selectedWorkflow.isEmpty()) {
            if (!workflowList.isEmpty()) {
                selectedWorkflow = workflowList.get(0);
            }
        }
    }

    public String finishPreparation() {
        calledWorkflows.add("Data Preparation");
        editMode = false;
        if (returnType.equals("individual")) {
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml", "info", "You can now start the workflow.");

            //DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=2", "info", "You can now start the workflow.");
        } else {
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml", "info", "Data preparation is complete.");

            //DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1", "info", "Data preparation is complete");
        }
        return "WorkflowView";
    }

    public String finishPreparation(String input) {
        calledWorkflows.add("Data Preparation");
        editMode = false;
        if (returnType.equals("individual")) {
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml", "info", "You can now start the workflow.");
            //DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=2", "info", "You can now start the workflow.");
        } else {
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml", "info", "Data preparation is complete.");
            //DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1", "info", "Data preparation is complete");
        }
        return "WorkflowView";
    }

    public void selectWorkflow() {
        sb.setNaviType((String) selectedWorkflow.get("module"));
        DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=2", "info", "You can now start the workflow.");

    }

    public String finishMultiPreparation(String input) {
        ArrayList<String> tbls = new ArrayList<>();
        tbls.add("Compound Table");
        tbls.add("Peak Table");
        tbls.add("LC-MS Spectra");
        tbls.add("Generic Table");
        if (tbls.contains(input)) {
            if (selectedGrp1 != null) {
                if (selectedGrp1.equals(selectedGrp2)) {
                    sb.addMessage("warn", "Both groups can not be the same.");
                    return null;
                }
            }
        }
        calledWorkflows.add("Data Preparation");
        editMode = false;
        //dv.addLinkToAnal((String) selectedWorkflow.get("module"));
        //dv.connectInputToProc(input);
        dv.selectInputNode(input);
        dv.disableAnalNodes(input);
        dv.setInput(input);
        //dv.prepareFilteredWorkflow(input);
        DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml", "info", "You can now start the workflow.");
        //DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1", "info", "You can now start the workflow.");
        return null;
    }

    public void viewWorkflow(HashMap<String, Object> selWorkflow) {
        sb.setAnalType((String) selWorkflow.get("module"));

        if (selWorkflow.get("type").equals("default")) {
            sb.setNaviType((String) selWorkflow.get("module"));
            sb.addMessage("info", "Please prepare your data before running the workflow!");
            setActiveIndex(2);
        } else {
            selectedWorkflow = selWorkflow;
            String res = loadWorkflow();
            if (res != null) {
                sb.addMessage("info", "Please prepare your data before running the workflow!");
                setActiveIndex(2);

            } else {
                sb.addMessage("Warn", "Please first start an analysis session before loading your saved workflows.");
            }
        }
    }

    public String loadWorkflow() {

        if (selectedWorkflow.get("type").equals("default")) {
            viewWorkflow(selectedWorkflow);
            return null;
        }

        if (sb.getCurrentUser() == null) {
            sb.addMessage("Warn", "Please start an analysis session first!");
            return null;
        }

        String destDirPath = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";
        String bucketObjectName = "/user_folders/" + fub.getEmail() + "/" + selectedWorkflow.get("filename") + ".json";
        String localFilePath = fbb.getProjectPath() + bucketObjectName;
        String bucketObjectName2 = "/user_folders/" + fub.getEmail() + "/" + selectedWorkflow.get("filename") + "_overview.json";
        String localFilePath2 = fbb.getProjectPath() + bucketObjectName2;
        File f = new File(localFilePath);
        if (f.exists()) {
            try {
                Files.copy(Paths.get(localFilePath), Paths.get(destDirPath + selectedWorkflow.get("filename") + ".json"), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                Files.copy(Paths.get(localFilePath2), Paths.get(destDirPath + selectedWorkflow.get("filename") + "_overview.json"), java.nio.file.StandardCopyOption.REPLACE_EXISTING);

            } catch (IOException ex) {
                Logger.getLogger(WorkflowBean.class.getName()).log(Level.SEVERE, null, ex);
            }
        } else {
            fbc.downloadObject(selectedWorkflow.get("location") + "", fub.getEmail(), selectedWorkflow.get("filename") + ".json", destDirPath + selectedWorkflow.get("filename") + ".json");
            fbc.downloadObject(selectedWorkflow.get("location") + "", fub.getEmail(), selectedWorkflow.get("filename") + "_overview.json", destDirPath + selectedWorkflow.get("filename") + "_overview.json");

        }
        functionInfos = FunctionInvoker.loadFunctionInfosFromFile(destDirPath + selectedWorkflow.get("filename") + ".json");
        dv.restoreDiagramState(destDirPath + selectedWorkflow.get("filename") + "_overview.json");
        sb.addMessage("Info", "Workflow loaded successfully!");
        return "Workflow loaded successfully!";
    }

    public void loadAllWorkflowsPrerender() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }
        loadAllWorkflows();
    }

    public void loadAllWorkflows() {
        fbc.reloadUserInfo();

        // Load user's saved workflows
        workflowList = dbc.getAllWorkflows(ab.getAppName(), fub.getEmail());
        if (workflowList == null) {
            workflowList = new ArrayList<>();
        }
        for (HashMap<String, Object> wf : workflowList) {
            wf.put("type", "Custom"); // mark user workflows
        }

        // Load default/example workflows and tag them
        defaultWorkflowList = loadDefaultWorkflows("/" + ab.getRealPath() + "/pro/default_workflows.json");
        if (defaultWorkflowList == null) {
            defaultWorkflowList = new ArrayList<>();
        }
        for (HashMap<String, Object> wf : defaultWorkflowList) {
            wf.put("type", "Example"); // mark examples
        }

        // Merge: examples first, then customs (adjust order if you prefer the opposite)
        final ArrayList<HashMap<String, Object>> merged = new ArrayList<>(defaultWorkflowList.size() + workflowList.size());
        merged.addAll(defaultWorkflowList);
        merged.addAll(workflowList);
        workflowList = merged;

        // Pick a default selection if none
        if ((selectedWorkflow == null || selectedWorkflow.isEmpty()) && !workflowList.isEmpty()) {
            selectedWorkflow = workflowList.get(0);
        }

        // Optional: quick debug line
        System.out.println("[loadAllWorkflows] total=" + workflowList.size()
                + ", examples=" + defaultWorkflowList.size()
                + ", custom=" + (workflowList.size() - defaultWorkflowList.size()));
    }

    public ArrayList<HashMap<String, Object>> loadDefaultWorkflows(String jsonFilePath) {
        ObjectMapper objectMapper = new ObjectMapper();
        ArrayList<HashMap<String, Object>> defaultWorkflowList;

        try {
            // Read JSON file into temporary list
            defaultWorkflowList = objectMapper.readValue(
                    new File(jsonFilePath),
                    new TypeReference<ArrayList<HashMap<String, Object>>>() {
            }
            );
            return (defaultWorkflowList);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public ArrayList<HashMap<String, Object>> filterWorkflowsByProperty(ArrayList<HashMap<String, Object>> workflows, String property, Object value) {
        ArrayList<HashMap<String, Object>> filteredWorkflows = new ArrayList<>();

        for (HashMap<String, Object> workflow : workflows) {
            if (workflow.containsKey(property) && value.equals(workflow.get(property))) {
                filteredWorkflows.add(workflow);
            }
        }

        //System.out.println("Filtered workflows by " + property + " = " + value + ": " + filteredWorkflows);
        return filteredWorkflows;
    }

    public ArrayList<HashMap<String, Object>> filterWorkflowsByPropertyList(
            ArrayList<HashMap<String, Object>> workflows, String property, List<String> values) {

        ArrayList<HashMap<String, Object>> filteredWorkflows = new ArrayList<>();

        for (HashMap<String, Object> workflow : workflows) {
            // Check if the property exists and the value is in the list
            if (workflow.containsKey(property) && values.contains((String) workflow.get(property))) {
                filteredWorkflows.add(workflow);
            }
        }

        System.out.println("Filtered workflows by " + property + " in " + values + ": " + filteredWorkflows);
        return filteredWorkflows;
    }

    public void updateWorkflow() {
        // This method can be used to persist changes if necessary
        // For now, it simply refreshes the list and retains changes made in selectedWorkflow
    }

    private String workflowType = "generic";
    private String dataFormat = "rowu";
    private String dataType = "conc";

    private String dataName = "";
    private String metaName = "";
    private String lblType = "";
    private String oraList = "";

    public String getOraList() {
        return oraList;
    }

    public void setOraList(String oraList) {
        this.oraList = oraList;
    }

    public String getWorkflowType() {
        return workflowType;
    }

    public void setWorkflowType(String workflowType) {
        this.workflowType = workflowType;
    }

    public String getLblType() {
        return lblType;
    }

    public void setLblType(String lblType) {
        this.lblType = lblType;
    }

    public String getDataName() {
        return dataName;
    }

    public void setDataName(String dataName) {
        this.dataName = dataName;
    }

    public String getMetaName() {
        return metaName;
    }

    public void setMetaName(String metaName) {
        this.metaName = metaName;
    }

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public String checkWorkflowInput() {
        if (getDataPreparationUrl().isEmpty()) {
            sb.addMessage("Error", "Please use 'Input Preparation' in the Workflow page to upload your data first.");
        } else {
            DataUtils.doRedirect(getDataPreparationUrl(), ab);
        }
        return null;
    }

    public String getImagePath(String module) {

        switch (module) {
            case "msetora", "msetqea", "msetssp" ->
                module = "mset";
            case "pathqea", "pathora", "pathinteg" ->
                module = "path";
            case "mass_all", "mass_table" ->
                module = "mummichog";
            default -> {

            }
        }
        String basePath = ab.getRealPath() + "/images/workflow/";
        String imagePath = basePath + module + ".png";
        File file = new File(imagePath);
        String res;
        if (file.exists()) {
            res = "/resources/images/workflow/" + module + ".png";
        } else {
            res = "/resources/images/workflow/" + "default.png";
        }
        //System.out.println(res + "====");
        return res;
    }

    public boolean isMultiReportRendered() {
        return moduleNames != null && moduleNames.size() > 1;
    }
    private static final Map<String, String> funcToArgumentMap = new HashMap<>();

    private void initFuncArgumentMap() {
        funcToArgumentMap.put("Missing Values", "performMissingImpute");
        funcToArgumentMap.put("Save Project", "saveProject");
        funcToArgumentMap.put("Data Processing", "Data check");
        funcToArgumentMap.put("Sanity Check", "Data check");
        funcToArgumentMap.put("Sanity Check Intensity", "Data check");
        funcToArgumentMap.put("Sanity Check Peak", "Data check");
        funcToArgumentMap.put("Filtering", "Filtering");
        funcToArgumentMap.put("Filtering_Table", "Filtering");
        funcToArgumentMap.put("Filtering Intensity", "Filtering");
        funcToArgumentMap.put("Normalization", "Normalization");
        funcToArgumentMap.put("Normalization_Table", "Normalization");
        funcToArgumentMap.put("Normalization Intensity", "Normalization");
        funcToArgumentMap.put("Volcano", "Volcano");
        funcToArgumentMap.put("PCA", "PCA");
        funcToArgumentMap.put("iPCA", "iPCA");
        funcToArgumentMap.put("ANOVA", "ANOVA");
        funcToArgumentMap.put("Fold change", "Fold change");
        funcToArgumentMap.put("T-test", "T-test");
        funcToArgumentMap.put("Pattern Search", "Pattern Search");
        funcToArgumentMap.put("Correlation Heatmap", "Correlation Heatmap");
        funcToArgumentMap.put("PLSDA", "PLSDA");
        funcToArgumentMap.put("sPLSDA", "sPLSDA");
        funcToArgumentMap.put("OrthoPLSDA", "OrthoPLSDA");
        funcToArgumentMap.put("SAM", "SAM");
        funcToArgumentMap.put("EBAM", "EBAM");
        funcToArgumentMap.put("Dendrogram", "Dendrogram");
        funcToArgumentMap.put("Heatmap", "Heatmap");
        funcToArgumentMap.put("K-means", "K-means");
        funcToArgumentMap.put("DSPC Networks", "doMnetworkAnalysis");
        funcToArgumentMap.put("SOM", "SOM");
        funcToArgumentMap.put("Random Forest", "Random Forest");
        funcToArgumentMap.put("Random Forest2", "Random Forest2");
        funcToArgumentMap.put("SVM", "SVM");
        funcToArgumentMap.put("SSP", "SSP");
        funcToArgumentMap.put("QEA", "QEA");
        funcToArgumentMap.put("ORA", "ORA");
        funcToArgumentMap.put("Functional Annotation", "InitLibrary");
        funcToArgumentMap.put("Scatter", "Scatter");
        funcToArgumentMap.put("Heatmap_mum", "performPeaks2Fun");
        funcToArgumentMap.put("Network", "Network");
        funcToArgumentMap.put("Name check", "Name check");
        funcToArgumentMap.put("Name check_List", "Name check");
        funcToArgumentMap.put("Name check_Table", "Name check");
        funcToArgumentMap.put("paBn_proceed_ora", "paBn_proceed");
        funcToArgumentMap.put("paBn_proceed_qea", "paBn_proceed");
        funcToArgumentMap.put("Network Builder_dspc", "doMnetworkAnalysis");
        funcToArgumentMap.put("DSPC Network", "doMnetworkAnalysis");
        funcToArgumentMap.put("KEGG Network", "doMnetworkAnalysis");
        funcToArgumentMap.put("Multivariate ROC", "Multivariate ROC");
        funcToArgumentMap.put("Model-based ROC", "Model-based ROC");
        funcToArgumentMap.put("Univariate ROC", "Univariate ROC");
        funcToArgumentMap.put("Metadata check", "Metadata check");
        funcToArgumentMap.put("Metadata Heatmap", "Metadata Heatmap");
        funcToArgumentMap.put("Multifactor anova", "Multifactor anova");
        funcToArgumentMap.put("ASCA", "ASCA");
        funcToArgumentMap.put("Clustering heatmap", "Clustering heatmap");
        funcToArgumentMap.put("Linear Models", "Linear Models");
        funcToArgumentMap.put("MEBA", "MEBA");
        funcToArgumentMap.put("Correlation Analysis", "Correlation Analysis");
        funcToArgumentMap.put("Correlation Networks (DSPC)", "Correlation Networks (DSPC)");
        funcToArgumentMap.put("DE Analysis", "DE Analysis");
        funcToArgumentMap.put("Curve Fitting", "Curve Fitting");
        funcToArgumentMap.put("Result", "Result");
        funcToArgumentMap.put("Combine P-values", "Combine P-values");
        funcToArgumentMap.put("Vote Counting", "Vote Counting");
        funcToArgumentMap.put("Direct Merging", "Direct Merging");
        funcToArgumentMap.put("Upset Diagram", "Upset Diagram");
        funcToArgumentMap.put("Pooling peaks", "Pooling peaks");
        funcToArgumentMap.put("Spectra Check", "Spectra Check");
        funcToArgumentMap.put("Spectra Parameters Settings", "Spectra Parameters Settings");
        funcToArgumentMap.put("Spectra Processing", "Spectra Processing");
    }

    public void setupSelectedRCommands() {
        selectedRCommands = new ArrayList<String>();
        String clickedElement = FacesContext.getCurrentInstance()
                .getExternalContext().getRequestParameterMap().get("clickedElement");
        String funcName = funcToArgumentMap.getOrDefault(clickedElement, clickedElement);
        FunctionInfo functionInfo = getFunctionInfo(funcName); // Retrieve FunctionInfo once
        if (functionInfo != null) {
            // Retrieve commands and remove duplicates using a Set
            selectedRCommands = new ArrayList<>(new LinkedHashSet<>(functionInfo.getrCommands()));

            // Iterate through the unique selectedRCommands
            for (String command : selectedRCommands) {
                // System.out.println("R Command: " + command);
                // Perform any required processing for each command
            }
        } else {
            System.out.println("FunctionInfo not found for: " + funcName);
        }
    }

    private Set<WorkflowParameters> workflowOptions = new HashSet<>();

    public Set<WorkflowParameters> getWorkflowOptions() {
        return workflowOptions;
    }

    public void setWorkflowOptions(Set<?> workflowOptions) {
        // Check if all elements are of type WorkflowParameters
        boolean allAreWorkflowParameters = workflowOptions.stream()
                .allMatch(obj -> obj instanceof WorkflowParameters);

        if (allAreWorkflowParameters) {
            // Safe to cast directly
            this.workflowOptions = (Set<WorkflowParameters>) workflowOptions;
        } else {
            // Populate with conversion
            populateWorkflowOptions(workflowOptions);
        }
    }

    public void populateWorkflowOptions(Set<?> deserializedOptions) {
        Set<WorkflowParameters> convertedOptions = new HashSet<>();

        for (Object option : deserializedOptions) {
            if (option instanceof LinkedHashMap) {
                LinkedHashMap<String, Object> map = (LinkedHashMap<String, Object>) option;

                // Extracting values from the map with correct casting
                boolean removeMissing = Boolean.TRUE.equals(map.get("removeMissing"));
                String missingImputeOpt = (String) map.get("missingImputeOpt");
                String replaceVarOpt = (String) map.get("replaceVarOpt");
                String imputeAlgOpt = (String) map.get("imputeAlgOpt");

                boolean doQCFiltering = Boolean.TRUE.equals(map.get("doQCFiltering"));
                int qcCutoff = map.get("qcCutoff") != null ? ((Number) map.get("qcCutoff")).intValue() : 0;
                String varFilterOpt = (String) map.get("varFilterOpt");
                int filterCutoff = map.get("filterCutoff") != null ? ((Number) map.get("filterCutoff")).intValue() : 0;
                String intFilterOpt = (String) map.get("intFilterOpt");
                int intFilterCutoff = map.get("intFilterCutoff") != null ? ((Number) map.get("intFilterCutoff")).intValue() : 0;

                String rowNormOpt = (String) map.get("rowNormOpt");
                String transNormOpt = (String) map.get("transNormOpt");
                String scaleNormOpt = (String) map.get("scaleNormOpt");
                String folderName = (String) map.get("folderName");

                // Create and add WorkflowParameters object
                convertedOptions.add(new WorkflowParameters(
                        removeMissing, missingImputeOpt, replaceVarOpt, imputeAlgOpt,
                        doQCFiltering, qcCutoff, varFilterOpt, filterCutoff,
                        intFilterOpt, intFilterCutoff, rowNormOpt, transNormOpt, scaleNormOpt, folderName
                ));
            }
        }

        this.workflowOptions = convertedOptions;
    }

    public void resetWorkflowOptions() {
        try {
            workflowOptions.clear(); // Clear all previously added options
            sb.addMessage("info", "All workflow options have been reset.");
        } catch (Exception e) {
            sb.addMessage("error", "Error resetting workflow options: " + e.getMessage());
        }
    }

    public void addParamOpts() {
        try {
            String folderName = "Workflow_" + (workflowOptions.size() + 1);
            WorkflowParameters params = new WorkflowParameters(
                    procBean.isRemoveMissing(),
                    procBean.getMissingImputeOpt(),
                    procBean.getReplaceVarOpt(),
                    procBean.getImputeAlgOpt(),
                    procBean.isDoQCFiltering(),
                    procBean.getQcCutoff(),
                    procBean.getVarFilterOpt(),
                    procBean.getFilterCutoff(),
                    procBean.getIntFilterOpt(),
                    procBean.getIntFilterCutoff(),
                    normBean.getRowNormOpt(),
                    normBean.getTransNormOpt(),
                    normBean.getScaleNormOpt(),
                    folderName
            );
            params.setRefGrp(normBean.getRefGrp());
            params.setRefSmpl(normBean.getRefSmpl());
            params.setRefVar(normBean.getRefVar());

            // Check if this combination already exists
            if (workflowOptions.contains(params)) {
                sb.addMessage("warn", "The selected combination of parameters already exists.");
                return;
            }

            // Check if the limit of three sets has been reached
            if (workflowOptions.size() >= 3) {
                sb.addMessage("warn", "You can only save up to three unique sets of parameters.");
                return;
            }

            // Add the new unique combination to the set
            workflowOptions.add(params);
            sb.addMessage("info", "New set of parameters added successfully.");
        } catch (Exception e) {
            sb.addMessage("error", "Error adding workflow options: " + e.getMessage());
        }
    }

    public void deleteOption(WorkflowParameters param) {
        sb.addMessage("info", param.getFolderName() + " is removed.");
        workflowOptions.remove(param);
    }

    @JsonIgnore
    public WorkflowParameters getWorkflowParameterByFolderName(String folderName) {
        if (folderName == null || folderName.isEmpty()) {
            throw new IllegalArgumentException("Folder name must not be null or empty");
        }
        for (RunPlan param : runPlans) {
            if (folderName.equals(param.folderName())) {
                return param.getParams();
            }
        }
        return null; // Return null if no match is found
    }

    private String selectedGrp1, selectedGrp2;

    @JsonIgnore
    public String getSelectedGrp1() {
        if (selectedGrp1 == null) {
            selectedGrp1 = mf.getUniqueMetaList().get(0).getValue().toString();
        }
        return selectedGrp1;
    }

    public void setSelectedGrp1(String selectedGrp1) {
        this.selectedGrp1 = selectedGrp1;
    }

    @JsonIgnore
    public String getSelectedGrp2() {
        if (selectedGrp2 == null) {
            selectedGrp2 = mf.getUniqueMetaList().get(1).getValue().toString();
        }
        return selectedGrp2;
    }

    public void setSelectedGrp2(String selectedGrp2) {
        this.selectedGrp2 = selectedGrp2;
    }

    public void updateMainComp() {
        if (selectedGrp1.equals(selectedGrp2)) {
            sb.addMessage("warn", "Both groups can not be the same.");
        } else {
            sb.addMessage("info", "Selected groups has been updated.");
        }
    }

    public void setFunctionInfos(Map<String, ?> rawFunctionInfos) {
        // Always reset the internal map
        this.functionInfos = new LinkedHashMap<>();

        if (rawFunctionInfos != null) {
            for (Map.Entry<String, ?> entry : rawFunctionInfos.entrySet()) {
                String key = entry.getKey();
                Object value = entry.getValue();

                if (value instanceof LinkedHashMap) {
                    // Convert map → FunctionInfo
                    FunctionInfo info
                            = DataUtils.convertLinkedHashMapToFunctionInfo(value);
                    this.functionInfos.put(key, info);

                } else if (value instanceof FunctionInfo info) {
                    // Already typed – just store it
                    this.functionInfos.put(key, info);

                } // else: silently ignore or throw, depending on your policy
            }
        }
    }

    public void startFromDetails() {
        try {
            calledWorkflows.add("Data Preparation");
            editMode = false;

            final String module = (String) selectedWorkflow.get("module");
            final String input = resolveInputForModule(module, sb.getDataType());
            postLoadCommon();

            initializeDiagramForInput(input);

            reloadingWorkflow = false;
            wf.generateWorkflowJson("project", false);   // only in "details"
            if (moduleNames.isEmpty()) {
                dv.selectNode(dv.convertToBlockName((String) selectedWorkflow.get("module")), true);
            } else {
                for (String moduleName : moduleNames) {
                    dv.selectNode(dv.convertToBlockName(moduleName), true);
                }
            }
            DataUtils.doRedirectWithGrowl(sb,
                    "/" + ab.getAppName() + "/Secure/xialabpro/WorkflowProjectView.xhtml",
                    "info",
                    "Parameters updated successfully! You can proceed to run the workflow.");
        } catch (Exception ex) {
            Logger.getLogger(WorkflowBean.class.getName())
                    .log(Level.SEVERE, "startFromDetails failed", ex);
            sb.addMessage("Error", "Failed to start from details: " + ex.getMessage());
        }
    }

    public void startFromTemplate(HashMap<String, Object> wfTemplate) {
        // === Record a workflow RUN row with status "pending" ===
        try {
            // 1) Gather fields
            final String email = fub.getEmail();
            final String module = sb.getAnalType();//;(String) selectedWorkflow.getOrDefault("module", sb.getAnalType());
            String wfName = String.valueOf(fbc.getFireDocName());
            String description = String.valueOf(fbc.getFireDocDescription());

            if (wfName == null || wfName.isEmpty()) {
                wfName = "WorkflowRun";
            }

            if (description.isEmpty()) {
                description = "None";
            }
            // Prefer a stable identifier if present; otherwise fall back to template filename
            final String workflowId = String.valueOf(selectedWorkflow.getOrDefault("id", ""));

            // Dataset info: dataset_id column in workflow_runs is INTEGER; our DatasetRow uses UUID,
            // so we only pass datasetName (title) here to avoid type mismatches.
            UUID datasetId = ds.getSelected().getId();
            String datasetName = null;
            if (ds != null && ds.getSelected() != null) {
                datasetName = ds.getSelected().getTitle();
            }

            // Optional "other" JSON payload with a few useful bits
            String otherJson = new com.google.gson.Gson().toJson(
                    new java.util.LinkedHashMap<String, Object>() {
                {
                    put("dataType", sb.getDataType());
                    put("dataFormat", sb.getDataFormat());
                    put("toolLocation", ab.getToolLocation());
                }
            }
            );

            // 2) Insert with status "pending"; no start/finish times yet
            String insertMsg = dbc.insertWorkflowRun(
                    email,
                    module,
                    wfName,
                    description,
                    "pending",
                    null, // startDateIso
                    null, // finishDateIso
                    workflowId,
                    datasetId, // null on purpose; see note above
                    datasetName,
                    otherJson,
                    ab.getAppName()
            );

            /*
            var runs = dbc.getAllWorkflowRuns(module, email); 
            String wfId = String.valueOf(selectedWorkflow.getOrDefault("id", selectedWorkflow.get("filename")));
            String dsName = ds.getSelected().getTitle();

            for (HashMap<String, Object> r : runs) {
                String status = String.valueOf(r.get("status"));
                String rid = String.valueOf(r.get("workflowId"));
                String rds = String.valueOf(r.get("datasetId"));
                if ("pending".equalsIgnoreCase(status)
                        && Objects.equals(rid, wfId)
                        && Objects.equals(rds, dsName)) {
                    setActiveWorkflowRunId(((Number) r.get("id")).intValue());
                    break;
                }
            }
             */
            DataUtils.doRedirectWithGrowl(sb,
                    "/" + ab.getAppName() + "/Secure/xialabpro/ProjectView.xhtml?faces-redirect=true&tabWidgetId=tabWidget&activeInx=1",
                    "info",
                    "Workflow loaded successfully! You can proceed to run the workflow.");

        } catch (Exception e) {
            Logger.getLogger(WorkflowBean.class.getName())
                    .log(Level.SEVERE, "insert workflow_run (pending) failed", e);
            sb.addMessage("Warn", "Could not record workflow run: " + e.getMessage());
        }
// === end insert workflow run ===

    }

    // === HELPERS ===
    /**
     * Central mapping: module -> input block label
     */
    private static final Map<String, String> INPUT_BY_MODULE = Map.ofEntries(
            Map.entry("stat", "Generic Table"),
            Map.entry("roc", "Generic Table"),
            Map.entry("dose", "Generic Table"),
            Map.entry("mf", "Generic Table"),
            // If plural is intentional, keep it; otherwise change to "Generic Table"
            Map.entry("metadata", "Generic Tables"),
            Map.entry("pathqea", "Compound Table"),
            Map.entry("msetqea", "Compound Table"),
            Map.entry("msetora", "Metabolite List"),
            Map.entry("pathora", "Metabolite List"),
            Map.entry("metapaths", "Peak Tables"),
            Map.entry("raw", "LC-MS Spectra"),
            Map.entry("network", "Gene List")
    );

    private String resolveInputForModule(String module, String dataType) {
        if (module == null) {
            return "Generic Table";
        }

        final String m = module.trim().toLowerCase(Locale.ROOT);

        if (m.equals("mummichog")) {
            final String dt = dataType == null ? "" : dataType.trim().toLowerCase(Locale.ROOT);
            return dt.equals("mass_all") ? "Peak List" : "Peak Table";
        }

        return INPUT_BY_MODULE.getOrDefault(m, "Generic Table");
    }

    /**
     * Apply input to the diagram: select proper node and disable unavailable
     * analytics.
     */
    private void initializeDiagramForInput(String input) {
        dv.selectInputNode(input);
        dv.disableAnalNodes(input);
        String nodeName = dv.convertToBlockName(input);
        dv.selectNode(nodeName, true);
    }

    /**
     * Shared tail: set active tab, infer meta/data names, etc.
     */
    private void postLoadCommon() {
        activeIndex = 2;

        // attach metadata/data names if available
        final String metaNm = ds.resolveRoleFilename(ds.getSelected(), "metadata");
        if (metaNm != null) {
            setMetaName(metaNm);
        }
        setDataName(ds.resolveRoleFilename(ds.getSelected(), "data"));

        Path home = Paths.get(sb.getCurrentUser().getHomeDir());

        // --- Ensure/attach mSetObj_after_sanity.qs ---
        //setLblType(sb.isRegression() ? "cont" : "disc");
    }

    /**
     * Copy template into user's working folder as workflow.json (user folder
     * overrides example).
     */
    private Path prepareTemplateWorkflowJson(String fileName) throws IOException {
        Objects.requireNonNull(fileName, "Template filename is required");
        if (sb.getCurrentUser() == null) {
            DatasetRow sel = ds.findById(selectedWorkflowRun.getDatasetId()); 
            ds.setSelected(sel);
            boolean res = ds.load(ds.getSelected(), true);
            if (res) {
                boolean res2 = ds.handleDataset(ds.getSelected());
            }
            //sb.addMessage("warn", "Please select a dataset in 'Data Center' first.");
        }
        final String destDirPath = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";
        final Path destPath = Paths.get(destDirPath, "workflow.json");

        final String userPath = fbb.getProjectPath() + "/user_folders/" + fub.getEmail() + "/" + fileName;
        final String examplePath = ab.getResourceDir() + "/pro/example_workflows/" + fileName;

        LOGGER.info(() -> "userPath====" + userPath);
        LOGGER.info(() -> "examplePath====" + examplePath);

        File srcFile = new File(userPath);
        if (!srcFile.exists()) {
            srcFile = new File(examplePath);
        }
        if (!srcFile.exists()) {
            throw new IOException("Workflow template not found in either user folder or example directory.");
        }

        Files.createDirectories(destPath.getParent());
        Files.copy(srcFile.toPath(), destPath, StandardCopyOption.REPLACE_EXISTING);
        return destPath;
    }

    public void deleteSelected() {
        String res = dbc.deleteWorkflowById("" + selectedWorkflow.get("id"));
        sb.addMessage("info", res);
        loadAllWorkflows();
    }

    public StreamedContent getDownloadSelected() {
        try {
            if (selectedWorkflow == null) {
                sb.addMessage("Error", "No workflow selected.");
                return null;
            }

            // Resolve the stored JSON (same location you used in startFromTemplate)
            String fileInBucket = (String) selectedWorkflow.get("filename"); // e.g. "my_workflow.json"
            if (fileInBucket == null || fileInBucket.isBlank()) {
                sb.addMessage("Error", "Selected workflow has no filename.");
                return null;
            }

            // e.g. <projectPath>/user_folders/<email>/<filename>
            Path wfPath = Paths.get(
                    fbb.getProjectPath(), "user_folders", fub.getEmail(), fileInBucket
            ).normalize();

            if (!Files.exists(wfPath)) {
                sb.addMessage("Error", "File not found: " + wfPath.getFileName());
                return null;
            }

            String downloadName = fileInBucket; // use original filename
            String contentType = Files.probeContentType(wfPath);
            if (contentType == null) {
                // JSON is safest guess here; browsers will still prompt “Save as”
                contentType = "application/json";
            }

            // Important: stream supplier must reopen each time; don't keep a single open stream
            return DefaultStreamedContent.builder()
                    .name(downloadName)
                    .contentType(contentType)
                    .stream(() -> {
                        try {
                            return Files.newInputStream(wfPath);
                        } catch (Exception ex) {
                            // If something goes wrong mid-download, log and surface a friendly message
                            Logger.getLogger(WorkflowBean.class.getName())
                                    .log(Level.SEVERE, "downloadSelected/open", ex);
                            sb.addMessage("Error", "Unable to open the workflow for download.");
                            return InputStream.nullInputStream();
                        }
                    })
                    .build();

        } catch (Exception e) {
            Logger.getLogger(WorkflowBean.class.getName())
                    .log(Level.SEVERE, "downloadSelected", e);
            sb.addMessage("Error", "Unable to prepare download: " + e.getMessage());
            return null;
        }
    }

    public StreamedContent getDownloadSelectedExample() {
        try {
            if (selectedWorkflow == null) {
                sb.addMessage("Error", "No workflow selected.");
                return null;
            }

            // Resolve the stored JSON (same location you used in startFromTemplate)
            String fileInBucket = (String) selectedWorkflow.get("filename") + ".json"; // e.g. "my_workflow.json"

            // e.g. <projectPath>/user_folders/<email>/<filename>
            Path wfPath = Paths.get(sb.getCurrentUser().getRelativeDir() + "/" + fileInBucket
            ).normalize();

            if (!Files.exists(wfPath)) {
                sb.addMessage("Error", "File not found: " + wfPath.getFileName());
                return null;
            }

            String downloadName = fileInBucket; // use original filename
            String contentType = Files.probeContentType(wfPath);
            if (contentType == null) {
                // JSON is safest guess here; browsers will still prompt “Save as”
                contentType = "application/json";
            }

            // Important: stream supplier must reopen each time; don't keep a single open stream
            return DefaultStreamedContent.builder()
                    .name(downloadName)
                    .contentType(contentType)
                    .stream(() -> {
                        try {
                            return Files.newInputStream(wfPath);
                        } catch (Exception ex) {
                            // If something goes wrong mid-download, log and surface a friendly message
                            Logger.getLogger(WorkflowBean.class.getName())
                                    .log(Level.SEVERE, "downloadSelected/open", ex);
                            sb.addMessage("Error", "Unable to open the workflow for download.");
                            return InputStream.nullInputStream();
                        }
                    })
                    .build();

        } catch (Exception e) {
            Logger.getLogger(WorkflowBean.class.getName())
                    .log(Level.SEVERE, "downloadSelected", e);
            sb.addMessage("Error", "Unable to prepare download: " + e.getMessage());
            return null;
        }
    }
    private static final Logger LOGGER = Logger.getLogger(WorkflowBean.class.getName());
    private String selectedWorkflowJson = "// Loading workflow JSON…";

    public void loadSelectedWorkflowJson() {
        try {
            if (sb.getCurrentUser() == null) {
                selectedWorkflowJson = "// Please start an analysis session first.";
                sb.addMessage("Warn", "Please select a dataset before starting a workflow!");
                return;
            }

            Path jsonPath = resolveWorkflowJsonPath(); // tries both approaches + home

            if (jsonPath == null || !Files.exists(jsonPath)) {
                selectedWorkflowJson = "// Workflow JSON not found in any known location.";
                sb.addMessage("Warn", "Workflow JSON not found.");
                return;
            }

            // Pretty-print JSON
            Map<String, FunctionInfo> funcs = DataUtils.loadFunctionInfosFromFile(jsonPath + "");
            setFunctionInfos(funcs);
            selectedWorkflowJson = readAndPrettyPrintJson(jsonPath);

        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, "Error loading workflow JSON", ex);
            selectedWorkflowJson = "// Error loading JSON:\n" + ex.getMessage();
            sb.addMessage("Error", "Failed to load workflow JSON: " + ex.getMessage());
        }
    }

    /**
     * Resolve JSON by checking (1) project storage, (2) example storage, (3)
     * user home copy.
     */
    private Path resolveWorkflowJsonPath() {
        String userHomeDir = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";
        Path homeJson = Paths.get(userHomeDir, "workflow.json").normalize();

        // If already present in home, prefer that (fast path)
        if (Files.exists(homeJson)) {
            return homeJson;
        }

        // We need a filename to try centralized locations
        String filename = null;
        if (selectedWorkflow != null) {
            Object fn = selectedWorkflow.get("filename");
            if (fn instanceof String && !((String) fn).isBlank()) {
                filename = (String) fn;
            }
        }
        if (filename == null) {
            // Nothing to resolve, just return null
            return null;
        }

        // 1) Primary (project storage): <projectPath>/user_folders/<email>/<filename>
        Path projectPath = null;
        try {
            if (fbb != null && fub != null && fub.getEmail() != null) {
                String bucketObjectName = "/user_folders/" + fub.getEmail() + "/" + filename;
                projectPath = Paths.get(fbb.getProjectPath() + bucketObjectName).normalize();
                if (Files.exists(projectPath)) {
                    // Optionally copy to home for consistency
                    copyToHome(projectPath, homeJson);
                    return homeJson; // return the canonical home copy
                }
            }
        } catch (Exception e) {
            LOGGER.log(Level.FINE, "Project storage check failed", e);
        }

        // 2) Example (resource) storage: <ab.getResourceDir()>/pro/<filename>
        try {
            Path examplePath = Paths.get(ab.getResourceDir(), "pro/example_workflows/", filename).normalize();
            if (Files.exists(examplePath)) {
                copyToHome(examplePath, homeJson);
                return homeJson;
            }
        } catch (Exception e) {
            LOGGER.log(Level.FINE, "Example storage check failed", e);
        }

        // 3) Final fallback: if home was created by other means between checks
        return Files.exists(homeJson) ? homeJson : null;
    }

    private void copyToHome(Path src, Path homeJson) {
        try {
            Files.createDirectories(homeJson.getParent());
            Files.copy(src, homeJson, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ex) {
            LOGGER.log(Level.WARNING, "Failed to copy workflow JSON to home: " + homeJson, ex);
            // If copy fails, we can still read from src directly if desired.
            // In this implementation we prefer a single canonical location, so we just warn.
        }
    }

    private String readAndPrettyPrintJson(Path path) throws IOException {
        String raw = Files.readString(path, StandardCharsets.UTF_8);
        ObjectMapper om = new ObjectMapper();
        Object parsed = om.readValue(raw, Object.class);
        return om.writerWithDefaultPrettyPrinter().writeValueAsString(parsed);
    }

    /**
     * Bind to
     * <pre>/<textarea> viewer */
    public String getSelectedWorkflowJson() {
        return selectedWorkflowJson;
    }

    public String goToWorkflowDetails() throws IOException {

        final String fileName = (String) selectedWorkflow.get("filename");
        final Path destPath = prepareTemplateWorkflowJson(fileName);

        Map<String, FunctionInfo> functionInfos = DataUtils.loadFunctionInfosFromFile(destPath.toString());
        setFunctionInfos(functionInfos);
        editMode = true;
        return "WorkflowDetails";
    }

    public String goToWorkflowOverview() {
        activeIndex = 0;
        return "WorkflowView";
    }

    public String goToWorkflowExec() {
        activeIndex = 2;
        return "WorkflowView";
    }

    public boolean isDisplayParams() {
        String input = resolveInputForModule((String) selectedWorkflow.get("module"), sb.getDataType());
        System.out.println(input + "==========input");
        if (input.equals("Generic Table") || input.equals("Peak Table") || input.equals("Compound Table") || input.equals("LC-MS Spectra")) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isDisplayEnrichParams() {
        String input = (String) selectedWorkflow.get("module");
        System.out.println("input========" + input);
        if (input.equals("msetora") || input.equals("qeaora")) {
            return true;
        } else {
            return false;
        }
    }

    private List<RunPlan> runPlans = new ArrayList<>();

    public List<RunPlan> getRunPlans() {
        return runPlans;
    }

    @SuppressWarnings("unchecked")
    public void setRunPlans(Object rawRunPlans) {
        if (rawRunPlans == null) {
            this.runPlans = new ArrayList<>();
            return;
        }

        // Normalize input → Collection<?>
        Collection<?> in;
        if (rawRunPlans instanceof Collection) {
            in = (Collection<?>) rawRunPlans;
        } else if (rawRunPlans instanceof Object[]) {
            in = Arrays.asList((Object[]) rawRunPlans);
        } else {
            // Single item case: try to wrap
            in = List.of(rawRunPlans);
        }

        // Fast path: if they are all RunPlan already, just collect (but re-order deterministically)
        boolean allRunPlans = in.stream().allMatch(o -> o instanceof RunPlan);
        if (allRunPlans) {
            this.runPlans = in.stream()
                    .map(o -> (RunPlan) o)
                    .sorted(RUNPLAN_COMPARATOR)
                    .collect(Collectors.toList());
            return;
        }

        // Else convert from LinkedHashMap / mixed content
        populateRunPlans(in);
    }

    private static final Comparator<RunPlan> RUNPLAN_COMPARATOR = Comparator
            .comparing((RunPlan rp) -> nullSafe(rp.getModuleName()))
            .thenComparing(rp -> nullSafe(rp.getParams() != null ? rp.getParams().getFolderName() : null))
            .thenComparing(rp -> nullSafe(rp.getOrigNaviType()));

    private static String nullSafe(String s) {
        return s == null ? "~zzz_null~" : s;
    }

    @SuppressWarnings("unchecked")
    private void populateRunPlans(Collection<?> deserialized) {
        List<RunPlan> converted = new ArrayList<>();

        for (Object item : deserialized) {
            if (item instanceof RunPlan) {
                converted.add((RunPlan) item);
                continue;
            }

            if (item instanceof Map) {
                Map<String, Object> m = (Map<String, Object>) item;

                // Expect keys: "moduleName", "origNaviType", "params"
                String moduleName = safeGetString(m.get("moduleName"));
                String origNaviType = safeGetString(m.get("origNaviType"));

                WorkflowParameters params = null;
                Object p = m.get("params");
                if (p != null) {
                    params = convertToWorkflowParameters(p);
                }

                converted.add(new RunPlan(params, moduleName, origNaviType));
            }
            // else: ignore unknown shapes
        }

        // Deterministic ordering
        converted.sort(RUNPLAN_COMPARATOR);
        this.runPlans = converted;
    }
    // Accepts either an actual WorkflowParameters or a Map (LinkedHashMap) representation

    @SuppressWarnings("unchecked")
    private WorkflowParameters convertToWorkflowParameters(Object obj) {
        if (obj == null) {
            return null;
        }

        if (obj instanceof WorkflowParameters) {
            return (WorkflowParameters) obj;
        }

        if (obj instanceof Map) {
            Map<String, Object> map = (Map<String, Object>) obj;

            boolean removeMissing = safeGetBool(map.get("removeMissing"));
            String missingImputeOpt = safeGetString(map.get("missingImputeOpt"));
            String replaceVarOpt = safeGetString(map.get("replaceVarOpt"));
            String imputeAlgOpt = safeGetString(map.get("imputeAlgOpt"));

            boolean doQCFiltering = safeGetBool(map.get("doQCFiltering"));
            int qcCutoff = safeGetInt(map.get("qcCutoff"));
            String varFilterOpt = safeGetString(map.get("varFilterOpt"));
            int filterCutoff = safeGetInt(map.get("filterCutoff"));
            String intFilterOpt = safeGetString(map.get("intFilterOpt"));
            int intFilterCutoff = safeGetInt(map.get("intFilterCutoff"));

            String rowNormOpt = safeGetString(map.get("rowNormOpt"));
            String transNormOpt = safeGetString(map.get("transNormOpt"));
            String scaleNormOpt = safeGetString(map.get("scaleNormOpt"));
            String folderName = safeGetString(map.get("folderName"));

            return new WorkflowParameters(
                    removeMissing, missingImputeOpt, replaceVarOpt, imputeAlgOpt,
                    doQCFiltering, qcCutoff, varFilterOpt, filterCutoff,
                    intFilterOpt, intFilterCutoff, rowNormOpt, transNormOpt, scaleNormOpt, folderName
            );
        }

        // Unknown shape → return null; caller decides whether to add plan without params
        return null;
    }

    /* ---------- Primitive extraction helpers ---------- */
    private static String safeGetString(Object o) {
        return (o == null) ? null : String.valueOf(o);
    }

    private static boolean safeGetBool(Object o) {
        if (o instanceof Boolean) {
            return (Boolean) o;
        }
        if (o instanceof String) {
            return Boolean.parseBoolean((String) o);
        }
        if (o instanceof Number) {
            return ((Number) o).intValue() != 0;
        }
        return false;
    }

    private static int safeGetInt(Object o) {
        if (o instanceof Number) {
            return ((Number) o).intValue();
        }
        if (o instanceof String) {
            try {
                return Integer.parseInt(((String) o).trim());
            } catch (Exception ignored) {
            }
        }
        return 0;
    }

// Classify data types into input kinds
    private enum InputKind {
        GENERIC_TABLE, COMPOUND_TABLE, PEAK_TABLE, PEAK_LIST, LCMS_SPECTRA, METABOLITE_LIST
    }

    private boolean hasSelectedDataset() {
        try {
            return ds.getSelected() != null;
        } catch (Exception e) {
            return false;
        }
    }

// Families (modules, not datatypes)
    private static final Set<String> TABLE_FAMILY = Set.of(
            "stat", "roc", "dose", "mf", "raw", "power", "mummichog", "pathqea", "msetqea", "pathway", "enrich"
    );
    private static final Set<String> LIST_FAMILY = Set.of(
            "pathora", "msetora", "pathway", "enrich"
    );

// (reuse your existing sets if you want the extra checks too)
    private static final Set<String> UNTARGETED_DATAS = Set.of("spec", "specbin", "pktable", "nmrpeak", "mspeak", "table");
    private static final Set<String> REGRES_ANALS = Set.of("pathway", "enrich");
    private static final Set<String> RUN_STATUS_ALLOWED = Set.of("pending", "running", "completed", "failed");

    public boolean workflowCompatible(Map<String, Object> wf) {
        final String TAG = "[workflowCompatible]";
        try {
            // dataset selection status
            if (ds.getSelected() == null) {
                System.out.println(TAG + " ds.selected = null -> TRUE (no dataset selected, allow all)");
                return true;
            }

            final String dsModule = safeStr(ds.getSelected().getModule());
            final String wfModule = safeStr(wf.get("module"));
            final String dataType = safeStr(sb.getDataType());
            final boolean regression = sb.isRegression();

            System.out.println(TAG + " dsModule=" + dsModule
                    + ", wfModule=" + wfModule
                    + ", dataType=" + dataType
                    + ", regression=" + regression);

            // 1) family rule
            final boolean dsInTable = TABLE_FAMILY.contains(dsModule);
            final boolean wfInTable = TABLE_FAMILY.contains(wfModule);
            final boolean dsInList = LIST_FAMILY.contains(dsModule);
            final boolean wfInList = LIST_FAMILY.contains(wfModule);

            final boolean sameTableFamily = dsInTable && wfInTable;
            final boolean sameListFamily = dsInList && wfInList;

            if (!(sameTableFamily || sameListFamily)) {
                return false;
            }

            // 2) mass_all constraint -> only mummichog
            if ("mass_all".equals(dataType) && !"mummichog".equals(wfModule)) {
                return false;
            }

            // 3) regression restriction
            if (regression && !REGRES_ANALS.contains(wfModule)) {
                return false;
            }

            // 4) QEA requires conc (and later also ID mapping)
            if ("pathqea".equals(wfModule) || "msetqea".equals(wfModule)) {
                if (!"conc".equalsIgnoreCase(dataType)) {
                    return false;
                }
            }

            // 5) mummichog acceptable data types
            if ("mummichog".equals(wfModule)) {
                boolean ok = "mass_all".equalsIgnoreCase(dataType)
                        || "mass_table".equalsIgnoreCase(dataType)
                        || "pktable".equalsIgnoreCase(dataType)
                        || "spec".equalsIgnoreCase(dataType)
                        || "nmrpeak".equalsIgnoreCase(dataType)
                        || "mspeak".equalsIgnoreCase(dataType);
                if (!ok) {
                    return false;
                }
            }

            // 6) ORA modules need ID mapping
            if ("pathora".equals(wfModule) || "msetora".equals(wfModule)) {

            }

            // 7) QEA: need conc + ID mapping (both)
            if ("pathqea".equals(wfModule) || "msetqea".equals(wfModule)) {
                if (!"conc".equalsIgnoreCase(dataType)) {
                    // (duplicate protection; kept for explicit logging)
                    return false;
                }

            }

            return true;

        } catch (Exception e) {
            System.out.println(TAG + " WARNING: Exception -> fail-open TRUE. " + e.getClass().getSimpleName() + ": " + e.getMessage());
            return true; // keep your fail-open behavior
        }
    }

    public String incompatReason(Map<String, Object> wf) {
        try {
            if (ds.getSelected() == null) {
                return "";
            }

            final String dsModule = safeStr(ds.getSelected().getModule());
            final String wfModule = safeStr(wf.get("module"));

            final boolean sameTableFamily = TABLE_FAMILY.contains(dsModule) && TABLE_FAMILY.contains(wfModule);
            final boolean sameListFamily = LIST_FAMILY.contains(dsModule) && LIST_FAMILY.contains(wfModule);
            if (!(sameTableFamily || sameListFamily)) {
                return "This workflow is not compatible with the current dataset family (list vs. table).";
            }

            // Extra, more specific messages (optional but helpful)
            String dataType = safeStr(sb.getDataType());
            if (wfModule.equals("mummichog")) {
                dataType = safeStr(sb.getUploadType());
            }

            if (sb.isRegression() && !REGRES_ANALS.contains(wfModule)) {
                return "This module does not support continuous class labels.";
            }
            if ("pathqea".equals(wfModule) || "msetqea".equals(wfModule)) {
                if (!"conc".equalsIgnoreCase(dataType)) {
                    return "Requires a quantified compound table (concentration).";
                }
            }
            if ("mummichog".equals(wfModule)) {
                if (!("spec".equalsIgnoreCase(dataType) || "mass_table".equalsIgnoreCase(dataType))) {
                    return "Functional Analysis requires raw spectra or a generic mass feature table (m/z & RT).";
                }
            }
            if ("pathora".equals(wfModule) || "msetora".equals(wfModule)) {

            }
            if ("pathqea".equals(wfModule) || "msetqea".equals(wfModule)) {
                if (!"conc".equalsIgnoreCase(dataType)) {
                    return "Requires a quantified compound table (concentration).";
                }

            }
            return "";
        } catch (Exception e) {
            return "Compatibility could not be determined.";
        }
    }

    private static String safeStr(Object o) {
        return (o == null) ? "" : String.valueOf(o);
    }

    private String trimToNull(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private String normalizeRunStatus(String status) {
        String trimmed = trimToNull(status);
        if (trimmed == null) {
            return null;
        }
        String canonical = trimmed.toLowerCase(Locale.ROOT);
        return RUN_STATUS_ALLOWED.contains(canonical) ? canonical : null;
    }

    @JsonIgnore
    private Integer activeWorkflowRunId;

    public Integer getActiveWorkflowRunId() {
        return activeWorkflowRunId;
    }

    public void setActiveWorkflowRunId(Integer id) {
        this.activeWorkflowRunId = id;
    }

    public void prepareNewRun() {
        try {
            String email = fub.getEmail();
            String tool = ab.getAppName(); // or a constant like "MetaboAnalyst"

            // Placeholders:
            String module = "NA";              // user will choose later
            final String name = java.time.LocalDateTime.now()
                    .format(java.time.format.DateTimeFormatter.ofPattern("'New run — 'yyyy-MM-dd HH:mm"));
            String description = "NA";
            String status = "pending";
            String startDateIso = null;              // or Instant.now().toString()
            String finishDateIso = null;
            String workflowId = null;
            UUID datasetId = null;
            String datasetName = "NA";
            String other = "{}";              // MUST be JSON (see step 1)

            // Use your existing function as-is:
            String newId = dbc.insertWorkflowRun(
                    email, module, name, description, status,
                    startDateIso, finishDateIso, workflowId,
                    datasetId, datasetName, other, tool
            );

            // Refresh table & select the new row
            setupWorkflowRunsTable();
            this.selectedWorkflowRun = findRunInModelById(Integer.parseInt(newId));

            sb.addMessage("info", "New run (" + newId + ") created. Pick a dataset and workflow first.");

        } catch (Exception e) {
            sb.addMessage("error", "Error creating run.");

        }
    }

    // Returns the first match, or null if not found
    public WorkflowRunModel findRunInModelById(int id) {
        final List<WorkflowRunModel> list = getWorkflowRunsTable();
        if (list == null || list.isEmpty()) {
            return null;
        }

        for (WorkflowRunModel r : list) {
            if (r.getId() == id) {
                return r;
            }
        }
        return null;
    }

    public void startRun(WorkflowRunModel run) {
        try {
            if (ds.getDatasetTableAll().isEmpty()) {
                ds.reloadTable();
            }

            if (workflowList.isEmpty()) {
                loadAllWorkflows();
            }

            this.selectedWorkflowRun = run;
            calledWorkflows.add("Data Preparation");

            editMode = false;
            boolean success = selectWorkflowById(getSelectedWorkflowRun().getWorkflowId());
            if (!success) {
                return;
            }
            ds.setSelected(ds.findById(getSelectedWorkflowRun().getDatasetId()));

            boolean res = ds.load(ds.getSelected(), true);

            if (res) {
                boolean res2 = ds.handleDataset(ds.getSelected());
            }
            final String module = sb.getAnalType();//(String) selectedWorkflow.get("module");
            final String input = resolveInputForModule(module, sb.getDataType());

            postLoadCommon();

            initializeDiagramForInput(input);

            reloadingWorkflow = false;
            wf.generateWorkflowJson("project", false);   // only in "details"
            if (moduleNames.isEmpty()) {
                dv.selectNode(dv.convertToBlockName(module), true);
            } else {
                for (String moduleName : moduleNames) {
                    dv.selectNode(dv.convertToBlockName(moduleName), true);
                }
            }

            //FireProjectBean pb = (FireProjectBean) DataUtils.findBean("fireProjectBean");
            fbc.setFireDocName(run.getName());
            fbc.setFireDocDescription(run.getDescription());
            dv.submitWorkflowJob();

        } catch (Exception ex) {
            Logger.getLogger(WorkflowBean.class.getName())
                    .log(Level.SEVERE, "startFromDetails failed", ex);
            sb.addMessage("Error", "Failed to start from details: " + ex.getMessage());
        }
    }

    public boolean selectWorkflowById(int id) {
        // Ensure lists are loaded
        if (workflowList == null) {
            loadAllWorkflows();
        }

        // 1) Try to find it in the already-loaded custom workflows
        if (workflowList != null) {
            for (HashMap<String, Object> wf : workflowList) {
                Object v = wf.get("id");
                if (v != null) {
                    try {
                        int vid = (v instanceof Number) ? ((Number) v).intValue() : Integer.parseInt(v.toString());
                        if (vid == id) {
                            selectedWorkflow = wf;
                            return true;
                        }
                    } catch (NumberFormatException ignore) {
                    }
                }
            }
        }

        if (defaultWorkflowList != null) {
            for (HashMap<String, Object> wf : defaultWorkflowList) {
                Object v = wf.get("id");
                if (v != null) {
                    try {
                        int vid = (v instanceof Number) ? ((Number) v).intValue() : Integer.parseInt(v.toString());
                        if (vid == id) {
                            selectedWorkflow = wf;
                            return true;
                        }
                    } catch (NumberFormatException ignore) {
                    }
                }
            }
        }

        // 3) Not found
        sb.addMessage("warn", "No workflow found with id = " + id);
        return false;
    }

// Convenience overload if id comes in as String
    public boolean selectWorkflowById(String idStr) {
        try {
            return selectWorkflowById(Integer.parseInt(idStr));
        } catch (Exception e) {
            sb.addMessage("warn", "Invalid workflow id: " + idStr);
            return false;
        }
    }

    private WorkflowRunModel selectedWorkflowRun;

    public WorkflowRunModel getSelectedWorkflowRun() {
        return selectedWorkflowRun;
    }

    public void setSelectedWorkflowRun(WorkflowRunModel selectedWorkflowRun) {
        this.selectedWorkflowRun = selectedWorkflowRun;
        if (this.selectedWorkflowRun != null) {
            String normalized = normalizeRunStatus(this.selectedWorkflowRun.getStatus());
            this.selectedWorkflowRun.setStatus(normalized);
        }
    }
// Simple filters (optional)
    private String dsFilter, wfFilter;

    public String getDsFilter() {
        return dsFilter;
    }

    public void setDsFilter(String s) {
        dsFilter = s;
    }

    public String getWfFilter() {
        return wfFilter;
    }

    public void setWfFilter(String s) {
        wfFilter = s;
    }

    public void prepareDialogByDataset(WorkflowRunModel run) {

        ds.reloadTable();
        this.selectedWorkflowRun = run;
        this.dsFilter = ""; // clear filter if you like
    }

    private static final Pattern UUID_RE
            = Pattern.compile("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");

    private boolean isBlank(String s) {
        return s == null || s.trim().isEmpty() || "NA".equalsIgnoreCase(s.trim());
    }

    private boolean isUuid(String s) {
        return s != null && UUID_RE.matcher(s.trim()).matches();
    }

    public void prepareDialogByWorkflow(WorkflowRunModel run) {
        this.selectedWorkflowRun = run;
        this.wfFilter = "";

        // ---- Guard: dataset UUID required
        String dsId = (run == null) ? null : run.getDatasetId().toString();  // assuming model.getDatasetId() returns String UUID
        boolean datasetMissing = isBlank(dsId) || !isUuid(dsId);

        if (datasetMissing) {
            sb.addMessage("warn",
                    "Click the Dataset column to choose a dataset, then select a workflow.");
            return;
        }

        // Ensure dataset table is ready
        if (ds.getDatasetTableAll().isEmpty()) {
            ds.reloadTable();
        }

        // Lookup dataset by UUID (adjust to your service API)
        // Prefer a UUID/String overload; if you only have by-int, create a new one.
        DatasetRow sel = ds.findById(run.getDatasetId()); // <-- implement this if not present

        if (sel == null) {
            sb.addMessage("warn",
                    "The selected dataset is missing. Please pick a dataset again.");
            PrimeFaces.current().ajax().addCallbackParam("ok", false);
            return;
        } else {
            ds.setSelected(sel);
        }

        // Load & filter workflows
        loadAllWorkflows();
        if (workflowList != null && !workflowList.isEmpty()) {
            ArrayList<HashMap<String, Object>> compatible = new ArrayList<>(workflowList.size());
            for (HashMap<String, Object> wf : workflowList) {
                if (workflowCompatible(wf)) {
                    compatible.add(wf);
                }
            }
            workflowList = compatible;

            if (selectedWorkflow != null && (workflowList == null || !workflowList.contains(selectedWorkflow))) {
                selectedWorkflow = workflowList.isEmpty() ? null : workflowList.get(0);
            }
        }
    }

    // In WorkflowBean.java
    public void selectDatasetOnly(DatasetRow dr) {
        ds.setSelected(dr);

        if (selectedWorkflowRun == null) {
            sb.addMessage("Warn", "No target run selected.");
            return;
        }

        // Update local model
        selectedWorkflowRun.setDatasetId(dr.getId());
        selectedWorkflowRun.setDatasetName(dr.getTitle());

        // Persist to DB (dataset_id UUID + dataset_name)
        try {
            Map<String, Object> updates = new HashMap<>();
            updates.put("dataset_id", dr.getId() == null ? null : dr.getId().toString());
            updates.put("dataset_name", (dr.getTitle() == null || dr.getTitle().isBlank()) ? null : dr.getTitle());

            String msg = dbc.updateWorkflowRunFields(String.valueOf(selectedWorkflowRun.getId()), updates);
            if (msg.contains("\"updated\":true")) {
                sb.addMessage("Info", "Dataset saved.");
            } else {
                sb.addMessage("Warn", "Dataset set locally, but DB update failed: " + msg);
            }
        } catch (Exception e) {
            sb.addMessage("Error", "Failed to update dataset: " + e.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    public void selectWorkflowOnly(HashMap<String, Object> wf) {
        this.selectedWorkflow = wf;

        if (selectedWorkflowRun == null) {
            sb.addMessage("Warn", "No target run selected.");
            return;
        }

        // Extract fields from your workflow list item
        Object wfIdObj = wf.get("id");                            // your unique workflow ID
        String wfId = wfIdObj == null ? null : wfIdObj.toString();
        String module = wf.get("module") == null ? null : String.valueOf(wf.get("module"));
        String name = wf.getOrDefault("name", "") == null ? null : String.valueOf(wf.getOrDefault("name", "")).trim();
        if (name != null && name.isEmpty()) {
            name = null;
        }

        // Update local model
        selectedWorkflowRun.setWorkflowId(wfId);
        selectedWorkflowRun.setModule(module);
        selectedWorkflowRun.setName(name);

        // Persist to DB (workflow_id + module + name)
        try {
            Map<String, Object> updates = new HashMap<>();
            updates.put("workflow_id", wfId);
            updates.put("module", module);
            //updates.put("name", name);

            String msg = dbc.updateWorkflowRunFields(String.valueOf(selectedWorkflowRun.getId()), updates);
            if (msg.contains("\"updated\":true")) {
                sb.addMessage("Info", "Workflow saved.");
            } else {
                sb.addMessage("Warn", "Workflow set locally, but DB update failed: " + msg);
            }
        } catch (Exception e) {
            sb.addMessage("Error", "Failed to update workflow: " + e.getMessage());
        }
    }

    public void updateRunInfo() {
        if (selectedWorkflowRun == null) {
            sb.addMessage("Warn", "No workflow run selected.");
            PrimeFaces pf = PrimeFaces.current();
            if (pf != null) {
                pf.ajax().addCallbackParam("runUpdateSuccess", false);
            }
            return;
        }

        WorkflowRunModel run = selectedWorkflowRun;
        final int runId = run.getId();

        String sanitizedName = trimToNull(run.getName());
        String sanitizedDescription = trimToNull(run.getDescription());

        Map<String, Object> updates = new HashMap<>();
        updates.put("name", sanitizedName);
        updates.put("description", sanitizedDescription);

        try {
            String msg = dbc.updateWorkflowRunFields(String.valueOf(runId), updates);
            boolean success = isUpdateSuccess(msg);

            PrimeFaces pf = PrimeFaces.current();
            if (pf != null) {
                pf.ajax().addCallbackParam("runUpdateSuccess", success);
            }

            if (success) {
                setupWorkflowRunsTable();
                this.selectedWorkflowRun = findRunInModelById(runId);
                sb.addMessage("Info", "Run " + runId + " updated.");
            } else {
                sb.addMessage("Warn", "Failed to update run " + runId + ": " + msg);
            }
        } catch (Exception e) {
            PrimeFaces pf = PrimeFaces.current();
            if (pf != null) {
                pf.ajax().addCallbackParam("runUpdateSuccess", false);
            }
            sb.addMessage("Error", "Failed to update run: " + e.getMessage());
        }
    }

    private List<WorkflowRunModel> workflowRunsTable = new ArrayList<>();

    public List<WorkflowRunModel> getWorkflowRunsTable() {
        // ensure it's never null for the UI
        if (workflowRunsTable == null) {
            workflowRunsTable = new ArrayList<>();
        }
        return workflowRunsTable;
    }

    public void setWorkflowRunsTable(List<WorkflowRunModel> workflowRunsTable) {
        this.workflowRunsTable = (workflowRunsTable != null) ? workflowRunsTable : new ArrayList<>();
    }

    public void initUserProjects() {
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }

        fbc.reloadUserInfo();

        if (fub.getEmail() == null || fub.getEmail().equals("")) {// on local do not need to login
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "error", "Please login first!");
        } else {
            setupWorkflowRunsTable();
        }

    }

    public boolean setupWorkflowRunsTable() {
        try {
            // Init target table
            if (getWorkflowRunsTable() == null) {
                setWorkflowRunsTable(new ArrayList<>());
            } else {
                getWorkflowRunsTable().clear();
            }

            final String email = fub.getEmail();

            // Fetch from DB/API (DatabaseClient delegates to Docker DB or REST)
            ArrayList<HashMap<String, Object>> res = dbc.getAllWorkflowRuns(ab.getAppName(), email);
            if (res == null) {
                return false;
            }

            boolean autoPickToLoad = false;

            for (HashMap<String, Object> row : res) {
                // Optional status filter

                // Build model
                WorkflowRunModel run = createWorkflowRunFromMap(row);
                getWorkflowRunsTable().add(run);
            }

            return true;

        } catch (Exception e) {
            sb.addMessage("Error", "Failed to load workflow runs!");
            e.printStackTrace();
            return false;
        }
    }

    private WorkflowRunModel createWorkflowRunFromMap(Map<String, Object> m) {
        WorkflowRunModel r = new WorkflowRunModel();

        // Core identifiers
        r.setId(safeIntFromDoubleLike(m.get("id")));                          // DB PK
        r.setWorkflowId(safeStr(getAny(m, "workflowId", "workflow_id")));
        r.setName(safeStr(m.get("name")));                      // display name (if present)
        r.setModule(safeStr(m.get("module")));
        r.setEmail(safeStr(m.get("email")));

        // Dataset linkage (UUID)
        Object dsRaw = getAny(m, "datasetId", "dataset_id");
        r.setDatasetId(safeUUID(dsRaw));                        // <-- UUID now
        r.setDatasetName(safeStr(getAny(m, "datasetName", "dataset_name")));

        // Status & timing
        String statusRaw = trimToNull(safeStr(m.get("status")));
        String statusNormalized = normalizeRunStatus(statusRaw);
        r.setStatus(statusNormalized != null ? statusNormalized : statusRaw);    // pending|running|completed|failed
        r.setStartDate(safeStr(getAny(m, "startDate", "start_date")));
        r.setFinishDate(safeStr(getAny(m, "finishDate", "finish_date")));
        r.setLastUpdated(safeStr(getAny(m, "lastUpdated", "last_updated")));

        // Description & misc
        r.setDescription(trimToNull(safeStr(m.get("description"))));
        r.setOtherJson(safeStr(m.get("other")));                // JSON blob for extra info
        r.setProjectId(trimToNull(safeStr(getAny(m, "projectId", "project_id"))));

        // Convenience: derive a readable title if none provided
        if (r.getName() == null || r.getName().isBlank()) {
            String fallback = r.getWorkflowId();
            if (fallback == null || fallback.isBlank()) {
                fallback = "Run-" + r.getId();
            }
            r.setName(fallback);
        }
        return r;
    }

    private Object getAny(Map<String, Object> m, String... keys) {
        for (String k : keys) {
            if (m.containsKey(k)) {
                return m.get(k);
            }
        }
        return null;
    }

    private UUID safeUUID(Object v) {
        if (v == null) {
            return null;
        }

        if (v instanceof UUID) {
            return (UUID) v;
        }

        if (v instanceof String) {
            String s = ((String) v).trim();
            if (s.isEmpty()) {
                return null;
            }
            try {
                return UUID.fromString(s);
            } catch (IllegalArgumentException ignore) {
                return null;
            }
        }

        // Postgres driver may surface uuid as PGobject
        if (v instanceof PGobject) {
            PGobject pgo = (PGobject) v;
            if ("uuid".equalsIgnoreCase(pgo.getType())) {
                String s = pgo.getValue();
                if (s != null) {
                    try {
                        return UUID.fromString(s);
                    } catch (IllegalArgumentException ignore) {
                    }
                }
            }
            return null;
        }

        // Some drivers return 16-byte arrays for UUID columns
        if (v instanceof byte[]) {
            byte[] b = (byte[]) v;
            if (b.length == 16) {
                ByteBuffer bb = ByteBuffer.wrap(b);
                long high = bb.getLong();
                long low = bb.getLong();
                return new UUID(high, low);
            }
        }

        // Fallback: try toString() parse
        try {
            return UUID.fromString(v.toString());
        } catch (Exception ignore) {
        }
        return null;
    }

    public void onRunCellEdit(CellEditEvent<WorkflowRunModel> event) {
        try {

            // Only handle the Name column
            UIColumn col = event.getColumn();
            String header = col != null && col.getHeaderText() != null ? col.getHeaderText().trim() : "";
            if (!"Name".equalsIgnoreCase(header)) {
                return; // ignore other columns if you add more editable ones later
            }

            WorkflowRunModel run = (WorkflowRunModel) event.getRowData();
            this.selectedWorkflowRun = run;

            Object oldVal = event.getOldValue();
            Object newVal = event.getNewValue();

            String oldName = oldVal == null ? null : String.valueOf(oldVal);
            String newName = newVal == null ? null : String.valueOf(newVal).trim();
            if (Objects.equals(oldName, newName)) {
                return; // nothing changed
            }
            if (newName != null && newName.isEmpty()) {
                newName = null; // treat empty as clearing the name
            }

            // optimistic update (table already shows newName in editor)
            String msg = dbc.updateWorkflowRunFields(String.valueOf(run.getId()), Map.of("name", newName));

            if (isUpdateSuccess(msg)) {
                run.setName(newName);
                sb.addMessage("Info", "Run name saved.");
            } else {
                // revert UI model
                run.setName(oldName);
                sb.addMessage("Warn", "Failed to save name: " + msg);
            }
        } catch (Exception e) {
            sb.addMessage("Error", "Save error: " + e.getMessage());
        }
    }

    private boolean isUpdateSuccess(String msg) {
        if (msg == null) {
            return false;
        }
        String t = msg.trim().toLowerCase();
        if (t.startsWith("ok")) {
            return true;           // direct DB path
        }
        if (t.contains("\"updated\":true")) {
            return true; // REST JSON
        }
        if (t.contains("\"success\":true")) {
            return true;
        }
        if (t.contains("\"status\":\"ok\"")) {
            return true;
        }
        return false;
    }

    private String selectedWorkflowFile;

    public String getSelectedWorkflowFile() {
        return selectedWorkflowFile;
    }

    @SuppressWarnings("unchecked")
    public void viewWorkflowJson() {
        try {
            if (selectedWorkflow == null) {
                sb.addMessage("Warn", "No workflow selected.");
                return;
            }

            final String type = String.valueOf(selectedWorkflow.get("type"));
            final String filename = String.valueOf(selectedWorkflow.get("filename")); // no .json suffix here
            if (filename == null || filename.isBlank()) {
                sb.addMessage("Warn", "Invalid workflow filename.");
                return;
            }

            // Resolve local path (same logic as loadWorkflow)
            final java.nio.file.Path jsonPath = resolveWorkflowJsonPath(selectedWorkflow);
            if (jsonPath == null) {
                sb.addMessage("Error", "Failed to resolve workflow JSON path.");
                return;
            }

            // Read + pretty print
            final String raw = java.nio.file.Files.readString(jsonPath, java.nio.charset.StandardCharsets.UTF_8);
            this.selectedWorkflowJson = prettyJsonOrRaw(raw);
            this.selectedWorkflowFile = jsonPath.getFileName().toString();

            // open dialog
            org.primefaces.PrimeFaces.current().executeScript("PF('wfJsonDlg').show();");
        } catch (Exception e) {
            this.selectedWorkflowJson = "Error loading JSON: " + e.getMessage();
            this.selectedWorkflowFile = "(error)";
            org.primefaces.PrimeFaces.current().executeScript("PF('wfJsonDlg').show();");
        }
    }

    /**
     * Resolves the on-disk JSON file, downloading from bucket if needed,
     * mirroring loadWorkflow()
     */
    private java.nio.file.Path resolveWorkflowJsonPath(java.util.Map<String, Object> wf) throws Exception {
        final String type = wf.get("type") == null ? "" : wf.get("type").toString();
        final String filename = wf.get("filename") == null ? "" : wf.get("filename").toString(); // no ".json"
        final String jsonName = filename + ".json";
        final String overview = filename + "_overview.json";

        if ("default".equalsIgnoreCase(type)) {
            // examplePath: {resourceDir}/pro/example_workflows/{fileName}
            final String examplePath = ab.getResourceDir() + "/pro/example_workflows/" + jsonName;
            final java.nio.file.Path p = java.nio.file.Paths.get(examplePath).normalize();
            if (!java.nio.file.Files.exists(p)) {
                throw new java.io.FileNotFoundException("Default workflow not found: " + p);
            }
            return p; // caller will read the JSON and (optionally) pretty print
        }

        // CUSTOM (user) workflows — same logic as loadWorkflow()
        if (sb.getCurrentUser() == null) {
            throw new IllegalStateException("Please start an analysis session first!");
        }

        final String destDir = ab.getRealUserHomePath() + "/" + sb.getCurrentUser().getName() + "/";
        final String bucketObjectName = "/user_folders/" + fub.getEmail() + "/" + jsonName;
        final String bucketObjectName2 = "/user_folders/" + fub.getEmail() + "/" + overview;
        final String localFilePath = fbb.getProjectPath() + bucketObjectName;
        final String localFilePath2 = fbb.getProjectPath() + bucketObjectName2;

        java.nio.file.Path localCache = java.nio.file.Paths.get(localFilePath);
        java.nio.file.Path localCache2 = java.nio.file.Paths.get(localFilePath2);
        java.nio.file.Path destJson = java.nio.file.Paths.get(destDir, jsonName);
        java.nio.file.Path destOverview = java.nio.file.Paths.get(destDir, overview);

        // If cached in project path, copy to dest; else download from bucket
        if (java.nio.file.Files.exists(localCache)) {
            java.nio.file.Files.createDirectories(java.nio.file.Paths.get(destDir));
            java.nio.file.Files.copy(localCache, destJson, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
            if (java.nio.file.Files.exists(localCache2)) {
                java.nio.file.Files.copy(localCache2, destOverview, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
            }
        } else {
            // Download both (same as loadWorkflow())
            fbc.downloadObject(String.valueOf(wf.get("location")), fub.getEmail(), jsonName, destJson.toString());
            fbc.downloadObject(String.valueOf(wf.get("location")), fub.getEmail(), overview, destOverview.toString());
        }
        return destJson;
    }

    private String prettyJsonOrRaw(String raw) {
        if (raw == null) {
            return "(empty)";
        }
        try {
            com.fasterxml.jackson.databind.ObjectMapper om = new com.fasterxml.jackson.databind.ObjectMapper();
            Object node = om.readValue(raw, Object.class);
            return om.writerWithDefaultPrettyPrinter().writeValueAsString(node);
        } catch (Exception ignore) {
            return raw; // not strict JSON? just show raw
        }
    }

    public boolean isSelected(WorkflowRunModel r) {
        if (r == null || selectedWorkflowRun == null) {
            return false;
        }
        return String.valueOf(r.getId()).equals(String.valueOf(selectedWorkflowRun.getId()));
    }

}
