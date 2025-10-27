package pro.metaboanalyst.api;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import com.google.gson.reflect.TypeToken;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.lang.reflect.Type;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.Collections;
import java.util.UUID;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;

import pro.metaboanalyst.datalts.DatasetFile;
import pro.metaboanalyst.lts.DatabaseController;
import pro.metaboanalyst.datalts.DatasetRow;

@RequestScoped
@Named("databaseClient")
public class DatabaseClient {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;

    @JsonIgnore
    @Inject
    private DatabaseController dbc;

    @JsonIgnore
    @Inject
    private SessionBean1 sb;

    private static final Logger LOGGER = Logger.getLogger(DatabaseClient.class.getName());
    private final ApiClient apiClient;

    public DatabaseClient() {
        apiClient = new ApiClient();
    }

    public String registerUser(String email, String password, String firstname, String lastname, String institution) {
        if (ab.isInDocker()) {
            return dbc.registerUser(email, password, firstname, lastname, institution);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("password", password);
                payload.put("firstname", firstname);
                payload.put("lastname", lastname);
                payload.put("institution", institution);
                return apiClient.post("/database/register", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error registering user", e);
                return "Error registering user: " + e.getMessage();
            }
        }
    }

    public String[] loginUser(String email, String password, String toolName) {
        if (ab.isInDocker()) {
            return dbc.loginUserDocker(email, password);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("password", password);
                payload.put("tool", toolName);

                String response = apiClient.post("/database/login", toJson(payload));
                return response.contains("[") ? parseJsonArray(response) : new String[]{response};
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error logging in user", e);
                return new String[]{"Error logging in user: " + e.getMessage()};
            }
        }
    }

    public int updateProjectTitleDescription(String newName, String newDescription, int id) {
        if (ab.isInDocker()) {
            return DatabaseController.updateProjectTitleDescription(newName, newDescription, id);
        } else {
            try {
                Map<String, Object> payload = new HashMap<>();
                payload.put("name", newName);
                payload.put("description", newDescription);
                String response = apiClient.put("/database/projects/" + id, toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error updating project title and description", e);
                return -1;
            }
        }
    }

    public ArrayList<HashMap<String, Object>> getProjectsFromPostgres(String email, String toolName, String toolLocation) {
        if (ab.isInDocker()) {
            return DatabaseController.getProjectsFromPostgres(email, toolName, toolLocation);
        } else {
            try {
                String response = apiClient.get("/database/projects?email=" + email + "&toolName=" + toolName + "&toolLocation=" + toolLocation);
                return parseJsonToList(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error getting projects from Postgres", e);
                return null;
            }
        }
    }

    public int deleteProjectById(Long id) {
        if (ab.isInDocker()) {
            return DatabaseController.deleteProjectById(id);
        } else {
            try {
                String response = apiClient.delete("/database/projects/" + id);
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error deleting project by ID", e);
                return -1;
            }
        }
    }

    public Map<String, Object> loadProject(String token) {
        if (ab.isInDocker()) {
            return DatabaseController.loadProject(token);
        } else {
            try {
                String response = apiClient.get("/database/projects/load?token=" + token);
                return parseJsonToMap(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error loading project", e);
                return null;
            }
        }
    }

    public Map<String, Object> loadProjectById(String id) {
        if (ab.isInDocker()) {
            return DatabaseController.loadProjectById(id);
        } else {
            try {
                String response = apiClient.get("/database/projects/loadById?id=" + id);
                return parseJsonToMap(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error loading project", e);
                return null;
            }
        }
    }

    public String insertToken(String email, String resetToken, String expDate) {
        if (ab.isInDocker()) {
            return DatabaseController.insertToken(email, resetToken, expDate);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("resetToken", resetToken);
                payload.put("expDate", expDate);
                return apiClient.post("/database/tokens", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error inserting token", e);
                return "Error inserting token: " + e.getMessage();
            }
        }
    }

    public String checkUserExists(String email) {
        if (ab.isInDocker()) {
            return DatabaseController.checkUserExists(email);
        } else {
            try {
                return apiClient.get("/database/tokens/check?email=" + email);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error checking if user exists", e);
                return "Error checking if user exists: " + e.getMessage();
            }
        }
    }

    public String verifyToken(String token) {
        if (ab.isInDocker()) {
            return DatabaseController.verifyToken(token);
        } else {
            try {
                String response = apiClient.get("/database/tokens/verify?token=" + token);
                return response;
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error verifying token", e);
                return "Error verifying token: " + e.getMessage();
            }
        }
    }

    public String deleteTokenForUser(String email) {
        if (ab.isInDocker()) {
            return DatabaseController.deleteTokenForUser(email);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                return apiClient.delete("/database/tokens", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error deleting token for user", e);
                return "Error deleting token for user: " + e.getMessage();
            }
        }
    }

    public String resetPassword(String email, String newPassword) {
        if (ab.isInDocker()) {
            return dbc.resetPassword(email, newPassword);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("new_password", newPassword);
                return apiClient.post("/database/users/resetpassword", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error resetting password", e);
                return "Error resetting password: " + e.getMessage();
            }
        }
    }

    public String addActivationCode(String activateCode, String expDate, String email) {
        if (ab.isInDocker()) {
            return DatabaseController.addActivationCode(activateCode, expDate, email);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("activateCode", activateCode);
                payload.put("expDate", expDate);
                payload.put("email", email);
                return apiClient.post("/database/users/activationcode", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error adding activation code", e);
                return "Error adding activation code: " + e.getMessage();
            }
        }
    }

    public String checkActivationCode(String email, String activationCode) {
        if (ab.isInDocker()) {
            return DatabaseController.checkActivationCode(email, activationCode);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("activationCode", activationCode);
                return apiClient.post("/database/users/activate", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error checking activation code", e);
                return "Error checking activation code: " + e.getMessage();
            }
        }
    }

    public String deleteUserAndProjects(String userId) {
        if (ab.isInDocker()) {
            return DatabaseController.deleteUserAndProjects(userId);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("userId", userId);
                return apiClient.delete("/database/users", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error deleting user and projects", e);
                return "Error deleting user and projects: " + e.getMessage();
            }
        }
    }

    public int checkMatchingFolderNameProject(String folderName) {
        if (ab.isInDocker()) {
            return DatabaseController.checkMatchingFolderNameProject(folderName);
        } else {
            try {
                String response = apiClient.get("/database/projects/match?folderName=" + folderName);
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error checking matching folder name project", e);
                return -1;
            }
        }
    }

    public int transferProject(int id, String newEmail, String suffix, String toolLocation) {
        if (ab.isInDocker()) {
            return DatabaseController.transferProject(id, newEmail, suffix, toolLocation);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("id", String.valueOf(id));
                payload.put("newEmail", newEmail);
                payload.put("suffix", suffix);
                payload.put("toolLocation", toolLocation);
                String response = apiClient.post("/database/projects/transfer", toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error transferring project", e);
                return -1;
            }
        }
    }

    // Raw spectra job functions
    public int recordRawJob(long JobID, String email, String Project_folder, String jobPos) {
        if (ab.isInDocker()) {
            return DatabaseController.recordRawJob(JobID, email, Project_folder, jobPos);
        } else {
            try {
                Map<String, Object> payload = new HashMap<>();
                payload.put("JobID", JobID);
                payload.put("email", email);
                payload.put("Project_folder", Project_folder);
                String response = apiClient.post("/database/recordRawJob", toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error recording raw job", e);
                return 0;
            }
        }
    }

    public int updateRawJobStatus(long JobID, String currentJobStatus, String jobPos) {
        if (ab.isInDocker()) {
            return DatabaseController.updateRawJobStatus(JobID, currentJobStatus, jobPos);
        } else {
            try {
                Map<String, Object> payload = new HashMap<>();
                payload.put("JobID", JobID);
                payload.put("currentJobStatus", currentJobStatus);
                String response = apiClient.post("/database/updateRawJobStatus", toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error updating raw job status", e);
                return 0;
            }
        }
    }

    public int extractRawJobStatus(String folder, String userid) {
        if (ab.isInDocker()) {
            return DatabaseController.extractRawJobStatus(folder, userid);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("folder", folder);
                payload.put("userid", userid);
                String response = apiClient.post("/database/extractRawJobStatus", toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error extracting raw job status", e);
                return 0;
            }
        }
    }

    public String deleteWorkflowById(String id) {
        if (ab.isInDocker()) {
            // If running on Docker, call the database method directly
            return DatabaseController.deleteWorkflow(id);
        } else {
            try {
                // Create the payload with the id
                Map<String, String> payload = new HashMap<>();
                payload.put("id", String.valueOf(id));  // Converting int to String

                // Make the POST request to the deleteWorkflow endpoint
                return apiClient.post("/database/workflow/delete", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error deleting workflow", e);
                return "Error deleting workflow: " + e.getMessage();
            }
        }
    }

    public ArrayList<HashMap<String, Object>> getAllWorkflows(String tool, String email) {
        if (ab.isInDocker()) {
            // If running on Docker, call the database method directly and assume db.getAllWorkflows(tool)
            // returns an ArrayList<HashMap<String, Object>>
            return DatabaseController.getAllWorkflows(tool, email);
        } else {
            try {
                // Create a payload with the tool
                Map<String, String> payload = new HashMap<>();
                payload.put("tool", tool);
                payload.put("email", email);

                // Send the payload to the API and get the response (assume apiClient.post returns JSON)
                String response = apiClient.post("/database/workflow/getall", toJson(payload));

                // Parse the JSON response to ArrayList<HashMap<String, Object>> (assuming a helper method `fromJson`)
                return parseJsonToList(response);

            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error retrieving workflows", e);
                // Return an empty list in case of error
                return new ArrayList<>();
            }
        }
    }

    public String insertWorkflow(String email, String name, String description, String module, String toolName, String filename, String location, String input, String analysisGoal, String analysisMethods, String output, String other) {
        if (ab.isInDocker()) {
            return DatabaseController.insertWorkflow(email, name, description, module, toolName, filename, location, input, analysisGoal, analysisMethods, output, other);
        } else {
            try {
                // Create the payload
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("name", name);
                payload.put("description", description);
                payload.put("module", module);
                payload.put("tool", toolName);
                payload.put("filename", filename);
                payload.put("location", location);
                payload.put("input", input);
                payload.put("analysisGoal", analysisGoal);
                payload.put("analysisMethods", analysisMethods);
                payload.put("output", output);
                payload.put("other", other);

                // Make the POST request to the insertWorkflow endpoint
                return apiClient.post("/database/workflow/insert", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error inserting workflow", e);
                return "Error inserting workflow: " + e.getMessage();
            }
        }
    }

    public Map<String, Object> obtainFolderNameProject(String folderName) {
        if (ab.isInDocker()) {
            return DatabaseController.obtainFolderNameProject(folderName);
        } else {
            try {
                String response = apiClient.get("/database/projects/details?folderName=" + URLEncoder.encode(folderName, StandardCharsets.UTF_8));

                // Parse the JSON response into a Map
                ObjectMapper objectMapper = new ObjectMapper();
                return objectMapper.readValue(response, new TypeReference<Map<String, Object>>() {
                });
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error obtaining folder name project details", e);
                return Collections.emptyMap(); // Return an empty map in case of an error
            }
        }
    }

    /**
     * Incremental dataset update: updates top-level fields and UPSERTS files. -
     * Existing files not listed remain untouched. - Files in 'files' are
     * inserted or updated (server-side ON CONFLICT DO UPDATE).
     */
    public String updateDataset(
            java.util.UUID datasetId,
            String email,
            String node,
            String title,
            String module,
            String dataType,
            String toolName,
            int sampleNum,
            java.util.List<DatasetFile> files
    ) {
        if (datasetId == null) {
            return "Error updating dataset: datasetId is required.";
        }
        if (files == null) {
            files = java.util.Collections.emptyList(); // allow metadata-only updates
        }

        if (ab.isInDocker()) {
            try {
                // Direct DB path (expects per-file UPSERT in DAO)
                return DatabaseController.updateDatasetAndFiles(
                        datasetId, title, toolName, module, dataType, sampleNum, null, files
                );
            } catch (Exception e) {
                LOGGER.log(java.util.logging.Level.SEVERE, "Error updating dataset (DB path)", e);
                return "Error updating dataset: " + e.getMessage();
            }
        } else {
            try {
                // Build nested JSON payload
                java.util.Map<String, Object> dataset = new java.util.LinkedHashMap<>();
                dataset.put("id", datasetId.toString());
                dataset.put("email", email);     // optional: for auth checks
                dataset.put("node", node);       // optional: for scoping
                dataset.put("title", title);
                dataset.put("module", module);
                dataset.put("dataType", dataType);
                dataset.put("toolname", toolName);
                dataset.put("samplenum", sampleNum);

                java.util.List<java.util.Map<String, Object>> fileList = new java.util.ArrayList<>();
                for (DatasetFile f : files) {
                    if (f == null) {
                        continue;
                    }
                    java.util.Map<String, Object> m = new java.util.LinkedHashMap<>();
                    m.put("role", safeRole(f.getRole()));            // keep same validator as insert
                    m.put("filename", f.getFilename());
                    m.put("type", nvl(f.getType(), "bin"));
                    m.put("sizeBytes", Math.max(0L, f.getSizeBytes()));
                    if (f.getUploadedAt() != null) {
                        m.put("uploadedAt", f.getUploadedAt().toString());
                    }
                    fileList.add(m);
                }

                java.util.Map<String, Object> payload = new java.util.LinkedHashMap<>();
                payload.put("dataset", dataset);
                payload.put("files", fileList);

                // Matches your insert route style
                return apiClient.post("/database/datasets/update", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(java.util.logging.Level.SEVERE, "Error updating dataset", e);
                return "Error updating dataset: " + e.getMessage();
            }
        }
    }

    /**
     * New: multi-file insert (data + metadata + etc.)
     */
    public String insertDataset(String email,
            String node,
            String title,
            String module,
            String dataType,
            String toolName,
            int sampleNum,
            java.util.List<DatasetFile> files) {
        if (files == null || files.isEmpty()) {
            return "Error inserting dataset: at least one file is required.";
        }

        if (ab.isInDocker()) {
            // Direct DB path
            return DatabaseController.insertDatasetWithFiles(email, node, title, module, dataType, "metaboanalyst", sampleNum, null, files);
        } else {
            try {
                // Build nested JSON payload
                java.util.Map<String, Object> dataset = new java.util.LinkedHashMap<>();
                dataset.put("email", email);
                dataset.put("node", node);
                dataset.put("module", module);
                dataset.put("dataType", dataType);

                dataset.put("title", title);
                dataset.put("toolname", toolName);

                dataset.put("samplenum", sampleNum);

                java.util.List<java.util.Map<String, Object>> fileList = new java.util.ArrayList<>();
                for (DatasetFile f : files) {
                    java.util.Map<String, Object> m = new java.util.LinkedHashMap<>();
                    m.put("role", safeRole(f.getRole()));
                    m.put("filename", f.getFilename());
                    m.put("type", nvl(f.getType(), "bin"));
                    m.put("sizeBytes", Math.max(0L, f.getSizeBytes()));
                    if (f.getUploadedAt() != null) {
                        m.put("uploadedAt", f.getUploadedAt().toString());
                    }
                    fileList.add(m);
                }

                java.util.Map<String, Object> payload = new java.util.LinkedHashMap<>();
                payload.put("dataset", dataset);
                payload.put("files", fileList);

                return apiClient.post("/database/datasets/insert", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(java.util.logging.Level.SEVERE, "Error inserting dataset", e);
                return "Error inserting dataset: " + e.getMessage();
            }
        }
    }

    /**
     * Convenience: accept a DatasetRow that already contains files[].
     */
    public String insertDataset(DatasetRow ds) {
        return insertDataset(
                ds.getEmail(), ds.getNode(), ds.getTitle(), sb.getAnalType(), sb.getDataType(), "metaboanalyst", ds.getSamplenum(),
                ds.getFiles() == null ? java.util.List.of() : ds.getFiles()
        );
    }

    /* ---- small helpers (same as DAO) ---- */
    private static String nvl(String s, String def) {
        return (s == null || s.trim().isEmpty()) ? def : s;
    }

    private static String safeRole(String role) {
        if (role == null) {
            return "other";
        }
        switch (role.toLowerCase(java.util.Locale.ROOT)) {
            case "data":
            case "metadata":
            case "supplement":
            case "archive":
                return role.toLowerCase();
            default:
                return "other";
        }
    }

    public ArrayList<DatasetRow> getDatasetsForEmail(String email, String toolname, boolean includeFiles) {
        ArrayList<DatasetRow> out = new ArrayList<>();
        if (email == null || email.isBlank()) {
            return out;
        }
        try {
            if (ab.isInDocker()) {
                // --- Direct DB path ---
                List<DatasetRow> rows = DatabaseController.getDatasetsForEmail(email, toolname);
                if (includeFiles && rows != null) {
                    for (DatasetRow d : rows) {
                        UUID id = d.getId();
                        if (id != null) {
                            List<DatasetFile> files = DatabaseController.getDatasetFiles(id);
                            d.setFiles(files);
                        }
                    }
                }
                if (rows != null) {
                    out.addAll(rows);
                }
                return out;
            } else {
                // --- Remote API path ---
                String qEmail = URLEncoder.encode(email, StandardCharsets.UTF_8.name());
                String qTool = URLEncoder.encode(toolname, StandardCharsets.UTF_8.name());
                String url = "/database/datasets/by-email?email=" + qEmail
                        + "&toolname=" + qTool
                        + "&includeFiles=" + includeFiles;

                String json = apiClient.get(url);

                ObjectMapper om = new ObjectMapper().findAndRegisterModules();
                List<DatasetRow> list = om.readValue(json, new TypeReference<List<DatasetRow>>() {
                });
                if (list != null) {
                    out.addAll(list);
                }
                return out;
            }
        } catch (Exception e) {
            LOGGER.log(java.util.logging.Level.SEVERE, "Error fetching datasets for email/toolname", e);
            return out; // empty list on error
        }
    }

    public List<DatasetFile> getDatasetFiles(UUID datasetId) {
        List<DatasetFile> out = new ArrayList<>();
        if (datasetId == null) {
            return out;
        }

        try {
            if (ab.isInDocker()) {
                // Direct Postgres path (DAO)
                return DatabaseController.getDatasetFiles(datasetId);
            } else {
                // Remote API path
                String json = apiClient.get("/database/datasets/" + datasetId + "/files");

                ObjectMapper om = new ObjectMapper().registerModule(new JavaTimeModule())
                        .findAndRegisterModules();
                List<DatasetFile> list = om.readValue(json, new TypeReference<List<DatasetFile>>() {
                });
                out.addAll(list);
                return out;
            }
        } catch (Exception e) {
            LOGGER.log(java.util.logging.Level.SEVERE, "Error fetching dataset files for " + datasetId, e);
            return out; // empty list on error
        }
    }

    public Map<String, Object> deleteDatasetFilesFolder(UUID datasetId, String email) {
        Map<String, Object> out = new HashMap<>();
        if (datasetId == null || email == null || email.isBlank()) {
            out.put("status", "error");
            out.put("message", "datasetId/email required");
            return out;
        }

        try {
            if (ab.isInDocker()) {
                // Direct DB/DAO path
                String result = DatabaseController.deleteDatasetById(datasetId);
                out.put("status", "OK".equals(result) ? "ok" : "error");
                out.put("datasetId", datasetId.toString());
                out.put("email", email);
                out.put("folder", result);
                return out;
            } else {
                // Remote API path
                String url = "/database/datasets/" + datasetId + "/files?email="
                        + URLEncoder.encode(email, StandardCharsets.UTF_8);
                String json = apiClient.delete(url);

                ObjectMapper om = new ObjectMapper().registerModule(new JavaTimeModule())
                        .findAndRegisterModules();
                return om.readValue(json, new TypeReference<Map<String, Object>>() {
                });
            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error deleting dataset files folder for " + datasetId, e);
            out.put("status", "error");
            out.put("message", e.getMessage());
            return out;
        }
    }

    public boolean deleteDatasetFilesFolderOk(UUID datasetId, String email) {
        Map<String, Object> resp = deleteDatasetFilesFolder(datasetId, email);
        Object s = resp.get("status");
        //System.out.println("String.valueOf(s)===" + String.valueOf(s));
        return s != null && "ok".equalsIgnoreCase(String.valueOf(s));
    }

    private String toJson(Map<String, ?> map) {
        return new Gson().toJson(map);
    }

    private String[] parseJsonArray(String json) {
        return new Gson().fromJson(json, String[].class);
    }

    private Map<String, Object> parseJsonToMap(String json) {
        return new Gson().fromJson(json, new TypeToken<Map<String, Object>>() {
        }.getType());
    }

// import com.google.gson.*;  // ensure you have these
// import java.lang.reflect.Type;
// import com.google.gson.reflect.TypeToken;
    private ArrayList<HashMap<String, Object>> parseJsonToList(String json) {
        ArrayList<HashMap<String, Object>> out = new ArrayList<>();
        if (json == null) {
            return out;
        }

        String trimmed = json.trim();
        System.out.println("workflowresponse======" + trimmed);

        if (trimmed.isEmpty() || "null".equalsIgnoreCase(trimmed)) {
            return out;
        }

        try {
            Gson gson = new Gson();
            JsonElement root = JsonParser.parseString(trimmed);

            if (root.isJsonArray()) {
                Type listType = new TypeToken<ArrayList<HashMap<String, Object>>>() {
                }.getType();
                return gson.fromJson(root, listType);
            }

            if (root.isJsonObject()) {
                // server sent a single object, wrap it
                HashMap<String, Object> one = gson.fromJson(root, new TypeToken<HashMap<String, Object>>() {
                }.getType());
                out.add(one);
                return out;
            }

            if (root.isJsonPrimitive() && root.getAsJsonPrimitive().isString()) {
                // text like "No records" — treat as empty
                String s = root.getAsString();
                if (s == null || s.trim().isEmpty()
                        || "No records".equalsIgnoreCase(s.trim())
                        || "none".equalsIgnoreCase(s.trim())) {
                    return out;
                }
                // If it *looks* like JSON despite being quoted (rare), try one more pass
                String unq = s.trim();
                if ((unq.startsWith("[") && unq.endsWith("]")) || (unq.startsWith("{") && unq.endsWith("}"))) {
                    return parseJsonToList(unq);
                }
                return out;
            }

            // anything else → empty
            return out;

        } catch (Exception e) {
            // Last-ditch: if server sent raw text like No records (unquoted), handle it
            if (trimmed.equalsIgnoreCase("No records")) {
                return out;
            }
            // Log and return empty rather than throwing
            LOGGER.severe("parseJsonToList(): tolerant parse failed: " + e.getMessage());
            return out;
        }
    }

    public int detectDockerUserNum() {
        return DatabaseController.detectDockerUserNum();
    }

    public ArrayList<HashMap<String, Object>> getAllWorkflowRuns(String tool, String email) {
        if (ab.isInDocker()) {
            try {
                // Direct DB call when running inside Docker
                return DatabaseController.getAllWorkflowRuns(tool, email);
            } catch (SQLException ex) {
                System.getLogger(DatabaseClient.class.getName()).log(System.Logger.Level.ERROR, (String) null, ex);
                return new ArrayList<>();

            }
        } else {
            try {
                // Build payload
                Map<String, String> payload = new HashMap<>();
                payload.put("tool", tool);  // aligns with workflow_runs.module
                payload.put("email", email);   // server can scope runs by owner

                // POST to API and parse JSON -> List<Map<String,Object>>
                String response = apiClient.post("/database/workflowruns/getall", toJson(payload));
                return parseJsonToList(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error retrieving workflow runs", e);
                return new ArrayList<>();
            }
        }
    }

    /**
     * Insert a new workflow RUN row (workflow_runs). Dates should be ISO-8601
     * strings (e.g., "2025-10-21T14:30:00Z" or "2025-10-21T10:30:00-04:00"). If
     * a field is not applicable, pass null.
     */
    // ==== INSERT (now with `tool`) ====
    public String insertWorkflowRun(String email,
            String module,
            String name,
            String description,
            String status, // e.g. pending/running/completed/failed
            String startDateIso, // nullable ISO-8601
            String finishDateIso, // nullable ISO-8601
            String workflowId, // workflow template/json identifier
            UUID datasetId, // <-- CHANGED: UUID (nullable)
            String datasetName, // nullable
            String other, // JSON string or plain text
            String tool) {        // NEW
        try {
            final String toolVal = tool;

            if (ab.isInDocker()) {
                // Direct DB path
                return DatabaseController.insertWorkflowRun(
                        email, module, name, description, status,
                        startDateIso, finishDateIso, workflowId,
                        datasetId, // pass UUID through
                        datasetName, other, toolVal
                ) + "";
            } else {
                // Build payload (as Strings for consistency with your existing toJson(..))
                Map<String, String> payload = new HashMap<>();
                payload.put("email", email);
                payload.put("module", module);
                payload.put("name", name == null ? "" : name);
                payload.put("description", description == null ? "" : description);
                if (status != null) {
                    payload.put("status", status);
                }
                if (startDateIso != null) {
                    payload.put("start_date", startDateIso);
                }
                if (finishDateIso != null) {
                    payload.put("finish_date", finishDateIso);
                }
                if (workflowId != null) {
                    payload.put("workflow_id", workflowId);
                }
                if (datasetId != null) {
                    payload.put("dataset_id", datasetId.toString()); // UUID -> string
                }
                if (datasetName != null) {
                    payload.put("dataset_name", datasetName);
                }
                if (other != null) {
                    payload.put("other", other); // if JSON, server should store as jsonb
                }
                if (tool != null) {
                    payload.put("tool", tool);
                }
                String res = apiClient.post("/database/workflowruns/insert", toJson(payload));
                System.out.println(res);
                return res;
            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error inserting workflow run", e);
            return "FAIL";
        }
    }

// ==== DELETE (unchanged signature; no need for tool/email here) ====
    /**
     * Delete a workflow run by id. - In Docker:
     * DatabaseController.deleteWorkflowRun(...) - Otherwise: POST
     * /database/workflowruns/delete { "id": "<id>" }
     */
    public String deleteWorkflowRun(String id) {
        if (ab.isInDocker()) {
            return DatabaseController.deleteWorkflowRun(id);
        } else {
            try {
                Map<String, String> payload = new HashMap<>();
                payload.put("id", id);
                return apiClient.post("/database/workflowruns/delete", toJson(payload));
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error deleting workflow run (id=" + id + ")", e);
                return "Error deleting workflow run: " + e.getMessage();
            }
        }
    }

    /**
     * Convenience overload that accepts an integer id.
     */
    public String deleteWorkflowRun(int id) {
        return deleteWorkflowRun(String.valueOf(id));
    }

// ==== UPDATE STATUS (unchanged externally; server sets start_date when running) ====
    /**
     * Update workflow_run status by id. - In Docker:
     * DatabaseController.updateWorkflowRunStatus(...) - Otherwise: POST
     * /database/workflowruns/status/update
     */
    // Back-compat: delegate to the 3-arg version (no project change)
    public String updateWorkflowRunStatus(String id, String newStatus) {
        return updateWorkflowRunStatus(id, newStatus, null);
    }

    /**
     * New overload with optional projectId (INT). If projectId is null,
     * project_id remains unchanged.
     */
    public String updateWorkflowRunStatus(String id, String newStatus, String projectId) {
        try {
            if (id == null || id.isBlank()) {
                return "Error updating workflow run status: 'id' is required.";
            }
            final int runId = Integer.parseInt(id.trim());

            // Normalize & validate status
            final String status = newStatus == null ? "" : newStatus.trim().toLowerCase();
            switch (status) {
                case "pending":
                case "running":
                case "completed":
                case "failed":
                    break;
                default:
                    return "Error updating workflow run status: invalid status. Allowed: pending, running, completed, failed.";
            }

            if (ab.isInDocker()) {
                return DatabaseController.updateWorkflowRunStatus(runId, status, projectId);
            } else {
                final Map<String, Object> payload = new HashMap<>();
                payload.put("id", runId);
                payload.put("status", status);
                if (projectId != null) {
                    payload.put("project_id", projectId);
                }
                return apiClient.post("/database/workflowruns/status/update", toJson(payload));
            }
        } catch (NumberFormatException nfe) {
            LOGGER.log(Level.SEVERE, "updateWorkflowRunStatus: invalid id '" + id + "'", nfe);
            return "Error updating workflow run status: 'id' must be an integer.";
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error updating workflow run status (id=" + id + ")", e);
            return "Error updating workflow run status: " + e.getMessage();
        }
    }

    public String updateWorkflowRunStatus(int id, String newStatus, String projectId) {
        return updateWorkflowRunStatus(String.valueOf(id), newStatus, projectId);
    }
    
public String updateWorkflowRunFields(String id, Map<String, Object> fields) {
    try {
        if (id == null || id.isBlank()) return "Error updating workflow run: 'id' is required.";
        final int runId = Integer.parseInt(id.trim());
        if (fields == null || fields.isEmpty()) return "Error updating workflow run: payload is empty.";

        if (ab.isInDocker()) {
            return DatabaseController.updateWorkflowRunFlexible(runId, fields);
        } else {
            final Map<String, Object> payload = new HashMap<>(fields);
            payload.put("id", runId);
            return apiClient.post("/database/workflowruns/update", toJson(payload));
        }
    } catch (NumberFormatException nfe) {
        LOGGER.log(Level.SEVERE, "updateWorkflowRunFields: invalid id '" + id + "'", nfe);
        return "Error updating workflow run: 'id' must be an integer.";
    } catch (Exception e) {
        LOGGER.log(Level.SEVERE, "Error updating workflow run (id=" + id + ")", e);
        return "Error updating workflow run: " + e.getMessage();
    }
}
}
