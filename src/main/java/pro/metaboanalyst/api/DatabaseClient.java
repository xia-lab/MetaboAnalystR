package pro.metaboanalyst.api;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.UUID;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
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

    public int writeProjectToPostgres(Map<String, Object> rawDocData, String projectType, String tableName) {
        if (ab.isInDocker()) {
            return DatabaseController.writeProjectToPostgres(rawDocData, projectType, tableName);
        } else {
            try {
                Map<String, Object> payload = new HashMap<>(rawDocData);
                payload.put("projectType", projectType);
                payload.put("tableName", tableName);
                String response = apiClient.post("/database/projects", toJson(payload));
                return Integer.parseInt(response);
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "Error writing project to Postgres", e);
                return -1;
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

// Backward-compatible wrapper: single primary file -> calls multi-file path
    public String insertDataset(String email,
            String node,
            String title,
            String filename,
            String type,
            long sizeBytes,
            int sampleNum) {
        DatasetFile primary = new DatasetFile();
        primary.setRole("data");
        primary.setFilename(filename);
        primary.setType(type);
        primary.setSizeBytes(sizeBytes);

        return insertDataset(email, node, title, sampleNum, java.util.List.of(primary));
    }

    /**
     * New: multi-file insert (data + metadata + etc.)
     */
    public String insertDataset(String email,
            String node,
            String title,
            int sampleNum,
            java.util.List<DatasetFile> files) {
        if (files == null || files.isEmpty()) {
            return "Error inserting dataset: at least one file is required.";
        }

        if (ab.isInDocker()) {
            // Direct DB path
            return DatabaseController.insertDatasetWithFiles(email, node, title, sampleNum, null, files);
        } else {
            try {
                // Build nested JSON payload
                java.util.Map<String, Object> dataset = new java.util.LinkedHashMap<>();
                dataset.put("email", email);
                dataset.put("node", node);
                dataset.put("title", title);
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
                ds.getEmail(), ds.getNode(), ds.getTitle(), ds.getSamplenum(),
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

    public ArrayList<DatasetRow> getDatasetsForEmail(String email) {
        ArrayList<DatasetRow> out = new ArrayList<>();
        if (email == null || email.isBlank()) {
            return out;
        }

        try {
            if (ab.isInDocker()) {
                // Direct Postgres path (your DAO we wrote earlier)
                return DatabaseController.getDatasetsForEmail(email);
            } else {
                // Remote path: call your API and parse JSON into DatasetRow[]
                String q = URLEncoder.encode(email, StandardCharsets.UTF_8.name());
                // Suggested endpoint: GET /database/datasets/by-email?email=<email>
                String json = apiClient.get("/database/datasets/by-email?email=" + q);

                ObjectMapper om = new ObjectMapper().findAndRegisterModules(); // handles OffsetDateTime
                List<DatasetRow> list = om.readValue(json, new TypeReference<List<DatasetRow>>() {
                });
                out.addAll(list);
                return out;
            }
        } catch (Exception e) {
            LOGGER.log(java.util.logging.Level.SEVERE, "Error fetching datasets for email", e);
            return out; // empty list on error (UI can show a growl)
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

    private ArrayList<HashMap<String, Object>> parseJsonToList(String json) {
        return new Gson().fromJson(json, new TypeToken<List<HashMap<String, Object>>>() {
        }.getType());
    }

    public int detectDockerUserNum() {
        return DatabaseController.detectDockerUserNum();
    }
}
