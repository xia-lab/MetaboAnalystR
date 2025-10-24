/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.datalts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DatasetRow implements Serializable {

    // --- DB-backed fields ---
    private UUID id;                 // datasets.id (uuid)
    private String title;            // datasets.title
    private String filename;         // datasets.filename
    private String type;             // datasets.type  (csv/tsv/txt/zip)
    private long sizeBytes;          // datasets.size_bytes
    private OffsetDateTime uploadedAt; // datasets.uploaded_at (timestamptz)
    private String email;            // datasets.email (citext)
    private String node;             // datasets.node
    private int samplenum;           // datasets.samplenum
    private String module;         // stat,mf,roc,etc
    private String dataType;         // table,list,etc
    private String toolName;         // table,list,etc

    // --- UI-only / optional fields (not in DB schema unless you add them) ---
    private String description;      // used by edit dialog
    private List<String> tags = new ArrayList<>(); // used by <p:chips>

    @JsonIgnore
    private boolean selected;

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    public DatasetRow() {
    }

    public String getToolName() {
        return toolName;
    }

    public void setToolName(String toolName) {
        this.toolName = toolName;
    }

    // ---------- Convenience ----------
    /**
     * Maps to your <h:outputText value="#{ds.humanSize}"/>
     */
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public String getHumanSize() {
        return humanReadable(sizeBytes);
    }

    public static String humanReadable(long bytes) {
        if (bytes < 0) {
            return "-" + humanReadable(-bytes);
        }
        final String[] units = {"B", "KB", "MB", "GB", "TB", "PB"};
        double v = bytes;
        int u = 0;
        while (v >= 1024 && u < units.length - 1) {
            v /= 1024.0;
            u++;
        }
        return (u == 0 ? String.format("%.0f %s", v, units[u])
                : String.format("%.1f %s", v, units[u]));
    }

    /**
     * Helper to build from a JDBC ResultSet row.
     */
    public static DatasetRow fromResultSet(ResultSet rs) throws SQLException {
        DatasetRow d = new DatasetRow();
        Object idObj = rs.getObject("id");
        if (idObj instanceof UUID) {
            d.setId((UUID) idObj);
        } else if (idObj != null) {
            d.setId(UUID.fromString(String.valueOf(idObj)));
        }

        d.setTitle(rs.getString("title"));
        d.setFilename(rs.getString("filename"));
        d.setType(rs.getString("type"));
        d.setSizeBytes(rs.getLong("size_bytes"));

        // PgJDBC >= 42.2 supports this:
        try {
            d.setUploadedAt(rs.getObject("uploaded_at", OffsetDateTime.class));
        } catch (Exception ignore) {
            // Fallback if driver/version doesn’t support the above:
            if (rs.getTimestamp("uploaded_at") != null) {
                d.setUploadedAt(rs.getTimestamp("uploaded_at").toInstant().atOffset(OffsetDateTime.now().getOffset()));
            }
        }

        d.setEmail(rs.getString("email"));
        d.setNode(rs.getString("node"));
        d.setSamplenum(rs.getInt("samplenum"));
        return d;
    }
// add to DatasetRow
    private List<DatasetFile> files = new ArrayList<>();

    public List<DatasetFile> getFiles() {
        return files;
    }

    public void setFiles(List<DatasetFile> files) {
        this.files = files != null ? files : new ArrayList<>();
    }

    // ---------- Getters / Setters ----------
    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public long getSizeBytes() {
        return sizeBytes;
    }

    public void setSizeBytes(long sizeBytes) {
        this.sizeBytes = sizeBytes;
    }

    public OffsetDateTime getUploadedAt() {
        return uploadedAt;
    }

    public void setUploadedAt(OffsetDateTime uploadedAt) {
        this.uploadedAt = uploadedAt;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getNode() {
        return node;
    }

    public void setNode(String node) {
        this.node = node;
    }

    public int getSamplenum() {
        return samplenum;
    }

    public void setSamplenum(int samplenum) {
        this.samplenum = samplenum;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<String> getTags() {
        return tags;
    }

    public void setTags(List<String> tags) {
        this.tags = tags != null ? tags : new ArrayList<>();
    }

    private int fileCount;         // derived
    private boolean hasMetadata;   // derived

    public int getFileCount() {
        return fileCount;
    }

    public int getFileCountAdjusted() {
        return fileCount;
    }

    public void setFileCount(int fileCount) {
        this.fileCount = fileCount;
    }

    public boolean isHasMetadata() {
        return hasMetadata;
    }

    public void setHasMetadata(boolean hasMetadata) {
        this.hasMetadata = hasMetadata;
    }

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public String getUploadedAtStr() {
        if (uploadedAt == null) {
            return "";
        }
        DateTimeFormatter fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
                .withZone(ZoneId.systemDefault());
        return fmt.format(uploadedAt);
    }

    public String getModule() {
        return module;
    }

    public void setModule(String module) {
        this.module = module;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    // In DatasetRow.java (fields section, under UI-only / optional fields)
    private String origin; // "Example" or "Custom"

// getters/setters
    public String getOrigin() {
        return origin;
    }

    public void setOrigin(String origin) {
        this.origin = origin;
    }

// Optional helper
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public boolean isExample() {
        return "Example".equalsIgnoreCase(origin);
    }

    @Override
    public String toString() {
        return "DatasetRow{"
                + "id=" + id
                + ", title='" + title + '\''
                + ", filename='" + filename + '\''
                + ", type='" + type + '\''
                + ", sizeBytes=" + sizeBytes
                + ", uploadedAt=" + (uploadedAt != null ? uploadedAt.toString() : "null")
                + ", email='" + email + '\''
                + ", node='" + node + '\''
                + ", samplenum=" + samplenum
                + ", module='" + module + '\''
                + ", dataType='" + dataType + '\''
                + ", fileCount=" + fileCount
                + ", hasMetadata=" + hasMetadata
                + '}';
    }
}
