package pro.metaboanalyst.datacenter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Captures session-specific metadata that should accompany a staged raw
 * dataset. It does not perform any IO; the {@link DatasetController} uses the
 * collected information to build a bundle and manifest on disk.
 */
public final class RawDatasetSnapshot implements Serializable {

    private static final long serialVersionUID = 1L;

    private final String title;
    private final boolean containsMetadata;
    private final List<String> uploadedArchives;
    private final String polarity;
    private final String msMode;
    private final Map<String, Object> extras;

    private RawDatasetSnapshot(Builder builder) {
        this.title = builder.title;
        this.containsMetadata = builder.containsMetadata;
        this.uploadedArchives = Collections.unmodifiableList(new ArrayList<>(builder.uploadedArchives));
        this.polarity = builder.polarity;
        this.msMode = builder.msMode;
        this.extras = Collections.unmodifiableMap(new LinkedHashMap<>(builder.extras));
    }

    public String getTitle() {
        return title;
    }

    public boolean isContainsMetadata() {
        return containsMetadata;
    }

    public List<String> getUploadedArchives() {
        return uploadedArchives;
    }

    public String getPolarity() {
        return polarity;
    }

    public String getMsMode() {
        return msMode;
    }

    public Map<String, Object> getExtras() {
        return extras;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {

        private String title;
        private boolean containsMetadata;
        private final List<String> uploadedArchives = new ArrayList<>();
        private String polarity;
        private String msMode;
        private final Map<String, Object> extras = new LinkedHashMap<>();

        private Builder() {
        }

        public Builder title(String title) {
            this.title = title;
            return this;
        }

        public Builder containsMetadata(boolean containsMetadata) {
            this.containsMetadata = containsMetadata;
            return this;
        }

        public Builder addArchive(String archive) {
            if (archive != null && !archive.isBlank()) {
                this.uploadedArchives.add(archive);
            }
            return this;
        }

        public Builder uploadedArchives(Collection<String> archives) {
            if (archives != null) {
                for (String archive : archives) {
                    addArchive(archive);
                }
            }
            return this;
        }

        public Builder polarity(String polarity) {
            this.polarity = polarity;
            return this;
        }

        public Builder msMode(String msMode) {
            this.msMode = msMode;
            return this;
        }

        public Builder extra(String key, Object value) {
            if (key != null && !key.isBlank() && value != null) {
                this.extras.put(key, value);
            }
            return this;
        }

        public RawDatasetSnapshot build() {
            return new RawDatasetSnapshot(this);
        }
    }

    @Override
    public String toString() {
        return "RawDatasetSnapshot{"
                + "title='" + title + '\''
                + ", containsMetadata=" + containsMetadata
                + ", uploadedArchives=" + uploadedArchives
                + ", polarity='" + polarity + '\''
                + ", msMode='" + msMode + '\''
                + ", extrasKeys=" + extras.keySet()
                + '}';
    }

    @Override
    public int hashCode() {
        return Objects.hash(title, containsMetadata, uploadedArchives, polarity, msMode, extras);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        RawDatasetSnapshot other = (RawDatasetSnapshot) obj;
        return containsMetadata == other.containsMetadata
                && Objects.equals(title, other.title)
                && Objects.equals(uploadedArchives, other.uploadedArchives)
                && Objects.equals(polarity, other.polarity)
                && Objects.equals(msMode, other.msMode)
                && Objects.equals(extras, other.extras);
    }
}
