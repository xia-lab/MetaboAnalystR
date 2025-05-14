package pro.metaboanalyst.project;
import java.util.Date;


public class JobDetails {
	
    private Long id;
    private int userNM;
    private String folder;
    private String description;
    private String status;
    private String command;
    private String errorMsg;
    private Date creationDate;
    private String priority;
    private Long pid;
    private int sampleNum;
    private String nodes;

    public JobDetails() {

    }

    public JobDetails(
            Long id,
            int userNM,
            String folder, 
            String description, 
            String status, 
            Date creationDate, 
            String command, 
            String errorMsg, 
            Long pid, 
            int sampleNum, 
            String priority,
            String nodes) {
        super();
        this.id = id;
        this.userNM = userNM;
        this.folder = folder;
        this.description = description;
        this.status = status;
        this.creationDate = creationDate;
        this.command = command;
        this.errorMsg = errorMsg;
        this.pid = pid;
        this.sampleNum = sampleNum;
        this.priority = priority;
        this.nodes = nodes;
    }

    public String getNodes() {
        return nodes;
    }

    public void setNodes(String nodes) {
        this.nodes = nodes;
    }

    public int getUserNM() {
        return userNM;
    }

    public void setUserNM(int userNM) {
        this.userNM = userNM;
    }

    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getFolder() {
        return folder;
    }

    public void setFolder(String folder) {
        this.folder = folder;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public String getCommand() {
        return command;
    }

    public void setCommand(String command) {
        this.command = command;
    }

    public String getErrorMsg() {
        return errorMsg;
    }

    public void setErrorMsg(String errorMsg) {
        this.errorMsg = errorMsg;
    }

    public Long getPid() {
        return pid;
    }

    public void setPid(Long pid) {
        this.pid = pid;
    }

    public int getSampleNum() {
        return sampleNum;
    }

    public void setSampleNum(int sampleNum) {
        this.sampleNum = sampleNum;
    }
	
}
