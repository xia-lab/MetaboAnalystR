/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.project;

import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;

/**
 *
 * @author xia
 */
@RequestScoped
@Named("projCtrl")
public class ProjectController implements Serializable {

    @Inject
    private ApplicationBean1 ab;
    @Inject
    private SessionBean1 sb;
    @Inject
    private UserLoginBean ulb;
    @Inject
    private ProjectBean pb;
    
    private MariaDBController mdbb;
    private long userNM;
    private String dataPlace = "";

    public void createProject() throws SQLException {


        // Get user and project infomation for creation
        userNM = ulb.getUserNM();

        String newProTitle = pb.getTitle();
        String newProDesp = pb.getDescription();
        String newProType = pb.getProjectType();

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String newProDate = sdf.format(new Date());

        Random random = new Random();
        String newProMarker = "startnv"
                + String.format("%09d", random.nextInt(999999999))
                + String.format("%09d", random.nextInt(999999999));

        if (ab.isOnProServer()) {
            dataPlace = "dev";
        } else if (ab.isOnQiangPc()) {
            dataPlace = "qiang";
        }

        // Update mysql datebase
        mdbb = ulb.getMdbb();

        //make sure the project title is identical
        String proVeriQuery = "SELECT EXISTS(select * from devUsers.projects where userNM=" + userNM + " AND title = '" + newProTitle + "');";
        ResultSet res;
        try {
            res = mdbb.runQuery(proVeriQuery);
            if (res.next()) {
                if (res.getInt(1) == 1) {
                    sb.addMessage("error", "Project exists! The project name should not be identical, please rename your project!");
                    return;
                }
            }
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Project server is down, report this code: xxxp00014 to administrator !");
            return;
        } finally {
            mdbb.disconnect();
        }

        //insert this project
        String proQuery = "insert into devUsers.projects (UserNM, title, description, "
                + "type, creationTime, status, projectFolderNM, dataplace) values ("
                + userNM + ",'"
                + newProTitle + "','"
                + newProDesp + "','"
                + newProType + "','"
                + newProDate + "','"
                + "naive" + "','"
                + newProMarker + "','"
                + dataPlace + "');";
        try {
            mdbb.runUpdate(proQuery);
            sb.addMessage("info", "Project Initialized successfully! Please click 'Start' to start!");
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Project server is down, report this code: xxxp00013 to administrator !");
        }

        //System.out.println("Print here to check: " + newProTitle + newProDesp + newProType + newProDate);
    }

    public void saveProject() {
        // Exit directly
        sb.doLogout(1);
    }

    public void callAccountDelete() {

        // 1. delete all projects & userNM of the corresponding user
        long usernm = ulb.getUserNM();
        MariaDBController mdbbim = pb.getMdbb();

        String deleteAcQuery = "delete from devUsers.projects where UserNM = " + usernm + ";";
        String deleteAcQuery2 = "delete from devUsers.userInfo where UserNM = " + usernm + ";";
        try {
            mdbbim.runUpdate(deleteAcQuery);
            mdbbim.runUpdate(deleteAcQuery2);
            sb.addMessage("info", "OK - the account has been deleted!");
        } catch (SQLException ex) {
            sb.addMessage("error",
                    "Account server is down, report this code: xxxp00019 to administrator !");
        }

        //2. logout and forward to home page
        sb.doLogout(1);

    }

    public void callProjectDelete() {

        ProjectModel selectedProject = pb.getSelectedProject();

        String Profolder = selectedProject.getFolder();
        long usernm = selectedProject.getUserNM();

        String deleteQuery = "delete from devUsers.projects where UserNM = "
                + usernm
                + " AND projectFolderNM = '"
                + Profolder
                //                + "' AND dataplace = '" 
                //                + dataPlace 
                + "';";
        MariaDBController mdbbim = pb.getMdbb();
        try {
            mdbbim.runUpdate(deleteQuery);
            sb.addMessage("info", "OK - the project has been deleted!");
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Project server is down, report this code: xxxp00018 to administrator !");
        }
    }

    public void setupProjectTable() throws SQLException {

        mdbb = ulb.getMdbb();
        userNM = ulb.getUserNM();
        List<ProjectModel> projectList = obtainProjectInfor();

        //HashMap<Long, ProjectModel> projectMap = new HashMap(); //disable project map for now to see
        int rowNum = projectList.size();
        if (rowNum < 11) {
            pb.setProjectTable(projectList);
        } else {
            pb.setProjectTable(projectList.subList(rowNum - 11, rowNum - 1));
        }

        if (rowNum < 11) {
            sb.getCurrentLoginUser().setNumOfProjects(rowNum);
            for (int m = rowNum - 1; m > -1; m--) {
                ProjectModel currentProjectItem = projectList.get(m);
                //projectMap.put(currentProjectItem.getUserNM(), currentProjectItem);
            }
        } else {
            sb.getCurrentLoginUser().setNumOfProjects(10);
            for (int m = rowNum - 1; m > rowNum - 11; m--) {
                ProjectModel currentProjectItem = projectList.get(m);
                //projectMap.put(currentProjectItem.getUserNM(), currentProjectItem);
            }
        }

    }

    public List<ProjectModel> obtainProjectInfor() throws SQLException {

        List<ProjectModel> projectTable = new ArrayList();

        //Check the application position: dev, genap?
//        if (ab.isOnDevServer() || ab.isOnQianglaptop()) {
//            dataPlace = "dev";
//        } else if (ab.isOnGenapPublic()) {
//            dataPlace = "genap";
//        } else {
//            dataPlace = "unk"; // an unknown place, maybe genap private
//        }
        // clean remote mysql
        projectClean();

        // Retrieve the data from db
        String query = "select * from devUsers.projects where userNM=" + userNM + ";";
        try {
            ResultSet res = mdbb.runQuery(query);
            int count = 0;
            while (res.next()) {
                ProjectModel project = new ProjectModel();
                //System.out.println(count + "checking projects res: " + res.getString(2) + res.getString(3) + res.getString(4) + res.getDate(5) + res.getString(6) + res.getString(7) + res.getString(8));

                project.setId(count + 1);
                project.setUserNM(userNM);
                project.setTitle(res.getString(2));
                project.setDescription(res.getString(3));
                project.setType(res.getString(4));
                project.setCreationDate(res.getDate(5));
                project.setStatus(res.getString(6));
                project.setFolder(res.getString(7));
                project.setHostname(res.getString(8));
                if (project.getType().equals("project")) {
                    projectTable.add(count, project);
                }
                count++;
            }
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Project server is down, report this code: pjtp00019 to administrator !");
        } finally {
            mdbb.disconnect();
        }
        return projectTable;
    }

    private void projectClean() {
        //remove the projects 90 days ago for dev and 90 days for genap
        String cleanQuery = "delete from projects where creationTime < CURDATE() - INTERVAL 90 DAY";
        try {
            mdbb.runUpdate(cleanQuery);
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Clean engine is down, report this code: xxxc00088 to administrator !");
        }
    }

    private final static int PROJECT_WARNING_DAYS = 30;

    public String createPartialProjectRaw(String naviString) {
        return "true";
    }

    public String savePartialRaw(String jsonNm, String naviString) {
        return "abc";
    }

    // section 3: projects information
    private String[] titles;
    private String[] descriptions;
    private String[] types;
    private Date[] ctreationtimes;
    private String[] statuses;
    private String[] guestfolders;
    private String[] hostnames;

    public String[] getTitles() {
        return titles;
    }

    public void setTitles(String[] titles) {
        this.titles = titles;
    }

    public String[] getDescriptions() {
        return descriptions;
    }

    public void setDescriptions(String[] descriptions) {
        this.descriptions = descriptions;
    }

    public String[] getTypes() {
        return types;
    }

    public void setTypes(String[] types) {
        this.types = types;
    }

    public Date[] getCtreationtimes() {
        return ctreationtimes;
    }

    public void setCtreationtimes(Date[] ctreationtimes) {
        this.ctreationtimes = ctreationtimes;
    }

    public String[] getStatuses() {
        return statuses;
    }

    public void setStatuses(String[] statuses) {
        this.statuses = statuses;
    }

    public String[] getGuestfolders() {
        return guestfolders;
    }

    public void setGuestfolders(String[] guestfolders) {
        this.guestfolders = guestfolders;
    }

    public String[] getHostnames() {
        return hostnames;
    }

    public void setHostnames(String[] hostnames) {
        this.hostnames = hostnames;
    }

}
