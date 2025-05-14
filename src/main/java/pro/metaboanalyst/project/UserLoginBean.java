/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.project;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.faces.application.FacesMessage;
import jakarta.inject.Named;
import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import kong.unirest.Unirest;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.utils.JacksonObjectMapper;
import java.util.concurrent.TimeUnit;
import org.primefaces.PrimeFaces;

/**
 *
 * @author Qiang
 */
@SessionScoped
@Named("userLoginBean")
public class UserLoginBean implements Serializable {

    @Inject
    SessionBean1 sb;
    
    @Inject
    ApplicationBean1 ab;

    //User details
    private String email; // email is used as the username
    private String password, fname, lname, institution;
    private String salt; // after encryption
    private long userNM;
    private String securedPassword;
    private String passwordSalt;

    //User status
    private boolean loggedin = false;
    private boolean mbrshipped = false;
    private int mbrshipOpt = 0;

    //access related flags
    boolean justRegistered = false;
    boolean justResetted = false;
    boolean justloggedin = false;

    public UserLoginBean() {
        Unirest.config().setObjectMapper(new JacksonObjectMapper());
    }

    //Section 1: User regular operation
    private MariaDBController mdbb;

    public MariaDBController getMdbb() {
        try {
            if (MysqlDBAvailabilityCheck()) {
                return mdbb;
            }
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Project server is down, report this code: wwwp00066 to administrator !");
        }
        return null;
    }

    public boolean MysqlDBAvailabilityCheck() throws Exception {

        if (ab.isOnQiangPc()) {
            mdbb = new MariaDBController("jdbc:mysql://localhost:3306/devUsers", "qiangPC", "1qazxDR%");
            //return true;
        } else if (ab.isOnProServer()) {
            mdbb = new MariaDBController("jdbc:mysql://198.168.185.149:3306/devUsers", "devglassfish", "1qazxDR%");
            //mdbb = new MariaDBController("jdbc:mysql://10.240.0.12:3306/devUsers", "devglassfish", "1qazxDR%");
        } else if(ab.isInDocker()) {
            return(false);
        } else {
            //For real application (dev + genap public)
            mdbb = new MariaDBController("jdbc:mysql://198.168.185.149:3306/devUsers", "devglassfish", "1qazxDR%");
        }

        boolean res = mdbb.connect();
        boolean res2 = mdbb.disconnect();
        System.out.println("MysqlDBAvailabilityCheck result: (true is working)" + res + "|" + res2);
        return (res && res2);
    }

    public String doSecuredLogin() throws Exception {

        if (!MysqlDBAvailabilityCheck()) {
            sb.addMessage("error", "Login database server is temporarily down! Please try to access later.");
            return "";
        }

        userNM = checkUserExist();

        if (userNM == 0) {
            sb.addMessage("error", "The account does not exist! Please register first.");
            return "";
        }

        checkLoginPassword();

        if (sb.getCurrentLoginUser() == null) {
            sb.addMessage("error", "Login error! Please check access details or create an account if you do not have one.");
            return "";
        } else {
            sb.addMessage("info", "Login Successful!.");
        }

        //TODO: Perform user access
        //populateProjectBeans();
        sb.setRegisteredLogin(true);
        return "projects";

    }

    public void doLoginKeep(String ProjectKey) throws Exception {

        if (!MysqlDBAvailabilityCheck()) {
            sb.addMessage("error", "The login database server is temporarily down! Please try to access later.");
        }

        boolean veriRes = linkverify(ProjectKey);
        ResultSet res = null;

        if (veriRes) {
            String checkQuery = "select email, name, institute  from devUsers.userInfo where userNM = " + userNM + ";";

            try {
                res = mdbb.runQuery(checkQuery);
                while (res.next()) {
                    email = res.getString(1);
                    fname = res.getString(2);
                    institution = res.getString(3);
                }

            } catch (Exception ex) {
                sb.addMessage("error",
                        "Login server is down, report this code: xxx00004 to administrator !");
            } finally {
                mdbb.disconnect();
            }
        }

        UserLoginModel UserInfo = new UserLoginModel(userNM, email, password, 1, "", new Date(), fname, institution);
        UserInfoSet = UserInfo;
        sb.setCurrentLoginUser(UserInfoSet);

        if (userNM == 0) {
            sb.addMessage("error", "The account does not exist! Please register first.");
        }

        if (sb.getCurrentLoginUser() == null) {
            sb.addMessage("error", "Login error! Please check access details or create an account if you do not have one.");
        } else {
            sb.addMessage("info", "Login Successful!");
        }

        //TODO: Perform user access
        //populateProjectBeans();
        sb.setRegisteredLogin(true);

    }

    private UserLoginModel UserInfoSet;

    public void checkLoginPassword() throws SQLException {

        String query = "select userNM, passwd, salt, name, institute from devUsers.userInfo where email=\"" + email + "\";";
        String pw = null;
        String slt = null;
        String userfname = "NA";
        String userInstitute = "NA";

        try {
            ResultSet res = mdbb.runQuery(query);

            if (res.next()) {
                userNM = res.getLong(1);
                pw = res.getString(2);
                slt = res.getString(3);
                userfname = res.getString(4);
                userInstitute = res.getString(5);
            }

        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00001 to administrator !");
        } finally {
            mdbb.disconnect();
        }

        System.out.println("password: " + password + "pw: " + pw + "slt: " + slt);
        boolean verRes = PasswordUtils.verifyUserPassword(password, pw, slt);

        if (verRes) {

            UserLoginModel UserInfo = new UserLoginModel(userNM, email, password, 1, "", new Date(), userfname, userInstitute);

            UserInfoSet = UserInfo;
            //UserInfo.setEmail(email);         

        } else {
            System.out.print("Your password is wrong!");
        }

        sb.setCurrentLoginUser(UserInfoSet);
    }

    public String doRegister() throws SQLException {

        //0. CHECK THE MARIADB IS CONNECTABLE OR NOT (to initialized the mddb)
        try {
            if (!MysqlDBAvailabilityCheck()) {
                sb.addMessage("error", "Login database server is temporarily down! Please try to access later.");
                return "";
            }
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00002 to administrator !");
        }

        //1. check first if the username is in the database
        int userId = checkUserExist();

        if (userId != 0) {
            sb.addMessage("error", "Account exists! This email has been registered as an account. Please login or use password reset.");
        } else {
            //2. encrypt password

            boolean encrypted = encryptPassword();

            if (encrypted) {
                //3. insert details of the new account to database
                boolean userInserted = insertUser();
                if (userInserted) {
                    justRegistered = true;
                    return "login";
                } else {
                    sb.addMessage("error",
                            "Login server is down, report this code: xxx00005 to administrator !");
                }
            } else {
                sb.addMessage("error",
                        "Login server is down, report this code: xxx00006 to administrator !");
            }

        }
        return "";
    }

    private boolean encryptPassword() {

        boolean encrypted = false;
        try {
            PasswordUtils encryptedPW = encryptPassword(password);

            securedPassword = encryptedPW.getSecuredPassword();
            passwordSalt = encryptedPW.getSalt();
            return true;
        } catch (Exception ex) {
            //
        }
        return encrypted;
    }

    public PasswordUtils encryptPassword(String pwd) {
        // Generate Salt. The generated value can be stored in DB. 
        String salttmp = PasswordUtils.getSalt(30);

        // Protect user's password. The generated value can be stored in DB.
        String securedPasswordtmp = PasswordUtils.generateSecurePassword(pwd, salttmp);
        return new PasswordUtils(salttmp, securedPasswordtmp);
    }

    private boolean insertUser() {

        String InsertQuery = "insert into devUsers.userInfo (email, passwd, salt, name, institute) values ('"
                + email + "','" + securedPassword + "','" + passwordSalt + "','"
                + fname + "','" + institution + "');";

        try {
            mdbb.runUpdate(InsertQuery);
            return true;
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00004 to administrator !");
        }
        return false;
    }

    private int checkUserExist() throws SQLException {

        int userID = 0;

        String query = "select userNM from devUsers.userInfo where email='" + email + "';";

        try {
            MysqlDBAvailabilityCheck();
            ResultSet res = mdbb.runQuery(query);
            if (res.next()) {
                userID = res.getInt(1);
            }
        } catch (Exception ex) {
            System.out.println("ex is :" + ex);
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00002 to administrator !");
        } finally {
            mdbb.disconnect();
        }

        return userID;
    }

    private boolean jobManager = false;

    public boolean isJobManager() {

        jobManager = "zhiqiang.pang@mail.mcgill.ca".equals(email)
                || "guangyan.zhou@mail.mcgill.ca".equals(email)
                || "jeff.xia@mcgill.ca".equals(email)
                || "pangzq2812@gmail.com".equals(email);
        System.out.println("is manager is ----> " + jobManager);
        return jobManager;
    }

    public boolean linkverify(String ProjectKey) throws SQLException {

        if (ProjectKey.length() < 16) {
            sb.addMessage("error",
                    "This is not a valid project loading link!");
        }

        boolean linkvalid = false;
//        String checkQuery = "insert into devUsers.userInfo (email, passwd, salt, name, institute) values ('"
//                + email + "','" + securedPassword + "','" + passwordSalt + "','"
//                + fname + "','" + institution + "');";

        String checkQuery = "select projectFolderNM from devUsers.projects where userNM = " + userNM + ";";

        try {

            ResultSet res = mdbb.runQuery(checkQuery);
            while (res.next()) {
                if (res.getString(1).contains(ProjectKey)) {
                    linkvalid = true;
                }
            }

            return linkvalid;
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00004 to administrator !");
        } finally {
            mdbb.disconnect();
        }
        return linkvalid;
    }

    //section 2: User infor getter and setter for registeration and login
    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getFname() {
        if (sb.getCurrentLoginUser() != null) {
            return sb.getCurrentLoginUser().getFname();
        } else {
            return fname;
        }
    }

    public void setFname(String fname) {
        this.fname = fname;
    }

    public String getLname() {
        if (sb.getCurrentLoginUser() != null) {
            return sb.getCurrentLoginUser().getLname();
        } else {
            return lname;
        }
    }

    public void setLname(String lname) {
        this.lname = lname;
    }

    public String getInstitution() {
        if (sb.getCurrentLoginUser() != null) {
            return sb.getCurrentLoginUser().getInstitution();
        } else {
            return institution;
        }
    }

    public void setInstitution(String institution) {
        this.institution = institution;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getSalt() {
        return salt;
    }

    public void setSalt(String salt) {
        this.salt = salt;
    }

    public String getSecuredPassword() {
        return securedPassword;
    }

    public void setSecuredPassword(String securedPassword) {
        this.securedPassword = securedPassword;
    }

    public int getNumOfProjects() {
        return sb.getCurrentLoginUser().getNumOfProjects();
    }

    public long getUserNM() {
        return userNM;
    }

    public void setUserNM(long userNM) {
        this.userNM = userNM;
    }

    public void doSendEmail() throws SQLException {

        int userID = checkUserExist();

        if (userID != 0) {

            // prepare tokens for password reset later
            String resetToken = UUID.randomUUID().toString().replaceAll("-", "");
            Date ExpDate = new Date();
            ExpDate = addHoursToJavaUtilDate(ExpDate, 1);

            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            System.out.println("Setting resetToken as: " + resetToken + sdf.format(ExpDate));

            String tokenstmp = "insert into devUsers.tokens (userNM, tokens, ExpDate) values ("
                    + userID + ",\'" + resetToken + "\', \'" + sdf.format(ExpDate) + "\');";

            try {
                mdbb.runUpdate(tokenstmp);
            } catch (SQLException ex) {
                sb.addMessage("error",
                        "Login server is down, report this code: xxx00007 to administrator !");
            }

            try {
                sendResetMessage(email, resetToken);
                sb.addMessage("info", "Reset Invoked successfully! Please your email (maybe in junk box) and follow the instruction to reset your password!");
            } catch (Exception ex) {
                sb.addMessage("error",
                        "Reseting mail server is down, report this code: xxx00008 to administrator !");
                System.out.println(ex);
            }

        } else {
            sb.addMessage("error", "Email does not exist! Please register first with this email!");
        }
        //boolean resetSent = SpringAPIUtils.resetEmailRequest(email);
        //if (resetSent) {
        //    sb.addMessage(FacesMessage.SEVERITY_INFO, "Reset is successful!", "Please check your email for the instructions.");
        //} else {
        //    sb.addMessage(FacesMessage.SEVERITY_ERROR, "Login server is temporarily down!", "Please try to reset password later.");
        //}
    }

    private Date addHoursToJavaUtilDate(Date date, int hours) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.HOUR_OF_DAY, hours);
        return calendar.getTime();
    }

    public String doResetPassword() throws SQLException {

        String token = sb.getResetToken();

        if (token == null) {
            sb.addMessage("error", "Invalid reset link! Please use the valid resrt link.");
            return "";
        }

        // check server is down or not
        try {
            if (!MysqlDBAvailabilityCheck()) {
                sb.addMessage("error", "Login database server is temporarily down! Please try to access later.");
                return "";
            }
        } catch (Exception ex) {
            System.out.println("Congs! The cloud server is working~~");
        }

        // check token is expired or not
        int unm = verifyToken(token);

        if (unm != 0) {
            // do the reset on the password - encrypt and update the cloud mysql
            boolean resetDone = resetPassword(unm, password);
            if (resetDone) {
                //delete the token infor from database
                String deleteQuery = "delete from devUsers.tokens where userNM = " + unm + ";";
                mdbb.runUpdate(deleteQuery);

                justResetted = true;
                return "login";
            }
        }

        return null;
    }

    public void welcomeJustRegistered() {
        if (justRegistered) {
            sb.addMessage("info", "Account created successfully! Please use your login details to access metaboanalyst.");
        } else if (justResetted) {
            sb.addMessage("info", "Password updated successfully! Please use your login details to access metaboanalyst.");
        }
    }

    private int verifyToken(String token) throws SQLException {

        String tokenquery = "select userNM,expDate from devUsers.tokens where tokens = '" + token + "';";
        ResultSet res = null;
        try {
            res = mdbb.runQuery(tokenquery);
        } catch (SQLException ex) {
            Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        Date currentTime = new Date();
        long diff = 100;
        int userid = 0;
        if (res.next()) {
            userid = res.getInt(1);
            Date expTime = res.getTimestamp(2);
            //caculate the time difference from now to the time of reseting invoking
            long timed = currentTime.getTime() - expTime.getTime();
            diff = TimeUnit.MINUTES.convert(timed, TimeUnit.MILLISECONDS);
        } else {
            sb.addMessage("Error", "Invalid resetting link! Please reinvoke the resetting or contacting with administrator!");
        }

        if (diff < 1) {
            return userid;
        } else if (diff > 1) {
            sb.addMessage( "Error", "Your reset link has expired!");
        }

        mdbb.disconnect();

        return 0;
    }

    private boolean resetPassword(int userNM, String password) {

        System.out.println("Now the new password is: " + password);

        PasswordUtils encryptPW = encryptPassword(password);

        String query = "update devUsers.userInfo set passwd = '"
                + encryptPW.getSecuredPassword()
                + "', salt = '" + encryptPW.getSalt() + "' where userNM = "
                + userNM + ";";

        try {
            mdbb.runUpdate(query);
            return true;
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Reseting mail server is down, report this code: xxx00010 to administrator !");
        }

        return false;
    }

    //Section 3: special section for genap private
    private String workspace = null;

    public String getWorkspace() {
        return workspace;
    }

    public void setWorkspace(String workspace) {
        this.workspace = workspace;
    }

    public int checkWorkSpaceExist() throws SQLException {

        int userID = 0;

        String query = "select userNM from devUsers.userInfo where name='" + workspace + "';";

        try {
            MysqlDBAvailabilityCheck();
            ResultSet res = mdbb.runQuery(query);
            if (res.next()) {
                userID = res.getInt(1);
            }
        } catch (Exception ex) {
            System.out.println("ex is :" + ex);
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00002 to administrator !");
        } finally {
            mdbb.disconnect();
        }

        return userID;
    }

    public boolean insertWorkSpace() {

        String InsertQuery = "insert into devUsers.userInfo (email, passwd, salt, name, institute) values ('NULL','NULL','NULL','"
                + workspace
                + "','Genap');";

        try {
            mdbb.runUpdate(InsertQuery);
            return true;
        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx000048 to administrator !");
        }
        return false;
    }

    public void SigninStatus(int userNM0) {

        email = "unknown";
        fname = workspace;
        institution = "genapUser";
        password = "";
        userNM = userNM0;

        UserLoginModel UserInfo = new UserLoginModel(userNM, email, password, 1, "", new Date(), fname, institution);
        UserInfoSet = UserInfo;
        sb.setCurrentLoginUser(UserInfoSet);

        sb.setRegisteredLogin(true);
    }

    public boolean sendResetMessage(String to, String resetToken) throws JsonProcessingException, IOException, InterruptedException {

        String url = "https://www.metaboanalyst.ca/faces/users/ResetView.xhtml?token=" + resetToken;

        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<head>\n"
                + "</head>\n"
                + "\n"
                + "<body style=\"font-family: Arial; font-size: 12px;\">\n"
                + "<div>\n"
                + "    <p>\n"
                + "        You have requested a password reset, please follow the link below to reset your password.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        Please ignore this email if you did not request a password change.\n"
                + "    </p>\n"
                + "\n"
                + "    <p>\n"
                + "        <a href=\"" + url + " \">\n"
                + "            Follow this link to reset your password.\n"
                + "        </a>\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        This reset link will expire in 60 minutes. Please don't reply.\n"
                + "    </p>\n"
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        return DataUtils.sendMail(to, "Resetting password (MetaboAnalyst)", htmlMsg);
    }


    // Section I: General Postgresql Utils
    private PSQLdbController psqlb;

    public boolean PsqlDB_create() {
        
        if (ab.isOnQiangPc()) {            
            psqlb = new PSQLdbController("jdbc:postgresql://132.216.38.4:5432/omicsdb", "devserver", "1qazxDR%^yHN");
            //return true;
        } else if (ab.isOnProServer()) {
            psqlb = new PSQLdbController("jdbc:postgresql://132.216.38.4:5432/omicsdb", "devserver", "1qazxDR%^yHN");
            //psqlb = new PSQLdbController("jdbc:postgresql://10.240.0.12:5432/omicsdb", "devserver", "1qazxDR%^yHN");
        } else {
            System.out.println("RUnning into this option 3 <---");
            //For real application (dev + genap public)            
            psqlb = new PSQLdbController("jdbc:postgresql://132.216.38.4:5432/omicsdb", "devserver", "1qazxDR%^yHN");
        }
        System.out.println("RUnning into this option 3xxxxxxx  <---" + psqlb);
        boolean res1 = false, res2 = false;
        try {
            res1 = psqlb.connect();
            res2 = psqlb.disconnect();
        } catch (SQLException ex) {
            System.out.println("There is an error when doing --> PsqlDB_create === 768");
            Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        if (res1 && res2) {
            psqlb.setCreated(true);
            return true;
        } else {
            psqlb.setCreated(false);
            return false;
        }
    }

    public boolean PsqlDBAvailabilityCheck() {
        System.out.println("PsqlDBAvailabilityCheck --> 1 " + psqlb);
        boolean created;
        if (psqlb == null) {
            created = false;
        } else {
            created = psqlb.isCreated();
        }

        if (created) {
            boolean res1 = false, res2 = false;
            try {
                res1 = psqlb.connect();
                res2 = psqlb.disconnect();
            } catch (SQLException ex) {
                System.out.println("There is an error when doing --> PsqlDBAvailabilityCheck === 795");
                //Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
            }
            return (res1 && res2);
        } else {
            return PsqlDB_create();
        }
    }

    private int checkUserRegistered() {

        int userID = 0;

        String query = "select userNM from userInfo where email='" + email + "';";
        try {

            ResultSet res = psqlb.runQuery(query);
            if (res.next()) {
                userID = res.getInt(1);
            }
        } catch (Exception ex) {
            System.out.println("ex is :" + ex);
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00002psql to administrator !");
        } finally {
            try {
                psqlb.disconnect();
            } catch (SQLException ex) {
                //Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
                System.out.println("There is an error when doing --> checkUserRegistered");
            }
        }

        return userID;
    }

    private boolean validatePassword(String passwd, int userNum) {
        /*Note: userNum is userNM, to avoid confusion here
        passwd: is the password from users' input
        userNum: is the userNM from database
        This function is used to validate if the passwd match the one in database
        **/

        String query = "select passwd, salt, name, institute from userInfo where userNM=" + userNum + ";";
        String pw = null;
        String slt = null;
        String userfname = "NA";
        String userInstitute = "NA";

        try {
            ResultSet res = psqlb.runQuery(query);

            if (res.next()) {                
                pw = res.getString(1);
                slt = res.getString(2);
                userfname = res.getString(3);
                userInstitute = res.getString(4);
            }

        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00001 to administrator !");
        } finally {
            try {
                psqlb.disconnect();
            } catch (SQLException ex) {
                System.out.println("There is an error when doing --> validatePassword");
                //Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        System.out.println("password: " + passwd + " --> pw: " + pw + "slt: " + slt);
        boolean verRes = PasswordBean.verifyUserPassword(passwd, pw, slt);

        if (verRes) {
            UserLoginModel UserInfo;
            UserInfo = new UserLoginModel(userNM, email, passwd, 1, "", new Date(), userfname, userInstitute);
            UserInfoSet = UserInfo;
            sb.setCurrentLoginUser(UserInfoSet);
            return true;
        } else {
            sb.addMessage("Error",
                    "Your password is invalid! Please retry or reset it!");
            System.out.print("Your password is wrong!");
            return false;
        }
    }
    
    private String generateToken(int type, int userNum, int valid_min){
        /*
            Valid time: the valid time of this link, unit: minutes.
        */
        String token_char = "";
        // Check user logged-in, in case of hacking
        if(!loggedin){
            sb.addMessage("Warn", "Your login session expired. Please redo the login!");
            return token_char;
        }
        
        // Generate a random token        
        token_char = UUID.randomUUID().toString();
        
        Date ExpDate = new Date();
        ExpDate = addHoursToJavaUtilDate(ExpDate, valid_min/60);
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        System.out.println("Setting Token as: " + token_char + sdf.format(ExpDate));
        
        // Insert a new line into psql token table
        String tokenscmd = "insert into tokens (userNM, token, ExpDate, type) values ("
                + userNum + ",\'" + token_char + "\', \'" + sdf.format(ExpDate) + "\', " + type + ");";
        
        psqlb.runUpdate(tokenscmd);        
        return token_char;
    }
    
    private void cleanOldTokens(){
        // TODO: to code this function later..
    }
    
    

    // Section II: General User Management function
    public boolean isLoggedin() {
        return loggedin;
    }

    public void setLoggedin(boolean loggedin) {
        this.loggedin = loggedin;
    }

    public boolean isJustloggedin() {
        return justloggedin;
    }

    public void setJustloggedin(boolean justloggedin) {
        this.justloggedin = justloggedin;
    }

    public void doGeneralLogin() {
        boolean stepBool1, stepBool2;
        System.out.println("doGeneralLogin --> step0");
        // Check PSQL db availability
        stepBool1 = PsqlDBAvailabilityCheck();
        System.out.println("doGeneralLogin --> step1 + psqldb avalib: " + stepBool1);
        if (!stepBool1) {
            sb.addMessage("error", "Login database server is temporarily down! Please try to access later.");
            return;
        }

        // Check user exists
        userNM = checkUserRegistered();
        if (userNM == 0) {
            sb.addMessage("error", "The account does not exist! Please register first.");
        }
        
        // Check password
        stepBool2 = validatePassword(password, (int) userNM);
       
        // Update as logged-in Status
        if (stepBool2) {
            setLoggedin(true);
            sb.setRegisteredLogin(true);
            setJustloggedin(true);
            setWelcomeLabel("Hello, " + UserInfoSet.getFname());
            FacesContext context = FacesContext.getCurrentInstance();
            context.addMessage(null, new FacesMessage("OK", "Login Succeeds! Welcome, " + UserInfoSet.getFname()));
        } else {
            sb.addMessage("error", 
                    "Login error! Please check access details or create an account if you do not have one.");
        }               
    }

    public void doGeneralSignOut(){
        sb.addMessage("info", "Thank you and see you next time, " + UserInfoSet.getFname()); 
        setLoggedin(false);
        sb.setRegisteredLogin(false);
        setJustloggedin(false);
        setMbrshipped(false);
            
    }
    
    public void doGeneralSignup(){
        // TODO: to develop this function
        // verify the email used or not        
        int res = checkUserRegistered();
        if(res == 0){
            // new user
        } else {
            // already registered, show a msg
        }
        
        // Generate password for this account
        generatePassword();
        
        // Insert into psql
    
    }
    
    private String generatePassword() {
        // This function is used for sign up

        return "";
    }
/*
    public int todoregister() {
        // This function is designed to switch to the Sign Up dialog
        PrimeFaces.current().executeScript("PF('signin').hide();");
        PrimeFaces.current().executeScript("PF('signup').show();");
        return 1;
    }
*/
    private String welcomeLabel;

    public String getWelcomeLabel() {
        return welcomeLabel;
    }

    public void setWelcomeLabel(String welcomeLabel) {
        this.welcomeLabel = welcomeLabel;
    }

    public String getUserabbre() {
        if (UserInfoSet != null) {
            String userabbre0 = UserInfoSet.getFname();
            if(userabbre0.length() < 5){
                return userabbre0;
            } else {
                String[] userabbrex = userabbre0.split(" ");
                if(userabbrex.length >= 2){
                    return userabbrex[0].substring(0, 1) + userabbrex[1].substring(0, 1);
                } else {                    
                    return userabbrex[0].substring(0, 1);
                }
            }
        }
        return "NA";
    }
    
    private boolean doquicklogin(int userNum){
        String query = "select email, name, institute, mbrspStatus from userInfo where userNM=" + userNum + ";";

        String userfname = "NA";
        String userInstitute = "NA";
        userNM = userNum;

        try {
            ResultSet res = psqlb.runQuery(query);
            if (res.next()) {
                email = res.getString(1);
                userfname = res.getString(2);
                userInstitute = res.getString(3);
                if(res.getString(4).equals("1")){
                    setMbrshipped(true);
                    sb.setPrivileged(true);
                }
            }

        } catch (Exception ex) {
            sb.addMessage("error",
                    "Login server is down, report this code: xxx00001 to administrator !");
            return false;
        } finally {
            try {
                psqlb.disconnect();
            } catch (SQLException ex) {
                System.out.println("There is an error when doing --> doquicklogin");
                return false;
                //Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
            }            
        }
        
        UserLoginModel UserInfo;
        UserInfo = new UserLoginModel(userNum, email, "", 1, "", new Date(), userfname, userInstitute);
        UserInfoSet = UserInfo;
        sb.setCurrentLoginUser(UserInfoSet);
        
        return true;
    }
    
    public String keepLoggedinToken(int userNum){
        // This function is designed to keep user logged-in when redirected among modules/servers
        // General idea is to create a temporary token with a limited valid time (10min)
        String token = generateToken(1, userNum, 60);    
        return userNum + "_" + token;
    }
    
    public boolean doKeepLoggedin(String redirToken){
        String[] res = redirToken.split("_");
        String unm = res[0];
        String tkn = res[1];
        Date currentTime = new Date();
        String queryCMD = "select * from tokens where usernm = " + unm + " AND token = \'" + tkn + "\';";        
        if(!PsqlDBAvailabilityCheck()){
            return false;
        }
        
        ResultSet qres;
        qres = psqlb.runQuery(queryCMD);                
        long diff = 100;
        try {
            if (qres.next()) {
                Date expTime = qres.getTimestamp(3);
                //caculate the time difference from now to the time of log-in
                long timed = currentTime.getTime() - expTime.getTime();
                diff = TimeUnit.MINUTES.convert(timed, TimeUnit.MILLISECONDS);
            } else {
                sb.addMessage("Error", "Invalid resetting link! Please reinvoke the resetting or contacting with administrator!");
            }
        } catch (SQLException ex) {
            Logger.getLogger(UserLoginBean.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        if(diff < 60){
            int unm_int = Integer.parseInt(unm);
            return doquicklogin(unm_int);        
        }
        return false;
    }
    
    // Section III: Membership Validation
    
    /*
    Explaination of the membership items in userInfo of psql
    
    mbrspStatus: int; 0, free users; 1, users paid;
    mbrspValid: timestamp; Null, Valid permenantly; Others, the expired date;
    mbrspOption: int; different numbers for different commericial plan (TODO: to be designed);
    
    */
    public boolean isMbrshipped() {
        System.out.println("Now the mbrshipped is --> " + mbrshipped);
        return mbrshipped;
    }

    public void setMbrshipped(boolean mbrshipped) {
        this.mbrshipped = mbrshipped;
    }

    public int getMbrshipOpt() {
        return mbrshipOpt;
    }

    public void setMbrshipOpt(int mbrshipOpt) {
        this.mbrshipOpt = mbrshipOpt;
    }

}
