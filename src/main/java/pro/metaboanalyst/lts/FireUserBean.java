/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.lts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.JsonProcessingException;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.DataUtils;
import pro.metaboanalyst.api.DatabaseClient;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Named;
import java.io.Serializable;
import java.net.HttpURLConnection;
import java.net.URL;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.UUID;
import java.util.logging.Level;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import org.omnifaces.util.Faces;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.logging.Logger;

/**
 * @author soufanom
 */
@SessionScoped
@Named("fireUserBean")
public class FireUserBean implements Serializable {

    @Inject
    private FireBase fb;
    @Inject
    private ApplicationBean1 ab;

    @Inject
    private SessionBean1 sb;

    @Inject
    private DatabaseClient db;

    @Inject
    private FireUserBean fub;

    @Inject
    private FireBaseController fbc;

    @JsonIgnore
    @Inject
    private MailService ms;

    //User details
    private String email = ""; // email is used as the username
    private String password, password2, fname, lname, institution;
    private String salt; // after encryption
    private long userNM;
    private String securedPassword;
    private String status;
    private boolean omicsquareVerified = false;
    private String omicsquareToken = "guest";
    //Section 1: User regular operation

    public FireUserBean() {
        // Unirest.config().setObjectMapper(new JacksonObjectMapper());
    }

    public void setFireUserBean(FireUserBean bean) {
        this.email = bean.getEmail();
        this.fname = bean.getFname();
        this.institution = bean.getInstitution();
        this.status = bean.getStatus();
        this.omicsquareVerified = bean.isOmicsquareVerified();
        this.omicsquareToken = bean.getOmicsquareToken();
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

    public String getPassword2() {
        return password2;
    }

    public void setPassword2(String password2) {
        this.password2 = password2;
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
        return 1;
    }

    public long getUserNM() {
        return userNM;
    }

    public void setUserNM(long userNM) {
        this.userNM = userNM;
    }

    /*
        private int numOfProjects;
    public void setNumOfProjects(int numOfProjects) {
        this.numOfProjects = numOfProjects;
    }
     */
    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getOmicsquareToken() {
        return omicsquareToken;
    }

    public void setOmicsquareToken(String omicsquareToken) {
        this.omicsquareToken = omicsquareToken;
    }

    public boolean isOmicsquareVerified() {
        return omicsquareVerified;
    }

    public void setOmicsquareVerified(boolean omicsquareVerified) {
        if (omicsquareVerified) {
            sb.setRegisteredLogin(true);
        } else {
            sb.setRegisteredLogin(false);
        }
        this.omicsquareVerified = omicsquareVerified;
    }

    public void doUserLogin() {
        boolean res = doUserLoginLocal();
        if (res) {
            Faces.addResponseCookie("user", email, "/", 3600);
            fb.getUserMap().put(email, fub);
            setOmicsquareVerified(true);
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/Secure/ModuleView.xhtml", "info", "Login successful!");
        }
    }

    public boolean doUserLoginLocal() {

        String[] res = db.loginUser(email, password, ab.getAppName());
        if (res == null) {
            sb.addMessage("error", "Login failed! Please check the validity of your project path!");
            return false;
        } else if (res.length == 1) {
            sb.addMessage("error", res[0]);
            return false;
        } else { // success
            email = res[0];
            fname = res[1];
            lname = res[2];
            institution = res[3];
            return true;
        }
    }

    public void doRegister() {

        //boolean stepBool1 = db.PsqlDBAvailabilityCheck();
        //System.out.println("doGeneralLogin --> step1 + psqldb avalib: " + stepBool1);
        if (!DataUtils.isValidEmail(email)) {
            sb.addMessage("error", "Please provide a valid email address!");
            return;
        }

        if (email.endsWith("epa.gov")) {
            sb.addMessage("error",
                    "We notice that this email domain can not receive emails from our tool, please register with another email address!");
            return;
        }

        if (password.length() < 8) {
            sb.addMessage("error", "Passwords need to be at least 8 characters long!");
            return;
        }

        if (!password.equals(password2)) {
            sb.addMessage("error", "Passwords do not match!");
            return;
        }

        if (ab.isInDocker()) {
            int userCount = db.detectDockerUserNum();
            if (userCount > 21) {
                sb.addMessage("error", "You have more than allowed number of users registered! You cannot register more. Please contact Xialab Analystic Team (support@xiaalb.ca)!");
                return;
            }
        }

        String res = db.registerUser(email, password, fname, lname, institution);

        if (res.equals("User registered successfully.")) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            String activateCode = DataUtils.getRandom();
            Date ExpDate = new Date();
            ExpDate = addHoursToJavaUtilDate(ExpDate, 1);
            String res2 = db.addActivationCode(activateCode, sdf.format(ExpDate), email);
            if (res2.equals("Update successful.")) {
                try {
                    //boolean res3 = doSendActivateCode(email, activateCode, ExpDate);
                    //if (res3) {
                    DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "info", "Registration successful! Please check your inbox for activating your account! If you are unable to receive the email, please contact the admins!");
                    //}
                } catch (Exception ex) {
                    java.util.logging.Logger.getLogger(FireUserBean.class.getName()).log(Level.SEVERE, null, ex);
                }

            } else {
                sb.addMessage("error", res2);
            }
        } else {
            sb.addMessage("error", res);
        }
    }

    public void doSendEmail() {

        String res = db.checkUserExists(email);
        System.out.println(res);
        if (res.equals("User exists.")) {
            // prepare tokens for password reset later
            String resetToken = UUID.randomUUID().toString().replaceAll("-", "");
            Date ExpDate = new Date();
            ExpDate = addHoursToJavaUtilDate(ExpDate, 1);

            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            System.out.println("Setting resetToken as: " + resetToken + sdf.format(ExpDate));

            String res2 = db.insertToken(email, resetToken, sdf.format(ExpDate));
            System.out.println(res2);
            try {
                boolean res3 = sendResetMessage(email, resetToken);
                if (res3) {
                    sb.addMessage("info",
                            "Reset Invoked successfully! Please check your email (maybe in junk box) and follow the instruction to reset your password!");
                } else {
                    sb.addMessage("error", "Unable to send email!");
                }
            } catch (Exception ex) {
                email = "";
                sb.addMessage("error",
                        "Reseting mail server is down, report this code: xxx00008 to administrator !");
                System.out.println(ex);
            }

        } else {
            email = "";
            sb.addMessage("error", "Email Not Exits! Please register first with this email!");
        }
    }

    public void doResetPassword() throws SQLException {
        String token = sb.getResetToken();
        System.out.println("toknreset======" + token);
        if (token == null) {
            sb.addMessage("error", "Invalid reset link! Please use a valid reset link.");
            return;
        }

        // check token is expired or not
        String res = db.verifyToken(token);

        if (!res.contains("Error encountered:")) {
            email = res;
            String resetDone = db.resetPassword(res, password);
            System.out.println(resetDone);
            if (resetDone.equals("Success")) {
                db.deleteTokenForUser(email);
                DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "info", "Please login with new password!");
            } else {
                sb.addMessage("error", resetDone);
            }
        } else {
            sb.addMessage("error", res);
        }
    }

    public boolean sendResetMessage(String to, String resetToken) throws JsonProcessingException, IOException, InterruptedException {

        String url = "";

        if (ab.isOnLocalServer()) {
            url = "http://localhost:8080/" + ab.getAppName() + "/users/ResetView.xhtml?token=" + resetToken;
        } else {
            url = "https://pro.metaboanalyst.ca/" + ab.getAppName() + "/users/ResetView.xhtml?token=" + resetToken;
        }
        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
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
                + "        Copy and paste this link into your browser to reset your password: " + url
                + "    </p>\n"
                + "\n Do NOT reply this email."
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        boolean res = ms.sendEmail(to, ab.getAppName() + " - Password reset", "text/html", htmlMsg);
        return res;
    }

    public static Date addHoursToJavaUtilDate(Date date, int hours) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.add(Calendar.HOUR_OF_DAY, hours);
        return calendar.getTime();
    }

    public void activateAccount() {
        String res = db.checkActivationCode(email, activationCode);
        if (res.equals("Success!")) {
            DataUtils.doRedirectWithGrowl(sb, "/" + ab.getAppName() + "/users/LoginView.xhtml", "info", "Account has been activated! You can login by entering your password!");
        } else {
            sb.addMessage("error", res);
        }
    }

    private long lastSuccessfulExecutionTime = 0; // Store the last successful execution time (in milliseconds)

    public void resendActivationCode() {

        long currentTime = System.currentTimeMillis();
        long timeDifference = currentTime - lastSuccessfulExecutionTime;
        if (timeDifference < 30000) {
            // Calculate the time remaining in seconds
            long timeRemaining = (30000 - timeDifference) / 1000;

            // Notify the user how long they have to wait
            sb.addMessage("error", String.format("Please wait %d more seconds before sending another request!", timeRemaining));
            return;
        }

        if (!DataUtils.isValidEmail(email)) {
            sb.addMessage("error", "Please provide your email address!");
            return;
        }
        String res = db.checkUserExists(email);

        if (res.equals("User exists.")) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            String activateCode = DataUtils.getRandom();
            Date ExpDate = new Date();
            ExpDate = addHoursToJavaUtilDate(ExpDate, 1);
            String res2 = db.addActivationCode(activateCode, sdf.format(ExpDate), email);
            if (res2.equals("Update successful.")) {
                try {
                    //boolean res3 = doSendActivateCode(email, activateCode, ExpDate);
                    // if (res3) {
                    sb.addMessage("info", "Activation code has been sent, please check your email!");
                    lastSuccessfulExecutionTime = System.currentTimeMillis();
                    // }
                } catch (Exception ex) {
                    java.util.logging.Logger.getLogger(FireUserBean.class.getName()).log(Level.SEVERE, null, ex);
                }

            } else {
                sb.addMessage("error", res2);
            }
        } else {
            sb.addMessage("error", "Email Not Exits! Please register first with this email!");
        }
    }

    /*
    public boolean doSendActivateCode(String to, String activateCode, Date ExpDate) throws JsonProcessingException, IOException, InterruptedException {
        if (!DataUtils.isValidEmail(email)) {
            sb.addMessage("error", "Please provide your email address!");
            return false;
        }

        MailService ms = (MailService) DataUtils.findBean("mailService");

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.format(ExpDate);
        String url;
        if (ab.isInDocker()) {
            String site_domain = System.getenv("SITE_DOMAIN");
            System.out.println("Now the SITE_DOMAIN ---> " + site_domain);
            url = "http://" + site_domain + "/" + ab.getAppName() + "/xialabpro/ActivateView.xhtml?code=" + activateCode + "&mail=" + email;
        } else {
            url = "https://pro.metaboanalyst.ca/" + ab.getAppName() + "/xialabpro/ActivateView.xhtml?code=" + activateCode + "&mail=" + email;
        }
        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<head>\n"
                + "</head>\n"
                + "\n"
                + "<body style=\"font-family: Arial; font-size: 12px;\">\n"
                + "<div>\n"
                + "    <p>\n"
                + "        You have requested to activate your account, please click the link below to activate your account.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        Please ignore this email if you did not request an account activation. The activation code is: \n <b>"
                + activateCode
                + "</b> \n"
                + "    </p>\n"
                + "\n"
                + "    <p>\n"
                + "            Copy and paste the following link into your browser to activate your account: "
                + url
                + "    </p>\n"
                + "    <p>\n"
                + "        This code will expire in 60 minutes or after another activation request is sent. <b><i>\"" + ExpDate + " \"</i></b>. Please don't reply.\n"
                + "    </p>\n"
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        boolean res = ms.sendEmail(to, ab.getAppName() + " - Account Activation", "text/html", htmlMsg);
        return res;
    }**/
    private String activationCode = "";

    public String getActivationCode() {
        return activationCode;
    }

    public void setActivationCode(String activationCode) {
        this.activationCode = activationCode;
    }

    public void sendPostRequest(String node, String type) {
        String urlString = "https://" + node + "." + ab.getAppName().toLowerCase() + ".ca/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=loginExternal";

        try {

            //Path path = Paths.get(urlString);
            //URI uri = path.toUri();
            //HttpURLConnection conn = (HttpURLConnection) uri.toURL().openConnection();
            //URL url = new URL(urlString);
            URI uri = null;
            try {
                uri = new URI(urlString);
            } catch (URISyntaxException ex) {
                Logger.getLogger(FireUserBean.class.getName()).log(Level.SEVERE, null, ex);
            }
            URL url = uri.toURL();

            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
            conn.setDoOutput(true);
            String urlParameters;

            if (type.equals("login")) {
                urlParameters = "username=" + URLEncoder.encode(email, "UTF-8")
                        + "&password=" + URLEncoder.encode(password, "UTF-8");
            } else {

                fbc.reloadUserInfo();
                urlParameters = "username=" + URLEncoder.encode(email, "UTF-8")
                        + "&password=" + URLEncoder.encode(fb.getUserMap().get(email).getPassword(), "UTF-8");
            }
            try (DataOutputStream wr = new DataOutputStream(conn.getOutputStream())) {
                wr.writeBytes(urlParameters);
                wr.flush();
            }
            int responseCode = conn.getResponseCode();
            if (responseCode == 200) {
                String token = "";
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
                    token = reader.readLine();  // Read only one line from the response
                } catch (IOException e) {
                    // Handle the exception (e.g., logging)
                }
                String redirection_url;
                if (type.equals("login")) {
                    redirection_url = "https://" + node + "." + ab.getAppName().toLowerCase() + ".ca/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=verified&token=" + token;
                } else {
                    redirection_url = "https://" + node + "." + ab.getAppName().toLowerCase() + ".ca/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=verified&token=" + token + "&navi=" + type;
                }
                FacesContext.getCurrentInstance().getExternalContext().redirect(redirection_url);
            }

            // Handle the response as needed
        } catch (IOException e) {
            e.printStackTrace();
            // Handle exception
        }
    }

    public boolean loginHandshake(String tokenId) {
        if (fb.getLoginUserMap().containsKey(tokenId)) {
            FireUserBean ubObj = fb.getLoginUserMap().get(tokenId);

            fub.setFireUserBean(ubObj);
            fub.setOmicsquareVerified(true);
            System.out.println("handshake=====" + fub.getEmail());
            fb.getLoginUserMap().remove(tokenId);

            Faces.addResponseCookie("user", email, "/", 3600);
            fb.getUserMap().put(email, ubObj);
            setOmicsquareVerified(true);
            System.out.println("success login handshake");
            return true;
        } else {
            return false;
        }
    }

    public void sendPostRequestLoadProject(String node, String projectId) {
        String urlString = "https://" + node + "." + ab.getAppName().toLowerCase() + ".ca/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=loginExternal";
        try {
            // Create the form parameters
            String urlParameters = "username=" + URLEncoder.encode(email, "UTF-8")
                    + "&password=" + URLEncoder.encode(fb.getUserMap().get(email).getPassword(), "UTF-8");

            // Create HTTP client
            HttpClient client = HttpClient.newHttpClient();

            // Build the request
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(urlString))
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .POST(HttpRequest.BodyPublishers.ofString(urlParameters))
                    .build();

            // Send request and get response
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

            // Check response status code (equivalent to responseCode in old version)
            if (response.statusCode() == 200) {
                // Get the response body (token)
                String token = response.body().lines().findFirst().orElse("");
                //System.out.println(token + "====token");

                // Build redirection URL
                String redirection_url = "https://" + node + "." + ab.getAppName().toLowerCase()
                        + ".ca/" + ab.getAppName() + "/faces/AjaxHandler.xhtml?funcNm=verified&token="
                        + token + "&projectId=" + projectId;

                // Perform redirection
                DataUtils.doRedirect(redirection_url, ab);
            }
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
            // Handle exception
        }
    }

    public boolean isAdminUserBool() {
        String[] myArray = {"jeff.xia@xialab.ca", "guangyan.zhou@xialab.ca", "zhiqiang.pang@xialab.ca", "guangyan.zhou@mcgill.ca"};
        ArrayList<String> myList = new ArrayList<>(Arrays.asList(myArray));
        return myList.contains(email);
    }
}
