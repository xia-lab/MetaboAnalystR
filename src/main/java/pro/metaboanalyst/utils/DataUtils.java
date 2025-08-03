/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.utils;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationConfig;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import jakarta.enterprise.inject.spi.Bean;
import jakarta.enterprise.inject.spi.BeanManager;
import jakarta.enterprise.inject.spi.CDI;
import jakarta.enterprise.util.AnnotationLiteral;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Set;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.LinkedHashSet;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.Flash;
import jakarta.faces.event.PhaseId;
import java.lang.reflect.Field;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.stream.Collectors;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.models.NameMapBean;
import pro.metaboanalyst.models.User;
import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.StreamedContent;
import org.primefaces.model.file.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.omnifaces.util.Faces;
import org.quartz.JobExecutionException;
import org.rosuda.REngine.Rserve.RserveException;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.controllers.multifac.HeatMap2Bean;
import pro.metaboanalyst.lts.MailService;
/* ------------- Jackson core ------------------------------- */
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.enterprise.inject.spi.AnnotatedField;

/* ------------- Java reflection + collections -------------- */
import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.List;
import pro.metaboanalyst.lts.FunctionInfo;

/**
 *
 * @author Jeff
 */
public class DataUtils {

    private static final Logger LOGGER = LogManager.getLogger(DataUtils.class);

    /*
    @SuppressWarnings("unchecked")
    public static Object findBean(String beanName) { // DOESN't work with project saving.
        Instance<Object> instance = CDI.current().select(new NamedLiteral(beanName));
        return instance.get();  // returns the bean as an Object
    }

    public static <T> T findBean2(String beanName) {
        FacesContext context = FacesContext.getCurrentInstance();
        return (T) context.getApplication().evaluateExpressionGet(context, "#{" + beanName + "}", Object.class);
    }
     */
    // NamedLiteral class to represent @Named(beanName)
    public static class NamedLiteral extends AnnotationLiteral<jakarta.inject.Named> implements jakarta.inject.Named {

        private final String value;

        public NamedLiteral(String value) {
            this.value = value;
        }

        @Override
        public String value() {
            return value;
        }
    }

    public static String getDomainURL(String myurl) {
        try {
            // Use URI to parse the URL
            URI uri = new URI(myurl);
            // Construct the domain URL from the URI components
            return uri.getScheme() + "://" + uri.getAuthority();
        } catch (URISyntaxException e) {
            LOGGER.error("getDomainURL", e);
            return null;
        }
    }

    public static String convertArrayToVecInR(String[] myVec) {
        String rcmd = Arrays.toString(myVec);
        rcmd = rcmd.replace("[", "c(\"");
        rcmd = rcmd.replace(", ", "\",\"");
        rcmd = rcmd.replace("]", "\")");
        return rcmd;
    }

    // create a tempature user accoutn if user log in as a guest will not remember, only session only
    public static User createTempUser(String realUsrPath) {
        try {
            //String realUsrPath = realPath + usr_home;
            //try to clean the user folder to remove old files (more than 1 day)
            DataUtils.deleteFilesOlderThanNdays(realUsrPath);
            //first create a random user names
            User user = new User();
            String guestName = File.createTempFile("metaboanalyst", "tmp").getName();
            //String guestDir = realUsrPath + File.separator + guestName;
            String guestDir = realUsrPath + guestName;
            File guestFolder = new File(guestDir);
            while (guestFolder.exists()) {
                guestName = File.createTempFile("metaboanalyst", "tmp").getName();
                //guestDir = realUsrPath + File.separator + guestName;
                guestFolder = new File(realUsrPath + guestName);
            }
            guestFolder.mkdir();
            user.setName(guestName);
            user.setRelativeDir("/resources/users/" + guestName);
            user.setOrigRelativeDir("/resources/users/" + guestName);

            user.setHomeDir(guestDir);
            user.setOrigHomeDir(guestDir);
            return user;
        } catch (Exception e) {
            LOGGER.error("createTempUser", e);
        }
        return null;
    }

    public static User createPreviousFolder(String realUsrPath, String previousFolderName) {
        try {
            //String realUsrPath = realPath + usr_home;
            //try to clean the user folder to remove old files (more than 1 day)
            DataUtils.deleteFilesOlderThanNdays(realUsrPath);
            //first create a random user names
            User user = new User();
            String guestName = previousFolderName;
            //String guestDir = realUsrPath + File.separator + guestName;
            String guestDir = realUsrPath + guestName;
            File guestFolder = new File(guestDir);
            if (guestFolder.exists()) {
                if (Files.isDirectory(Paths.get("/home/qiang/Documents/Regular_commands"))) {
                    System.out.println("Not delete user=====" + guestDir + " from Zhiqiang's local");
                } else {
                    System.out.println("delete user=====" + guestDir);
                    deleteDir(guestDir);
                }

            }

            guestFolder.mkdir();
            user.setName(guestName);
            user.setRelativeDir("/resources/users/" + guestName);
            user.setOrigRelativeDir("/resources/users/" + guestName);

            user.setHomeDir(guestDir);
            user.setOrigHomeDir(guestDir);
            return user;
        } catch (Exception e) {
            LOGGER.error("createTempUser", e);
        }
        return null;
    }

    // create a tempature user accoutn if user log in as a guest will not remember, only session only
    public static User createRawSpecUser(String realUsrPath) {

        try {
            //DataUtils.deleteFilesOlderThanNdays(realUsrPath, 187);
            //first create a random user names
            User user = new User();
            String guestName;

            //check if it is project saving
            guestName = File.createTempFile("metaboanalyst", "tmp").getName();

            String guestDir = realUsrPath + guestName;
            File guestFolder = new File(guestDir);

            while (guestFolder.exists()) {
                guestName = File.createTempFile("metaboanalyst", "tmp").getName();
                guestFolder = new File(realUsrPath + guestName);
            }

            boolean created = guestFolder.mkdir();

            user.setName(guestName);
            user.setRelativeDir(realUsrPath + guestName);
            user.setOrigRelativeDir(realUsrPath + guestName);

            user.setHomeDir(guestDir);
            user.setOrigHomeDir(guestDir);
            return user;
        } catch (Exception e) {
            LOGGER.error("createRawSpecUser", e);
        }
        return null;
    }

    public static NameMapBean[] removeHyperlinks(NameMapBean[] nmaps) {
        for (NameMapBean nameMap : nmaps) {
            if (nameMap.getQuery().contains("<strong")) {
                nameMap.setQuery(nameMap.getQuery().replace("<strong style=\"background-color:var(--orange-500); font-size=125%;>", ""));
                nameMap.setQuery(nameMap.getQuery().replace("</strong>", ""));
            }
            if (nameMap.getHmdb_id().contains(">")) {
                nameMap.setHmdb_id(nameMap.getHmdb_id().split(">")[1].split("</a")[0]);
            }
            if (nameMap.getChebi_id().contains(">")) {
                nameMap.setChebi_id(nameMap.getChebi_id().split(">")[1].split("</a")[0]);
            }
            if (nameMap.getKegg_id().contains(">")) {
                nameMap.setKegg_id(nameMap.getKegg_id().split(">")[1].split("</a")[0]);
            }
            if (nameMap.getPubchem_id().contains(">")) {
                nameMap.setPubchem_id(nameMap.getPubchem_id().split(">")[1].split("</a")[0]);
            }
            if (nameMap.getMetlin_id().contains(">")) {
                nameMap.setMetlin_id(nameMap.getMetlin_id().split(">")[1].split("</a")[0]);
            }

        }
        return nmaps;
    }

    public static DefaultStreamedContent getDownloadFile(String filePath) {
        try {
            File file = new File(filePath);
            InputStream input = new FileInputStream(file);
            ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            return DefaultStreamedContent.builder().contentType(externalContext.getMimeType(file.getName())).name(file.getName()).stream(() -> input).build();
            // return (new DefaultStreamedContent(input, externalContext.getMimeType(file.getName()), file.getName()));
        } catch (Exception e) {
            LOGGER.error("getDownloadFile", e);
        }
        return null;
    }

    public static StreamedContent getStreamedImage(String path, String fileName) throws IOException {
        if (FacesContext.getCurrentInstance().getCurrentPhaseId() == PhaseId.RENDER_RESPONSE) {
            // So, we're rendering the view. Return a stub StreamedContent so that it will generate right URL.
            return new DefaultStreamedContent();
        } else {
            // So, browser is requesting the image. Return a real StreamedContent with the image bytes. 
            // return new DefaultStreamedContent(new FileInputStream(new File(path, fileName)), "image/png");
            File file = new File(path, fileName);
            InputStream input = new FileInputStream(file);
            return DefaultStreamedContent.builder().contentType(FacesContext.getCurrentInstance().getExternalContext().getMimeType(file.getName())).name(file.getName()).stream(() -> input).build();
        }
    }

    public static void setupFileDownloadZip(User currentUser) {

        File folder = new File(currentUser.getHomeDir());

        //remove previous (if any) zip file
        DataUtils.deleteFile(currentUser, "Download.zip");

        File[] listOfFiles = folder.listFiles((File dir, String name) -> name.endsWith(".csv"));
        DataUtils.createZipFile(listOfFiles, "Download.zip", currentUser.getHomeDir());
    }

    public static String readTextFile(String filePath) {

        BufferedReader br = null;
        String text = "";
        String line;
        try {
            br = new BufferedReader(new FileReader(filePath));
            while ((line = br.readLine()) != null) {
                // use comma as separator
                //String[] country = line.split(splitBy);
                line = line.replace("\t", "  ");
                text = text + "\n" + line;
            }

        } catch (Exception e) {
            LOGGER.error("readTextFile", e);
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    LOGGER.error("readTextFile", e);
                }
            }
        }
        return text;
    }

    //Note: for Class uploadedFile
    public static String getJustFileName(String uploadedFileName) {
        int index = uploadedFileName.lastIndexOf('/');
        String justFileName;
        if (index >= 0) {
            justFileName = uploadedFileName.substring(index + 1);
        } else {
            // Try backslash
            index = uploadedFileName.lastIndexOf('\\');
            if (index >= 0) {
                justFileName = uploadedFileName.substring(index + 1);
            } else { // No forward or back slashes
                justFileName = uploadedFileName;
            }
        }
        return justFileName;
    }

    public static void copyFile(File in, File out) {
        try {
            FileInputStream fis = new FileInputStream(in);
            FileOutputStream fos = new FileOutputStream(out);
            copyInputStream(fis, fos);
            System.out.println("File copied successfully from " + in.getAbsolutePath() + " to " + out.getAbsolutePath());
        } catch (IOException e) {
            LOGGER.error("copyFile", e);
            System.out.println("Failed to copy file from " + in.getAbsolutePath() + " to " + out.getAbsolutePath());
        }
    }

    /**
     * public static void fetchFile(String in, File out) { try { InputStream fis
     * = new URL(in).openStream(); FileOutputStream fos = new
     * FileOutputStream(out); copyInputStream(fis, fos); } catch (IOException e)
     * { LOGGER.error("fetchFile", e); } }
     *
     * @param in
     * @param out
     */
    public static void fetchFile(String in, File out) {
        HttpClient client = HttpClient.newHttpClient();

        try {
            // Create a HttpRequest from the URI
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(new URI(in))
                    .GET()
                    .build();

            // Send request and get response
            HttpResponse<InputStream> response = client.send(request, HttpResponse.BodyHandlers.ofInputStream());

            // Check if the response is successful
            if (response.statusCode() == 200) {
                // Write the output stream to the file
                try (InputStream fis = response.body()) {
                    Files.copy(fis, out.toPath(), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
                }
            } else {
                LOGGER.error("Failed to fetch file. HTTP response code: " + response.statusCode());
            }

        } catch (IOException | InterruptedException | URISyntaxException e) {
            LOGGER.error("fetchFile", e);
            Thread.currentThread().interrupt(); // Handle the InterruptedException properly
        }
    }

    public static void copyDir(String source, String destination) {

        // String source = "C:/your/source";
        File srcDir = new File(source);

        // String destination = "C:/your/destination";
        File destDir = new File(destination);

        try {
            FileUtils.copyDirectory(srcDir, destDir, false);
        } catch (IOException e) {
            LOGGER.error("copyDir", e);
        }

    }

    private static void copyInputStream(InputStream in, OutputStream out) throws IOException {
        try (in) {
            byte[] buffer = new byte[1024];
            int len;
            while ((len = in.read(buffer)) >= 0) {
                out.write(buffer, 0, len);
            }
        }
        out.close();
    }

    private static final int BUFFER_SIZE = 4096;

    public static void createZipFile(File[] files, String zipName, String path) {
        // Create a buffer for reading the files
        byte[] buf = new byte[18024];

        try {
            // Create the ZIP file
            String outFilename = path + File.separator + zipName;
            // Compress the files
            try (ZipOutputStream out = new ZipOutputStream(new FileOutputStream(outFilename))) {
                // Compress the files
                for (File file : files) {
                    FileInputStream in = new FileInputStream(file);
                    // Add ZIP entry to output stream.
                    out.putNextEntry(new ZipEntry(file.getName()));
                    int len;
                    while ((len = in.read(buf)) > 0) {
                        out.write(buf, 0, len);
                    }
                    out.closeEntry();
                    in.close();
                }
            }
        } catch (IOException e) {
            LOGGER.error("createZipFile", e);
        }
    }

    public static String setupTable(String lbl, double[][] sigmat, String[] rownames, String[] colnames) {

        if (rownames == null || rownames.length == 0) {
            return ("No significant feature was identified");
        } else {
            String str = "<table border=\"1\" cellpadding=\"5\">";
            str = str + "<tr><th>" + lbl + "</th>";
            for (String colname : colnames) {
                str = str + "<th>" + colname + "</th>";
            }
            str = str + "</tr>";
            for (int i = 0; i < rownames.length; i++) {
                str = str + "<tr><td>" + rownames[i] + "</td>";
                for (int j = 0; j < colnames.length; j++) {
                    str = str + "<td>" + sigmat[i][j] + "</td>";
                }
                str = str + "</tr>";
            }
            str = str + "</table>";
            return str;
        }
    }

    // note since Rserver only return [][] for double, we can get double[][] and String[] and combine them here
    // note: the colnames include the last name for the extraCol
    public static String setupTable(String lbl, double[][] sigmat, String[] extraCol, String[] rownames, String[] colnames) {

        if (rownames == null || rownames.length == 0) {
            return ("No significant feature was identified");
        } else {
            String str = "<table border=\"1\" cellpadding=\"5\">";
            str = str + "<tr><th>" + lbl + "</th>";
            for (String colname : colnames) {
                str = str + "<th>" + colname + "</th>";
            }

            //remember colnames here is longer than matrix
            int col_len = colnames.length - 1;
            str = str + "</tr>";
            for (int i = 0; i < rownames.length; i++) {
                str = str + "<tr><td>" + rownames[i] + "</td>";
                for (int j = 0; j < col_len; j++) {
                    str = str + "<td>" + sigmat[i][j] + "</td>";
                }
                str = str + "<td>" + extraCol[i] + "</td></tr>";
            }
            str = str + "</table>";
            return str;
        }
    }

    public static ArrayList<String> getQueryNames(String text) {
        try {
            ArrayList<String> nmVec = new ArrayList();
            StringTokenizer st = new StringTokenizer(text, "\n");
            while (st.hasMoreTokens()) {
                String line = st.nextToken();
                line = line.trim();//remove both leading and end space
                if (line.length() == 0) { //empty line
                    continue;
                }

                if (line.indexOf(";") > 0) {
                    nmVec.addAll(Arrays.asList(line.split(";")));
                } else {
                    nmVec.add(line);
                }

            }
            return nmVec;
        } catch (NumberFormatException e) {
            LOGGER.error("getQueryNames", e);
            return null;
        }
    }

    //parse String between one layer of HTML tag
    public static String getStringHTMLTag(String htmlString) {
        Pattern pattern = Pattern.compile("<([A-Za-z][A-Za-z0-9]*)\\b[^>]*>(.*?)</\\1>");
        Matcher m = pattern.matcher(htmlString);
        if (m.find()) {
            return (m.group(2));
        }

        return htmlString;
    }

    public static String[] getQueryNames(String text, String sep) {
        return getNamesArray(text, sep);
    }

    public static String[] getQueryNames(UploadedFile uploadedFile, String sep) {
        try {
            return getNamesArray(convertStreamToString(uploadedFile.getInputStream()), sep);
        } catch (IOException e) {
            LOGGER.error("getQueryNames", e);
            return null;
        }
    }

    //compound names need to be  need to be one name per column
    public static String[] getNamesArray(String content, String sep) {
        ArrayList<String> nmVec = new ArrayList();
        if (sep == null) {
            sep = System.getProperty("line.separator");
        }
        //seperate by line separator or ";"
        StringTokenizer st = new StringTokenizer(content, sep);

        while (st.hasMoreTokens()) {
            String line = st.nextToken();
            line = cleanString(line);//remove both leading and end space
            if (line.length() == 0) { //empty line
                continue;
            }
            nmVec.add(line);
        }
        //remove duplicates, if any
        Set<String> mySet = new LinkedHashSet(nmVec);

        return mySet.toArray(String[]::new);
    }

    public static String convertStreamToString(java.io.InputStream is) {
        //java.util.Scanner s = new java.util.Scanner(is).useDelimiter("\\A");
        return new java.util.Scanner(is).useDelimiter("\\A").next();
        //return s.hasNext() ? s.next() : "";
    }

    // clear string spaces and punctuations
    public static String cleanString(String s) {
        s = s.replaceAll("^\\s+", ""); //remove leading space
        s = s.replaceAll("\\s+$", ""); //remove trailing space
        s = s.replaceAll("[^a-zA-Z0-9)]$", ""); //remove last one if not character/number/) (i.e. punctuation)
        return s;
    }

    // perform bash command when RC is available
    public static boolean systemExec(String cmd, RConnection RC) {
        try {
            String rcmd = "system(\"" + cmd + "\", intern = T)";
            System.out.println("R systemExec boolean => " + cmd);
            RC.voidEval(rcmd);
            return true;
        } catch (Exception e) {
            LOGGER.error("systemExec Boolean option", e);
            return false;
        }
    }

    // perform bash with Rserve available
    public static void systemExec(String cmd) {
        RConnection RC = null;
        String rcmd = "system(\"" + cmd + "\", intern = T)";
        System.out.println("R systemExec void => " + cmd);
        try {
            RC = new RConnection("127.0.0.1", 6311);
            RC.voidEval(rcmd);
        } catch (RserveException ex) {
            java.util.logging.Logger.getLogger(DataUtils.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            RC.close();
        }
    }

    // perform bash command when RC is available and return Single string, indexed by "index"
    public static String systemExec(String cmd, RConnection RC, int index) {
        String res = null;
        try {
            String rcmd = "system(\"" + cmd + "\", intern = T)[" + index + "]";
            System.out.println("R systemExec boolean => " + cmd);
            RC.eval(rcmd).asString();
            return res;
        } catch (Exception e) {
            LOGGER.error("systemExec Boolean option", e);
            return res;
        }
    }

    // perform bash command when RC is available and return Single string
    public static String[] systemExecStrings(String cmd, RConnection RC) {
        String res[] = null;
        try {
            String rcmd = "system(\"" + cmd + "\", intern = T)";
            System.out.println("R systemExec boolean => " + cmd);
            res = RC.eval(rcmd).asStrings();
            return res;
        } catch (Exception e) {
            LOGGER.error("systemExec Boolean option", e);
            return res;
        }
    }

    public static boolean cropImage(String convertPath, String imgPath, String targetPath, int x, int y, int width, int height, int quality) {

        if (quality < 0 || quality > 100) {
            quality = 75;
        }

        ArrayList command = new ArrayList(10);

        // note: CONVERT_PROG is a class variable that stores the location of ImageMagick's convert command
        // need to supply full path to the covert command
        command.add(convertPath);
        command.add("-crop");
        command.add(width + "x" + height + "+" + x + "+" + y);

        command.add("-quality");
        command.add("" + quality);
        command.add("+repage");
        command.add(imgPath);
        command.add(targetPath);

        return myExec((String[]) command.toArray(new String[1]));
    }

    /**
     * Tries to exec the command, waits for it to finish, logs errors if exit
     * status is nonzero, and returns true if exit status is 0 (success).
     *
     * @param command Description of the Parameter
     * @return Description of the Return Value
     */
    public static boolean myExec(String[] command) {
        Process proc;
        try {
            //System.out.println("Trying to execute command " + Arrays.asList(command));
            proc = Runtime.getRuntime().exec(command);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("IOException while trying to execute " + Arrays.toString(command));
            return false;
        }
        //System.out.println("Got process object, waiting to return.");
        int exitStatus;
        while (true) {
            try {
                exitStatus = proc.waitFor();
                break;
            } catch (java.lang.InterruptedException e) {
                LOGGER.error("myExec", e);
                //System.out.println("Interrupted: Ignoring and waiting");
            }
        }
        if (exitStatus != 0) {

            System.out.println("Error executing command: " + Arrays.toString(command));
        }
        return (exitStatus == 0);
    }

    public static boolean myExec(String command) {
        Process proc;
        try {
            //System.out.println("Trying to execute command " + Arrays.asList(command));
            //proc = Runtime.getRuntime().exec(command);
            List<String> commands = Arrays.asList(command.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            proc = processBuilder.start();

            try (BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()))) {
                String jobString;
                while ((jobString = stdInput.readLine()) != null) {
                    System.out.println(jobString + "\n");
                }
            }

        } catch (Exception e) {
            LOGGER.error("myExec", e);
            System.out.println("IOException while trying to execute " + command);
            return false;
        }
        //System.out.println("Got process object, waiting to return.");
        int exitStatus;
        while (true) {
            try {
                exitStatus = proc.waitFor();
                break;
            } catch (java.lang.InterruptedException e) {
                System.out.println("Interrupted: Ignoring and waiting");
            }
        }
        if (exitStatus != 0) {
            System.out.println("Error executing command: " + exitStatus);
        }
        return (exitStatus == 0);
    }

    //do system call with return the result from sytem output
    public static String executeCommand(String command) {

        StringBuilder output = new StringBuilder();
        Process proc;
        try {
            List<String> commands = Arrays.asList(command.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            proc = processBuilder.start();

            BufferedReader reader = new BufferedReader(new InputStreamReader(proc.getInputStream()));

            String line;
            while ((line = reader.readLine()) != null) {
                output.append(line).append("\n");
            }
        } catch (Exception e) {
            LOGGER.error("executeCommand", e);
        }
        return output.toString();
    }

    public static String uploadFile(SessionBean1 sb, UploadedFile file, String homeDir, String outFileNm, boolean onServer) {

        if (file == null || file.getSize() == 0) {
            sb.addMessage("Error", "Empty file?");
            return null;
        }

        if (onServer & file.getSize() > ApplicationBean1.MAX_UPLOAD_SIZE) {
            sb.addMessage("Error", "The file size exceeds limit:" + ApplicationBean1.MAX_UPLOAD_SIZE);
            return null;
        }

        String fileName = file.getFileName();

        if (fileName.endsWith(".csv") | fileName.endsWith(".txt") | fileName.endsWith(".zip") | fileName.endsWith(".mzTab") | fileName.endsWith(".mztab")) {
            try {
                OutputStream out;
                try (InputStream in = file.getInputStream()) {
                    if (outFileNm == null) {
                        outFileNm = fileName;
                    }
                    out = new FileOutputStream(new File(homeDir + File.separator + outFileNm));
                    byte[] buffer = new byte[1024];
                    int len;
                    while ((len = in.read(buffer)) >= 0) {
                        out.write(buffer, 0, len);
                    }
                }
                out.close();

            } catch (IOException e) {
                LOGGER.error("uploadFile", e);
            }
        } else {
            sb.addMessage("Error", "Only tab delimited (.txt) or comma separated (.csv) files are accepted. If file is mzTab, ensure it has been validated!");
            return null;
        }
        return fileName;
    }

    public static String uploadMSPFile(SessionBean1 sb, UploadedFile file, String homeDir, String outFileNm, boolean onPublicServer) {

        if (file == null || file.getSize() == 0) {
            sb.addMessage("Error", "Empty file?");
            return null;
        }

        if (onPublicServer & file.getSize() > 5000000) {
            sb.addMessage("Error", "The file size exceeds limit: 5M");
            return null;
        }

        String fileName = file.getFileName();

        if (fileName.endsWith(".msp") || fileName.endsWith(".mgf")) {
            try {
                OutputStream out;
                try (InputStream in = file.getInputStream()) {
                    if (outFileNm == null) {
                        outFileNm = fileName;
                    }
                    out = new FileOutputStream(new File(homeDir + File.separator + outFileNm));
                    byte[] buffer = new byte[1024];
                    int len;
                    while ((len = in.read(buffer)) >= 0) {
                        out.write(buffer, 0, len);
                    }
                }
                out.close();

            } catch (IOException e) {
                LOGGER.error("uploadFile", e);
            }
        } else {
            sb.addMessage("Error", "Only *msp or *.mgf file is accepted!");
            return null;
        }
        return fileName;
    }

    public static void deleteFile(User usr, String filename) {
        File f1 = new File(usr.getHomeDir() + "/" + filename);
        if (f1.exists()) {
            boolean sucess = f1.delete();
            if (!sucess) {
                System.out.println("=== Delete file - " + filename + " failed.");
            }
        }
    }

    public static void deleteFile(String projectHomePath, String filename) {
        File f1 = new File(projectHomePath + "/" + filename);
        if (f1.exists()) {
            boolean sucess = f1.delete();
            if (!sucess) {
                System.out.println("=== Delete file - " + filename + " failed.");
            }
        }
    }

    //a utility function to remove the old user folders
    //called everytime a new user folder is created, default 12 hours
    public static void deleteFilesOlderThanNdays(String dirWay) {

        File directory = new File(dirWay);
        //Calendar cal = Calendar.getInstance();
        //cal.add(Calendar.DAY_OF_MONTH, -1);
        //long purgeTime = cal.getTimeInMillis();
        long currentTime = new Date().getTime();
        long purgeTime = 12 * 60 * 60 * 1000;
        if (directory.exists()) {
            File[] listFiles = directory.listFiles();
            for (File listFile : listFiles) {
                if (listFile.getName().startsWith("guest")) {
                    if (currentTime - listFile.lastModified() > purgeTime) {
                        if (!deleteDir(listFile.getAbsolutePath())) {
                            System.err.println("Unable to delete file: " + listFile);
                        }
                    }
                }
            }
        }
    }

    //a utility function to remove the old user folders
    //called everytime a new user folder is created, default 1 day
    public static void deleteFilesOlderThanNdays(String dirWay, int n) {
        //System.out.println(dirWay + "===olderthanndays");
        File directory = new File(dirWay);

        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_MONTH, -1 * n);
        long purgeTime = cal.getTimeInMillis();

        // for project saving folders
        Calendar cal2 = Calendar.getInstance();
        cal2.add(Calendar.DAY_OF_MONTH, -1 * 90);

        long purgeTime3Months = cal2.getTimeInMillis();
        if (directory.exists()) {
            File[] listFiles = directory.listFiles();
            for (File listFile : listFiles) {
                if (listFile.getName().endsWith("tmp")) {
                    if (listFile.lastModified() < purgeTime) {
                        if (!deleteDir(listFile.getAbsolutePath())) {
                            System.err.println("Unable to delete file: " + listFile);
                        }
                    }
                } else if (listFile.getName().endsWith("project")) {
                    if (listFile.lastModified() < purgeTime3Months) {
                        if (!deleteDir(listFile.getAbsolutePath())) {
                            System.err.println("Unable to delete file: " + listFile);
                        }
                    }
                }
            }
        }
    }

    //a utility function to kill long running Rserver process 
    //Note 1) spare the mother (the oldest one) 
    //     2) default 1 hour    
    //     3) if an Rserve process is 100% CPU (>99%) for all measured (at least 3) time points, then kill it (i.e. don't wait for 1 hour
    // Only works on Linux !!!! Mac does not recognize --sort=start_time
    // filter need to apply in order day filter, hour filter and minutes filter (not used)
    //for filter based on minutes (20min)
    //String[] rmMinLongCmd = new String[]{bashPath, "-c", "ps -eo pid,etime,args --sort=start_time | grep 'Rserve'| grep -v 'grep' | tail -n +2 | grep '[0-9][0-9]:[0-9][0-9]'| awk 'substr($2,1,(index($2,\":\")-1))-20>=0' | awk '{print $1}' | xargs kill -9"};
    // System.out.println("Process__ :: " + bashPath);
    // System.out.println(Arrays.toString(rmDayLongCmd));
    /*
    public static void performResourceCleaning(String bashPath, String RScriptHome) {

        try {

            String sysCmd = bashPath + " " + RScriptHome + "/_clean_jobs.sh";
            Process p = Runtime.getRuntime().exec(sysCmd);
            p.waitFor();
            System.out.println("Successfully performed resource cleaning!");

        } catch (Exception e) {
            System.out.println("Exception in resource cleaning -  ");
            //System.out.println("Exception in resource cleaning - here's what I know: ");
            //LOGGER.error("performResourceCleaning", e);
        }
    }
    **/
    // based on: https://www.dontpanicblog.co.uk/2023/05/07/handling-blocked-process-output-stream/
    // wait for 2 sec
    public static boolean runExternalCommand(String sysCmd, ExecutorService streamHandlers) {

        try {
            //Process p = Runtime.getRuntime().exec(sysCmd);
            Process p;
            List<String> commands = Arrays.asList(sysCmd.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            p = processBuilder.start();
            InputStream stdOut = p.getInputStream();
            InputStream stdErr = p.getErrorStream();

            streamHandlers.execute(() -> handleStream(stdOut));
            streamHandlers.execute(() -> handleStream(stdErr));

            int exitStatus;
            while (true) {
                try {
                    exitStatus = p.waitFor();
                    break;
                } catch (java.lang.InterruptedException e) {
                    LOGGER.error("runExternalCommand", e);
                    System.out.println("Interrupted: Ignoring and waiting");
                }
            }
            if (exitStatus != 0) {
                System.out.println("Error executing command: " + sysCmd);
            } else {
                System.out.println("Successfully executed command:  " + sysCmd);
            }
            return (exitStatus == 0);

        } catch (Exception e) {
            System.out.println("Error executing command:  " + sysCmd);
            //System.out.println("Exception in resource cleaning - here's what I know: ");
            //LOGGER.error("performResourceCleaning", e);
            return false;
        }
    }

    private static void handleStream(InputStream inputStream) {
        try (BufferedReader stdOutReader = new BufferedReader(new InputStreamReader(inputStream))) {
            while (stdOutReader.readLine() != null) {
                // Just reading the line is enough. We don't need to do anything else with it.
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    //use Unix command to remvoe (non-empty) folder
    // '"curl -H "C-H Content-Type: application/json" -X POST -d "{\"name\": \"prof_website\",  \"entities\":{\"page\": \"entity_value\"}}" https://www.omicsbot.ca/conversations/19dcb8ae575447748cbdba384a22af51/trigger_intent?output_channel=latest"'
    public static boolean executeCurlCommand(String parameters, String url_sessionid) {
        //first make sure they are removable 
        ArrayList command = new ArrayList(8);
        command.add("curl");
        command.add("-H");
        command.add("\"C-H Content-Type: application/json\"");
        command.add("-X");
        command.add("POST");
        command.add("-d");
        command.add(parameters);
        command.add(url_sessionid);
        myExec((String[]) command.toArray(new String[1]));
        return true;
    }

    //use Unix command to remvoe (non-empty) folder
    public static boolean deleteDir(String fdPath) {
        //first make sure they are removable 
        ArrayList command = new ArrayList(4);
        command.add("chmod");
        command.add("-R");
        command.add("777");
        command.add(fdPath);
        boolean res = myExec((String[]) command.toArray(new String[1]));

        if (res) {
            command = new ArrayList(3);
            command.add("rm");
            command.add("-rf");
            command.add(fdPath);
            return myExec((String[]) command.toArray(new String[1]));
        } else {
            return false;
        }
    }

    public static boolean generateReportCMD(String bashPath, String RScriptHome, String userDir) {
        try {
            //System.setProperty("user.dir", userDir);
            //String sysCmd = cmdPath + " " + userDir + "/Analysis_Report.tex";
            //System.out.println("===========" + sysCmd);
            String sysCmd = bashPath + " " + RScriptHome + "/_bash_to_r.sh" + " " + RScriptHome + "/_sweave_cmd.R" + " " + userDir;

            //System.out.println("=====sysCmd======" + sysCmd);
            //Process p = Runtime.getRuntime().exec(sysCmd);
            //p.waitFor();
            processExec(sysCmd);

        } catch (Exception e) {
            //System.out.println("exception happened - here's what I know: ");
            LOGGER.error("generateReportCMD", e);
            return false;
        }
        return true;
    }

    //give a string vector ["a", "b", "c"], return a single string 
    public static String createStringVector(String[] nms) {
        StringBuilder sb = new StringBuilder();
        for (String s : nms) {
            sb.append(s);
            sb.append("\",\"");
        }
        String res = sb.toString();
        //trim the last comma and quote
        //System.out.println(res + "====");
        res = res.substring(0, res.length() - 2);
        res = "c(\"" + res + ")";
        return (res);
    }

    public static boolean unzipData(String zipFilePath, String destDirectory) {
        File destDir = new File(destDirectory);
        if (!destDir.exists()) {
            destDir.mkdir();
        }
        try (ZipInputStream zipIn = new ZipInputStream(new FileInputStream(zipFilePath))) {
            ZipEntry entry = zipIn.getNextEntry();
            // iterates over entries in the zip file
            while (entry != null) {
                String name = entry.getName();
                String filePath = destDirectory + File.separator + name;
                if (!entry.isDirectory() && !(name.contains("MACOSX") | name.contains("DS_Store"))) {
                    // if the entry is a file, extracts it

                    extractFile(zipIn, filePath);
                } else {
                    // if the entry is a directory, make the directory
                    File dir = new File(filePath);
                    dir.mkdir();
                }
                zipIn.closeEntry();
                entry = zipIn.getNextEntry();
            }
            return true;
        } catch (Exception e) {
            LOGGER.error("unzipData", e);
            return false;
        }
    }

    /**
     * Extracts a zip entry (file entry)
     *
     * @param zipIn
     * @param filePath
     * @throws IOException
     */
    public static void extractFile(ZipInputStream zipIn, String filePath) throws IOException {
        try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(filePath))) {
            byte[] bytesIn = new byte[BUFFER_SIZE];
            int read;
            while ((read = zipIn.read(bytesIn)) != -1) {
                bos.write(bytesIn, 0, read);
            }
        }
    }

    /**
     * *
     * Extract zipfile to outdir with complete directory structure
     *
     * @param zipfile Input .zip file
     * @param outdir Output directory
     */
    public static void extract(String zipfileName, String outdirName) {
        File zipfile = new File(zipfileName);
        File outdir = new File(outdirName);
        try {
            try (ZipInputStream zin = new ZipInputStream(new FileInputStream(zipfile))) {
                ZipEntry entry;
                String name, dir;
                while ((entry = zin.getNextEntry()) != null) {
                    name = entry.getName();
                    if (entry.isDirectory()) {
                        mkdirs(outdir, name);
                        continue;
                    }
                    /* this part is necessary because file entry can come before
                    * directory entry where is file located
                    * i.e.:
                    *   /foo/foo.txt
                    *   /foo/
                     */
                    dir = dirpart(name);
                    if (dir != null) {
                        mkdirs(outdir, dir);
                    }

                    extractFile2(zin, outdir, name);
                }
            }
        } catch (IOException e) {
            LOGGER.error("extract", e);
        }
    }

    private static void extractFile2(ZipInputStream in, File outdir, String name) throws IOException {
        byte[] buffer = new byte[BUFFER_SIZE];
        try (BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(new File(outdir, name)))) {
            int count;
            while ((count = in.read(buffer)) != -1) {
                out.write(buffer, 0, count);
            }
        }
    }

    private static void mkdirs(File outdir, String path) {
        File d = new File(outdir, path);
        if (!d.exists()) {
            d.mkdirs();
        }
    }

    private static String dirpart(String name) {
        int s = name.lastIndexOf(File.separatorChar);
        return s == -1 ? null : name.substring(0, s);
    }
    private static String rawUploadErrorFile = "";

    public static String getRawUploadErrorFile() {
        return rawUploadErrorFile;
    }

    public static void setRawUploadErrorFile(String rawUploadErrorFile) {
        DataUtils.rawUploadErrorFile = rawUploadErrorFile;
    }

    public static int moveFileToSubDirectory(String fileName, String destDirectory) {
        File rawFile = new File(fileName);
        File destDir = new File(destDirectory);
        String name = rawFile.getName();
        if (!destDir.exists()) {
            destDir.mkdir();
        }
        try {
            if (name.endsWith("mzXML") || name.endsWith("mzML") || name.endsWith("mzData")) {
                rawFile.renameTo(new File(destDir.getName() + rawFile.getName()));
            } else {
                return 0;
            }
        } catch (Exception e) {
            LOGGER.error("moveFileToSubDirectory", e);
        }
        return 1;
    }

    public static int unzipDataRaw(String zipFilePath, String destDirectory) {
        File destDir = new File(destDirectory);
        if (!destDir.exists()) {
            destDir.mkdir();
        }
        try (ZipInputStream zipIn = new ZipInputStream(new FileInputStream(zipFilePath))) {
            ZipEntry entry = zipIn.getNextEntry();
            // iterates over entries in the zip file
            while (entry != null) {
                String name = entry.getName();
                String filePath = destDirectory + File.separator + name;
                //System.out.println(filePath + "===filepath");
                //if (!entry.isDirectory() && !(name.contains("MACOSX") || name.contains("DS_Store"))) {
                if (!entry.isDirectory()) {
                    // if the entry is a file, extracts it
                    //if (name.endsWith("mzXML") || name.endsWith("mzML") || name.endsWith("mzData")) {
                    extractFile(zipIn, filePath);
                    //} else {
                    //    setRawUploadErrorFile(name);
                    //    return -1;
                    //}
                } else {
                    // if the entry is a directory, make the directory
                    if (!name.contains("_MACOSX")) {
                        File dir = new File(filePath);
                        dir.mkdir();
                    }
                }
                zipIn.closeEntry();
                entry = zipIn.getNextEntry();
            }
            return 1;
        } catch (Exception e) {
            LOGGER.error("unzipDataRaw", e);
            return 0;
        }
    }

    public static void runRawSpecScript(RConnection RC, String RScriptHome, String bashPath, String userPath, String usrNm,
            String rCommandOpt, String rCommandProcess, String fileNms) {
        try {
            //String sysCmd = bashPath + " " + RScriptHome + "/_raw_spec_cmd.sh" + " " + userPath + "/ExecuteRawSpec.R";
            String Rcmd = null;
            if ("opt".equals(rCommandProcess)) {
                Rcmd = "library(OptiLCMS); "
                        + "setwd(\'" + userPath + "\'); "
                        + "mSet <- InitDataObjects('spec', 'raw', FALSE);\n "
                        + "mSet <- UpdateRawfiles(mSet," + fileNms.replaceAll("\"", "\'") + ");\n "
                        + "plan <- InitializaPlan('raw_opt');\n "
                        + rCommandOpt.replaceAll("\"", "\'") + ";\n"
                        + "res <- ExecutePlan(plan);\n Export.Annotation(res[['mSet']]);\n Export.PeakTable(res[['mSet']]);\n Export.PeakSummary(res[['mSet']])";
            } else if ("default".equals(rCommandProcess)) {
                Rcmd = "library(OptiLCMS); "
                        + "setwd(\'" + userPath + "\'); "
                        + "mSet <- InitDataObjects('spec', 'raw', FALSE);\n "
                        + "mSet <- UpdateRawfiles(mSet," + fileNms.replaceAll("\"", "\'") + ");\n "
                        + "plan <- InitializaPlan('raw_ms');\n "
                        + rCommandOpt.replaceAll("\"", "\'") + ";\n"
                        + "res <- ExecutePlan(plan);\n Export.Annotation(res[['mSet']]);\n Export.PeakTable(res[['mSet']]);\n Export.PeakSummary(res[['mSet']])";
            }

            try (FileWriter myWriter = new FileWriter(userPath + "/my_spec_cmds.R")) {
                myWriter.write(Rcmd);
            }
            String sysCmd = bashPath + " " + RScriptHome + "/_bash_to_r.sh" + " " + userPath + "/my_spec_cmds.R";

            //Runtime.getRuntime().exec(sysCmd);
            processExec(sysCmd);

            System.out.println(sysCmd + "|||||Things are running !");
        } catch (Exception e) {
            //System.out.println("exception happened - here's what I know: ");
            LOGGER.error("runRawSpecScript", e);
        }
    }

    public static void doRedirectWithGrowl(SessionBean1 sb, String url, String messageType, String message) {
        try {
            FacesContext facesContext = FacesContext.getCurrentInstance();
            Flash flash = facesContext.getExternalContext().getFlash();

            // Store the message in the flash scope
            flash.put("growlMessage", message);
            flash.put("growlMessageType", messageType);  // New: store the message type

            // Perform the redirect
            facesContext.getExternalContext().redirect(url);
            sb.addMessage(messageType, message);
        } catch (IOException ioe) {
            LOGGER.error("doRedirect", ioe);
        }
    }

    public static void doRedirect(String url, ApplicationBean1 ab) {
        FacesContext context = FacesContext.getCurrentInstance();
        if (context == null) {
            System.out.println("FacesContext is null");
            return;
        }

        try {
            if (!context.getResponseComplete()) {
                if (url.startsWith("/")) {
                    String redirectUrl = ab.getApp_url() + url;
                    System.out.println("Redirecting to URL: " + redirectUrl);
                    context.getExternalContext().redirect(redirectUrl);
                } else {
                    System.out.println("Redirecting to URL: " + url);
                    context.getExternalContext().redirect(url);
                }
                context.responseComplete(); // Mark response as complete
            } else {
                System.out.println("Response already complete");
            }
        } catch (IOException ioe) {
            System.out.println("Error during redirect: " + ioe.getMessage());
            ioe.printStackTrace();
        }
    }

    public static User loadUser(String guestName, String realUserHomePath) {
        try {

            DataUtils.deleteFilesOlderThanNdays(realUserHomePath, 1);
            //first create a random user names
            User user = new User();
            String guestDir = realUserHomePath + File.separator + guestName;
            File guestFolder = new File(guestDir);
            if (!guestFolder.exists()) {
                guestFolder.mkdir();
            }
            user.setName(guestName);
            user.setRelativeDir("/resources/users/" + guestName);
            user.setOrigRelativeDir("/resources/users/" + guestName);

            user.setHomeDir(guestDir);
            user.setOrigHomeDir(guestDir);
            return user;
        } catch (Exception e) {
            //System.out.println("Error in creating users! ===============");
            LOGGER.error("loadUser", e);
        }
        return null;
    }

    public static User loadRawUser(String guestName, ApplicationBean1 ab) {
        try {
            //try to clean the user folder to remove old files (more than 1 day)
            String userHomePath;
            if (ab.shouldUseScheduler()) {
                userHomePath = ab.getRaw_spec_folder();
            } else {
                userHomePath = ab.getRealUserHomePath();
            }
            DataUtils.deleteFilesOlderThanNdays(userHomePath, 14);
            //first create a random user names
            User user = new User();
            String guestDir = userHomePath + File.separator + guestName;
            File guestFolder = new File(guestDir);
            if (!guestFolder.exists()) {
                guestFolder.mkdir();
            }
            user.setName(guestName);
            user.setRelativeDir("/resources/users/" + guestName);
            user.setOrigRelativeDir("/resources/users/" + guestName);

            user.setHomeDir(guestDir);
            user.setOrigHomeDir(guestDir);
            return user;
        } catch (Exception e) {
            // System.out.println("Error in creating users! ===============");
            LOGGER.error("loadRawUser", e);
        }
        return null;
    }

    // '"curl -H "C-H Content-Type: application/json" -X POST -d "{\"name\": \"prof_website\",  \"entities\":{\"page\": \"entity_value\"}}" https://www.omicsbot.ca/conversations/19dcb8ae575447748cbdba384a22af51/trigger_intent?output_channel=latest"'
    public static String uploadXLSXFile(SessionBean1 sb, UploadedFile file, String homeDir, String outFileNm, boolean onServer) {

        if (file == null || file.getSize() == 0) {
            sb.addMessage("Error", "Empty file?");
            return null;
        }

        if (onServer & file.getSize() > ApplicationBean1.MAX_UPLOAD_SIZE) {
            sb.addMessage("Error", "The file size exceeds limit:" + ApplicationBean1.MAX_UPLOAD_SIZE);
            return null;
        }

        String fileName = file.getFileName();

        if (fileName.endsWith(".xlsx") | fileName.endsWith(".xls") | fileName.endsWith(".XLSX")) {
            try {
                OutputStream out;
                try (InputStream in = file.getInputStream()) {
                    if (outFileNm == null) {
                        outFileNm = fileName;
                    }
                    out = new FileOutputStream(new File(homeDir + File.separator + outFileNm));
                    byte[] buffer = new byte[1024];
                    int len;
                    while ((len = in.read(buffer)) >= 0) {
                        out.write(buffer, 0, len);
                    }
                }
                out.close();

            } catch (IOException e) {
                LOGGER.error("uploadFile", e);
            }
        } else {
            sb.addMessage("Error", "Only xlsx/XLSX or xls files are accepted for metabolon dataset!");
            return null;
        }
        return fileName;
    }

    public static String getRandom() {
        Random rnd = new Random();
        int number = rnd.nextInt(999999);
        return String.format("%06d", number);
    }

    public static boolean isValidEmail(String email) {
        // Define a regular expression pattern for a valid email address
        String regex = "^[A-Za-z0-9+_.-]+@(.+)$";

        // Create a Pattern object
        Pattern pattern = Pattern.compile(regex);

        // Create a Matcher object
        Matcher matcher = pattern.matcher(email);

        // Check if the string matches the pattern
        return matcher.matches();
    }

    public static boolean internalizeFile(String fileName, String ori_path, String new_path) {

        //new File ori_file 
        File f = new File(ori_path + "/" + fileName);
        File np = new File(new_path);
        if (!np.exists()) {
            np.mkdir();
        }
        File npf = new File(new_path + "/" + fileName);
        copyFile(f, npf);
        return true;
    }

    public static void removeCookie(String name) {
        Faces.addResponseCookie(name, "", "/", 0);
    }

    //generate always same token for an inputstring
    public static String generateToken(String inputString) {
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
            byte[] hashedBytes = messageDigest.digest(inputString.getBytes());

            // Convert to hexadecimal
            StringBuilder hexString = new StringBuilder();
            for (byte b : hashedBytes) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }

            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            // Handle the exception
            e.printStackTrace();
            return null;
        }
    }

    public static void setupFlashGrowl(String messageType, String message) {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        Flash flash = facesContext.getExternalContext().getFlash();
        // Store the message in the flash scope
        flash.put("growlMessage", message);
        flash.put("growlMessageType", messageType);  // New: store the message type

        // Perform the redirect
    }

    public static HeatMap2Bean findeBean(String hm2Bean) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public static void runRawSpecScriptAsari(RConnection RC, String RScriptHome, String bashPath, String userPath, String usrNm,
            String rCommandOpt, String rCommandProcess, String fileNms) {
        try {
            //String sysCmd = bashPath + " " + RScriptHome + "/_raw_spec_cmd.sh" + " " + userPath + "/ExecuteRawSpec.R";
            String Rcmd = null;
            if ("opt".equals(rCommandProcess)) {
                Rcmd = "library(OptiLCMS); ";
            } else if ("default".equals(rCommandProcess)) {
                Rcmd = "library(OptiLCMS); ";
            }

            try (FileWriter myWriter = new FileWriter(userPath + "/my_spec_cmds.R")) {
                myWriter.write(Rcmd);
            }
            String sysCmd = bashPath + " " + RScriptHome + "/_bash_to_r.sh" + " " + userPath + "/my_spec_cmds.R";

            //Runtime.getRuntime().exec(sysCmd);
            processExec(sysCmd);

            System.out.println(sysCmd + "|||||Things are running !");
        } catch (Exception e) {
            //System.out.println("exception happened - here's what I know: ");
            LOGGER.error("runRawSpecScript", e);
        }
    }

    public static String convertObjToJson(Object obj) {

        ObjectMapper mapper = new ObjectMapper()
                .configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false)
                .setSerializationInclusion(JsonInclude.Include.NON_NULL)
                .setSerializationInclusion(JsonInclude.Include.NON_EMPTY)
                .setVisibility(PropertyAccessor.ALL, JsonAutoDetect.Visibility.DEFAULT);

        /* ---- custom filter: drop “getter-only” props ---------------- */
        mapper.setSerializerFactory(
                mapper.getSerializerFactory()
                        .withSerializerModifier(new BeanSerializerModifier() {

                            @Override
                            public List<BeanPropertyWriter> changeProperties(
                                    SerializationConfig cfg,
                                    BeanDescription beanDesc,
                                    List<BeanPropertyWriter> props) {

                                // collect every declared field name in this class hierarchy
                                Set<String> fieldNames = new HashSet<>();
                                Class<?> c = beanDesc.getBeanClass();
                                while (c != null && c != Object.class) {
                                    for (Field f : c.getDeclaredFields()) {
                                        fieldNames.add(f.getName());
                                    }
                                    c = c.getSuperclass();
                                }

                                // keep property if it came from a FIELD
                                // or the getter’s name matches a real field
                                return props.stream()
                                        .filter(p -> p.getMember() instanceof AnnotatedField // came from a field
                                        || fieldNames.contains(p.getName())) // getter backed by field
                                        .toList();
                            }
                        }));

        try {
            String json = mapper.writeValueAsString(obj);
            // replace literal nulls with empty quotes if you still need that rule
            return json.replace(":null", ":\"\"");
        } catch (JsonProcessingException ex) {
            LOGGER.error("convertObjToJson", ex);
            return null;
        }
    }

    public static void convertJsonToObj(Object bean, String jsonString, String type) throws JsonProcessingException {
        // Creating Object of ObjectMapper define in Jakson Api
        JsonMapper.builder().build().readerForUpdating(bean).readValue(jsonString);
    }

    public static int testRserve() {
        //ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
        ApplicationBean1 ab = CDI.current().select(ApplicationBean1.class).get();
        if (!ab.isInDocker()) {
            System.out.println("Not on docker container.");
            return 1;
        } else {
            System.out.println("In docker container...");
            int rserve_num = 1;
            try {

                String line2;
                Process p2 = Runtime.getRuntime().exec(new String[]{"/usr/bin/bash", "-c", "ps axu | grep Rserve"});
                BufferedReader input2 = new BufferedReader(
                        new InputStreamReader(p2.getInputStream()));
                while ((line2 = input2.readLine()) != null) {
                    System.out.println("Check Rserve Process => " + line2);
                }
                input2.close();

                String line;
                Process p = Runtime.getRuntime().exec(new String[]{"/usr/bin/bash", "-c", "ps axu | grep Rserve | wc -l"});
                BufferedReader input = new BufferedReader(
                        new InputStreamReader(p.getInputStream()));
                while ((line = input.readLine()) != null) {
                    rserve_num = Integer.parseInt(line);
                }
                input.close();
            } catch (IOException | NumberFormatException ex) {
                System.out.println("Have no idea about how many rserve connections exits!");
            }
            System.out.println("Successfully detected " + rserve_num + " Rserve connections!");

            if (rserve_num <= 2) {
                // No rserve connection exits
                try {
                    String sysCmd = "/usr/bin/bash /home/restart_rserve.sh";
                    //Process p = Runtime.getRuntime().exec(sysCmd);
                    //p.waitFor();
                    processExec(sysCmd);
                    System.out.println("Successfully performed restartRserve!");
                } catch (Exception e) {
                    System.out.println("Exception in restartRserve -  ");
                }
            } else {
                return 1;
            }

        }
        return 1;
    }

    public static void createAndCopyFolder(String currentFolderPath, String newFolderPath) {
        File currentFolder = new File(currentFolderPath);
        File newFolder = new File(newFolderPath);

        try {
            // Check if the current folder exists
            if (!currentFolder.exists() || !currentFolder.isDirectory()) {
                throw new IOException("Current folder does not exist or is not a directory: " + currentFolderPath);
            }

            // Create the new folder
            if (!newFolder.exists()) {
                if (!newFolder.mkdirs()) {
                    throw new IOException("Failed to create new folder: " + newFolderPath);
                }
            }

            // Copy the contents of the current folder to the new folder
            File[] files = currentFolder.listFiles();
            if (files == null) {
                throw new IOException("Cannot list files in the directory: " + currentFolderPath);
            }

            for (File file : files) {
                // Skip the new folder and its subfolders
                if (file.getAbsolutePath().equals(newFolder.getAbsolutePath())) {
                    continue;
                }

                File targetFile = new File(newFolder, file.getName());
                if (file.isDirectory()) {
                    //createAndCopyFolder(file.getAbsolutePath(), targetFile.getAbsolutePath());
                } else {
                    // Copy files
                    Files.copy(file.toPath(), targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                }
            }

            System.out.println("Contents copied successfully from " + currentFolderPath + " to " + newFolderPath);
        } catch (IOException e) {
            System.err.println("Error during folder copy: " + e.getMessage());
        }
    }

    public static void removeFilesByExtensions(String folderPath, List<String> extensions) {
        File folder = new File(folderPath);

        try {
            // Check if the folder exists and is a directory
            if (!folder.exists() || !folder.isDirectory()) {
                throw new IOException("Specified folder does not exist or is not a directory: " + folderPath);
            }

            // Get all files and subdirectories in the folder
            File[] files = folder.listFiles();
            if (files == null) {
                throw new IOException("Cannot list files in the directory: " + folderPath);
            }

            for (File file : files) {
                if (file.isDirectory()) {
                    // do not apply to subdirectories

                    // Recursive call to handle subdirectories
                    //removeFilesByExtensions(file.getAbsolutePath(), extensions);
                } else {
                    // Check if the file has any of the specified extensions
                    String fileName = file.getName().toLowerCase();
                    for (String ext : extensions) {
                        if (fileName.endsWith(ext.toLowerCase())) {
                            if (file.delete()) {
                                System.out.println("Deleted: " + file.getAbsolutePath());
                            } else {
                                System.err.println("Failed to delete: " + file.getAbsolutePath());
                            }
                            break; // Exit loop once the file is deleted
                        }
                    }
                }
            }
        } catch (IOException e) {
            System.err.println("Error during file removal: " + e.getMessage());
        }
    }

    public static boolean sendPostRequest(String node, String appName, String token, String funName, String email, String type, String folderName, String jobId, String baseUrl) throws JobExecutionException {
        String urlString;

        if (node.equals("localhost")) {
            urlString = baseUrl + "/faces/AjaxHandler.xhtml?funcNm=" + funName;
        } else {
            urlString = "https://" + node + "." + appName.toLowerCase() + ".ca/" + appName + "/faces/AjaxHandler.xhtml?funcNm=" + funName;
        }

        try {
            HttpClient client = HttpClient.newHttpClient();
            System.out.println(type);
            System.out.println(folderName);

            // Add token and email as parameters
            String urlParameters = Map.of(
                    "tokenId", token,
                    "email", email,
                    "type", type,
                    "folderName", folderName,
                    "jobId", jobId
            ).entrySet().stream()
                    .map(entry -> URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8) + "="
                    + URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8))
                    .collect(Collectors.joining("&"));

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(new URI(urlString))
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .POST(HttpRequest.BodyPublishers.ofString(urlParameters))
                    .build();

            // Send the request and get HttpResponse
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

            int responseCode = response.statusCode();

            // Check the response code
            return responseCode == 200;

        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public static String constructNavigationURL(String node, String appName, String token, String funName, ApplicationBean1 ab) {
        String baseUrl;

        if (node.equals("localhost")) {
            baseUrl = ab.getBaseUrlDyn() +  "/faces/AjaxHandler.xhtml";
        } else {
            baseUrl = "https://" + node + "." + appName.toLowerCase() + ".ca/" + appName + "/faces/AjaxHandler.xhtml";
        }

        // Build query parameters
        String queryParameters = Map.of(
                "funcNm", funName,
                "tokenId", token
        ).entrySet().stream()
                .map(entry -> URLEncoder.encode(entry.getKey(), StandardCharsets.UTF_8) + "="
                + URLEncoder.encode(entry.getValue(), StandardCharsets.UTF_8))
                .collect(Collectors.joining("&"));

        return baseUrl + "?" + queryParameters;
    }

    public static boolean sendMail(String email, String subject, String htmlMsg) throws JsonProcessingException, IOException, InterruptedException {

        HashMap values = new HashMap<String, String>() {
            {
                put("recipient", email);
                put("subject", subject);
                put("htmlBody", htmlMsg);
            }
        };
        ObjectMapper objectMapper = new ObjectMapper();
        String requestBody = objectMapper
                .writeValueAsString(values);

        HttpClient client = HttpClient.newHttpClient();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://132.216.38.10:8082/mail/send"))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();

        HttpResponse<String> response = client.send(request,
                HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());

        return response.statusCode() == 200;

    }

    public static boolean SubmitJob(String jobSubmission) {
        Process proc;
        boolean jobSubmitted = false;

        try {
            List<String> commands = Arrays.asList(jobSubmission.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            proc = processBuilder.start();

            try (BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()))) {
                String jobString;
                while ((jobString = stdInput.readLine()) != null) {
                    int jobID = Integer.parseInt(jobString.replaceAll("[^0-9]", ""));
                    System.out.println("Job " + jobID + " has been submitted successfully!");
                }
            }

            jobSubmitted = true;
        } catch (Exception ex) {
            LOGGER.error("submitJob", ex);
        }

        return jobSubmitted;
    }

    public static boolean processExec(String cmd) {
        Process proc;
        boolean res = false;
        try {
            List<String> commands = Arrays.asList(cmd.split(" ")); // Split the command into arguments
            ProcessBuilder processBuilder = new ProcessBuilder(commands);
            proc = processBuilder.start();

            try (BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()))) {
                String jobString;
                while ((jobString = stdInput.readLine()) != null) {
                    System.out.println(jobString + "\n");
                }
            }

            res = true;
        } catch (Exception ex) {
            LOGGER.error("processExec => ", ex);
        }
        return res;
    }

    public static String obtainTimestampText() {
        // Get the current date and time.
        LocalDateTime now = LocalDateTime.now();

        // Define the desired format. Here it's "yyyy-MM-dd HH:mm:ss".
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

        // Format the current date and time using the formatter.
        return now.format(formatter);
    }

    public static boolean sendRawFinishEmail(MailService ms, String node, String to, String jobId, String folderName) {
        // Construct base URL
        String baseUrl = "https://" + node + ".metaboanalyst.ca/MetaboAnalyst/faces/AjaxHandler.xhtml";

        // Prepare URL parameters with encoding, preserving order
        String queryParameters = "funcNm=" + URLEncoder.encode("finishRawProject", StandardCharsets.UTF_8) + "&"
                + "folderName=" + URLEncoder.encode(folderName, StandardCharsets.UTF_8) + "&"
                + "jobId=" + URLEncoder.encode(jobId, StandardCharsets.UTF_8) + "&"
                + "email=" + URLEncoder.encode(to, StandardCharsets.UTF_8);

        // Return the complete URL
        String url = baseUrl + "?" + queryParameters;
        String htmlMsg = "<!DOCTYPE html>\n"
                + "<html>\n"
                + "<body style=\"font-family: Arial; font-size: 12px;\">\n"
                + "<div>\n"
                + "    <p>\n"
                + "        Your Spectra processing (ID: " + jobId + ") status has become <b>COMPLETED</b>.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        You can access your project by clicking the following link: \n"
                + "        <a href=\"" + url + "\" style=\"color: blue; text-decoration: underline;\">Click Here</a>.\n"
                + "    </p>\n"
                + "    <p>\n"
                + "        Please ignore this email if you did not submit any jobs to MetaboAnalyst.\n"
                + "    </p>\n"
                + "\n"
                + "\n Do NOT reply to this email."
                + "</div>\n"
                + "</body>\n"
                + "</html>";

        boolean res = false;
        try {
            res = ms.sendEmail(to, "MetaboAnalyst - Workflow Completed", "text/html", htmlMsg);
        } catch (IOException ex) {
            java.util.logging.Logger.getLogger(DataUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return res;
    }

    public static void copyFileToFolder(String inputFilePath, String outputFolderPath) {
        // Convert input paths to File objects
        Path inputPath = Paths.get(inputFilePath);
        Path outputFolder = Paths.get(outputFolderPath);

        // Validate input file
        if (!Files.exists(inputPath) || !Files.isRegularFile(inputPath)) {
            try {
                throw new IOException("Input file does not exist or is not a valid file: " + inputFilePath);
            } catch (IOException ex) {
                java.util.logging.Logger.getLogger(DataUtils.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        // Validate output folder and create it if it doesn't exist
        if (!Files.exists(outputFolder)) {
            try {
                Files.createDirectories(outputFolder);
            } catch (IOException ex) {
                java.util.logging.Logger.getLogger(DataUtils.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        // Determine output file path (retain the original file name)
        Path outputPath = outputFolder.resolve(inputPath.getFileName());

        try {
            Files.copy(inputPath, outputPath);
        } catch (IOException ex) {
            java.util.logging.Logger.getLogger(DataUtils.class.getName()).log(Level.SEVERE, null, ex);
        }

        System.out.println("File copied successfully to: " + outputPath.toAbsolutePath());
    }

    public static boolean isLocalNetwork(String hostname) {
        return hostname.startsWith("http://localhost")
                || hostname.startsWith("http://127.0.0.1")
                || hostname.startsWith("http://192.168.")
                || hostname.startsWith("http://10.");
    }

    public static boolean extractFileFromZip(String zipFilePath, String targetFileName, String outputPath) {
        try (ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFilePath))) {
            ZipEntry entry;
            while ((entry = zis.getNextEntry()) != null) {
                if (entry.getName().equals(targetFileName)) {
                    File outFile = new File(outputPath);
                    try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(outFile))) {
                        byte[] buffer = new byte[4096];
                        int read;
                        while ((read = zis.read(buffer)) != -1) {
                            bos.write(buffer, 0, read);
                        }
                        System.out.println("Extracted: " + targetFileName + " to " + outputPath);
                        return true;
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.err.println("File not found in archive: " + targetFileName);
        return false;
    }

    public static Object getBeanInstanceByName(String beanName) {
        try {
            BeanManager beanManager = CDI.current().getBeanManager();

            Set<Bean<?>> beans = beanManager.getBeans(beanName);

            if (beans.isEmpty()) {
                System.err.println("No CDI bean found with name: {0}" + beanName);
                return null;
            }

            Bean<?> bean = beanManager.resolve(beans);

            if (bean == null) {
                System.err.println("Could not resolve CDI bean with name: {0}" + beanName);
                return null;
            }

            // Get the contextual reference without knowing the exact type at compile time for the return.
            return beanManager.getReference(
                    bean,
                    bean.getBeanClass(),
                    beanManager.createCreationalContext(bean)
            );
        } catch (Exception e) {
            System.err.println("CDI container is not available. Ensure this code runs within a Jakarta EE environment or after CDI is initialized. Error: " + e.getMessage());
            return null;
        }
    }
    
    public static FunctionInfo convertLinkedHashMapToFunctionInfo(Object obj) {
        if (obj instanceof LinkedHashMap) {
            ObjectMapper mapper = new ObjectMapper();
            return mapper.convertValue(obj, FunctionInfo.class);

        } else if (obj instanceof FunctionInfo) {
            return (FunctionInfo) obj;

        } else {
            throw new IllegalArgumentException(
                    "Unsupported object type: " + obj.getClass());
        }
    }
}
