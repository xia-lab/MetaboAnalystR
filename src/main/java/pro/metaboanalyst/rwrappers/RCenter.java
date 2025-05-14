/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.rwrappers;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.RList;
import org.apache.commons.lang.StringEscapeUtils;
import org.rosuda.REngine.REngineException;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.utils.DataUtils;

/**
 *
 * @author Jeff
 */
public class RCenter {

    // public static final String RserveIP = "129.128.246.14";
    public static final String RserveIP = "127.0.0.1";
    public static final String RserveIP2 = "127.0.0.2";//to be edited if not local
    public static final int Rport = 6311;
    public static final int Rport2 = 6312; //define ports for Rserve, default 6311 is for MetaboAnalyst
    private static final Logger LOGGER = LogManager.getLogger(RCenter.class);

    //return an RConnection object and specify the home directory
    public static RConnection getRConnection(String homeDir, String scriptPath, String moduleNm) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            String rCommand = "setwd(\"" + homeDir + "\")\n"
                    + "unlink(dir(), recursive=T)\n"
                    + "file.create(\"Rhistory.R\")\n";
            RC.voidEval(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.voidEval("LoadRscripts(\"" + moduleNm + "\")");
            recordRserveConnection(RC, homeDir);
            return RC;
        } catch (RserveException e) {
            LOGGER.error("getRConnection", e);
            return null;
        }
    }

    //for switching module
    public static RConnection updateRConnection(RConnection RC1, RConnection RC2, String scriptPath, String moduleNm, String homeDir) {
        try {
            //String rCommand = "rm(list=setdiff(ls(), \"mSet\"))\n";
            String rCommand = "qs::qsave(mSet, file=\"mSet.qs\");";
            RC1.voidEval(rCommand);
            RC1.close();
            DataUtils.testRserve();
            rCommand = "setwd(\"" + homeDir + "\");"
                    + "source(\"" + scriptPath + "\");"
                    + "LoadRscripts(\"" + moduleNm + "\");"
                    + "mSet <<- qs::qread(\"mSet.qs\")\n";
            RC2.voidEval(rCommand);
            return RC2;
        } catch (RserveException e) {
            LOGGER.error("updateRConnection", e);
            return null;
        }
    }

    public static RConnection getRConnectionRawSharing(String homeDir, String scriptPath, String moduleNm) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            String rCommand = "setwd(\"" + homeDir + "\")\n";
            RC.voidEval(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.voidEval("LoadRscripts(\"" + moduleNm + "\")");
            recordRserveConnection(RC, homeDir);
            return RC;
        } catch (RserveException e) {
            LOGGER.error("getRConnectionRawSharing", e);
            return null;
        }
    }

    public static RConnection getRConnectionRawSharingMulti(String homeDir, String scriptPath, String[] moduleNm) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            String rCommand = "setwd(\"" + homeDir + "\")\n";
            RC.voidEval(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("moduleNms.vec", moduleNm);
            RC.voidEval("LoadRscriptsMulti()");
            recordRserveConnection(RC, homeDir);
            return RC;
        } catch (RserveException e) {
            LOGGER.error("getRConnectionRawSharing", e);
            return null;
        } catch (REngineException ex) {
            java.util.logging.Logger.getLogger(RCenter.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
    }

    //R connection without preloading anything
    public static RConnection getCleanRConnection() {

        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            //System.out.println(RC);
            return RC;
        } catch (RserveException e) {
            LOGGER.error("getCleanRConnection", e);
            return null;
        }
    }

    public static boolean recordRserveConnection(RConnection RC, String homeDIR) {
        String record_cmd = "if(file.exists(\"/data/glassfish/projects/RServePID_History\")){write.table(paste0(Sys.getpid(), \" - \" , basename(\" " + homeDIR + "\"), \" ---> MetaboAnalyst <--- \", Sys.time()), file = \"/data/glassfish/projects/RServePID_History\", append = T, col.names = F, quote = F, row.names = F)}";

        try {
            RC.voidEval(record_cmd);
        } catch (RserveException ex) {
            java.util.logging.Logger.getLogger(RCenter.class.getName()).log(Level.SEVERE, null, ex);
        }
        return false;
    }

    public static boolean compileRScripts(String homeDir, String scriptPath) {
        try {
            //System.out.println("=============" + homeDir);
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            RC.voidEval("setwd(\"" + homeDir + "\"); source(\"" + scriptPath + "\")");
            recordRserveConnection(RC, homeDir);
            return RC.eval("CompileScripts()").asInteger() == 1;
        } catch (Exception e) {
            LOGGER.error("compileRScripts", e);
            return false;
        }
    }

    //this should be the better one
    public static void recordRCommand(RConnection RC, String rCommand, Boolean doEval) {
        try {
            rCommand = cleanRCmd(rCommand);
            if (doEval) {
                RC.voidEval("RecordRCommand(NA" + ", \"" + rCommand + "\")");
                updateBatchTemplate(RC, rCommand);
            } else {
                recordRCommand(RC, rCommand);
            }
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("recordRCommand", e);
        }
    }

    public static void recordMessage(RConnection RC, String msg) {
        try {
            RC.voidEval("RecordSysMessage(\"" + msg + "\")");
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("recordRCommand", e);
        }
    }

    public static String[] getSysMessages(RConnection RC) {
        try {
            return RC.eval("GetSysMessages()").asStrings();
        } catch (Exception e) {
            LOGGER.error("getSysMessages", e);
        }
        return new String[]{"ERROR", "Unknown error occurred."};
    }

    //this should be the better one
    public static void saveCurrentSession(RConnection RC) {
        try {
            RC.voidEval("SaveCurrentSession()");
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("saveCurrentSession", e);
        }
    }

    public static void recordRCommand(RConnection RC, String rCmd) {
        try {
            String rCommand = cleanRCmd(rCmd);
            //for local MetaboAnalystR package
            if (rCommand.contains("(NA")) {
                rCommand = rCommand.replace("(NA", "(mSet");
                if (!rCommand.contains("<-")) {
                    rCommand = "mSet<-" + rCommand;
                }
            }
            if (rCommand.contains("InitDataObjects")) {
                if (!rCommand.contains("<-")) {
                    rCommand = "mSet<-" + rCommand;
                }
            }
            //System.out.println("RecordRCommand(NA" + ", \"" + rCommand + "\")");
            RC.voidEval("RecordRCommand(NA" + ", \"" + rCommand + "\")");
            updateBatchTemplate(RC, rCommand);
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("recordRCommand", e);
        }
    }

    public static String cleanRCmd(String rCommand) {
        int inx = 0;
        while (rCommand.indexOf("\"", inx) != -1) {
            inx = rCommand.indexOf("\"", inx);
            StringBuffer strB = new StringBuffer(rCommand).insert(inx, "\\");
            rCommand = strB.toString();
            inx = inx + 2; //move one step, plus the position of backslash
        }
        return rCommand;
    }

    public static String[] getRCommandHistory(RConnection RC) {
        try {
            return RC.eval("tryCatch(GetRCommandHistory(NA), error = function(e) { return(c(\"ERROR\", e$message)) })").asStrings();
        } catch (Exception e) {
            LOGGER.error("getRCommandHistory", e);
        }
        return new String[]{"ERROR", "Unknown error occurred."};
    }

    public static void loadReporterFuns(SessionBean1 sb, String module) {
        try {
            RConnection RC = sb.getRConnection();
            if (sb.getCurrentUser().getHomeDir().equals(sb.getCurrentUser().getOrigHomeDir())) {
                String rCommand = "LoadReporter(\"" + module + "\");";
                RC.voidEval(rCommand);
            } else {
                String origDir = sb.getCurrentUser().getOrigHomeDir();
                String newDir = sb.getCurrentUser().getHomeDir();

                RC.voidEval("setwd(\"" + origDir + "\")");
                String rCommand = "LoadReporter(\"" + module + "\");";
                RC.voidEval(rCommand);
                RC.voidEval("setwd(\"" + newDir + "\")");

            }
        } catch (Exception e) {
            LOGGER.error("loadReporterFuns", e);
        }
    }

    public static boolean prepareReport(RConnection RC, String usrName, String link) {
        try {
            if (usrName.endsWith("tmp")) {
                usrName = usrName.substring(0, usrName.length() - 3);
            }
            //String rCommand = "PreparePDFReport(NA" + ", \"" + usrName + "\")\n";
            String rCommand = "PrepareHTMLReport(NA" + ", \"" + usrName + "\", \"" + link + "\")\n";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (Exception e) {//Catch exception if any
            LOGGER.error("prepareReport", e);
            return false;
        }
    }

    public static boolean prepareReportByModule(RConnection RC, String usrName, String link, String module) {
        try {
            if (usrName.endsWith("tmp")) {
                usrName = usrName.substring(0, usrName.length() - 3);
            }
            //String rCommand = "PreparePDFReport(NA" + ", \"" + usrName + "\")\n";
            String rCommand = "PrepareHTMLReportModule(NA" + ", \"" + usrName + "\", \"" + link + "\", \"" + module + "\")\n";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (Exception e) {//Catch exception if any
            LOGGER.error("prepareReport", e);
            return false;
        }
    }

    public static String getBashFullPath(RConnection RC) {
        try {
            //System.out.println("======= RC ++++----: " + RC);
            return RC.eval("GetBashFullPath()").asString();
        } catch (Exception rse) {
            LOGGER.error("getBashFullPath", rse);
            return null;
        }
    }

    public static void showMemoryUsage(RConnection RC) {
        try {
            String rCommand = "ShowMemoryUse();";
            RC.voidEval(rCommand);
            //compound names for hypergeometric test
            //RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            LOGGER.error("showMemoryUsage", e);
        }
    }

    public static void cleanMemory(RConnection RC) {
        try {
            RC.voidEval("cleanMem()");
        } catch (Exception e) {
            //e.printStackTrace();
            //LOGGER.error("cleanMem", e);
        }
    }

    public static void loadRScripts(RConnection RC, String moduleNm) {
        try {
            RC.voidEval("LoadRscripts(\"" + moduleNm + "\")");
        } catch (Exception e) {
            LOGGER.error("loadRScripts", e);
        }
    }

    //this use a NEW Rconnection,  dat.in and my.fun 
    public static int performRserveMicro(String homeDir) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            //System.out.println("===== now peforming microservice .........");
            String rCommand = "setwd(\"" + homeDir + "\");"
                    + "dat.in <- qs::qread(\"dat.in.qs\");"
                    + "dat.in$my.res <- dat.in$my.fun();"
                    + "qs::qsave(dat.in, file=\"dat.in.qs\");";
            int res = RC.eval(rCommand).asInteger();
            recordRserveConnection(RC, homeDir);
            RC.close();
            return res;
        } catch (Exception e) {
            LOGGER.error("performRserveMicro", e);
            return 0;
        }
    }

public static void saveRLoadImg(RConnection RC) {
    try {
RC.voidEval(
    "obj <- ls(envir = .GlobalEnv, all.names = TRUE)\n" +
    "obj <- obj[!vapply(obj, function(n) is.function(get(n, envir = .GlobalEnv)), logical(1))]\n" +
    "save(list = obj, file = 'Rload.RData', envir = .GlobalEnv)\n"
);

    } catch (Exception e) {
        LOGGER.error("saveRLoadImg", e);
    }
}



    public static void LoadRLoadImg(RConnection RC, String ImageNm) { // NOTE: this function is used to load Batch image ONLY!!!
        try {
            String rCommand = "load(\"Rload_batch.RData\")";
            RC.voidEval(rCommand);
        } catch (Exception e) {
            LOGGER.error("LoadRLoadImg", e);
        }
    }

    public static void loadHistory(RConnection RC) {
        try {
            String rCommand = "LoadRHistory()";
            RC.voidEval(rCommand);
            String rCommand2 = "Reload.scripts.on.demand();";
            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            //System.out.println(rse);
            LOGGER.error("loadHistory", rse);
        }
    }

    public static void setReportImgMap(RConnection RC, String mapStr) {
        try {
            String escapedMapStr = StringEscapeUtils.escapeJava(mapStr);
            String rCommand = "SetReportImgMap(NA,\"" + escapedMapStr + "\")";
            System.out.println(rCommand);
            RC.eval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("setReportImgMap", rse);
        }
    }

    public static void createProjectDb(String scriptPath, String path) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return;
            }
            RC.voidEval("source(\"" + scriptPath + "\")");
            String rCommand = "CreateProjectDb()";
            RC.assign("projectDbPath", path);
            System.out.println(rCommand);
            RC.eval(rCommand);
            RC.close();
        } catch (Exception rse) {
            LOGGER.error("CreateProjectDb", rse);
        }
    }

    public static void deleteProjectById(
            String scriptPath,
            String dbPath,
            Long id) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            String rCommand = "DeleteProjectById(" + id + ")";
            System.out.println(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);
            RC.eval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("deleteProjectById", rse);
        }
    }

    public static String registerUser(
            String scriptPath,
            String dbPath,
            String password,
            String email,
            String firstname,
            String lastname,
            String institution
    ) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                return "Please start your Rserver with the right permission!";
            }

            // Construct R function call as a string
            String rCommand = String.format(
                    "RegisterUser( '%s', '%s', '%s', '%s', '%s')",
                    password, email, firstname, lastname, institution
            );

            File dbFile = new File(dbPath);
            if (!dbFile.exists()) {
                return "The project path cannot be detected: " + dbPath;
            }

            // Print and execute the R command
            System.out.println(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception e) {
            LOGGER.error("Error in registerUser: ", e);
            return "Unknown error occurred. Please contact your support.";
        }
    }

    public static String[] doLoginUser(
            String scriptPath,
            String dbPath,
            String password,
            String email
    ) {
        try {

            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                return new String[]{"Please start your Rserver with the right permission!"};
            }

            // Construct R function call as a string
            String rCommand = String.format(
                    "LoginUser( '%s', '%s')",
                    password, email
            );
            // Print and execute the R command
            System.out.println(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);
            String[] res = RC.eval(rCommand).asStrings();
            RC.close();
            return res;
        } catch (Exception e) {
            LOGGER.error("Error in registerUser: ", e);
            return null;
        }
    }

    public static void setProjectDbPath(RConnection RC, String path) {
        try {
            String rCommand = "SetProjectDbPath(\"" + path + "\")";
            System.out.println(rCommand);
            RC.eval(rCommand);
        } catch (Exception rse) {
            LOGGER.error("SetProjectDbPath", rse);
        }
    }

    public static int writeProjectToSQLite(String scriptPath,
            String dbPath, Map<String, Object> rawDocData, String projectType) {
        try {
            if (rawDocData.values().stream().anyMatch(Objects::isNull)) {
                throw new IllegalArgumentException("docData contains null values");
            }
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);

            Map<String, String> docData = new HashMap<>();
            for (Map.Entry<String, Object> entry : rawDocData.entrySet()) {
                String key = entry.getKey();
                // Check if the value is null before converting to string and replacing characters
                String value = (entry.getValue() == null) ? "" : entry.getValue().toString().replace("'", "''");
                docData.put(key, value);
            }

            RC.assign("javaHistoryString", docData.get("javaHistory"));
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);

            String rCommand = String.format(
                    "writeProjectToSQLite('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                    docData.get("userId"),
                    docData.get("name"),
                    docData.get("description"),
                    docData.get("module"),
                    docData.get("moduleReadable"),
                    //docData.get("imgMap"),
                    docData.get("dataType"),
                    docData.get("date"),
                    docData.get("folderName"),
                    "",//docData.get("javaHistory"), 
                    //docData.get("naviTrack"), 
                    docData.get("naviString"),
                    docData.get("naviCode"),
                    docData.get("id"),
                    docData.get("org"),
                    docData.get("partialToken"),
                    docData.get("toolName"),
                    docData.get("toolCode"),
                    projectType,
                    docData.get("paired"),
                    docData.get("regression"));
            //RCenter.recordRCommand(RC, rCommand);
            System.out.println(rCommand);
            int res = RC.eval(rCommand).asInteger();
            RC.close();
            return res;
        } catch (Exception e) {
            e.printStackTrace();
            return 0;
        }
    }

    public static ArrayList<HashMap<String, Object>> getProjectsFromSQLite(
            String scriptPath,
            String dbPath,
            String uid,
            String toolName
    ) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            // Construct R function call as a string
            String rCommand = String.format(
                    "getProjectsFromSQLite('%s', '%s')",
                    uid, toolName
            );

            // Print and execute the R command
            System.out.println(rCommand);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);
            REXP result = RC.eval(rCommand);

            // Check if the R function returned NULL
            if (result.isNull()) {
                System.out.println("No matches found.");
                RC.close();
                return null;
            }
            // Convert the R list of lists to Java List of HashMaps
            RList rList = result.asList();
            ArrayList<HashMap<String, Object>> javaList = new ArrayList<>();

            for (int i = 0; i < rList.size(); i++) {
                RList row = rList.at(i).asList();
                HashMap<String, Object> map = new HashMap<>();

                for (String key : row.keys()) {
                    Object value = row.at(key).asNativeJavaObject();

                    // Check if the value is an array and convert the first element to a string
                    if (value instanceof String[] && ((String[]) value).length > 0) {
                        value = ((String[]) value)[0];
                    } else if (value instanceof int[] && ((int[]) value).length > 0) {
                        value = ((int[]) value)[0];
                    }

                    // Convert value to String
                    map.put(key, String.valueOf(value));
                }

                javaList.add(map);
            }
            RC.close();
            return javaList;
        } catch (Exception e) {
            System.err.println("Error in getProjectsFromSQLite: " + e);
            return null;
        }
    }

    public static Map<String, Object> loadProject(String scriptPath,
            String dbPath, String token) {
        Map<String, Object> resultMap = new HashMap<>();
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);
            // Construct the R function call as a string
            String rCommand = String.format("loadProject('%s')", token);

            // Execute the R command
            REXP res = RC.eval(rCommand);

            // Assuming the R function returns a data.frame which is transferred as RList
            RList list = res.asList();

            // Convert the RList to a Java Map
            for (String key : list.keys()) {
                Object value = list.at(key).asNativeJavaObject();
                // Check if the value is an array and convert the first element to a string
                if (value instanceof String[] && ((String[]) value).length > 0) {
                    value = ((String[]) value)[0];
                } else if (value instanceof int[] && ((int[]) value).length > 0) {
                    value = ((int[]) value)[0];
                }

                // Convert value to String
                resultMap.put(key, String.valueOf(value));
            }
        } catch (Exception e) {
            e.printStackTrace();
            // Log the error or handle it as required
        }
        return resultMap;
    }

    public static String checkUserExists(String scriptPath, String path, String email) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }
            RC.voidEval("source(\"" + scriptPath + "\")");
            String rCommand = "CheckUserExists(\"" + email + "\")";
            RC.assign("projectDbPath", path);
            System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception rse) {
            LOGGER.error("CreateProjectDb", rse);
            return "User checking error!";
        }
    }

    public static String insertToken(String scriptPath, String path, String email, String resetToken, String expDate) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }
            RC.voidEval("source(\"" + scriptPath + "\")");
            String rCommand = "InsertToken(\"" + email + "\", \"" + resetToken + "\", \"" + expDate + "\")";
            RC.assign("projectDbPath", path);
            System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception rse) {
            LOGGER.error("CreateProjectDb", rse);
            return "insertToken error!";
        }
    }

    public static String verifyToken(String scriptPath, String path, String resetToken) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }
            RC.voidEval("source(\"" + scriptPath + "\")");
            String rCommand = "VerifyToken(\"" + resetToken + "\")";
            RC.assign("projectDbPath", path);
            System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception rse) {
            LOGGER.error("CreateProjectDb", rse);
            return "verifyToken error!";
        }
    }

    public static String resetPassword(String scriptPath, String path, String password, String email) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }
            RC.voidEval("source(\"" + scriptPath + "\")");
            String rCommand = "ResetPassword(\"" + email + "\", \"" + password + "\")";
            RC.assign("projectDbPath", path);
            //System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception rse) {
            LOGGER.error("CreateProjectDb", rse);
            return "Reset password error!";
        }
    }

    public static String deleteTokenForUser(String scriptPath, String path, String email) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }
            RC.voidEval("source(\"" + scriptPath + "\")");
            String rCommand = "DeleteTokenForUser(\"" + email + "\")";
            RC.assign("projectDbPath", path);
            System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception rse) {
            LOGGER.error("CreateProjectDb", rse);
            return "Reset password error!";
        }
    }

    public static String checkActivationCode(String scriptPath, String projectDbPath, String email, String activationCode) {
        try {
            // Establish an R connection
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }

            // Load the R script
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", projectDbPath);

            // Construct the R command
            String rCommand = "CheckActivationCode(\"" + email + "\", \"" + activationCode + "\")";

            // Execute the R command
            System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();

            // Close the R connection
            RC.close();

            return res;
        } catch (Exception rse) {
            LOGGER.error("SelectUserData", rse);
            return "SelectUserData error!";
        }
    }

    public static String addActivationCode(String scriptPath, String projectDbPath, String activateCode, String expDate, String email) {
        try {
            // Establish an R connection
            RConnection RC = null;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (RserveException e) {
                return "Error: Please start your Rserve with the right permission!";
            }

            // Load the R script
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", projectDbPath);

            // Construct the R command to call AddActivationCode function
            String rCommand = "AddActivationCode(\"" + activateCode + "\", \"" + expDate + "\", \"" + email + "\")";

            System.out.println("R Command: " + rCommand);

            // Execute the R command
            String res = RC.eval(rCommand).asString();
            RC.close();

            return res;
        } catch (Exception rse) {
            LOGGER.error("SelectUserData", rse);
            return "SelectUserData error!";
        }
    }

    public static int SetSharingLink(RConnection RC, String shareLink) {
        try {
            return RC.eval("SetSharingLink(\"" + shareLink + "\")").asInteger();
        } catch (Exception rse) {
            LOGGER.error("SetSharingLink", rse);
            return 0;
        }
    }

    public static int updateTitleAndDescription(String scriptPath,
            String dbPath, String title, String description, int projectId) {
        try {
            DataUtils.testRserve();
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            // Print and execute the R command
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", dbPath);

            String rCommand = String.format("UpdateProjectTitleDescription('%s', '%s', %d)", title, description, projectId);
            int res = RC.eval(rCommand).asInteger();
            return res;
        } catch (Exception e) {
            e.printStackTrace();
            return 0;
        }
    }

    public static int checkMatchingFolderNameProject(String scriptPath, String projectDbPath, String folderName) {
        try {
            // Establish an R connection
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return -1;
            }

            // Load the R script
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", projectDbPath);

            // Construct the R command
            String rCommand = "CheckMatchingFolderNameProject(\"" + folderName + "\")";

            // Execute the R command
            System.out.println(rCommand);
            int res = RC.eval(rCommand).asInteger();

            // Close the R connection
            RC.close();

            return res;
        } catch (Exception rse) {
            LOGGER.error("DeleteUserAndProjects", rse);
            return -1;
        }
    }

    public static String deleteUserAndProjects(String scriptPath, String projectDbPath, String userId) {
        try {
            // Establish an R connection
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }

            // Load the R script
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", projectDbPath);

            // Construct the R command
            String rCommand = "DeleteUserAndProjects(\"" + userId + "\")";

            // Execute the R command
            System.out.println(rCommand);
            String res = RC.eval(rCommand).asString();

            // Close the R connection
            RC.close();

            return res;
        } catch (Exception rse) {
            LOGGER.error("DeleteUserAndProjects", rse);
            return "DeleteUserAndProjects error!";
        }
    }

    public static void updateBatchTemplate(RConnection RC, String rCommand) {
        // this function is pro tools specific
        String rCmd = "UpdateBatchTemplate(\"" + rCommand + "\")";
        try {
            RC.voidEval(rCmd);
        } catch (RserveException ex) {
            java.util.logging.Logger.getLogger(RCenter.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public static int transferJSFHistory(RConnection RC, String projectDbPath, String template_TK, String batchPrj_TK) {
        try {
            String rCommand = "transferJSFHistory(\"" + projectDbPath + "\", \"" + template_TK + "\", \"" + batchPrj_TK + "\")";
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
        return 1;
    }

    public static String updateJSFHistory(RConnection RC, String projectDbPath, String targetPrj_TK) {
        try {
            String rCommand = "updateJSFHistory(\"" + projectDbPath + "\", \"" + targetPrj_TK + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return "";
    }

    public static String[] extractGraphicsMap(RConnection RC, String projectDbPath, String targetPrj_TK) {
        try {
            String rCommand = "extractGraphicsMap(\"" + projectDbPath + "\", \"" + targetPrj_TK + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] extractImgMap(RConnection RC, String projectDbPath, String targetPrj_TK) {
        try {
            String rCommand = "extractImgMap(\"" + projectDbPath + "\", \"" + targetPrj_TK + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void setReportFormat(RConnection RC, String format) {
        try {
            RC.voidEval("SetReportFormat(NA, \"" + format + "\")");
        } catch (Exception rse) {
            LOGGER.error("setRenderOutput", rse);
        }
    }

    public static String hashPassword(String scriptPath, String projectDbPath, String password) {
        try {
            RConnection RC;
            try {
                DataUtils.testRserve();
                RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            } catch (Exception e) {
                SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
                sb.addMessage("Error", "Please start your Rserver with the right permission!");
                return "";
            }
            // Load the R script
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.assign("projectDbPath", projectDbPath);
            String rCommand = ("HashPassword(\"" + password + "\")");
            String res = RC.eval(rCommand).asString();
            RC.close();
            return res;
        } catch (Exception rse) {
            LOGGER.error("HashPassword", rse);
            return "HashPassword error!";
        }
    }

    public static void setWd(RConnection RC, String path) {
        try {
            RC.voidEval("setwd(\"" + path + "\")");

        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("setWd", e);
        }
    }

    public static void setResourceDir(RConnection RC, String path) {
        try {
            RC.voidEval("setResourceDir(\"" + path + "\")");
        } catch (Exception e) {
            //e.printStackTrace();
            LOGGER.error("setResourceDir", e);
        }
    }

}
