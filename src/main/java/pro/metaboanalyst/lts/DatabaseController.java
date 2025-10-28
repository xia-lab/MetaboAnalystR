/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.gson.Gson;
import com.google.gson.JsonParser;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import jakarta.inject.Named;
import java.sql.Types;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.postgresql.util.PGobject;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.controllers.general.SessionBean1;
import pro.metaboanalyst.datalts.DatasetFile;
import pro.metaboanalyst.datalts.DatasetRow;
import pro.metaboanalyst.rwrappers.RCenter;

/**
 *
 * @author zgy
 */
@RequestScoped
@Named("databaseController")
public class DatabaseController implements Serializable {

    @JsonIgnore
    @Inject
    private ApplicationBean1 ab;
    @JsonIgnore
    @Inject
    private SessionBean1 sb;
    @JsonIgnore
    @Inject
    private FireBase fb;

    public String registerUser(String email, String password, String firstname, String lastname, String institution) {

        String hashedPassword = RCenter.hashPassword(sb, fb.getRscriptsDBPath(), fb.getProjectDBPath(), password);

        Connection con = null;
        PreparedStatement checkStmt = null;
        PreparedStatement insertStmt = null;
        ResultSet res = null;

        try {
            con = DatabaseConnectionPool.getConnection(); // Get a connection from the pool
            int activated = 0;
            if (!ab.isInDocker()) {
                String checkQuery = "SELECT tool_metaboanalyst FROM registration WHERE email = ?";
                checkStmt = con.prepareStatement(checkQuery);
                checkStmt.setString(1, email);
                res = checkStmt.executeQuery();

                if (res.next()) {
                    boolean isToolMetaboanalyst = res.getBoolean("tool_metaboanalyst");

                    if (!isToolMetaboanalyst) {
                        return "Registration Error - User registered but is not authorized for MetaboAnalyst.";
                    }
                } else {
                    return "Registration Error - User is not registered to XiaLab Pro! Please visit www.xialab.ca for more info!";
                }
                activated = 0;
            } else {
                activated = 1;

            }

            // Insert new user
            String insertQuery = "INSERT INTO users (password, email, firstname, lastname, institution, activated) VALUES (?, ?, ?, ?, ?, ?)";
            insertStmt = con.prepareStatement(insertQuery);
            insertStmt.setString(1, hashedPassword);
            insertStmt.setString(2, email);
            insertStmt.setString(3, firstname);
            insertStmt.setString(4, lastname);
            insertStmt.setString(5, institution);
            insertStmt.setInt(6, activated);

            int updateCount = insertStmt.executeUpdate();

            return updateCount > 0 ? "User registered successfully." : "User registration failed.";

        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            return "Registration Error - " + ex.getMessage();
        } finally {
            // Close all resources
            try {
                if (res != null) {
                    res.close();
                }
                if (checkStmt != null) {
                    checkStmt.close();
                }
                if (insertStmt != null) {
                    insertStmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool, not really closing it
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
    }

    public String[] loginUserDocker(String email, String password) {
        //System.out.println("loginUser --> step0");
        // Check PSQL db availability (This can be a method that checks if pool is available or not)
        // Assuming DatabaseConnectionPool.getDataSource() is always available or throws an exception if not.
        // Hash the password using SHA-256
        String hashedPassword = RCenter.hashPassword(sb, fb.getRscriptsDBPath(),
                fb.getProjectDBPath(), password);

        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet user_data = null;

        try {
            con = DatabaseConnectionPool.getConnection(); // Get a connection from the pool
            con.setAutoCommit(false); // Start transaction

            // Fetch stored password and additional fields
            String query = "SELECT * FROM users WHERE email = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            user_data = stmt.executeQuery();

            if (!user_data.next()) {
                con.rollback(); // Roll back the transaction
                return new String[]{"Username not found."};
            }

            String storedPassword = user_data.getString("password");
            int activationStatus = user_data.getInt("activated");

            if (activationStatus != 1) {
                con.rollback(); // Roll back the transaction
                return new String[]{"Account needs to be activated first."};
            }

            if (storedPassword.equals(hashedPassword)) {
                con.commit(); // Commit the transaction
                // Return additional user data
                return new String[]{
                    user_data.getString("email"),
                    user_data.getString("firstname"),
                    user_data.getString("lastname"),
                    user_data.getString("institution"),
                    "docker"
                };
            } else {
                con.rollback(); // Roll back the transaction
                return new String[]{"Invalid password."};
            }

        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            return new String[]{"Login Error - " + ex.getMessage()};
        } finally {
            // Close all resources
            try {
                if (user_data != null) {
                    user_data.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool, not really closing it
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
    }

    //the table name is defined
    public static int writeProjectToPostgres(Map<String, Object> rawDocData, String projectType, String tableName) {
        // Convert all values in rawDocData to String, handling single quotes
        Map<String, String> docData = new HashMap<>();
        for (Map.Entry<String, Object> entry : rawDocData.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue().toString().replace("'", "''"); // Handling single quotes
            docData.put(key, value);
        }

        int result = 0;
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet existingCount = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false); // Start transaction

            // Prepare the query string using the tableName parameter
            String checkQuery = String.format("SELECT COUNT(*) FROM %s WHERE folderName=? AND projectType='project'", tableName);
            PreparedStatement checkStmt = con.prepareStatement(checkQuery);
            checkStmt.setString(1, docData.get("foldername"));
            existingCount = checkStmt.executeQuery();

            if (existingCount.next() && existingCount.getInt(1) > 0) {
                String existingName = null;
                if ("Autosaved Project".equals(docData.get("name"))) {
                    String fetchNameQuery = String.format("SELECT name FROM %s WHERE folderName=? AND projectType='project'", tableName);
                    try (PreparedStatement fetchNameStmt = con.prepareStatement(fetchNameQuery)) {
                        fetchNameStmt.setString(1, docData.get("foldername"));
                        try (ResultSet rs = fetchNameStmt.executeQuery()) {
                            if (rs.next()) {
                                existingName = rs.getString("name");
                            }
                        }
                    }
                    if (existingName != null) {
                        docData.put("name", existingName);
                    }
                }

                String updateQuery = String.format("UPDATE %s SET userId=?, name=?, description=?, module=?, moduleReadable=?, dataType=?, date=?, javaHistory=?, naviString=?, naviCode=?, org=?, partialToken=?, toolName=?, toolCode=?, projectType=?, paired=?, regression=?, location=? WHERE folderName=?", tableName);
                stmt = con.prepareStatement(updateQuery);
            } else {
                String insertQuery = String.format("INSERT INTO %s (userId, name, description, module, moduleReadable, dataType, date, javaHistory, naviString, naviCode, org, partialToken, toolName, toolCode, projectType, paired, regression, location, folderName) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", tableName);
                stmt = con.prepareStatement(insertQuery);
            }

            setPreparedStatementParameters(stmt, docData, projectType);
            stmt.executeUpdate();
            con.commit();
            result = 1;
        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            if (con != null) {
                try {
                    con.rollback();
                } catch (SQLException exRollback) {
                    System.out.println("Rollback failed: " + exRollback.getMessage());
                }
            }
            result = -1;
        } finally {
            try {
                if (existingCount != null) {
                    existingCount.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return result;
    }

    private static void setPreparedStatementParameters(PreparedStatement pstmt, Map<String, String> docData, String projectType) throws SQLException {
        pstmt.setString(1, docData.get("userid"));
        pstmt.setString(2, docData.get("name"));
        pstmt.setString(3, docData.get("description"));
        pstmt.setString(4, docData.get("module"));
        pstmt.setString(5, docData.get("modulereadable"));
        pstmt.setString(6, docData.get("datatype"));
        pstmt.setString(7, docData.get("date"));
        pstmt.setString(8, docData.get("javahistory"));
        pstmt.setString(9, docData.get("navistring"));
        pstmt.setString(10, docData.get("navicode"));
        pstmt.setString(11, docData.get("org"));
        pstmt.setString(12, docData.get("partialtoken"));
        pstmt.setString(13, docData.get("toolname"));
        pstmt.setString(14, docData.get("toolcode"));
        pstmt.setString(15, projectType);
        pstmt.setBoolean(16, Boolean.parseBoolean(docData.get("paired")));
        pstmt.setBoolean(17, Boolean.parseBoolean(docData.get("regression")));
        pstmt.setString(18, docData.get("location"));
        pstmt.setString(19, docData.get("foldername"));
    }

    public static int updateProjectTitleDescription(String newName, String newDescription, int id) {
        int result = 0;
        Connection con = null;
        PreparedStatement checkStmt = null;
        PreparedStatement updateStmt = null;
        ResultSet tables = null;
        ResultSet existingRow = null;

        try {
            con = DatabaseConnectionPool.getConnection(); // Get a connection from the pool
            con.setAutoCommit(false); // Start transaction

            // Check if 'project' table exists
            DatabaseMetaData dbm = con.getMetaData();
            tables = dbm.getTables(null, null, "project", null);
            if (!tables.next()) {
                throw new SQLException("Table 'project' does not exist in the database.");
            }

            // Check if a row with the given id exists
            String checkQuery = "SELECT id FROM project WHERE id = ?";
            checkStmt = con.prepareStatement(checkQuery);
            checkStmt.setInt(1, id);
            existingRow = checkStmt.executeQuery();

            if (existingRow.next()) {
                // Update the existing row
                String updateQuery = "UPDATE project SET name = ?, description = ? WHERE id = ?";
                updateStmt = con.prepareStatement(updateQuery);

                updateStmt.setString(1, newName);
                updateStmt.setString(2, newDescription);
                updateStmt.setInt(3, id);

                updateStmt.executeUpdate();
                result = 1; // Successfully updated
                con.commit();
            } else {
                System.out.println("No row exists with the given id.");
            }

        } catch (SQLException ex) {
            System.out.println("updateProjectTitleDescription SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback(); // Roll back in case of any failure
                }
            } catch (SQLException exRollback) {
                System.out.println("Rollback failed: " + exRollback.getMessage());
            }
            result = -1; // Indicate an error
        } finally {
            // Close all resources
            try {
                if (existingRow != null) {
                    existingRow.close();
                }
                if (tables != null) {
                    tables.close();
                }
                if (checkStmt != null) {
                    checkStmt.close();
                }
                if (updateStmt != null) {
                    updateStmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return result;
    }

    public static ArrayList<HashMap<String, Object>> getProjectsFromPostgres(String email, String toolName, String toolLocation) {
        ArrayList<HashMap<String, Object>> projects = new ArrayList<>();
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection(); // Get a connection from the pool
            //con.setAutoCommit(false); // Start transaction, should not be used for read/select
            String query = "SELECT * FROM project WHERE userId = ? AND toolName = ? AND (projectType != 'batch_project' OR projectType IS NULL) AND (location = ? OR location = ? OR location = ? OR location = ?) ORDER BY id DESC";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            stmt.setString(2, toolName);
            stmt.setString(3, toolLocation);
            stmt.setString(4, "vip");
            stmt.setString(5, "pro");
            stmt.setString(6, "eu");
            rs = stmt.executeQuery();
            //System.out.println(stmt.toString());

            while (rs.next()) {
                HashMap<String, Object> row = new HashMap<>();
                ResultSetMetaData metaData = rs.getMetaData();
                int columnCount = metaData.getColumnCount();
                for (int i = 1; i <= columnCount; i++) {
                    Object val = rs.getObject(i);
                    if (val == null) {
                        val = "NA";
                    }
                    row.put(metaData.getColumnName(i), val);
                }

                projects.add(row);
            }
            //con.commit(); // Commit the transaction

        } catch (SQLException ex) {
            System.out.println("getProjectsFromPostgres SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback(); // Roll back in case of any failure
                }
            } catch (SQLException exRollback) {
                System.out.println("Rollback failed: " + exRollback.getMessage());
            }
        } finally {
            // Close all resources
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return projects; // Return the list of projects
    }

    public static int deleteProjectById(Long id) {
        int result = 0;
        Connection con = null;
        PreparedStatement stmt = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false);

            String query = "DELETE FROM project WHERE id = ?";
            System.out.println("DELETE PROJECT ID=" + id);
            stmt = con.prepareStatement(query);
            stmt.setLong(1, id);
            int rowsAffected = stmt.executeUpdate();

            con.commit();

            if (rowsAffected > 0) {
                result = 1; // Successfully deleted
            }

        } catch (SQLException ex) {
            System.out.println("deleteProjectById SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return result;
    }

    public static Map<String, Object> loadProject(String token) {
        Map<String, Object> projectData = new HashMap<>();
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false);

            String query = "SELECT * FROM project WHERE partialToken = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, token);
            rs = stmt.executeQuery();

            if (rs.next()) {
                ResultSetMetaData metaData = rs.getMetaData();
                int columnCount = metaData.getColumnCount();
                for (int i = 1; i <= columnCount; i++) {
                    projectData.put(metaData.getColumnName(i), rs.getObject(i));
                }
            } else {
                con.rollback();
                return null;
            }

            con.commit();

        } catch (SQLException ex) {
            System.out.println("loadProject SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return projectData;
    }

    public static String insertToken(String email, String resetToken, String expDate) {
        String result = "Insert unsuccessful."; // Default failure message
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet tables = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false);

            // Check if 'tokens' table exists, create if not
            DatabaseMetaData dbm = con.getMetaData();
            tables = dbm.getTables(null, null, "tokens", null);
            if (!tables.next()) {
                String createTableQuery = "CREATE TABLE tokens (usernm TEXT, tokens TEXT, expdate TEXT)";
                con.prepareStatement(createTableQuery).execute();
            }

            // Prepare and execute the INSERT query
            String insertQuery = "INSERT INTO tokens (usernm, tokens, expdate) VALUES (?, ?, ?)";
            stmt = con.prepareStatement(insertQuery);
            stmt.setString(1, email);
            stmt.setString(2, resetToken);
            stmt.setString(3, expDate);
            stmt.executeUpdate();

            con.commit();
            result = "Insert successful.";

        } catch (SQLException ex) {
            System.out.println("insertToken SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
            result = "Error encountered: " + ex.getMessage();
        } finally {
            try {
                if (tables != null) {
                    tables.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return result;
    }

    public static String checkUserExists(String email) {
        String result = "Error checking user existence."; // Default error message
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Prepare and execute the SELECT query
            String query = "SELECT COUNT(*) as count FROM users WHERE email = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            rs = stmt.executeQuery();

            if (rs.next()) {
                int count = rs.getInt("count");
                if (count > 0) {
                    result = "User exists."; // User exists message
                } else {
                    result = "User does not exist."; // User does not exist message
                }
            }

        } catch (SQLException ex) {
            System.out.println("checkUserExists SQLException occurred: " + ex.getMessage());
            result = "Error encountered: " + ex.getMessage(); // Return error message
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result; // Return the result message
    }

    public static String verifyToken(String token) {
        String result = "Error encountered: Token not found."; // Default error message for token not found
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Prepare and execute the SELECT query
            String query = "SELECT usernm, expdate FROM tokens WHERE tokens = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, token);
            rs = stmt.executeQuery();

            // Process the ResultSet
            if (rs.next()) {
                // Assuming you want to return the user name
                result = rs.getString("usernm");
            }

        } catch (SQLException ex) {
            System.out.println("verifyToken SQLException occurred: " + ex.getMessage());
            result = "Error encountered: " + ex.getMessage(); // Return error message
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result; // Return the userNM or error message
    }

    public static String deleteTokenForUser(String email) {
        String result = "Error encountered during deletion."; // Default error message
        Connection con = null;
        PreparedStatement stmt = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Prepare and execute the DELETE query
            String query = "DELETE FROM tokens WHERE userNM = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            int rowsAffected = stmt.executeUpdate();

            if (rowsAffected > 0) {
                result = "Success"; // Successful deletion message
            }

        } catch (SQLException ex) {
            System.out.println("deleteTokenForUser SQLException occurred: " + ex.getMessage());
            result = "Error encountered: " + ex.getMessage(); // Return error message
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result; // Return the result message
    }

    public String resetPassword(String email, String new_password) {
        System.out.println("email====" + email);
        String result = "Reset Error - Unknown Error"; // Default error message
        Connection con = null;
        PreparedStatement checkStmt = null;
        PreparedStatement updateStmt = null;
        ResultSet user_data = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false); // Start transaction
            // Hash the new password using SHA-256
            String hashedPassword = RCenter.hashPassword(sb, fb.getRscriptsDBPath(),
                    fb.getProjectDBPath(), new_password);

            // Check if the email exists
            String checkQuery = "SELECT * FROM users WHERE email = ?";
            checkStmt = con.prepareStatement(checkQuery);
            checkStmt.setString(1, email);
            user_data = checkStmt.executeQuery();

            if (!user_data.next()) {
                con.rollback(); // Roll back the transaction
                return "Error encountered: Email not found.";
            }

            // Update the password for the provided email
            String updateQuery = "UPDATE users SET password = ? WHERE email = ?";
            updateStmt = con.prepareStatement(updateQuery);
            updateStmt.setString(1, hashedPassword);
            updateStmt.setString(2, email);
            updateStmt.executeUpdate();

            con.commit(); // Commit the transaction
            result = "Success"; // Return success message

        } catch (SQLException ex) {
            System.out.println("resetPassword SQLException occurred: " + ex.getMessage());
            result = "Reset Error - " + ex.getMessage(); // Return error message
            if (con != null) {
                try {
                    con.rollback();
                } catch (SQLException e) {
                    System.out.println("Reset Password rollback failed: " + e.getMessage());
                }
            }
        } finally {
            try {
                if (user_data != null) {
                    user_data.close();
                }
                if (checkStmt != null) {
                    checkStmt.close();
                }
                if (updateStmt != null) {
                    updateStmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result; // Return the result message
    }

    public static String addActivationCode(String activateCode, String expDate, String email) {
        String result = "Error encountered during update."; // Default error message
        Connection con = null;
        PreparedStatement stmt = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Prepare and execute the UPDATE query
            String query = "UPDATE users SET activatecode = ?, expdate = ? WHERE email = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, activateCode);
            stmt.setString(2, expDate);
            stmt.setString(3, email);
            int rowsAffected = stmt.executeUpdate();

            if (rowsAffected > 0) {
                result = "Update successful."; // Successful update message
            }

        } catch (SQLException ex) {
            System.out.println("addActivationCode SQLException occurred: " + ex.getMessage());
            result = "Error encountered: " + ex.getMessage(); // Return error message
        } finally {
            try {
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result; // Return the result message
    }

    public static String checkActivationCode(String email, String activationCode) {
        String result = "Error encountered during activation."; // Default error message
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Prepare and execute the SELECT query
            String query = "SELECT email, activatecode, expdate FROM users WHERE email = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            rs = stmt.executeQuery();

            if (!rs.next()) {
                return "No user found with the provided email.";
            }

            String dbActivationCode = rs.getString("activatecode");
            String dbExpDate = rs.getString("expdate");

            // Check if the activation code matches
            if (!dbActivationCode.equals(activationCode)) {
                return "Wrong activation code.";
            }

            // Check if the activation code has expired
            LocalDateTime expDate = LocalDateTime.parse(dbExpDate, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            LocalDateTime currentDateTime = LocalDateTime.now();

            if (expDate.isBefore(currentDateTime)) {
                return "Activation code has expired.";
            }

            // If all checks pass, update the 'activated' column to 1
            String updateQuery = "UPDATE users SET activated = 1 WHERE email = ?";
            PreparedStatement updateStmt = con.prepareStatement(updateQuery);
            updateStmt.setString(1, email);
            updateStmt.executeUpdate();
            updateStmt.close(); // Close the update statement

            result = "Success!"; // Return success message

        } catch (SQLException ex) {
            System.out.println("checkActivationCode SQLException occurred: " + ex.getMessage());
            result = "Error encountered: " + ex.getMessage(); // Return error message
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result; // Return the result message
    }

    public static String deleteUserAndProjects(String userId) {
        String result = "Initialization"; // Default message
        Connection con = null;
        PreparedStatement projectStmt = null;
        PreparedStatement userStmt = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false); // Start transaction

            // Delete associated projects
            String deleteProjectsQuery = "DELETE FROM project WHERE userId = ?";
            projectStmt = con.prepareStatement(deleteProjectsQuery);
            projectStmt.setString(1, userId);
            projectStmt.executeUpdate();

            // Delete the user
            String deleteUserQuery = "DELETE FROM users WHERE email = ?";
            userStmt = con.prepareStatement(deleteUserQuery);
            userStmt.setString(1, userId);
            userStmt.executeUpdate();

            con.commit(); // Commit transaction
            result = "Successfully deleted user and associated projects.";

        } catch (SQLException ex) {
            System.out.println("deleteUserAndProjects SQLException occurred: " + ex.getMessage());
            result = "Error while deleting user and projects: " + ex.getMessage();
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
        } finally {
            try {
                if (projectStmt != null) {
                    projectStmt.close();
                }
                if (userStmt != null) {
                    userStmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result;
    }

    public static int checkMatchingFolderNameProject(String folderName) {
        int matchedId = -1; // Default id indicating no match found
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Query to check for a matching folderName and projectType
            String checkQuery = "SELECT id FROM project WHERE foldername = ? AND projectType != 'batch_project'";
            stmt = con.prepareStatement(checkQuery);
            stmt.setString(1, folderName);
            rs = stmt.executeQuery();

            if (rs.next()) {
                matchedId = rs.getInt("id"); // Get the id of the matched project
            }

        } catch (SQLException ex) {
            System.out.println("checkMatchingFolderNameProject SQLException occurred: " + ex.getMessage());
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return matchedId; // Return the matched id or -1 if no match found
    }

    public static int transferProject(int id, String newEmail, String suffix, String toolLocation) {
        int result = 0;
        Connection con = null;
        PreparedStatement fetchStmt = null;
        PreparedStatement insertStmt = null;
        ResultSet fetchedRow = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false);

            // Fetch the existing project data
            String fetchQuery = "SELECT * FROM project WHERE id = ?";
            fetchStmt = con.prepareStatement(fetchQuery);
            fetchStmt.setInt(1, id);
            fetchedRow = fetchStmt.executeQuery();

            if (fetchedRow.next()) {
                // Prepare the insert query without specifying an ID (assuming it's auto-incremented)
                String insertQuery = "INSERT INTO project (userId, name, description, module, moduleReadable, dataType, date, javaHistory, naviString, naviCode, org, partialToken, toolName, toolCode, projectType, paired, regression, location, folderName) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";

                insertStmt = con.prepareStatement(insertQuery);

                // Set parameters for the insert statement using values from the fetched row
                // Assume column indices are known and correspond to the table's structure
                insertStmt.setString(1, newEmail);
                insertStmt.setString(2, fetchedRow.getString("name"));
                insertStmt.setString(3, fetchedRow.getString("description"));
                insertStmt.setString(4, fetchedRow.getString("module"));
                insertStmt.setString(5, fetchedRow.getString("moduleReadable"));
                insertStmt.setString(6, fetchedRow.getString("dataType"));
                insertStmt.setString(7, fetchedRow.getString("date"));
                insertStmt.setString(8, fetchedRow.getString("javaHistory"));
                insertStmt.setString(9, fetchedRow.getString("naviString"));
                insertStmt.setString(10, fetchedRow.getString("naviCode"));
                insertStmt.setString(11, fetchedRow.getString("org"));
                insertStmt.setString(12, fetchedRow.getString("partialToken"));
                insertStmt.setString(13, fetchedRow.getString("toolName"));
                insertStmt.setString(14, fetchedRow.getString("toolCode"));
                insertStmt.setString(15, fetchedRow.getString("projectType"));
                insertStmt.setBoolean(16, fetchedRow.getBoolean("paired"));
                insertStmt.setBoolean(17, fetchedRow.getBoolean("regression"));
                insertStmt.setString(18, toolLocation);
                insertStmt.setString(19, fetchedRow.getString("folderName") + suffix);

                insertStmt.executeUpdate();
                con.commit();
                result = 1;
            } else {
                System.out.println("No project found with ID: " + id);
                result = -1;
            }

        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException exRollback) {
                System.out.println("Rollback failed: " + exRollback.getMessage());
            }
        } finally {
            try {
                if (fetchedRow != null) {
                    fetchedRow.close();
                }
                if (fetchStmt != null) {
                    fetchStmt.close();
                }
                if (insertStmt != null) {
                    insertStmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return result;
    }

    public static int detectDockerUserNum() {

        Connection con = null;
        PreparedStatement insertStmt = null;
        ResultSet rs;
        int userCount = 0;

        try {
            con = DatabaseConnectionPool.getConnection(); // Get a connection from the pool

            // Insert new user
            String countQuery = "SELECT count(*) FROM users;";
            insertStmt = con.prepareStatement(countQuery);
            rs = insertStmt.executeQuery();
            if (rs.next()) {
                userCount = rs.getInt("count"); // Get the id of the matched project
            }
        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
        } finally {
            // Close all resources
            try {
                if (insertStmt != null) {
                    insertStmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool, not really closing it
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return userCount;
    }

    // the following section is used for raw spectra jobs
    public static int recordRawJob(long JobID, String email, String Project_folder, String JobPos) {

        int result = 0;
        Date CurrentDate = new Date();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String CurDate = sdf.format(CurrentDate);

        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet tables = null;

        try {
            con = DatabaseConnectionPool.getDataSource().getConnection();
            con.setAutoCommit(false);

            // Check if 'tokens' table exists, create if not
            DatabaseMetaData dbm = con.getMetaData();
            tables = dbm.getTables(null, null, "rawjobs", null);
            if (!tables.next()) {
                String createTableQuery = "CREATE TABLE rawjobs (usernm VARCHAR(30) DEFAULT NULL,"
                        + "  jobid INTEGER DEFAULT NULL,"
                        + "  jobsubtime TEXT DEFAULT NULL,"
                        + "  jobdonetime TEXT DEFAULT NULL,"
                        + "  jobstatus VARCHAR(10) DEFAULT NULL,"
                        + "  jobfolder VARCHAR(80) DEFAULT NULL,"
                        + "  jobposition VARCHAR(5) DEFAULT NULL,"
                        + "  jobtype VARCHAR(5) DEFAULT NULL,"
                        + "  partiallink VARCHAR(100) DEFAULT NULL)";
                con.prepareStatement(createTableQuery).execute();
            }

            // Prepare and execute the INSERT query
            String insertQuery = "insert into rawjobs (usernm, jobid, jobsubtime, jobstatus, jobfolder, jobposition, jobtype) values (?, ?, ?, ?, ?, ?, ?)";
            stmt = con.prepareStatement(insertQuery);
            stmt.setString(1, email);
            stmt.setInt(2, (int) JobID);
            stmt.setString(3, CurDate);
            stmt.setString(4, "submitted");
            stmt.setString(5, Project_folder);
            stmt.setString(6, JobPos);
            stmt.setString(7, "raw_spec");
            stmt.executeUpdate();

            con.commit();
            result = 1;
            System.out.println("recordRawJob ===== done!");

        } catch (SQLException ex) {
            System.out.println("insertToken SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
            result = 0;
            System.out.println("recordRawJob ===== failed!");
        } finally {
            try {
                if (tables != null) {
                    tables.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result;
    }

    public static int updateRawJobStatus(long JobID, String currentJobStatus, String JobPos) {

        int result = 0;

        if (JobID > 0) {

            Connection con = null;
            PreparedStatement stmt = null;
            ResultSet tables = null;

            try {
                con = DatabaseConnectionPool.getDataSource().getConnection();
                con.setAutoCommit(false);

                // Prepare and execute the INSERT query "update devUsers.jobs set jobStatus = '"
                String updateQuery = "update rawjobs set jobStatus= ?  where jobID = ? AND jobPosition = ? ";
                stmt = con.prepareStatement(updateQuery);
                stmt.setString(1, currentJobStatus);
                stmt.setInt(2, (int) JobID);
                stmt.setString(3, JobPos);
                stmt.executeUpdate();

                con.commit();
                result = 1;
                System.out.println("updateRawJobStatus ===== done!");

            } catch (SQLException ex) {
                System.out.println("insertToken SQLException occurred: " + ex.getMessage());
                try {
                    if (con != null) {
                        con.rollback();
                    }
                } catch (SQLException e) {
                    System.out.println("Rollback failed: " + e.getMessage());
                }
                result = 0;
                System.out.println("updateRawJobStatus ===== failed!");
            } finally {
                try {
                    if (tables != null) {
                        tables.close();
                    }
                    if (stmt != null) {
                        stmt.close();
                    }
                    if (con != null) {
                        con.close();
                    }
                } catch (SQLException ex) {
                    System.out.println("Error when closing resources: " + ex.getMessage());
                }
            }
        }
        return result;
    }

    public static int extractRawJobStatus(String folder, String userid) {

        int result = 0;

        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs;
        ResultSet tables = null;
//        
        try {
            con = DatabaseConnectionPool.getDataSource().getConnection();
            con.setAutoCommit(false);

            // Check if 'tokens' table exists, create if not
            DatabaseMetaData dbm = con.getMetaData();
            tables = dbm.getTables(null, null, "rawjobs", null);

            // Prepare and execute the query
            String query = "SELECT jobid FROM rawJobs WHERE usernm = ? AND jobfolder = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, userid);
            stmt.setString(2, folder);
            rs = stmt.executeQuery();
            if (rs.next()) {
                int jobid = rs.getInt(1);
                return jobid;
            }

        } catch (SQLException ex) {
            System.out.println("extractRawJobStatus SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
            result = 0;
            System.out.println("extractRawJobStatus ===== failed!");
        } finally {
            try {
                if (tables != null) {
                    tables.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return result;
    }

    public static ArrayList<HashMap<String, Object>> getAllWorkflows(String toolName, String email) {
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet res = null;
        ArrayList<HashMap<String, Object>> workflows = new ArrayList<>();

        try {
            // Get a connection from the connection pool
            con = DatabaseConnectionPool.getDataSource().getConnection();

            // Updated query to retrieve all required fields for a workflow filtered by email and tool
            String query = "SELECT id, email, name, description, module, tool, filename, location, input, analysisGoal, analysisMethods, output, other "
                    + "FROM workflow WHERE email = ? AND tool = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            stmt.setString(2, toolName);

            // Execute the query
            res = stmt.executeQuery();

            // Iterate over the result set and populate the list of HashMaps
            while (res.next()) {
                HashMap<String, Object> workflowData = new HashMap<>();

                // Add each column's data to the HashMap
                workflowData.put("id", res.getInt("id"));
                workflowData.put("email", res.getString("email"));
                workflowData.put("name", res.getString("name"));
                workflowData.put("description", res.getString("description"));
                workflowData.put("module", res.getString("module"));
                workflowData.put("tool", res.getString("tool"));
                workflowData.put("filename", res.getString("filename"));
                workflowData.put("location", res.getString("location"));
                workflowData.put("input", res.getString("input"));
                workflowData.put("analysisGoal", res.getString("analysisGoal"));
                workflowData.put("analysisMethods", res.getString("analysisMethods"));
                workflowData.put("output", res.getString("output"));
                workflowData.put("other", res.getString("other"));

                // Add the HashMap to the ArrayList
                workflows.add(workflowData);
            }

            // Return the list of workflows as HashMap
            return workflows;

        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            return workflows;  // Return the empty list if an exception occurs
        } finally {
            // Close all resources in the finally block
            try {
                if (res != null) {
                    res.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
    }

    public static String insertWorkflow(String email, String name, String description, String module, String toolName, String filename, String location, String input, String analysisGoal, String analysisMethods, String output, String other) {
        Connection con = null;
        PreparedStatement insertStmt = null;

        try {
            // Get a connection from the connection pool
            con = DatabaseConnectionPool.getDataSource().getConnection();

            // Insert new workflow
            String insertQuery = "INSERT INTO workflow (email, name, description, module, tool, filename, location, input, analysisGoal, analysisMethods, output, other) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
            insertStmt = con.prepareStatement(insertQuery);

            insertStmt.setString(1, email);
            insertStmt.setString(2, name);
            insertStmt.setString(3, description);
            insertStmt.setString(4, module);
            insertStmt.setString(5, toolName);
            insertStmt.setString(6, filename);
            insertStmt.setString(7, location);
            insertStmt.setString(8, input);
            insertStmt.setString(9, analysisGoal);
            insertStmt.setString(10, analysisMethods);
            insertStmt.setString(11, output);
            insertStmt.setString(12, other);

            // Execute the insert statement
            int updateCount = insertStmt.executeUpdate();

            // Return success message if insert was successful
            return updateCount > 0 ? "Workflow entry inserted successfully." : "Workflow insertion failed.";

        } catch (SQLException ex) {
            // Handle any SQL exceptions with logger
            System.err.println("SQLException occurred: " + ex.getMessage()); // Use a logger if available
            return "Error inserting workflow entry - " + ex.getMessage();
        } finally {
            // Close all resources in the finally block
            try {
                if (insertStmt != null) {
                    insertStmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool
                }
            } catch (SQLException ex) {
                System.err.println("Error when closing resources: " + ex.getMessage()); // Use a logger if available
            }
        }
    }

    public static Map<String, Object> loadProjectById(String id) {
        Map<String, Object> projectData = new HashMap<>();
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false);

            String query = "SELECT * FROM project WHERE id = ?";
            stmt = con.prepareStatement(query);
            stmt.setInt(1, Integer.parseInt(id));
            rs = stmt.executeQuery();

            if (rs.next()) {
                ResultSetMetaData metaData = rs.getMetaData();
                int columnCount = metaData.getColumnCount();
                for (int i = 1; i <= columnCount; i++) {
                    projectData.put((String) metaData.getColumnName(i), rs.getObject(i));
                }
            } else {
                con.rollback();
                return null;
            }

            con.commit();

        } catch (SQLException ex) {
            System.out.println("loadProject SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException e) {
                System.out.println("Rollback failed: " + e.getMessage());
            }
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
        return projectData;
    }

    public static Map<String, Object> obtainFolderNameProject(String folderName) {
        Map<String, Object> projectDetails = new HashMap<>(); // To store all column values of the matched project
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection();

            // Query to get all columns for the matching folderName and projectType
            String query = "SELECT * FROM project WHERE folderName = ?";
            stmt = con.prepareStatement(query);
            stmt.setString(1, folderName);
            rs = stmt.executeQuery();

            if (rs.next()) {
                // Get metadata of the ResultSet to retrieve column names
                ResultSetMetaData metaData = rs.getMetaData();
                int columnCount = metaData.getColumnCount();

                // Loop through all columns and store their names and values in the map
                for (int i = 1; i <= columnCount; i++) {
                    String columnName = metaData.getColumnName(i);
                    Object columnValue = rs.getObject(i);
                    projectDetails.put(columnName, columnValue);
                }
            }

        } catch (SQLException ex) {
            System.out.println("getFolderNameProject SQLException occurred: " + ex.getMessage());
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (stmt != null) {
                    stmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }

        return projectDetails; // Return all column values as a map or an empty map if no match is found
    }

    public static String insertDataset(String email,
            String node,
            String title,
            String module,
            String dataType,
            String filename,
            String type,
            long sizeBytes,
            int sampleNum) {
        // Wrap into a single-file call
        DatasetFile primary = new DatasetFile();
        primary.setRole("data");
        primary.setFilename(filename);
        primary.setType(type);
        primary.setSizeBytes(sizeBytes);
        primary.setUploadedAt(null); // let DB default to now()

        return insertDatasetWithFiles(email, node, title, module, dataType, "metaboanalyst", sampleNum, null, List.of(primary));
    }

    /**
     * New: insert a dataset with one or more physical files (data + metadata +
     * etc.).
     */
    public static String insertDatasetWithFiles(String email,
            String node,
            String title,
            String module, // unchanged
            String dataType, // unchanged
            String toolName,
            int sampleNum,
            OffsetDateTime uploadedAt,
            List<DatasetFile> files) {

        final String dsSql
                = "INSERT INTO datasets "
                + "(title, filename, type, size_bytes, uploaded_at, email, node, samplenum, toolname, module, data_type) "
                + // <-- toolname added
                "VALUES (?, ?, ?, ?, COALESCE(?, now()), ?, ?, ?, ?, ?, ?) "
                + // <-- +1 placeholder
                "RETURNING id";

        final String fileSql
                = "INSERT INTO dataset_files (dataset_id, role, filename, type, size_bytes, uploaded_at) "
                + "VALUES (?, ?, ?, ?, ?, COALESCE(?, now()))";

        // 0) Basic guards
        if (files == null || files.isEmpty()) {
            return "Error inserting dataset - no files supplied.";
        }
        DatasetFile primary = files.get(0);
        if (primary.getRole() == null || primary.getFilename() == null || primary.getType() == null) {
            return "Error inserting dataset - primary file must have role/filename/type.";
        }

        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection()) {
            con.setAutoCommit(false);
            UUID datasetId;

            // 1) Insert dataset (legacy columns from first file) + new columns
            try (PreparedStatement ps = con.prepareStatement(dsSql)) {
                ps.setString(1, nvl(title, primary.getFilename()));          // title fallback = filename
                ps.setString(2, primary.getFilename());
                ps.setString(3, nvl(primary.getType(), "bin"));
                ps.setLong(4, Math.max(0L, primary.getSizeBytes()));
                ps.setObject(5, uploadedAt);                                  // null -> COALESCE(now())
                ps.setString(6, email);
                ps.setString(7, node);
                ps.setInt(8, Math.max(0, sampleNum));
                ps.setString(9, toolName);                                    // <-- NEW bind for toolname
                ps.setString(10, module);
                ps.setString(11, dataType);

                try (ResultSet rs = ps.executeQuery()) {
                    if (!rs.next()) {
                        con.rollback();
                        return "Dataset insertion failed.";
                    }
                    Object obj = rs.getObject(1);
                    datasetId = (obj instanceof UUID) ? (UUID) obj : UUID.fromString(String.valueOf(obj));
                }
            }

            // 2) Insert all files exactly as provided
            try (PreparedStatement fps = con.prepareStatement(fileSql)) {
                for (DatasetFile f : files) {
                    if (f.getRole() == null || f.getFilename() == null || f.getType() == null) {
                        con.rollback();
                        return "Error inserting dataset - each file must have role/filename/type.";
                    }
                    fps.setObject(1, datasetId);
                    fps.setString(2, f.getRole());
                    fps.setString(3, f.getFilename());
                    fps.setString(4, nvl(f.getType(), "bin"));
                    fps.setLong(5, Math.max(0L, f.getSizeBytes()));
                    fps.setObject(6, f.getUploadedAt());                      // null -> COALESCE(now())
                    fps.addBatch();
                }
                fps.executeBatch();
            }

            con.commit();
            return "Dataset inserted successfully. id=" + datasetId + " files=" + files.size();

        } catch (SQLException ex) {
            String sqlState = ex.getSQLState();
            if ("23514".equals(sqlState)) {
                return "Error inserting dataset - invalid value violates a check constraint (e.g., role). Detail: " + ex.getMessage();
            }
            if ("23503".equals(sqlState)) {
                return "Error inserting dataset - foreign key issue: " + ex.getMessage();
            }
            System.err.println("SQLException in insertDatasetWithFiles: " + ex.getMessage());
            return "Error inserting dataset - " + ex.getMessage();
        }
    }

    private static String nvl(String s, String def) {
        return (s == null || s.trim().isEmpty()) ? def : s;
    }

    public static String updateDatasetAndFiles(
            UUID datasetId,
            String title,
            String toolName,
            String module,
            String dataType,
            int sampleNum,
            OffsetDateTime uploadedAt,
            List<DatasetFile> files
    ) {
        final String dsUpd = """
        UPDATE datasets
        SET title = ?, toolname = ?, module = ?, data_type = ?, samplenum = ?, uploaded_at = COALESCE(?, uploaded_at)
        WHERE id = ?
    """;

        final String fileUpsert = """
        INSERT INTO dataset_files (dataset_id, role, filename, type, size_bytes, uploaded_at)
        VALUES (?, ?, ?, ?, ?, COALESCE(?, now()))
        ON CONFLICT (dataset_id, role, filename)
        DO UPDATE SET
            type = EXCLUDED.type,
            size_bytes = EXCLUDED.size_bytes,
            uploaded_at = COALESCE(EXCLUDED.uploadedAt, dataset_files.uploaded_at)
    """;

        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection()) {
            con.setAutoCommit(false);

            try (PreparedStatement ps = con.prepareStatement(dsUpd)) {
                ps.setString(1, title);
                ps.setString(2, toolName);
                ps.setString(3, module);
                ps.setString(4, dataType);
                ps.setInt(5, Math.max(0, sampleNum));
                ps.setObject(6, uploadedAt);
                ps.setObject(7, datasetId);
                if (ps.executeUpdate() == 0) {
                    con.rollback();
                    return "No dataset updated (id not found).";
                }
            }

            try (PreparedStatement fps = con.prepareStatement(fileUpsert)) {
                for (DatasetFile f : files) {
                    if (f.getRole() == null || f.getFilename() == null || f.getType() == null) {
                        con.rollback();
                        return "Each file must have role/filename/type.";
                    }
                    fps.setObject(1, datasetId);
                    fps.setString(2, f.getRole());
                    fps.setString(3, f.getFilename());
                    fps.setString(4, f.getType());
                    fps.setLong(5, Math.max(0L, f.getSizeBytes()));
                    fps.setObject(6, f.getUploadedAt());
                    fps.addBatch();
                }
                fps.executeBatch();
            }

            con.commit();
            return "Dataset updated successfully. id=" + datasetId + " filesUpserted=" + files.size();
        } catch (SQLException ex) {
            return "Error updating dataset - " + ex.getMessage();
        }
    }

    public static ArrayList<DatasetRow> getDatasetsForEmail(String email, String toolname) {
        final ArrayList<DatasetRow> out = new ArrayList<>();
        if (email == null || email.isBlank() || toolname == null || toolname.isBlank()) {
            return out; // require both email and toolname
        }

        final String sql
                = "SELECT d.id, d.title, d.filename, d.type, d.size_bytes, d.uploaded_at, "
                + "       d.email, d.node, d.samplenum, d.toolname, d.module, d.data_type, "
                + "       COALESCE(df.file_count, 0) AS file_count, "
                + "       COALESCE(df.has_metadata, false) AS has_metadata "
                + "FROM datasets d "
                + "LEFT JOIN ( "
                + "  SELECT dataset_id, COUNT(*) AS file_count, BOOL_OR(role = 'metadata') AS has_metadata "
                + "  FROM dataset_files GROUP BY dataset_id "
                + ") df ON df.dataset_id = d.id "
                + "WHERE d.email = ? AND d.toolname = ? "
                + // <-- filter by toolname
                "ORDER BY d.uploaded_at DESC, d.node";

        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection(); PreparedStatement ps = con.prepareStatement(sql)) {

            ps.setString(1, email);
            ps.setString(2, toolname);

            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    DatasetRow d = new DatasetRow();
                    Object idObj = rs.getObject("id");
                    if (idObj instanceof java.util.UUID) {
                        d.setId((java.util.UUID) idObj);
                    } else if (idObj != null) {
                        d.setId(java.util.UUID.fromString(String.valueOf(idObj)));
                    }

                    d.setTitle(rs.getString("title"));
                    d.setFilename(rs.getString("filename"));
                    d.setType(rs.getString("type"));
                    d.setSizeBytes(rs.getLong("size_bytes"));

                    try {
                        d.setUploadedAt(rs.getObject("uploaded_at", java.time.OffsetDateTime.class));
                    } catch (Exception ignore) {
                        if (rs.getTimestamp("uploaded_at") != null) {
                            d.setUploadedAt(
                                    rs.getTimestamp("uploaded_at").toInstant()
                                            .atOffset(java.time.OffsetDateTime.now().getOffset())
                            );
                        }
                    }

                    d.setEmail(rs.getString("email"));
                    d.setNode(rs.getString("node"));
                    d.setSamplenum(rs.getInt("samplenum"));
                    // Use whichever setter your POJO provides:
                    // d.setToolname(rs.getString("toolname"));
                    d.setToolName(rs.getString("toolname"));
                    d.setModule(rs.getString("module"));
                    d.setDataType(rs.getString("data_type"));
                    d.setFileCount(rs.getInt("file_count"));
                    d.setHasMetadata(rs.getBoolean("has_metadata"));
                    out.add(d);
                }
            }
        } catch (SQLException ex) {
            System.err.println("getDatasetsForEmail SQL error: " + ex.getMessage());
        }
        return out;
    }

    public static List<DatasetFile> getDatasetFiles(java.util.UUID datasetId) {
        final String sql
                = "SELECT id, dataset_id, role, filename, type, size_bytes, uploaded_at "
                + "FROM dataset_files WHERE dataset_id = ? ORDER BY role, filename";

        List<DatasetFile> files = new ArrayList<>();
        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection(); PreparedStatement ps = con.prepareStatement(sql)) {

            ps.setObject(1, datasetId);
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    DatasetFile f = new DatasetFile();

                    // id: BIGSERIAL -> Long
                    Long fid = rs.getObject("id", Long.class); // PGJDBC 42.2+ supports this
                    if (fid == null) {
                        long v = rs.getLong("id");
                        fid = rs.wasNull() ? null : v;
                    }
                    f.setId(fid);

                    // dataset_id: UUID
                    java.util.UUID did = null;
                    try {
                        did = rs.getObject("dataset_id", java.util.UUID.class);
                    } catch (Exception ignore) {
                        Object obj = rs.getObject("dataset_id");
                        if (obj != null) {
                            did = java.util.UUID.fromString(String.valueOf(obj));
                        }
                    }
                    f.setDatasetId(did);

                    f.setRole(rs.getString("role"));
                    f.setFilename(rs.getString("filename"));
                    f.setType(rs.getString("type"));
                    f.setSizeBytes(rs.getLong("size_bytes"));
                    try {
                        f.setUploadedAt(rs.getObject("uploaded_at", java.time.OffsetDateTime.class));
                    } catch (Exception ignore) {
                        /* optional fallback */ }

                    files.add(f);
                }
            }
        } catch (SQLException ex) {
            System.err.println("getDatasetFiles SQL error: " + ex.getMessage());
        }
        return files;
    }

    public static String deleteDatasetById(java.util.UUID datasetId) {
        final String delFiles = "DELETE FROM dataset_files WHERE dataset_id = ?";
        final String delDataset = "DELETE FROM datasets WHERE id = ?";

        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection()) {
            con.setAutoCommit(false);
            try (PreparedStatement ps1 = con.prepareStatement(delFiles); PreparedStatement ps2 = con.prepareStatement(delDataset)) {

                // bind UUID directly
                ps1.setObject(1, datasetId);
                ps1.executeUpdate();

                ps2.setObject(1, datasetId);
                int n = ps2.executeUpdate();

                if (n == 0) {
                    con.rollback();
                    return "No dataset found for id=" + datasetId;
                }

                con.commit();
                return "OK";
            } catch (Exception e) {
                con.rollback();
                return "Error deleting dataset: " + e.getMessage();
            } finally {
                con.setAutoCommit(true);
            }
        } catch (Exception ex) {
            return "Error (connection): " + ex.getMessage();
        }
    }

    public static String deleteWorkflow(String id) {
        Connection con = null;
        PreparedStatement checkStmt = null;
        PreparedStatement deleteStmt = null;
        ResultSet res = null;

        try {
            // Get a connection from the connection pool
            con = DatabaseConnectionPool.getDataSource().getConnection();

            // Check if a workflow entry with the provided id exists
            String checkQuery = "SELECT id FROM workflow WHERE id = ?";
            checkStmt = con.prepareStatement(checkQuery);
            checkStmt.setInt(1, Integer.parseInt(id));
            res = checkStmt.executeQuery();

            if (!res.next()) {
                // If no workflow with the id exists, return an error message
                return "No workflow entry found with the provided id.";
            }

            // Delete the workflow entry
            String deleteQuery = "DELETE FROM workflow WHERE id = ?";
            deleteStmt = con.prepareStatement(deleteQuery);
            deleteStmt.setInt(1, Integer.parseInt(id));

            // Execute the delete statement
            int deleteCount = deleteStmt.executeUpdate();

            // Return success message if the deletion was successful
            return deleteCount > 0 ? "Workflow entry deleted successfully." : "Workflow deletion failed.";

        } catch (SQLException ex) {
            // Handle any SQL exceptions
            System.out.println("SQLException occurred: " + ex.getMessage());
            return "Error deleting workflow entry - " + ex.getMessage();
        } finally {
            // Close all resources in the finally block
            try {
                if (res != null) {
                    res.close();
                }
                if (checkStmt != null) {
                    checkStmt.close();
                }
                if (deleteStmt != null) {
                    deleteStmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
    }

    public static ArrayList<HashMap<String, Object>> getAllWorkflowRuns(String tool, String email)
            throws SQLException {
        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection()) {
            // delegate; NOTE: do not close 'con' in the delegate
            return getAllWorkflowRuns(con, tool, email);
        }
    }

    public static ArrayList<HashMap<String, Object>> getAllWorkflowRuns(Connection con, String tool, String email)
            throws SQLException {

        final String base
                = "SELECT id, module, name, description, status, start_date, finish_date, "
                + "       workflow_id, dataset_id, dataset_name, other, last_updated, email, tool "
                + // NEW cols
                "  FROM workflow_runs ";

        String where = "";
        if (tool != null && !tool.isBlank()) {
            where += (where.isEmpty() ? "WHERE " : " AND ") + "tool = ?";
        }
        if (email != null && !email.isBlank()) {
            where += (where.isEmpty() ? "WHERE " : " AND ") + "email = ?";
        }

        final String sql = base + where + " ORDER BY id DESC";

        try (PreparedStatement ps = con.prepareStatement(sql)) {
            int i = 1;
            if (tool != null && !tool.isBlank()) {
                ps.setString(i++, tool);
            }
            if (email != null && !email.isBlank()) {
                ps.setString(i++, email);
            }

            ArrayList<HashMap<String, Object>> out = new ArrayList<>();
            try (ResultSet rs = ps.executeQuery()) {
                while (rs.next()) {
                    HashMap<String, Object> m = new HashMap<>();
                    m.put("id", rs.getInt("id"));
                    m.put("module", rs.getString("module"));
                    m.put("name", rs.getString("name"));
                    m.put("description", rs.getString("description"));
                    m.put("status", rs.getString("status"));
                    m.put("startDate", rs.getString("start_date"));
                    m.put("finishDate", rs.getString("finish_date"));
                    m.put("workflowId", rs.getString("workflow_id"));
                    m.put("datasetId", rs.getObject("dataset_id") == null ? null : rs.getInt("dataset_id"));
                    m.put("datasetName", rs.getString("dataset_name"));
                    m.put("other", rs.getString("other"));
                    m.put("lastUpdated", rs.getString("last_updated"));
                    m.put("email", rs.getString("email"));   // NEW
                    m.put("tool", rs.getString("tool"));     // NEW
                    out.add(m);
                }
            }
            return out;
        }
    }

    public static String deleteWorkflowRun(String id) {
        Connection con = null;
        PreparedStatement checkStmt = null;
        PreparedStatement deleteStmt = null;
        ResultSet res = null;

        try {
            // Parse ID safely
            final int runId = Integer.parseInt(id);

            // Get a connection from the connection pool
            con = DatabaseConnectionPool.getDataSource().getConnection();

            // Check if a workflow_run entry with the provided id exists
            String checkQuery = "SELECT id FROM workflow_runs WHERE id = ?";
            checkStmt = con.prepareStatement(checkQuery);
            checkStmt.setInt(1, runId);
            res = checkStmt.executeQuery();

            if (!res.next()) {
                // If no workflow_run with the id exists, return an error message
                return "No workflow run entry found with the provided id.";
            }

            // Delete the workflow_run entry
            String deleteQuery = "DELETE FROM workflow_runs WHERE id = ?";
            deleteStmt = con.prepareStatement(deleteQuery);
            deleteStmt.setInt(1, runId);

            // Execute the delete statement
            int deleteCount = deleteStmt.executeUpdate();

            // Return success message if the deletion was successful
            return deleteCount > 0 ? "Workflow run entry deleted successfully." : "Workflow run deletion failed.";

        } catch (NumberFormatException nfe) {
            return "Invalid id format (must be an integer).";
        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            return "Error deleting workflow run entry - " + ex.getMessage();
        } finally {
            // Close all resources in the finally block
            try {
                if (res != null) {
                    res.close();
                }
                if (checkStmt != null) {
                    checkStmt.close();
                }
                if (deleteStmt != null) {
                    deleteStmt.close();
                }
                if (con != null) {
                    con.close(); // Return connection back to the pool
                }
            } catch (SQLException ex) {
                System.out.println("Error when closing resources: " + ex.getMessage());
            }
        }
    }

    public static int insertWorkflowRunReturningId(
            Connection con,
            String email, String module, String name, String description, String status,
            String startDateIso, String finishDateIso,
            String workflowId, UUID datasetId, String datasetName, // <-- UUID here
            String otherJson, String tool
    ) throws SQLException {

        final String sql
                = "INSERT INTO workflow_runs ("
                + "  module, name, description, status,"
                + "  start_date, finish_date,"
                + // TIMESTAMPTZ
                "  workflow_id, dataset_id, dataset_name,"
                + "  other, last_updated, email, tool"
                + ") VALUES ("
                + "  ?, ?, ?, ?,"
                + "  ?, ?,"
                + // bind TIMESTAMPTZ via setObject
                "  ?, ?, ?, "
                + "  ?, now(), ?, ?"
                + ") RETURNING id";

        try (PreparedStatement ps = con.prepareStatement(sql)) {
            int i = 1;

            // core + status
            ps.setString(i++, module);
            setOrNull(ps, i++, name);
            setOrNull(ps, i++, description);
            ps.setString(i++, (status == null ? "pending" : status));

            // start_date / finish_date (TIMESTAMPTZ)
            setTimestamptz(ps, i++, startDateIso);
            setTimestamptz(ps, i++, finishDateIso);

            // workflow_id (text)
            setOrNull(ps, i++, workflowId);

            // dataset_id (UUID)
            if (datasetId == null) {
                ps.setNull(i++, Types.OTHER);          // PostgreSQL UUID is OTHER via JDBC
            } else {
                ps.setObject(i++, datasetId, Types.OTHER);
            }

            // dataset_name
            setOrNull(ps, i++, datasetName);

            // other (jsonb)
            setJsonb(ps, i++, otherJson);

            // email, tool
            setOrNull(ps, i++, email);
            setOrNull(ps, i++, tool);

            try (ResultSet rs = ps.executeQuery()) {
                if (rs.next()) {
                    return rs.getInt(1);
                }
                throw new SQLException("INSERT returned no id");
            }
        }
    }

    private static void setTimestamptz(PreparedStatement ps, int idx, String iso) throws SQLException {
        if (iso == null || iso.isBlank()) {
            ps.setNull(idx, Types.TIMESTAMP_WITH_TIMEZONE);
            return;
        }
        try {
            OffsetDateTime odt = OffsetDateTime.parse(iso);
            ps.setObject(idx, odt); // JDBC will send as TIMESTAMPTZ
        } catch (DateTimeParseException e) {
            ps.setNull(idx, Types.TIMESTAMP_WITH_TIMEZONE);
        }
    }

    public static int insertWorkflowRun(String email,
            String module,
            String name,
            String description,
            String status, // pending/running/completed/failed
            String startDateIso, // nullable ISO-8601
            String finishDateIso, // nullable ISO-8601
            String workflowId, // template/json identifier
            UUID datasetId, // <-- UUID now
            String datasetName, // nullable
            String other, // JSON string or plain text
            String tool) {          // tool name (MetaboAnalyst, etc.)
        Connection con = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            con = DatabaseConnectionPool.getConnection();
            con.setAutoCommit(false);

            final String sql
                    = "INSERT INTO workflow_runs ("
                    + "  email, module, name, description, status,"
                    + "  start_date, finish_date, workflow_id,"
                    + "  dataset_id, dataset_name, other, tool, last_updated"
                    + ") VALUES ("
                    + "  ?,     ?,      ?,    ?,          ?,"
                    + "  ?,          ?,           ?,"
                    + "  ?,         ?,           ?,    ?,   now()"
                    + ") RETURNING id";

            ps = con.prepareStatement(sql);

            // 1) basics
            ps.setString(1, email);
            ps.setString(2, module);
            ps.setString(3, name == null ? "" : name);
            ps.setString(4, description == null ? "" : description);
            ps.setString(5, status == null ? "pending" : status);

            // 2) timestamptz (accept ISO-8601 or null)
            //    Column types should be TIMESTAMPTZ; the driver accepts OffsetDateTime
            if (startDateIso != null && !startDateIso.isBlank()) {
                try {
                    OffsetDateTime odt = OffsetDateTime.parse(startDateIso);
                    ps.setObject(6, odt);
                } catch (DateTimeParseException e) {
                    ps.setNull(6, Types.TIMESTAMP_WITH_TIMEZONE);
                }
            } else {
                ps.setNull(6, Types.TIMESTAMP_WITH_TIMEZONE);
            }

            if (finishDateIso != null && !finishDateIso.isBlank()) {
                try {
                    OffsetDateTime odt = OffsetDateTime.parse(finishDateIso);
                    ps.setObject(7, odt);
                } catch (DateTimeParseException e) {
                    ps.setNull(7, Types.TIMESTAMP_WITH_TIMEZONE);
                }
            } else {
                ps.setNull(7, Types.TIMESTAMP_WITH_TIMEZONE);
            }

            // 3) workflow id (store as text; adjust to setInt if your column is integer)
            if (workflowId != null && !workflowId.isBlank()) {
                ps.setString(8, workflowId);
            } else {
                ps.setNull(8, Types.VARCHAR);
            }

            // 4) dataset_id UUID
            if (datasetId != null) {
                ps.setObject(9, datasetId, Types.OTHER); // postgres uuid
            } else {
                ps.setNull(9, Types.OTHER);
            }

            // 5) dataset_name
            if (datasetName != null) {
                ps.setString(10, datasetName);
            } else {
                ps.setNull(10, Types.VARCHAR);
            }

            // 6) other -> jsonb
            if (other != null) {
                PGobject json = new PGobject();
                json.setType("jsonb");
                json.setValue(other); // should be valid JSON if you want jsonb semantics
                ps.setObject(11, json);
            } else {
                ps.setNull(11, Types.OTHER);
            }

            // 7) tool
            ps.setString(12, tool == null ? "" : tool);

            rs = ps.executeQuery();
            con.commit();

            if (rs.next()) {
                return rs.getInt(1); // return inserted id
            }
            return 1; // fallback success without id

        } catch (SQLException ex) {
            System.out.println("insertWorkflowRun SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException ignore) {
            }
            return -1;
        } finally {
            try {
                if (rs != null) {
                    rs.close();
                }
                if (ps != null) {
                    ps.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ignore) {
            }
        }
    }
// Overwrite status, touch last_updated = NOW(), and conditionally set start/finish.

    public static String updateWorkflowRunStatus(int id, String newStatus) {
        return updateWorkflowRunStatus(id, newStatus, (String) null);
    }

    public static String updateWorkflowRunStatus(int id, String newStatus, String projectId) {
        Connection con = null;
        PreparedStatement checkStmt = null;
        PreparedStatement updateStmt = null;
        ResultSet res = null;

        try {
            con = DatabaseConnectionPool.getDataSource().getConnection();

            // Ensure row exists
            final String checkSql = "SELECT id FROM workflow_runs WHERE id = ?";
            checkStmt = con.prepareStatement(checkSql);
            checkStmt.setInt(1, id);
            res = checkStmt.executeQuery();
            if (!res.next()) {
                return "No workflow run entry found with the provided id.";
            }
            res.close();
            checkStmt.close();

            final String status = (newStatus == null) ? "" : newStatus.trim().toLowerCase();

            Integer projectIdInt = null;
            if (projectId != null) {
                final String s = projectId.trim();
                if (!s.isEmpty()) {
                    try {
                        projectIdInt = Integer.valueOf(s);
                    } catch (NumberFormatException nfe) {
                        // If it's not numeric, treat as "no change"
                        System.err.println("updateWorkflowRunStatus: ignoring non-numeric projectId '" + projectId + "'");
                        projectIdInt = null;
                    }
                }
            }

            final String updateSql
                    = "UPDATE workflow_runs "
                    + "SET status = ?, "
                    + "    project_id = COALESCE(?, project_id), "
                    + "    last_updated = NOW(), "
                    + "    start_date  = CASE WHEN ? = 'running' AND start_date IS NULL THEN NOW() ELSE start_date END, "
                    + "    finish_date = CASE WHEN ? IN ('completed','failed') THEN NOW() ELSE finish_date END "
                    + "WHERE id = ?";

            updateStmt = con.prepareStatement(updateSql);
            int idx = 1;
            updateStmt.setString(idx++, status);                    // status
            if (projectIdInt == null) {
                updateStmt.setNull(idx++, java.sql.Types.INTEGER);  // keep project_id unchanged via COALESCE
            } else {
                updateStmt.setInt(idx++, projectIdInt);             // set project_id
            }
            updateStmt.setString(idx++, status);                    // CASE for start_date
            updateStmt.setString(idx++, status);                    // CASE for finish_date
            updateStmt.setInt(idx++, id);                           // WHERE id

            int updated = updateStmt.executeUpdate();
            return (updated > 0) ? "Workflow run updated successfully."
                    : "Workflow run update failed.";

        } catch (SQLException ex) {
            System.err.println("SQLException occurred: " + ex.getMessage());
            return "Error updating workflow run - " + ex.getMessage();
        } finally {
            try {
                if (res != null) {
                    res.close();
                }
                if (checkStmt != null) {
                    checkStmt.close();
                }
                if (updateStmt != null) {
                    updateStmt.close();
                }
                if (con != null) {
                    con.close();
                }
            } catch (SQLException ex) {
                System.err.println("Error when closing resources: " + ex.getMessage());
            }
        }
    }

    private static void setOrNull(PreparedStatement ps, int idx, String v) throws SQLException {
        if (v == null || v.isBlank()) {
            ps.setNull(idx, java.sql.Types.VARCHAR);
        } else {
            ps.setString(idx, v);
        }
    }

    private static void setJsonb(PreparedStatement ps, int idx, String val) throws SQLException {
        if (val == null || val.isBlank() || "null".equalsIgnoreCase(val.trim())) {
            ps.setNull(idx, java.sql.Types.OTHER);
            return;
        }
        String payload = val.trim();

        // If it's valid JSON, pass through; otherwise, wrap as JSON string
        boolean isJson = false;
        try {
            JsonParser.parseString(payload);
            isJson = true;
        } catch (Exception ignore) {
        }

        if (!isJson) {
            payload = new Gson().toJson(val); // turns plain text into a JSON string
        }

        PGobject jsonb = new PGobject();
        jsonb.setType("jsonb");
        jsonb.setValue(payload);
        ps.setObject(idx, jsonb);
    }

    public static String updateWorkflowRunFlexible(int id, Map<String, Object> fields) {
        if (fields == null || fields.isEmpty()) {
            return "No fields to update.";
        }

        // Whitelist & per-column typing/validation
        final Set<String> allowed = Set.of(
                "status", "project_id",
                "dataset_id", "dataset_name",
                "workflow_id", "module", "name", "description"
        );

        List<String> sets = new ArrayList<>();
        List<Object> params = new ArrayList<>();
        List<Integer> types = new ArrayList<>();

        // Validate status if present
        Object statusVal = fields.get("status");
        String normalizedStatus = null;
        if (statusVal != null) {
            String s = String.valueOf(statusVal).trim().toLowerCase();
            if (!Set.of("pending", "running", "completed", "failed").contains(s)) {
                return "Invalid status. Allowed: pending, running, completed, failed.";
            }
            normalizedStatus = s;
            fields.put("status", s);
        }

        for (Map.Entry<String, Object> e : fields.entrySet()) {
            final String col = e.getKey();
            if (!allowed.contains(col)) {
                continue; // ignore unknown
            }
            Object v = e.getValue(); // keep nulls to clear
            switch (col) {
                case "dataset_id":
                    sets.add("dataset_id = ?");
                    if (v == null) {
                        params.add(null);
                        types.add(Types.OTHER); // postgres uuid
                    } else {
                        try {
                            params.add(UUID.fromString(String.valueOf(v)));
                            types.add(Types.OTHER);
                        } catch (IllegalArgumentException iae) {
                            return "dataset_id must be a valid UUID.";
                        }
                    }
                    break;

                case "project_id":
                    sets.add("project_id = ?");
                    if (v == null || String.valueOf(v).isBlank()) {
                        params.add(null);
                        types.add(Types.VARCHAR);
                    } else {
                        params.add(String.valueOf(v));
                        types.add(Types.VARCHAR);
                    }
                    break;

                case "dataset_name":
                case "workflow_id":
                case "module":
                case "name":
                case "status":
                case "description":
                    sets.add(col + " = ?");
                    if (v == null || String.valueOf(v).isBlank()) {
                        params.add(null);
                        types.add(Types.VARCHAR);
                    } else {
                        params.add(String.valueOf(v));
                        types.add(Types.VARCHAR);
                    }
                    break;

                default:
                // skip any non-whitelisted
            }
        }

        if (normalizedStatus != null) {
            sets.add("start_date = CASE WHEN ? = 'running' AND start_date IS NULL THEN NOW() ELSE start_date END");
            params.add(normalizedStatus);
            types.add(Types.VARCHAR);

            sets.add("finish_date = CASE WHEN ? IN ('completed','failed') THEN NOW() ELSE finish_date END");
            params.add(normalizedStatus);
            types.add(Types.VARCHAR);
        }

        if (sets.isEmpty()) {
            return "No valid columns in payload.";
        }

        final String sql = "UPDATE workflow_runs SET "
                + String.join(", ", sets) + ", last_updated = NOW() WHERE id = ?";

        try (Connection con = DatabaseConnectionPool.getDataSource().getConnection(); PreparedStatement ps = con.prepareStatement(sql)) {

            int idx = 1;
            for (int i = 0; i < params.size(); i++) {
                Object val = params.get(i);
                int t = types.get(i);
                if (val == null) {
                    ps.setNull(idx++, t);
                } else if (t == Types.OTHER && val instanceof UUID) {
                    ps.setObject(idx++, val, Types.OTHER); // UUID
                } else {
                    ps.setObject(idx++, val);
                }
            }
            ps.setInt(idx, id);

            int n = ps.executeUpdate();
            return (n > 0) ? "OK" : "No row updated (id not found).";

        } catch (SQLException e) {
            System.err.println("updateWorkflowRunFlexible SQL error: " + e.getMessage());
            return "SQL error: " + e.getMessage();
        }
    }

}
