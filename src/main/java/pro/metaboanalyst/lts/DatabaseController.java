/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.lts;

import jakarta.enterprise.context.RequestScoped;
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
import jakarta.inject.Inject;
import jakarta.inject.Named;
import pro.metaboanalyst.controllers.general.ApplicationBean1;
import pro.metaboanalyst.rwrappers.RCenter;

/**
 *
 * @author zgy
 */
@RequestScoped
@Named("databaseController")
public class DatabaseController implements Serializable {

    @Inject
    FireBase fb;
    @Inject
    ApplicationBean1 ab;

    public String registerUser(String email, String password, String firstname, String lastname, String institution) {
        String hashedPassword = RCenter.hashPassword(fb.getRscriptsDBPath(), fb.getProjectDBPath(), password);

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

    public String[] loginUser(String email, String password, String tool) {
        System.out.println("loginUser --> step0");
        tool = tool.toLowerCase();
        // Check PSQL db availability (This can be a method that checks if pool is available or not)
        // Assuming DatabaseConnectionPool.getDataSource() is always available or throws an exception if not.

        // Hash the password using SHA-256
        String hashedPassword = RCenter.hashPassword(fb.getRscriptsDBPath(),
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

            if (!tool.equalsIgnoreCase("xialab")) {
                String checkQuery = "SELECT tool_" + tool.toLowerCase() + ", node FROM registration WHERE email = ?";
                System.out.println("checkQuery --> " + checkQuery);
                PreparedStatement checkStmt = con.prepareStatement(checkQuery);
                checkStmt.setString(1, email);
                ResultSet res = checkStmt.executeQuery();

                if (res.next()) {
                    boolean isTool = res.getBoolean("tool_" + tool.toLowerCase());
                    if (!isTool) {
                        return new String[]{"Login Error - User exists but is not authorized for tool " + tool + "."};
                    }
                } else {
                    // The ResultSet was empty, no user found with the given email
                    return new String[]{"Login Error - No entry found for the provided email in the registration table."};
                }

                if (storedPassword.equals(hashedPassword)) {
                    con.commit(); // Commit the transaction
                    // Return additional user data
                    return new String[]{
                        user_data.getString("email"),
                        user_data.getString("firstname"),
                        user_data.getString("lastname"),
                        user_data.getString("institution"),
                        res.getString("node")
                    };
                } else {
                    con.rollback(); // Roll back the transaction
                    return new String[]{"Invalid password."};
                }
            } else {
                // For tool_xialab, skip the tool check
                if (storedPassword.equals(hashedPassword)) {
                    con.commit(); // Commit the transaction
                    // Return additional user data
                    return new String[]{
                        user_data.getString("email"),
                        user_data.getString("firstname"),
                        user_data.getString("lastname"),
                        user_data.getString("institution"),
                        "" // Assuming node is not needed for tool_xialab
                    };
                } else {
                    con.rollback(); // Roll back the transaction
                    return new String[]{"Invalid password."};
                }
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

    public String[] loginUserDocker(String email, String password) {
        System.out.println("loginUser --> step0");
        // Check PSQL db availability (This can be a method that checks if pool is available or not)
        // Assuming DatabaseConnectionPool.getDataSource() is always available or throws an exception if not.

        // Hash the password using SHA-256
        String hashedPassword = RCenter.hashPassword(fb.getRscriptsDBPath(),
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

    public int writeProjectToPostgres(Map<String, Object> rawDocData, String projectType, String tableName) {
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
        ResultSet tables = null;
        ResultSet existingCount = null;

        try {
            con = DatabaseConnectionPool.getDataSource().getConnection();
            con.setAutoCommit(false); // Start transaction

            // Prepare the query string using the tableName parameter
            String checkQuery = String.format("SELECT COUNT(*) FROM %s WHERE folderName=? AND userId=?", tableName);
            PreparedStatement checkStmt = con.prepareStatement(checkQuery);
            checkStmt.setString(1, docData.get("foldername"));
            checkStmt.setString(2, docData.get("userid"));

            existingCount = checkStmt.executeQuery();

            if (existingCount.next() && existingCount.getInt(1) > 0) {
                // Update existing row in the specified table
                String updateQuery = String.format("UPDATE %s SET userId=?, name=?, description=?, module=?, moduleReadable=?, dataType=?, date=?, javaHistory=?, naviString=?, naviCode=?, org=?, partialToken=?, toolName=?, toolCode=?, projectType=?, paired=?, regression=?, location=? WHERE folderName=?", tableName);
                stmt = con.prepareStatement(updateQuery);
            } else {
                // Insert new row in the specified table
                String insertQuery = String.format("INSERT INTO %s (userId, name, description, module, moduleReadable, dataType, date, javaHistory, naviString, naviCode, org, partialToken, toolName, toolCode, projectType, paired, regression, location, folderName) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)", tableName);
                stmt = con.prepareStatement(insertQuery);
            }

            // Set parameters for the update/insert statement
            // Assuming you have a method like setPreparedStatementParameters to set these
            setPreparedStatementParameters(stmt, docData);

            // Execute the update/insert operation
            stmt.executeUpdate();

            con.commit(); // Commit the transaction
            result = 1; // Indicate success

        } catch (SQLException ex) {
            System.out.println("SQLException occurred: " + ex.getMessage());
            try {
                if (con != null) {
                    con.rollback();
                }
            } catch (SQLException exRollback) {
                System.out.println("Rollback failed: " + exRollback.getMessage());
            }
            result = -1; // Indicate an error
        } finally {
            // Close all resources
            try {
                if (tables != null) {
                    tables.close();
                }
                if (existingCount != null) {
                    existingCount.close();
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
        return result;
    }

    private void setPreparedStatementParameters(PreparedStatement pstmt, Map<String, String> docData) throws SQLException {
        // Assuming that you handle data type conversion where necessary, e.g., converting string to integer/date
        pstmt.setString(1, docData.get("userid"));
        pstmt.setString(2, docData.get("name"));
        pstmt.setString(3, docData.get("description"));
        pstmt.setString(4, docData.get("module"));
        pstmt.setString(5, docData.get("modulereadable"));
        pstmt.setString(6, docData.get("datatype"));

        // Handle date conversion from String to Date
        String dateStr = docData.get("date");
        if (dateStr != null && !dateStr.isEmpty()) {
            Date date = null;
            try {
                date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(dateStr);
            } catch (ParseException ex) {
                Logger.getLogger(DatabaseController.class.getName()).log(Level.SEVERE, null, ex);
            }
            pstmt.setDate(7, new java.sql.Date(date.getTime()));
        } else {
            pstmt.setDate(7, null);
        }

        pstmt.setString(8, docData.get("javahistory"));
        pstmt.setString(9, docData.get("navistring"));
        pstmt.setString(10, docData.get("naviCcode"));
        pstmt.setString(11, docData.get("org"));
        pstmt.setString(12, docData.get("partialtoken"));
        pstmt.setString(13, docData.get("toolname"));
        pstmt.setString(14, docData.get("toolcode"));

        // Assuming projectType, paired, and regression are stored in docData
        pstmt.setString(15, docData.get("projectType"));

        // Convert string "true"/"false" to boolean
        pstmt.setBoolean(16, "true".equalsIgnoreCase(docData.get("paired")));
        pstmt.setBoolean(17, "true".equalsIgnoreCase(docData.get("regression")));

        pstmt.setString(18, docData.get("location"));
        pstmt.setString(19, docData.get("foldername"));
    }

    public int updateProjectTitleDescription(String newName, String newDescription, int id) {
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

    public ArrayList<HashMap<String, Object>> getProjectsFromPostgres(String email, String toolName, String toolLocation) {
        ArrayList<HashMap<String, Object>> projects = new ArrayList<>();
        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            con = DatabaseConnectionPool.getConnection(); // Get a connection from the pool
            con.setAutoCommit(false); // Start transaction
            String query = "SELECT * FROM project WHERE userId = ? AND toolName = ? AND (projectType != 'batch_project' OR projectType IS NULL) AND (location = ? OR location = ? OR location = ? OR location = ?) ORDER BY id DESC";
            stmt = con.prepareStatement(query);
            stmt.setString(1, email);
            stmt.setString(2, toolName);
            stmt.setString(3, toolLocation);
            stmt.setString(4, "vip");
            stmt.setString(5, "pro");
            stmt.setString(6, "eu");
            rs = stmt.executeQuery();
            System.out.println(stmt.toString());

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
            con.commit(); // Commit the transaction

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

    public int deleteProjectById(Long id) {
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

    public Map<String, Object> loadProject(String token) {
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

    public String insertToken(String email, String resetToken, String expDate) {
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

    public String checkUserExists(String email) {
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

    public String verifyToken(String token) {
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

    public String deleteTokenForUser(String email) {
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
            String hashedPassword = RCenter.hashPassword(fb.getRscriptsDBPath(),
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

    public String addActivationCode(String activateCode, String expDate, String email) {
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

    public String checkActivationCode(String email, String activationCode) {
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

    public String deleteUserAndProjects(String userId) {
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

    public int checkMatchingFolderNameProject(String folderName) {
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

    public int transferProject(int id, String newEmail, String suffix, String toolLocation) {
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

    public int detectDockerUserNum() {

        Connection con = null;
        PreparedStatement insertStmt = null;
        ResultSet rs = null;
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
    public int recordRawJob(long JobID, String email, String Project_folder, String JobPos) {

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

    public int updateRawJobStatus(long JobID, String currentJobStatus, String JobPos) {

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

    public int extractRawJobStatus(String folder, String userid) {

        int result = 0;

        Connection con = null;
        PreparedStatement stmt = null;
        ResultSet rs = null;
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

    public ArrayList<HashMap<String, Object>> getAllWorkflows(String toolName, String email) {
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

    public String insertWorkflow(String email, String name, String description, String module, String toolName, String filename, String location, String input, String analysisGoal, String analysisMethods, String output, String other) {
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

    public Map<String, Object> loadProjectById(String id) {
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

    public Map<String, Object> obtainFolderNameProject(String folderName) {
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
}
