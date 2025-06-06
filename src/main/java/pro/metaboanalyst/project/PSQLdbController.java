/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.project;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author qiang
 */
public class PSQLdbController {

    private Connection connection;

    private final String HOST;
    private final String devUserNM;
    private final String devPassWD;
    private boolean created = false;

    public Connection getConnection() {
        return connection;
    }

    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    public PSQLdbController(String host, String userNM, String password) {

        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            System.out.println("Class not found " + e);
        }

        // Class.forName("com.mysql.cj.jdbc.Driver").getDeclaredConstructor().newInstance();
        HOST = host;// + "?useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=GMT-4";
        devUserNM = userNM;
        devPassWD = password;
    }

    public boolean connect() throws SQLException {
        connection = DriverManager.getConnection(HOST, devUserNM, devPassWD);
        return !connection.isClosed();
    }

    public boolean disconnect() throws SQLException {
        if (!connection.isClosed()) {
            connection.close();
            if (connection.isClosed()) {
                return true;
            }
        }
        return false;
    }

    public ResultSet runQuery(String query) {
        System.out.println("This PSQLdb query has been received: " + query);
        try {
            connect();
            PreparedStatement statement = connection.prepareStatement(query);
            ResultSet res = statement.executeQuery();
            return res;
        } catch (SQLException ex) {
            System.out.print("Something wrong when doing the PSQL remote query:" + ex);
        }
        return null;
    }

    public void runUpdate(String query) {
        System.out.println("This PSQLdb query has been received: " + query);
        try {
            connect();
            PreparedStatement statement = connection.prepareStatement(query);
            statement.executeUpdate();
        } catch (SQLException ex) {
            System.out.print("Something wrong when doing the PSQL remote query");
        } finally {
            try {
                disconnect();
            } catch (SQLException ex) {
                Logger.getLogger(PSQLdbController.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    public boolean isCreated() {
        return created;
    }

    public void setCreated(boolean created) {
        this.created = created;
    }

}
