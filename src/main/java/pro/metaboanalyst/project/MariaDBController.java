/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pro.metaboanalyst.project;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 *
 * @author qiang
 */
public class MariaDBController {

    private Connection connection;

    private final String HOST;
    private final String devUserNM;
    private final String devPassWD;

    public MariaDBController(String host, String userNM, String password) throws Exception {
        Class.forName("com.mysql.cj.jdbc.Driver").getDeclaredConstructor().newInstance();
        HOST = host + "?useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=GMT-4";
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

    public ResultSet runQuery(String query) throws SQLException {

        System.out.println("This DB query has been received: " + query);

        try {
            connect();
            PreparedStatement statement = connection.prepareStatement(query);
            ResultSet res = statement.executeQuery();
            return res;
        } catch (SQLException ex) {
            System.out.print("Something wrong when doing the mysql remote query:" + ex);
        }
        return null;

    }

    public void runUpdate(String query) throws SQLException {

        System.out.println("This DB query has been received: " + query);

        try {
            connect();
            PreparedStatement statement = connection.prepareStatement(query);
            statement.executeUpdate();
        } catch (SQLException ex) {
            System.out.print("Something wrong when doing the mysql remote query");
        } finally {
            disconnect();
        }

    }
}
