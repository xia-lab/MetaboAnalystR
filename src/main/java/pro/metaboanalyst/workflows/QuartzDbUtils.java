/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pro.metaboanalyst.workflows;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author zgy
 */
public class QuartzDbUtils {

    public static Connection getConnection() throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(QuartzDbUtils.class.getName()).log(Level.SEVERE, null, ex);
        }

        String url = "jdbc:postgresql://localhost:5432/quartzdb";
        return DriverManager.getConnection(url, "quartz", "quartz_xialab_$Omics!");
    }

    public static void insertJobStatus(String jobId, String token, String status) {
        String sql = "INSERT INTO workflow_job_status (job_id, token, status, updated_time) "
                + "VALUES (?, ?, ?, ?)";

        try (Connection conn = QuartzDbUtils.getConnection(); PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, jobId);
            stmt.setString(2, token);
            stmt.setString(3, status);
            stmt.setTimestamp(4, new Timestamp(System.currentTimeMillis()));

            stmt.executeUpdate();

        } catch (SQLException e) {
            e.printStackTrace();
            // Possibly re-throw or handle
        }
    }

    public static void updateJobStatus(String jobId, String status) {
        String sql = "UPDATE workflow_job_status "
                + "SET status = ?, updated_time = ? "
                + "WHERE job_id = ?";

        try (Connection conn = QuartzDbUtils.getConnection(); PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, status);
            stmt.setTimestamp(2, new Timestamp(System.currentTimeMillis()));
            stmt.setString(3, jobId);

            int rowsUpdated = stmt.executeUpdate();
            if (rowsUpdated == 0) {
                // No row found for that jobId
                System.out.println("No row found to update for jobId=" + jobId);
            } 

        } catch (SQLException e) {
            e.printStackTrace();
            // Possibly re-throw or handle
        }
    }

    public static String getTokenByJobId(String jobId) {
        String sql = "SELECT token FROM workflow_job_status WHERE job_id = ?";
        String token = null;

        try (Connection conn = QuartzDbUtils.getConnection(); PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, jobId);

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    token = rs.getString("token");
                }
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return token;
    }
}
