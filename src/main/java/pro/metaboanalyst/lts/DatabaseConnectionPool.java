package pro.metaboanalyst.lts;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import javax.sql.DataSource;

//@ApplicationScoped
//@Named("connectionPool")
public class DatabaseConnectionPool {

    private static DataSource dataSource;

    public static DataSource getDataSource() {
        return dataSource;
    }

    public static void setDataSource(DataSource dataSource) {
        DatabaseConnectionPool.dataSource = dataSource;
    }

    public static void setupDataSource(int SeverOption) {

        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            System.out.println("Class not found " + e);
        }

        HikariConfig config = new HikariConfig();
        switch (SeverOption) {
            case 0 -> {
                String psql_addr = "172.17.0.1:5433";
                String psql_addr_env = System.getenv("POSTGRES_ENV_ADDR");
                if (psql_addr_env != null) {
                    psql_addr = psql_addr_env;
                }
                config.setJdbcUrl("jdbc:postgresql://" + psql_addr + "/xialabdb"); // Docker's host IP and PSQL port
                config.setUsername("glassfish"); // Replace with your database username
                config.setPassword("xialab_docker_PWD!"); // Replace with your database password
                config.setMinimumIdle(5); // Minimum number of idle connections to maintain in the pool
            }
            case 1 -> {
                String psql_addr = "localhost:5432";
                config.setJdbcUrl("jdbc:postgresql://" + psql_addr + "/xialabdb"); // Docker's host IP and PSQL port
                config.setUsername("glassfish"); // Replace with your database username
                config.setPassword("xialab_docker_PWD!"); // Replace with your database password
                config.setMinimumIdle(5); // Minimum number of idle connections to maintain in the pool
            }
            case 2 -> {
                String psql_addr = "localhost:5432";
                config.setJdbcUrl("jdbc:postgresql://" + psql_addr + "/xialabdb"); // Docker's host IP and PSQL port
                config.setUsername("glassfish"); // Replace with your database username
                config.setPassword("1qazxDR%"); // Replace with your database password
                config.setMinimumIdle(5); // Minimum number of idle connections to maintain in the pool
            }
            case 9 -> {
                // this option is designed for enterprise docker user to customize the remote database option
                String psql_addr = "172.17.0.1:5433";
                String psql_addr_env = System.getenv("POSTGRES_ENV_ADDR");
                if (psql_addr_env != null) {
                    psql_addr = psql_addr_env;
                }
                String psql_db = "xialabdb";
                String psql_remote_addr = "172.17.0.1";
                String psql_remote_port = "5432";
                String psql_password = "xialab_docker_PWD!";
                String psql_usernm = "glassfish";

                try (BufferedReader br = new BufferedReader(new FileReader("/home/psql_tool.conf"))) {
                    String line;
                    br.readLine();  // Skip the header line

                    while ((line = br.readLine()) != null) {
                        //System.out.println("Now this validate_psql_conf line is ==> " + line);
                        String[] values = line.split(":");
                        if (values[0].equals("PSQL_DB_Name")) {
                            if (!values[1].equals("")) {
                                psql_db = values[1];
                            }
                        }
                        if (values[0].equals("PSQL_DB_Password")) {
                            if (!values[1].equals("")) {
                                psql_password = values[1];
                            }
                        }
                        if (values[0].equals("PSQL_DB_ADDR")) {
                            if (!values[1].equals("")) {
                                psql_remote_addr = values[1];
                            }

                        }
                        if (values[0].equals("PSQL_DB_Port")) {
                            if (!values[1].equals("")) {
                                psql_remote_port = values[1];
                            }
                        }
                        if (values[0].equals("PSQL_USER_Name")) {
                            if (!values[1].equals("")) {
                                psql_usernm = values[1];
                            }
                        }
                    }

                } catch (IOException e) {
                    e.printStackTrace();
                }                
                psql_addr = psql_remote_addr + ":" + psql_remote_port;
                //System.out.println("Now the psql_addr info is here: " + psql_addr + " | " + psql_db + " | " + psql_usernm + psql_password);
                config.setJdbcUrl("jdbc:postgresql://" + psql_addr + "/" + psql_db); // Docker's host IP and PSQL port
                config.setUsername(psql_usernm); // Replace with your database username
                config.setPassword(psql_password); // Replace with your database password
                config.setMinimumIdle(5);
            }
            default -> {
                config.setJdbcUrl("jdbc:postgresql://34.133.74.138:5432/xialabdb"); // Replace with your database URL
                config.setUsername("glassfish"); // Replace with your database username
                config.setPassword("1qazxDR%"); // Replace with your database password
                config.setMinimumIdle(5); // Minimum number of idle connections to maintain in the pool
            }
        }

        // Optional: HikariCP settings to tweak the connection pool behavior
        config.setMaximumPoolSize(50); // Maximum number of connections in the pool
        config.setConnectionTimeout(30000); // Maximum wait milliseconds for a connection from the pool
        config.setIdleTimeout(600000); // Maximum time milliseconds a connection can sit idle in the pool
        config.setLeakDetectionThreshold(5000);
        dataSource = new HikariDataSource(config);
    }

    public static Connection getConnection() throws SQLException {
        return dataSource.getConnection(); // Get a connection from the pool
    }

    public static void shutdownPool() {
        if (dataSource instanceof HikariDataSource hikariDataSource) {
            hikariDataSource.close();
        }
    }

    public static String getPoolStatus() {
        if (dataSource instanceof HikariDataSource hds) {
            return "Total connections: " + hds.getHikariPoolMXBean().getTotalConnections()
                    + ", Idle connections: " + hds.getHikariPoolMXBean().getIdleConnections()
                    + ", Active connections: " + hds.getHikariPoolMXBean().getActiveConnections();
        }
        return "Not a HikariCP dataSource";
    }

    public static boolean testConnection() {
        try (Connection conn = dataSource.getConnection()) {
            return conn.isValid(1); // Check if connection is valid, 1 second timeout
        } catch (SQLException e) {
            return false;
        }
    }

    public static void setMaximumPoolSize(int maxPoolSize) {
        if (dataSource instanceof HikariDataSource hikariDataSource) {
            hikariDataSource.setMaximumPoolSize(maxPoolSize);
        }
    }

    public static void setIdleTimeout(long idleTimeout) {
        if (dataSource instanceof HikariDataSource hikariDataSource) {
            hikariDataSource.setIdleTimeout(idleTimeout);
        }
    }

}
