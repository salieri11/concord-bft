package com.vmware.blockchain.api.test.base;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;
import java.net.HttpURLConnection;
import java.net.URI;

import com.vmware.blockchain.api.test.log.ApiLogger;
import com.vmware.blockchain.api.test.blockchains.BlockchainApiTest;
import com.vmware.blockchain.api.test.util.ConfigUtil;
import com.vmware.blockchain.api.test.util.ServiceUtil;

import org.junit.BeforeClass;
import org.junit.AfterClass;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    BlockchainApiTest.class,
})

public class ApiTestSuite {
    // Use the classname for the logger, this way you can refactor
    private static Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    private static List<Container> containers = new ArrayList<>();
    private static int containerIdIndex;
    private static int containerNameIndex;

    @BeforeClass
    public static void setUpClass() {
        ConfigUtil.setUpLogger();
        logger.info("ApiTestSuite setup");
        if (!ServiceUtil.isApiServiceReachable()) {
            logger.severe("API service is not available. Timed out waiting for the Service. Quit running tests.");
            System.exit(1);
        }

        // Get the container Ids to retrieve logs for.
        populateContainerIds();

        // Prefer lazy loading, so move it to ConfigUtil.
        /*ApiTestConfig apiTestConfig = new ApiTestConfig();
        ConfigUtil.setApiTestConfig(apiTestConfig);
        logger.info("Setting the API Test config object to util.");*/
    }


    @AfterClass
    public static void tearDownClass() {
        logger.info("ApiTestSuite tearDown");
        // Get logs from docker containers before tearing down.
        getLogs();
    }

    /**
     * Grab container Ids from docker
     */
    private static void populateContainerIds() {
        InputStream pis = null;
        BufferedReader reader = null;
        try {
            Process process = new ProcessBuilder("docker", "ps", "-a").start();
            logger.info("after running process.....");
            pis = process.getInputStream();
            reader = new BufferedReader(new InputStreamReader(pis));
            if (reader != null) {
                reader.lines().forEach(line -> addContainerId(line));
            }
            int exitCode = process.waitFor();
        } catch (InterruptedException | IOException ioe) {
            logger.severe("Error while running docker ps command.");
        } finally {
            if (pis != null) {
                try {
                    pis.close();
                } catch (IOException ioe) {}
            }
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException ioe) {}
            }
        }
    }

    /**
     * Parse the docker ps command line output line, and capture the container Id and add it to the list of container
     * Ids.
     * @param line Output line from docker ps
     */
    private static void addContainerId(String line) {
        // Special case for header.
        if (line != null && (line.contains("CONTAINER ID") || line.contains("NAMES"))) {
            int currToken = 0;
            for (String token : line.split("[ ]{2,}")) {
                if (token.equalsIgnoreCase("CONTAINER ID")) {
                    containerIdIndex = currToken;
                } else if (token.equalsIgnoreCase("NAMES")) {
                    containerNameIndex = currToken;
                }
                currToken++;
            }
            // Done with header processing, return.
            return;
        }
        int currToken = 0;
        Container container = new Container();
        for (String token : line.split("[ ]{2,}")) {
            if (currToken == containerIdIndex) {
                container.containerId = token;
            } else if (currToken == containerNameIndex) {
                container.containerName = token;
                // Name is the last header, so add the object to the list.
                containers.add(container);
                return;
            }
            currToken++;
        }
    }

    /**
     * Grab docker logs for each container, and write to a file.
     */
    private static void getLogs() {
        // For each container Id, run the log command.
        if (containers != null) {
            for (Container container : containers) {
                FileOutputStream fos = null;
                InputStream pis = null;
                try {
                    Process process = new ProcessBuilder("docker", "logs", container.containerId).start();
                    File tempDir = new File(System.getProperty("java.io.tmpdir"));
                    File logFile = new File(tempDir.getAbsolutePath() + "/" + container.containerName + ".log");
                    fos = new FileOutputStream(logFile);

                    logger.info("Logging to " + logFile.getAbsolutePath());
                    logger.info("after running docker logs for container " + container.containerId);
                    pis = process.getInputStream();

                    int byteRead = -1;
                    while ((byteRead = pis.read()) != -1) {
                        fos.write(byteRead);
                    }

                    int exitCode = process.waitFor();
                } catch (InterruptedException | IOException ioe) {
                    logger.severe("Error while running docker ps command: " + ioe.getLocalizedMessage());
                } finally {
                    if (fos != null) {
                        try {
                            fos.close();
                        } catch (IOException ioe) {
                            logger.severe("Troubling closing the output file " + ioe.getLocalizedMessage());
                        }
                    }
                    if (pis != null) {
                        try {
                            pis.close();
                        } catch (IOException ioe) {
                            logger.severe("Troubling closing the process input stream " + ioe.getLocalizedMessage());
                        }
                    }
                }
            }
        }
    }

    /**
     * A holder class for containerIds and containerNames.
     */
    static class Container {
        String containerId;
        String containerName;

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            Container container = (Container) o;
            return Objects.equals(containerId, container.containerId);
        }

        @Override
        public int hashCode() {
            return Objects.hash(containerId);
        }
    }

    public static void main(String[] args) {
       ConfigUtil.setUpLogger();
       ServiceUtil.isApiServiceReachable();
    }


}