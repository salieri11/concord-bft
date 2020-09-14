package com.vmware.blockchain.api.test.config;

import java.util.Properties;
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.logging.Logger;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.nio.file.FileSystems;

public class ApiTestConfig {

    private static Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    private static final String WAIT_FOR_SERVICE_IN_MILLIS_PARAM = "wait.for.service.in.millis";
    private static final long WAIT_FOR_SERVICE_IN_MILLIS_DEFAULT_VAL = 5 * 60 * 1000;

    private static final String SLEEP_FOR_SERVICE_IN_MILLIS_PARAM = "sleep.for.service.in.millis";
    private static final long SLEEP_FOR_SERVICE_IN_MILLIS_DEFAULT_VAL = 30 * 1000;

    private static final String API_SERVICE_URL_PARAM = "service.url";
    private static final String API_SERVICE_URL_DEFAULT_VAL = "http://localhost:8080/api";

    private static final String CSP_AUTH_HEADER_NAME = "csp.auth.token";
    private static final String CSP_AUTH_HEADER_DEFAULT_VAL = "csp-auth-token";

    private static final String CSP_URL_PARAM = "csp.url";
    private static final String CSP_URL_DEFAULT_VAL = "https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize";

    private static final String CONFIG_FILE_NAME_PARAM = "config.file.name";
    private static final String CONFIG_FILE_NAME_DEFAULT_VAL = "api_test_config.properties";

    private static final String CONFIG_FILE_PATH_PARAM = "config.file.path";
    private static final String CONFIG_FILE_PATH_DEFAULT_VAL = "src/test/resources";

    private static final String CSP_REFRESH_TOKEN_PARAM = "csp.refresh.token";
    private static final String CSP_REFRESH_TOKEN_DEFAULT_VAL = "e0HWXg5LHxJnvZxgBuHM6rXyq5rNwCY5Ppohyz1lZZiqygt1eXtS9HwVn7nPs3Vv";

    private static final String MIN_REPLICAS_PARAM = "min.replicas";
    private static final String MIN_REPLICAS_DEFAULT_VAL = "4";
    private static final String MAX_REPLICAS_PARAM = "max.replicas";
    private static final String MAX_REPLICAS_DEFAULT_VAL = "7";
    private static final String MIN_CLIENTS_PARAM = "min.clients";
    private static final String MIN_CLIENTS_DEFAULT_VAL = "1";
    private static final String MAX_CLIENTS_PARAM = "max.clients";
    private static final String MAX_CLIENTS_DEFAULT_VAL = "10";

    // Publicly available Constants
    public static final String HTTP_POST_PARAM = "POST";
    public static final String HTTP_ACCEPT_PARAM = "Accept";
    public static final String HTTP_ACCEPT_TYPE_JSON = "application/json";
    public static final String HTTP_CONNECTION_PARAM = "Connection";
    public static final String HTTP_CONNECTION_CLOSE = "close";
    public static final String HTTP_CONTENT_LEN_PARAM = "Content-Length";
    public static final String ACCESS_TOKEN_PARAM = "access_token";
    public static final String TOKEN_PARAM = "token";
    public static final String REFRESH_TOKEN = "refresh_token";

    Properties configProperties = new Properties();

    public ApiTestConfig() {
        initConfig();
        logger.info("after init config " + configProperties);
        // Watch for any changes
        watch();
        logger.info("after watch " + configProperties);
    }

    private void initConfig() {
        configProperties.setProperty(WAIT_FOR_SERVICE_IN_MILLIS_PARAM, String.format("%d", WAIT_FOR_SERVICE_IN_MILLIS_DEFAULT_VAL));
        configProperties.setProperty(SLEEP_FOR_SERVICE_IN_MILLIS_PARAM, String.format("%d", SLEEP_FOR_SERVICE_IN_MILLIS_DEFAULT_VAL));
        configProperties.setProperty(API_SERVICE_URL_PARAM, API_SERVICE_URL_DEFAULT_VAL);
        configProperties.setProperty(CSP_AUTH_HEADER_NAME, CSP_AUTH_HEADER_DEFAULT_VAL);
        configProperties.setProperty(CSP_URL_PARAM, CSP_URL_DEFAULT_VAL);
        configProperties.setProperty(CONFIG_FILE_NAME_PARAM, CONFIG_FILE_NAME_DEFAULT_VAL);
        configProperties.setProperty(CONFIG_FILE_PATH_PARAM, CONFIG_FILE_PATH_DEFAULT_VAL);
        configProperties.setProperty(CSP_REFRESH_TOKEN_PARAM, CSP_REFRESH_TOKEN_DEFAULT_VAL);
        configProperties.setProperty(MIN_REPLICAS_PARAM, MIN_REPLICAS_DEFAULT_VAL);
        configProperties.setProperty(MAX_REPLICAS_PARAM, MAX_REPLICAS_DEFAULT_VAL);
        configProperties.setProperty(MIN_CLIENTS_PARAM, MIN_CLIENTS_DEFAULT_VAL);
        configProperties.setProperty(MAX_CLIENTS_PARAM, MAX_CLIENTS_DEFAULT_VAL);
    }

    protected void watch() {
        Thread thread = new Thread(new Runnable() {
            public void run() {
                try {
                    new ConfigFileWatcher().watch(configProperties.getProperty(CONFIG_FILE_PATH_PARAM));
                } catch (Exception e) {
                    logger.severe("Exception while watching for config file changes: " + e.getLocalizedMessage());
                }
            }
        });
        thread.start();
    }

    protected void process(File file) {
        InputStream inputStream = null;
        try {
            inputStream = new FileInputStream(file);
            Properties props = new Properties();
            props.load(inputStream);
            if (props != null) {
                for (String propName : props.stringPropertyNames​()) {
                    String propVal = props.getProperty(propName);
                    if (propVal != null && !propVal.isEmpty()) {
                        configProperties.setProperty(propName, propVal);
                    }
                }

            }
        } catch (IOException ioe) {
            logger.severe("IOexception while loading properties " + ioe.getLocalizedMessage());
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException ioe) {}
            }
        }
    }

    /**
     * Get time to wait for service.
     * @return wait time.
     */
    public long getWaitForServiceInMillis() {
        String waitTime = configProperties.getProperty(WAIT_FOR_SERVICE_IN_MILLIS_PARAM, "0");
        return Long.parseLong(waitTime);
    }

    /**
     * Get sleep time waiting for service.
     * @return sleep time.
     */
    public long getSleepForServiceInMillis() {
        String sleepTime = configProperties.getProperty(SLEEP_FOR_SERVICE_IN_MILLIS_PARAM, "0");
        return Long.parseLong(sleepTime);
    }

    /**
     * Get API service URL.
     * @return URL as String.
     */
    public String getApiServiceUrl() {
        String url = configProperties.getProperty(API_SERVICE_URL_PARAM, "");
        return url;
    }

    /**
     * Get CSP auth header name.
     * @return CSP auth header name as String.
     */
    public String getCspAuthHeader() {
        String authHeader = configProperties.getProperty(CSP_AUTH_HEADER_NAME, "");
        return authHeader;
    }

    /**
     * Get CSP URL.
     * @return URL as String
     */
    public String getCspUrl() {
        String url = configProperties.getProperty(CSP_URL_PARAM, "");
        return url;
    }

    /**
     * Get the name of config properties file.
     * @return config file name as String.
     */
    public String getConfigFileName() {
        String name = configProperties.getProperty(CONFIG_FILE_NAME_PARAM, "");
        return name;
    }

    /**
     * Get the path of the config file.
     * @return config file path as String.
     */
    public String getConfigFilePath() {
        String path = configProperties.getProperty(CONFIG_FILE_PATH_PARAM, "");
        return path;
    }

    /**
     * Get CSP refresh token.
     * @return refresh token as String.
     */
    public String getCspRefreshToken() {
        String path = configProperties.getProperty(CSP_REFRESH_TOKEN_PARAM, "");
        return path;
    }

    /**
     * Get minimum number of replicas configuration.
     * @return min replicas in string format.
     */
    public String getMinReplicas() {
        String path = configProperties.getProperty(MIN_REPLICAS_PARAM, "");
        return path;
    }

    /**
     * Get maximum number of replicas configuration.
     * @return max replicas in string format.
     */
    public String getMaxReplicas() {
        String path = configProperties.getProperty(MAX_REPLICAS_PARAM, "");
        return path;
    }

    /**
     * Get minimmum number of clients configuration.
     * @return min clients in string format.
     */
    public String getMinClients() {
        String path = configProperties.getProperty(MIN_CLIENTS_PARAM, "");
        return path;
    }

    /**
     * Get maximum clients configuration.
     * @return max clients in string format.
     */
    public String getMaxClients() {
        String path = configProperties.getProperty(MAX_CLIENTS_PARAM, "");
        return path;
    }

    /**
     * String representation of this class.
     * @return A string
     */
    @Override
    public String toString() {
        return "ApiTestConfig{" +
               "configProperties=" + configProperties +
               '}';
    }

    /**
     * A class responsible for watching configuration file changes.
     */
    public class ConfigFileWatcher {
        public void watch(String configFilePath) {
            try {
                WatchService watchService = FileSystems.getDefault().newWatchService();
                Path path = Paths.get(configFilePath);
                path.register(
                        watchService,
                        StandardWatchEventKinds.ENTRY_CREATE,
                        StandardWatchEventKinds.ENTRY_DELETE,
                        StandardWatchEventKinds.ENTRY_MODIFY);

                WatchKey key;
                while ((key = watchService.take()) != null) {
                    for (WatchEvent<?> event : key.pollEvents()) {
                        logger.info("Event kind:" + event.kind() + ". File affected: " + event.context() + ".");
                        File file = path.resolve((Path) event.context()).toFile();
                        String configFile = configProperties.getProperty(CONFIG_FILE_NAME_PARAM);
                        if (file.getName().equalsIgnoreCase(configFile)) {
                            process(file);
                        }
                    }
                    key.reset();
                }
            } catch (java.lang.InterruptedException | java.io.IOException e) {
                logger.severe("Exception during watch operation " + e.getLocalizedMessage());
            }
        }
    }
}