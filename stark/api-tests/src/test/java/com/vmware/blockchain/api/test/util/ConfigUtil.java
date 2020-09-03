package com.vmware.blockchain.api.test.util;

import java.util.logging.Logger;

import com.vmware.blockchain.api.test.log.ApiLogger;
import com.vmware.blockchain.api.test.config.ApiTestConfig;

public class ConfigUtil {
    private static Logger logger = null;

    private static ApiTestConfig apiTestConfig = null;

    /**
     * Get API Test Config object.
     * @return ApiTestConfig
     */
    public static ApiTestConfig getApiTestConfig() {
        logger.info("apiTestconfig = " + apiTestConfig);
        if (apiTestConfig == null) {
            apiTestConfig = new ApiTestConfig();
        }
        logger.info("apiTestconfig = " + apiTestConfig);
        return apiTestConfig;
    }

    /**
     * Setup logger.
     */
    public static void setUpLogger() {
        try {
            ApiLogger.setup();
            logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
        } catch (java.io.IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Problem creating the log file.");
        }
    }



}