package com.vmware.blockchain.api.test.util;

import java.net.URL;
import java.util.Map;
import java.util.logging.Logger;
import java.net.HttpURLConnection;
import java.net.URI;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import com.vmware.blockchain.api.test.log.ApiLogger;
import com.vmware.blockchain.api.test.config.ApiTestConfig;

public class ServiceUtil {
    // Use the classname for the logger, this way you can refactor
    private static Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static boolean isApiServiceReachable() {
        logger.info("isApiServiceReachable ");
        // Get the config object to read the custom/default values.
        ApiTestConfig apiTestConfig = ConfigUtil.getApiTestConfig();
        logger.info("apiTestConfig " + apiTestConfig);
        URL helenUrl = null;
        HttpURLConnection helenConn = null;
        InputStream inputStream = null;
        boolean connected = false;
        long startTime = System.currentTimeMillis();
        long endTime = startTime + apiTestConfig.getWaitForServiceInMillis();
        try {
            // wait till Helen service is up.
            // Check if service is up, wait until is it available.
            helenUrl = new URL(apiTestConfig.getApiServiceUrl());
            while (!connected && ((endTime - System.currentTimeMillis()) > 0)) {
                logger.fine("In while loop...");
                try {
                    helenConn = (HttpURLConnection) helenUrl.openConnection();
                    if (helenConn != null) {
                        StringBuilder builder = new StringBuilder();
                        int responseCode = helenConn.getResponseCode();
                        logger.info("response code = " + responseCode);
                        if (responseCode == 200 || responseCode == 401) {
                            // Got response 200 or 401, that means service is up.
                            connected = true;
                        }
                    }
                    if (!connected) {
                        logger.info("Still waiting for Helen to be available. Sleep for " +
                                    apiTestConfig.getSleepForServiceInMillis());
                        Thread.sleep(apiTestConfig.getSleepForServiceInMillis());
                    }
                } catch (java.io.IOException | java.lang.InterruptedException e) {
                    logger.severe("error is " + e.getLocalizedMessage());
                    try {
                        logger.info("Sleep for " + apiTestConfig.getSleepForServiceInMillis());
                        Thread.sleep(apiTestConfig.getSleepForServiceInMillis());
                    } catch (java.lang.InterruptedException ex) {}
                }
            }
            if (!connected) {
                logger.info("API service is not up. Quit.");
                return false;
            }
            logger.info(helenConn.toString());
        } catch (java.net.MalformedURLException e) {
            logger.info("API service URL is incorrect. Quit.");
            return false;
        } finally {
            if (helenConn != null) {
                helenConn.disconnect();
            }
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException io) {}
            }
        }
        logger.info("API service is up. Go ahead.");
        return true;
    }
}