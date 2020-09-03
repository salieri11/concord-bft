package com.vmware.blockchain.api.test.base;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import java.net.URL;
import java.util.Map;
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

    @BeforeClass
    public static void setUpClass() {
        ConfigUtil.setUpLogger();
        logger.info("ApiTestSuite setup");
        if (!ServiceUtil.isApiServiceReachable()) {
            logger.severe("API service is not available. Quit running tests.");
            System.exit(1);
        }

        // Prefer lazy loading, so move it to ConfigUtil.
        /*ApiTestConfig apiTestConfig = new ApiTestConfig();
        ConfigUtil.setApiTestConfig(apiTestConfig);
        logger.info("Setting the API Test config object to util.");*/


    }

    @AfterClass
    public static void tearDownClass() {
        logger.info("ApiTestSuite tearDown");
    }


    public static void main(String[] args) {
       ConfigUtil.setUpLogger();
       ServiceUtil.isApiServiceReachable();
    }
}