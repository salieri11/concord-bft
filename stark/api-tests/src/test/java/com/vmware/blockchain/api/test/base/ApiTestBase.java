package com.vmware.blockchain.api.test.base;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.util.Map;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;

import org.junit.BeforeClass;

import com.google.gson.Gson;
import com.vmware.blockchain.api.test.log.ApiLogger;


/**
 * Base class for API tests
 */
public class ApiTestBase {

    public static final String AUTH_HEADER_NAME = "csp-auth-token";
    public static Map<String, String> authMap;

    // Use the classname for the logger, this way you can refactor
    private static Logger logger = null;

    @BeforeClass
    /**
     * Initialize the required variables.
     */
    public static void init() {
        setUpLogger();
        authenticate();
    }

    /**
     * Setup logger.
     */
    protected static void setUpLogger() {
        try {
            ApiLogger.setup();
            logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Problem creating the log file.");
        }
    }


    /**
     * Authenticate with CSP and get an access token.
     */
    protected static void authenticate() {
        OutputStream out = null;
        InputStream inputStream = null;
        HttpsURLConnection uc = null;
        try {
            URL url = new URL("https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize");
            logger.info("Trying to connect to " + url + "...");

            uc = (HttpsURLConnection) url.openConnection();
            logger.info("CONNECTED " + uc);
            HostnameVerifier allHostsValid = new HostnameVerifier() {
                public boolean verify(String hostname, SSLSession session) {
                    return true;
                }
            };
            uc.setHostnameVerifier(allHostsValid);
            uc.setDoOutput(true);
            byte[] data = "refresh_token=e0HWXg5LHxJnvZxgBuHM6rXyq5rNwCY5Ppohyz1lZZiqygt1eXtS9HwVn7nPs3Vv".getBytes();

            // Add headers
            uc.setRequestMethod("POST");
            uc.addRequestProperty("Accept", "application/json");
            uc.addRequestProperty("Connection", "close");
            uc.addRequestProperty("Content-Length", String.valueOf(data.length));

            out = uc.getOutputStream();
            out.write(data);
            out.flush();
            //out.close();

            StringBuilder builder = new StringBuilder();
            if(uc.getResponseCode() == 200){
                if (inputStream == null) {
                    inputStream = uc.getInputStream();
                }
                BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                String line = null;

                while ((line = reader.readLine()) != null) {
                    builder.append(line);
                    logger.fine("response line = " + line);
                }
            }

            logger.info("builder " + builder.toString());
            //uc.disconnect();

            Gson gson = new Gson();
            authMap = (Map<String, String>)gson.fromJson(builder.toString(), Map.class);
            logger.fine("Auth map now " + authMap);

            String accessToken = authMap.get("access_token");
            logger.info("Token received = " + accessToken);
        } catch (java.net.MalformedURLException | java.net.ProtocolException e) {
            logger.severe("Unable to connect using the URL. Is the URL valid? " + e.getLocalizedMessage());
        } catch (java.io.IOException ioe) {
            logger.severe("Exception " + ioe.getLocalizedMessage());
        } finally {
            try {
                out.close();
                uc.disconnect();
                inputStream.close();
            } catch (IOException ioe) {
                logger.severe(ioe.getLocalizedMessage());
            }
        }
    }
}

