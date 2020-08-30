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
import java.net.HttpURLConnection;

import org.junit.BeforeClass;

import com.google.gson.Gson;
import com.vmware.blockchain.api.test.log.ApiLogger;
import com.vmware.blockchain.api.test.config.ApiTestConfig;
import com.vmware.blockchain.api.test.util.ConfigUtil;

/**
 * Base class for API tests
 */
public class ApiTestBase {

    public static Map<String, String> authMap;

    // Use the classname for the logger, this way you can refactor
    private static Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    @BeforeClass
    /**
     * Initialize the required variables.
     */
    public static void init() {
        authenticate();
    }

    /**
     * Authenticate with CSP and get an access token.
     */
    protected static void authenticate() {
        // Get app config
        ApiTestConfig apiTestConfig = ConfigUtil.getApiTestConfig();

        OutputStream out = null;
        InputStream inputStream = null;
        HttpsURLConnection cspConn = null;
        try {
            URL cspUrl = new URL(apiTestConfig.getCspUrl());
            logger.info("Trying to connect to " + cspUrl + "...");

            cspConn = (HttpsURLConnection) cspUrl.openConnection();
            logger.info("Connected to CSP site " + cspConn);
            HostnameVerifier allHostsValid = new HostnameVerifier() {
                public boolean verify(String hostname, SSLSession session) {
                    return true;
                }
            };
            cspConn.setHostnameVerifier(allHostsValid);
            cspConn.setDoOutput(true);
            byte[] data = (apiTestConfig.REFRESH_TOKEN + "=" + apiTestConfig.getCspRefreshToken()).getBytes();
            // Add headers
            cspConn.setRequestMethod(apiTestConfig.HTTP_POST_PARAM);
            cspConn.addRequestProperty(apiTestConfig.HTTP_ACCEPT_PARAM, apiTestConfig.HTTP_ACCEPT_TYPE_JSON);
            cspConn.addRequestProperty(apiTestConfig.HTTP_CONNECTION_PARAM, apiTestConfig.HTTP_CONNECTION_CLOSE);
            cspConn.addRequestProperty(apiTestConfig.HTTP_CONTENT_LEN_PARAM, String.valueOf(data.length));

            out = cspConn.getOutputStream();
            out.write(data);
            out.flush();

            StringBuilder builder = new StringBuilder();
            if(cspConn.getResponseCode() == 200){
                if (inputStream == null) {
                    inputStream = cspConn.getInputStream();
                }
                BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                String line = null;

                while ((line = reader.readLine()) != null) {
                    builder.append(line);
                    logger.fine("response line = " + line);
                }
            }

            logger.info("builder " + builder.toString());

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
                if (out != null) {
                    out.close();
                }
                if (cspConn != null) {
                    cspConn.disconnect();
                }
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException ioe) {
                logger.severe(ioe.getLocalizedMessage());
            }
        }
    }
}

