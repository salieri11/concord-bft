/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.web.util.UriTemplate;

import com.vmware.blockchain.common.restclient.interceptor.RequestAuthenticationInterceptor;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * An implementation of {@link RequestAuthenticationInterceptor} that injects csp-auth-token in header.
 * This is completely independent of CspApiClient so that it can be USED by CspApiClient as well as any other
 * client that uses CSP authentication (refresh) tokens.
 */
public class CspAuthenticationInterceptor extends RequestAuthenticationInterceptor {

    private static final Logger logger = LogManager.getLogger(CspAuthenticationInterceptor.class);
    private static final long MIN_TOKEN_TTL = TimeUnit.MINUTES.toMillis(5);

    private ReadWriteLock authTokenLock = new ReentrantReadWriteLock();

    private CspAuthenticationHelper cspAuthenticationHelper;
    private String authToken;
    private CspCredModel cspCredModel;
    private List<UriTemplate> noOpUrls; // list of urls that don't require authentication.

    /**
     * Credentials types supported.
     */
    public enum CspCredTypes {
        REFRESH_TOKEN,
        PASSWORD,
        CLIENT_CREDENTIALS,
        CALL_BACK
    }

    /**
     * Class to hold an oAuth client's credentials.
     */
    @Data
    @AllArgsConstructor
    public static class OauthClientCredentials {
        private String clientId;
        private String clientSecret;
        private String orgId;
    }

    /**
     * Class to hold callback info.
     */
    @Data
    @AllArgsConstructor
    public static class CallbackInfo {
        Function<Object, String> getAuthToken;
        Object cbKey;
    }

    /**
     * Convenience class for passing in any CSP auth info.
     * Useful to avoid lots of constructors.
     */
    @Data
    @NoArgsConstructor
    public static class CspCredModel {
        private CspCredTypes credType;
        private String username;
        private String password;
        private String refreshToken;
        private String key;
        private OauthClientCredentials clientCredential;
        private String cspUrl; // CSP endpoint to use to get tokens
        private CallbackInfo cbInfo;
    }

    /**
     * Initialize a CspCredModel with username/password.
     */
    public static CspCredModel initWithUsername(String cspUrl, String username, String password) {
        CspCredModel cspCredModel = new CspCredModel();
        cspCredModel.setCredType(CspCredTypes.PASSWORD);
        cspCredModel.setCspUrl(cspUrl);
        cspCredModel.setUsername(username);
        cspCredModel.setPassword(password);
        return cspCredModel;
    }

    /**
     * Initialize a CspCredModel with refresh token.
     */
    public static CspCredModel initWithRefreshToken(String cspUrl, String refreshToken) {
        CspCredModel cspCredModel = new CspCredModel();
        cspCredModel.setCredType(CspCredTypes.REFRESH_TOKEN);
        cspCredModel.setCspUrl(cspUrl);
        cspCredModel.setRefreshToken(refreshToken);
        return cspCredModel;
    }

    /**
     * Initialize a CspCredModel with callback.
     */
    public static CspCredModel initWithCallback(CallbackInfo cb) {
        CspCredModel cspCredModel = new CspCredModel();
        cspCredModel.setCredType(CspCredTypes.CALL_BACK);
        cspCredModel.setCbInfo(cb);
        cspCredModel.setCspUrl("http://notcsp");
        return cspCredModel;
    }

    /**
     * Create an instance of refresh token based login for csp which injects token automatically for each request.
     * @param cspBaseUrl - csp base url.
     * @param refreshToken -  to use to generate authToken
     */
    public CspAuthenticationInterceptor(String cspBaseUrl, String refreshToken, List<UriTemplate> noOpUrls) {
        checkNotNull(refreshToken, "refreshToken cannot be null");

        cspCredModel = new CspCredModel();
        cspCredModel.setCredType(CspCredTypes.REFRESH_TOKEN);
        cspCredModel.setRefreshToken(refreshToken);
        initCommon(cspBaseUrl, noOpUrls);
    }

    /**
     * Create an instance of username/password login for csp which injects token automatically for each request.
     */
    public CspAuthenticationInterceptor(String cspBaseUrl, String username, String password,
                                        List<UriTemplate> noOpUrls) {
        checkNotNull(username, "username cannot be null");
        checkNotNull(password, "password cannot be null");

        cspCredModel = new CspCredModel();
        cspCredModel.setCredType(CspCredTypes.PASSWORD);
        cspCredModel.setUsername(username);
        cspCredModel.setPassword(password);
        initCommon(cspBaseUrl, noOpUrls);
    }

    /**
     * Create an instance of client_credentials login for csp which injects token automatically for each request.
     * @param cspBaseUrl - CSP's base URL.
     * @param clientCredentials - Client Credentials for the oAuth Client.
     * @param noOpUrls - URLs to ignore - these URLs won't have tokens injected automatically.
     */
    public CspAuthenticationInterceptor(String cspBaseUrl, OauthClientCredentials clientCredentials,
                                        List<UriTemplate> noOpUrls) {
        checkNotNull(clientCredentials, "clientCredentials cannot be null");
        checkNotNull(clientCredentials.clientId, "clientCredentials.clientId cannot be null");
        checkNotNull(clientCredentials.clientSecret, "clientCredentials.clientSecret cannot be null");

        cspCredModel = new CspCredModel();
        cspCredModel.setCredType(CspCredTypes.CLIENT_CREDENTIALS);
        cspCredModel.setClientCredential(clientCredentials);
        initCommon(cspBaseUrl, noOpUrls);
    }

    /**
     * Create an instance using CspCredModel.
     */
    public CspAuthenticationInterceptor(CspCredModel cspCredModel, List<UriTemplate> noOpUrls) {
        this.cspCredModel = cspCredModel;
        initCommon(cspCredModel.getCspUrl(), noOpUrls);
    }

    private void initCommon(String cspBaseUrl, List<UriTemplate> noOpUrls) {
        checkNotNull(cspBaseUrl, "CspBaseUrl cannot be null");
        this.cspAuthenticationHelper = new CspAuthenticationHelper(cspBaseUrl);
        this.noOpUrls = noOpUrls;
    }

    @Override
    protected HttpHeaders getAuthHeaders() {
        HttpHeaders headers = new HttpHeaders();
        String authToken = getAuthToken();
        // Check if authToken valid for a period.
        if (!CspAuthenticationHelper.isTokenValidFor(authToken, MIN_TOKEN_TTL)) {
            logger.debug("Authtoken not valid, retrying");
            // try generating the token again.
            setAuthToken();
            authToken = getAuthToken();
        }
        headers.add(CspConstants.AUTH_HEADER_NAME, authToken);
        return headers;
    }

    @Override
    protected boolean shouldIntercept(HttpRequest request) {
        return noOpUrls.stream().noneMatch(noOpUrl -> noOpUrl.matches(request.getURI().getPath()));
    }

    /**
     * set the refreshToken to a new value. This will trigger a token refresh.
     * @param refreshToken - The new refreshToken.
     */
    public void setNewRefreshToken(String refreshToken) {
        checkNotNull(refreshToken);
        try {
            authTokenLock.writeLock().lock();
            cspCredModel.setRefreshToken(refreshToken);
        } finally {
            authTokenLock.writeLock().unlock();
        }
        setAuthToken();
    }

    private String getAuthToken() {
        try {
            authTokenLock.readLock().lock();
            return authToken;
        } finally {
            authTokenLock.readLock().unlock();
        }
    }

    /**
     * Generate and set auth token based on the credentials.
     */
    private void setAuthToken() {
        String newAuthToken = null;
        try {
            newAuthToken = cspAuthenticationHelper.getClientCredentialsGrant(
                    cspCredModel.getClientCredential().getClientId(),
                    cspCredModel.getClientCredential().getClientSecret(),
                    cspCredModel.getClientCredential().getOrgId());
        } catch (Exception e) {
            logger.warn("Exception while generating auth token credType {} csp server {}: {}: {} ",
                    cspCredModel.getCredType(), cspCredModel.getCspUrl(), e.toString(), e.getMessage());
        }
        try {
            authTokenLock.writeLock().lock();
            this.authToken = newAuthToken;
        } finally {
            authTokenLock.writeLock().unlock();
        }
    }

    /**
     * Helper function to create appropriate interceptor.
     * @param cspCredModel information on type of credential
     */
    public static CspAuthenticationInterceptor createInterceptor(CspCredModel cspCredModel) {
        return new CspAuthenticationInterceptor(cspCredModel, CspAuthenticationHelper.NOLOGURLS);
    }
}
