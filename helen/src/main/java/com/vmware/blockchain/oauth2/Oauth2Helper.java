/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.backoff.ExponentialBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.google.common.base.Strings;
import com.vmware.blockchain.common.restclient.RestClientBuilder;
import com.vmware.blockchain.common.restclient.interceptor.retry.DefaultHttpRequestRetryInterceptor;


/**
 * This class is responsible to handle the OAuth2 Authorization code and Auth Token requests. Motive to create this
 * class that any consumer for Oauth2 requests can use this common jar
 *
 */
public class Oauth2Helper {

    private static Logger logger = LogManager.getLogger(Oauth2Helper.class);
    private static final RestTemplate restTemplate = getOauthRestTemplate(3, TimeUnit.SECONDS.toMillis(1));

    /**
     * Common helper method which can be used by any consumer to get the authorization code which internally redirects
     * to the discovery page.
     *
     * @param request HttpServletRequest
     * @param response HttpServletResponse
     * @param oauth2AuthCodeRequestParams authorization code parameters which are required to construct the redirect url
     * @throws RuntimeException redirect failure
     */
    public void handleAuthorizationCodeRequest(HttpServletRequest request, HttpServletResponse response,
            Oauth2AuthCodeRequestParams oauth2AuthCodeRequestParams) throws RuntimeException {
        String redirectUri;
        if (oauth2AuthCodeRequestParams.isSessionCleanRequired()) {
            redirectUri = UriComponentsBuilder.fromUriString(request.getRequestURI()).query(request.getQueryString())
                    .queryParam(Oauth2CommonUtility.SESSION_CLEANED, "true").build().toUriString();
            logger.info("Clearing session {}", redirectUri);

        } else {
            UriComponentsBuilder builder = UriComponentsBuilder.fromUriString(oauth2AuthCodeRequestParams.getBaseUrl())
                    .queryParam("client_id", oauth2AuthCodeRequestParams.getClientId())
                    .queryParam("redirect_uri", oauth2AuthCodeRequestParams.getRedirectUri())
                    .queryParam("state", oauth2AuthCodeRequestParams.getState());
            String relativePath = oauth2AuthCodeRequestParams.getRelativePath();
            if (relativePath != null && relativePath.length() > 0) {
                builder.path(relativePath);
            }
            String orgId = oauth2AuthCodeRequestParams.getOrgId();
            if (orgId != null && orgId.length() > 0) {
                builder = builder.queryParam("org_link", Oauth2CommonUtility.CSP_ORG_API + "/" + orgId);
            }
            Map<String, List<String>> extraQueryParams = oauth2AuthCodeRequestParams.getExtraQueryParams();
            if (extraQueryParams != null) {
                for (Map.Entry<String, List<String>> extraQueryParam : extraQueryParams.entrySet()) {
                    for (String value : extraQueryParam.getValue()) {
                        builder = builder.queryParam(extraQueryParam.getKey(), value);
                    }
                }
            }
            redirectUri = builder.build().toUriString();
            logger.info("login redirect.  Discovery page {}, state {}", redirectUri,
                    oauth2AuthCodeRequestParams.getState());
        }
        try {
            response.sendRedirect(redirectUri);
        } catch (IOException e) {
            throw new RuntimeException("Generating Authorization code has an exception with redirect failure", e);
        }

    }

    /**
     * Common helper method which can be used by any consumer to get the auth token using the clientId & clientSecret.
     *
     * @param request HttpServletRequest
     * @param response HttpServletResponse
     * @param oauth2AuthTokenRequestParams parameters required to construct the post URL.
     * @return Oauth2CommonUtility.Oauth2AuthTokenResponse response which contains the required tokens
     * @throws RuntimeException Authorization Token failure
     */
    public ResponseEntity<Oauth2AuthTokenResponse> handleAuthTokenRequest(
            HttpServletRequest request, HttpServletResponse response,
            Oauth2AuthTokenRequestParams oauth2AuthTokenRequestParams) throws RuntimeException {
        // First, verify that the state has a valid value, and error out if not
        // The state contains a key we need to retrieve
        String state = oauth2AuthTokenRequestParams.getState();
        String code = oauth2AuthTokenRequestParams.getAuthorizationCode();
        logger.info("Oauth callback, code {} state {}", code, state);
        HttpSession session = request.getSession(false);
        // if we don't have a session or the state stored in the session is different
        // than the presented, throw an oauth error
        if (session == null || !Strings.nullToEmpty(oauth2AuthTokenRequestParams.getState())
                .equals((String) session.getAttribute(Oauth2CommonUtility.STATE_KEY))) {
            logger.error("Invalid state {}", state);
            throw new RuntimeException("Invalid Auth state" + state);
        }

        // headers for rest call: csp-auth-token: <token> Content-Type: application/json
        HttpHeaders headers = new HttpHeaders();;
        String clientId = oauth2AuthTokenRequestParams.getClientId();
        String clientSecret = oauth2AuthTokenRequestParams.getClientSecret();
        if (Strings.nullToEmpty(clientId).length() > 0 && Strings.nullToEmpty(clientSecret).length() > 0) {
            headers = getBasicAuthHeader(clientId, clientSecret);
        } else {
            String authenticationHeader = oauth2AuthTokenRequestParams.getAuthenticationHeader();
            if (Strings.nullToEmpty(authenticationHeader).length() > 0) {
                headers = getBasicAuthHeader(authenticationHeader);
            }
        }
        headers.setContentType(oauth2AuthTokenRequestParams.getMediaType());

        HttpMethod method = HttpMethod.POST;

        // Build the URL to exchange the code
        UriComponentsBuilder builder = UriComponentsBuilder.fromUriString(oauth2AuthTokenRequestParams.getBaseUrl());
        String relativePath = oauth2AuthTokenRequestParams.getRelativePath();
        if (relativePath != null && relativePath.length() > 0) {
            builder.path(relativePath);
        }
        Map<String, List<String>> extraQueryParams = oauth2AuthTokenRequestParams.getExtraQueryParams();
        if (extraQueryParams != null) {
            for (Map.Entry<String, List<String>> extraQueryParam : extraQueryParams.entrySet()) {
                for (String value : extraQueryParam.getValue()) {
                    builder = builder.queryParam(extraQueryParam.getKey(), value);
                }
            }
        }

        // build the query params string. Substring(1) at end is remove the ? at the start of the string
        String queryParams = UriComponentsBuilder.newInstance()
                .queryParam("grant_type", oauth2AuthTokenRequestParams.getGrantType()).queryParam("code", code)
                .queryParam("state", state).queryParam("redirect_uri", oauth2AuthTokenRequestParams.getRedirectUri())
                .build().toString().substring(1);

        // In the new form of calling, the header is ContentType: application/x-www-form-urlencoded
        // And the body is in form format (key=value&key=value...)

        String body = queryParams;

        URI tokenUrl = builder.build().toUri();

        HttpEntity<String> tokenReq = new HttpEntity<>(body, headers);

        try {
            // exchange the code for a JWT token
            return restTemplate.exchange(tokenUrl, method, tokenReq, Oauth2AuthTokenResponse.class);
        } catch (HttpClientErrorException e) {
            String errorMessage = "Exception while exchanging token with csp with for code {} and state {}. "
                                  + "Csp responded with status {} ";
            logger.error(errorMessage + " and body {}", code, state, e.getStatusCode(), e.getResponseBodyAsString());
            throw new RuntimeException("Generating Authorization Token has an exception", e);
        }
    }

    /**
     * Handles the refresh token request.
     *
     * @param refreshTokenParams parameters required to handle the refresh token request.
     * @throws IOException Refresh Token failure.
     * @throws ServletException Refresh Token failure.
     */
    public Oauth2AuthTokenResponse handleRefreshTokenFilterRequest(
            Oauth2RefreshTokenRequestParams refreshTokenParams)
            throws IOException, ServletException, HttpClientErrorException {
        long now = System.currentTimeMillis();

        // some clients want to refresh the token before the expiry time by some amount.
        long refreshBeforeTime = refreshTokenParams.getRefreshBefore();
        long tokenExpiresAt = refreshTokenParams.getTokenExpiresAt();
        if (refreshBeforeTime > 0) {
            tokenExpiresAt = tokenExpiresAt - refreshBeforeTime;
        }
        if (now > tokenExpiresAt) {
            // headers for rest call: Content-Type: application/json
            HttpHeaders headers = new HttpHeaders();;
            String clientId = refreshTokenParams.getClientId();
            String clientSecret = refreshTokenParams.getClientSecret();
            if (Strings.nullToEmpty(clientId).length() > 0 && Strings.nullToEmpty(clientSecret).length() > 0) {
                headers = getBasicAuthHeader(clientId, clientSecret);
            } else {
                String authenticationHeader = refreshTokenParams.getAuthenticationHeader();
                if (Strings.nullToEmpty(authenticationHeader).length() > 0) {
                    headers = getBasicAuthHeader(authenticationHeader);
                }
            }

            // start the uri builder and the body
            UriComponentsBuilder builder = UriComponentsBuilder.fromUriString(refreshTokenParams.getBaseUrl())
                    .path(refreshTokenParams.getRelativePath());
            // build the query params string. Substring(1) at end is remove the ? at the start of the string
            String queryParams = UriComponentsBuilder.newInstance()
                    .queryParam("grant_type", refreshTokenParams.getGrantType())
                    .queryParam("refresh_token", refreshTokenParams.getRefreshToken()).build().toString().substring(1);
            headers.setContentType(refreshTokenParams.getMediaType());

            HttpMethod method = HttpMethod.POST;
            HttpEntity<String> tokenReq = new HttpEntity<>(queryParams, headers);
            URI tokenUrl = builder.build().toUri();
            try {
                // exchange the code for a JWT token
                ResponseEntity<Oauth2AuthTokenResponse> tokenResponse =
                        restTemplate.exchange(tokenUrl, method, tokenReq, Oauth2AuthTokenResponse.class);
                return tokenResponse.getBody();
            } catch (HttpClientErrorException e) {
                throw e;
            }

        }
        return null;
    }

    /**
     * Return a restemplate that will retry on POST failure. Used by TokenRefreshFilter and Oauth2Controller
     * (currently). Note that this is a little different than the restTemplate used in the common init above.
     *
     * @param retries How many times to retry
     * @param millisToWait How many millis to wait until you retry again
     * @return restTemplate
     */
    public static RestTemplate getOauthRestTemplate(int retries, long millisToWait) {

        // This is a little different than the restTemplate used in common
        SimpleRetryPolicy retryPolicy = new SimpleRetryPolicy(retries);
        ExponentialBackOffPolicy backOffPolicy = new ExponentialBackOffPolicy();
        backOffPolicy.setMaxInterval(millisToWait);
        DefaultHttpRequestRetryInterceptor retry =
                new DefaultHttpRequestRetryInterceptor(retryPolicy, backOffPolicy,
                          Arrays.stream(HttpStatus.values()).filter(s -> !s.is5xxServerError()).collect(
                                  Collectors.toList()),
                          Collections.singletonList(
                                  HttpMethod.POST));
        return new RestClientBuilder().withInterceptor(retry).build();
    }

    /**
     * Method to create HttpHeaders with basic authentication for given credentials.
     *
     * @param username for basic authentication
     * @param password for basic authentication
     * @return HttpHeaders with authorization configured
     */
    private HttpHeaders getBasicAuthHeader(final String username, final String password) {

        String usernameAndPassword = new StringBuffer(username).append(":").append(password).toString();
        String authHeader = new StringBuffer("Basic ").append(
                java.util.Base64.getEncoder().encodeToString(usernameAndPassword.getBytes(StandardCharsets.UTF_8)))
                .toString();
        return getBasicAuthHeader(authHeader);
    }

    /**
     * Method to create HttpHeaders with basic authentication for given credentials.
     *
     * @param authHeader which is the combination of clientId+clientSecret
     * @return HttpHeaders with authorization configured
     */
    private HttpHeaders getBasicAuthHeader(String authHeader) {

        HttpHeaders requestHeaders = new HttpHeaders();
        requestHeaders.add(HttpHeaders.AUTHORIZATION, authHeader);
        return requestHeaders;
    }
}
