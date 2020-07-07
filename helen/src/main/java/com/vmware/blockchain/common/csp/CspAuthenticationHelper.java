/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.vmware.blockchain.common.csp.CspConstants.CLIENT_CREDENTIALS;
import static com.vmware.blockchain.common.csp.CspConstants.CSP_OAUTH_TOKEN;
import static com.vmware.blockchain.common.csp.CspConstants.GRANT_TYPE;
import static com.vmware.blockchain.common.csp.CspConstants.ORG_ID;


import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.backoff.ExponentialBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.web.util.UriTemplate;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.common.restclient.RestClientBuilder;
import com.vmware.blockchain.common.restclient.interceptor.retry.DefaultHttpRequestRetryInterceptor;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Header;
import io.jsonwebtoken.Jwt;
import io.jsonwebtoken.Jwts;
import lombok.Data;

/**
 * Helper class for dealing with CSP authentication APIs. These are POSTs so require a slightly different retry
 * model.
 * This is completely independent of CspApiClient so that it can be USED by CspApiClient as well as any other
 * client that uses CSP authentication (refresh) tokens.
 */
public class CspAuthenticationHelper {
    private static final Logger logger = LogManager.getLogger(CspAuthenticationHelper.class);

    private RestTemplate restTemplate;


    public static final List<UriTemplate> NOLOGURLS =
            ImmutableList.of(new UriTemplate(CspConstants.CSP_LOGIN_API),
                               new UriTemplate(CspConstants.CSP_LOGIN_API_KEY),
                               new UriTemplate(CspConstants.CSP_VALIDATE_TOKEN));

    /**
     * Holds the response from the csp authorize call.
     */
    @Data
    public static class CspAuthorizeResponse {
        @JsonProperty("access_token")
        String cspAuthToken;
        @JsonProperty("refresh_token")
        String cspRefreshToken;
        @JsonProperty("id_token")
        String idToken;
        @JsonProperty("token_type")
        String tokenType;
        @JsonProperty("expires_in")
        int expiresIn;
        String scope;
    }

    /**
     * Constructor.
     */
    public CspAuthenticationHelper(String cspBaseUrl) {
        checkNotNull(cspBaseUrl, "CspBaseUrl cannot be null");

        // Create a retry interceptor that retries 5xx error on post.  This is typically caused by a slow response
        // from CSP, and goes away on retry.
        SimpleRetryPolicy retryPolicy = new SimpleRetryPolicy(3);
        ExponentialBackOffPolicy backOffPolicy = new ExponentialBackOffPolicy();
        // Start with a two second timeout
        backOffPolicy.setInitialInterval(TimeUnit.SECONDS.toMillis(2));
        backOffPolicy.setMaxInterval(TimeUnit.SECONDS.toMillis(15));
        DefaultHttpRequestRetryInterceptor retry = new DefaultHttpRequestRetryInterceptor(retryPolicy, backOffPolicy,
                Arrays.stream(HttpStatus.values()).filter(s -> !s.is5xxServerError()).collect(Collectors.toList()),
                Collections.singletonList(HttpMethod.POST));

        // Don't gag on unknown
        ObjectMapper om = new ObjectMapper();
        om.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        this.restTemplate = new RestClientBuilder().withBaseUrl(cspBaseUrl)
                .withInterceptor(retry)
                .withObjectMapper(om)
                .build();
    }

    /**
     * Return a restemplate that will retry on POST failure.  Used by TokenRefreshFilter and
     * Oauth2Controller (currently).  Note that this is a little different than the restTemplate
     * used in the common init above.
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
                      Arrays.stream(HttpStatus.values())
                              .filter(s -> !s.is5xxServerError()).collect(Collectors.toList()),
                      Collections.singletonList(HttpMethod.POST));
        return new RestClientBuilder().withInterceptor(retry).build();
    }

    /**
     * Check whether authToken is valid for at least requireTtlMillis.
     * N.B. This does NOT validate token - just parses it and looks at expiration time.
     */
    public static boolean isTokenValidFor(String authToken, long requiredTtlMillis) {

        if (authToken == null) {
            return false;
        }

        try {
            // just decode - not really worried about verification.
            int i = authToken.lastIndexOf('.');
            String withoutSignature = authToken.substring(0, i + 1);
            Jwt<Header, Claims> parsedToken = Jwts.parser().parseClaimsJwt(withoutSignature);
            Date exp = parsedToken.getBody().getExpiration();

            if (exp == null) {
                return false;
            }
            logger.debug("exp {} current {}", exp, new Date(System.currentTimeMillis()));
            if (exp.before(new Date(System.currentTimeMillis() + requiredTtlMillis))) {
                logger.info("token expired at {}", exp);
                return false;
            }
        } catch (Exception ex) {
            // don't really care (might be ExpiredJwtException) or otherwise - try to get a new one
            return false;
        }
        return true;
    }

    /**
     * Login using refresh token.
     */
    public String fetchAuthTokenFromRefreshToken(String token) {
        String loginUri = UriComponentsBuilder.fromUriString(CspConstants.CSP_LOGIN_REFRESH_TOKEN)
                .build().toString();

        HttpHeaders headers = new HttpHeaders();
        headers.add(CspConstants.AUTH_HEADER_NAME, token);
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        String body = String.format("refresh_token=%s", token);
        HttpEntity<String> loginReq = new HttpEntity<>(body, headers);
        ResponseEntity<CspAuthorizeResponse> resp = restTemplate.exchange(loginUri, HttpMethod.POST, loginReq,
                CspAuthorizeResponse.class);
        return resp.getBody().getCspAuthToken();
    }

    /**
     * Generate a token using the client_credentials grant. This is essentially a token that is used to identify a
     * service, instead of a user.
     *
     * @param clientId     - The client_id.
     * @param clientSecret - the secret.
     * @return The client credentials auth token.
     */
    public String getClientCredentialsGrant(String clientId, String clientSecret, String orgId) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        Charset charset = Charset.defaultCharset();
        byte[] clientBasicAuth = Base64.getEncoder().encode(
                (clientId + ":" + clientSecret).getBytes(charset));
        headers.add("Authorization", "Basic " + new String(clientBasicAuth, charset));
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add(GRANT_TYPE, CLIENT_CREDENTIALS);
        formData.add(ORG_ID, orgId);
        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(formData, headers);

        ResponseEntity<CspAuthorizeResponse> cspLoginResponseResponseEntity =
                restTemplate
                        .exchange(CSP_OAUTH_TOKEN, HttpMethod.POST, request, CspAuthorizeResponse.class);
        return cspLoginResponseResponseEntity.getBody().getCspAuthToken();
    }
}
