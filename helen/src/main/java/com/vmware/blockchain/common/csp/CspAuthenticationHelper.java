/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.backoff.ExponentialBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
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
    private static final Logger logger = LoggerFactory.getLogger(CspAuthenticationHelper.class);

    private RestTemplate restTemplate;

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
     * Check whether authToken is valid for at least requireTtlMillis.
     * N.B. This does NOT validate token - just parses it and looks at expiration time.
     */
    public boolean isTokenValidFor(String authToken, long requiredTtlMillis) {

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


}
