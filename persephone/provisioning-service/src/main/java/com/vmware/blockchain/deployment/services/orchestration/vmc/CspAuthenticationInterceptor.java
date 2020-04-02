/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vmc;

import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.google.common.collect.ImmutableSet;
import com.vmware.blockchain.deployment.services.orchestration.model.vmc.VmcOnAwsData;
import com.vmware.blockchain.deployment.services.restclient.RestClientBuilder;
import com.vmware.blockchain.deployment.services.restclient.RestClientUtils;
import com.vmware.blockchain.deployment.services.restclient.interceptor.LoggingInterceptor;
import com.vmware.blockchain.deployment.services.restclient.interceptor.RequestAuthenticationInterceptor;
import com.vmware.blockchain.deployment.services.restclient.interceptor.retry.DefaultHttpRequestRetryInterceptor;

import lombok.extern.slf4j.Slf4j;

/**
 * An implementation of {@link RequestAuthenticationInterceptor} that injects csp-auth-token in header.
 * This is completely independent of CspApiClient so that it can be USED by CspApiClient as well as any other
 * client that uses CSP authentication (refresh) tokens.
 */
@Slf4j
public class CspAuthenticationInterceptor extends RequestAuthenticationInterceptor {

    private ReadWriteLock authTokenLock = new ReentrantReadWriteLock();

    private String authToken;

    private RestTemplate cspRestTemplate;
    private String cspUrl;
    private String refreshToken;

    /**
     * Constructor.
     */
    public CspAuthenticationInterceptor(String cspUrl, String refreshToken) {
        this.cspUrl = cspUrl;
        this.refreshToken = refreshToken;

        LoggingInterceptor loggingInterceptor = new LoggingInterceptor(
                LoggingInterceptor.ApiLogLevel.URL_STATUS_RESPONSE_FAILURE,
                ImmutableSet.of(VmcEndpoints.VMC_AUTHENTICATION.getPath()),
                ImmutableSet.of("refresh_token", "token", "client_id", "client_secret", "csp-auth-token"),
                ImmutableSet.of());

        this.cspRestTemplate = new RestClientBuilder().withBaseUrl(cspUrl)
                .withInterceptor(DefaultHttpRequestRetryInterceptor.getDefaultInstance())
                .withInterceptor(loggingInterceptor)
                .withObjectMapper(RestClientUtils.getDefaultMapper())
                .build();

        refreshCredential();
    }

    @Override
    protected HttpHeaders getAuthHeaders() {
        HttpHeaders headers = new HttpHeaders();
        String authToken = getAuthToken();
        headers.add("csp-auth-token", authToken);
        return headers;
    }

    @Override
    protected boolean authRetryErrorCode(HttpStatus status) {
        return status == HttpStatus.UNAUTHORIZED || status == HttpStatus.FORBIDDEN;
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
    @Override
    public void refreshCredential() {
        String newAuthToken = null;
        try {
            String loginUri = VmcEndpoints.VMC_AUTHENTICATION.getPath();

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
            String body = String.format("refresh_token=%s", refreshToken);
            HttpEntity<String> loginReq = new HttpEntity<>(body, headers);

            ResponseEntity<VmcOnAwsData.VmcAuthenticationResponse> resp =
                    cspRestTemplate.exchange(loginUri, HttpMethod.POST, loginReq,
                                             VmcOnAwsData.VmcAuthenticationResponse.class);
            newAuthToken = resp.getBody().getAccessToken();

        } catch (Exception e) {
            log.warn("Exception while generating auth token csp server {}: {}: {} ",
                        cspUrl, e.toString(), e.getMessage());
        }
        try {
            authTokenLock.writeLock().lock();
            this.authToken = newAuthToken;
        } finally {
            authTokenLock.writeLock().unlock();
        }
    }
}
