/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import java.security.KeyStore;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import com.google.common.collect.ImmutableSet;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorUtils;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.VSphereAuthenticationResponse;
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
public class VsphereSessionAuthenticationInterceptor extends RequestAuthenticationInterceptor {

    private ReadWriteLock authTokenLock = new ReentrantReadWriteLock();

    private String authToken;

    private RestTemplate vsphereRestTemplate;
    private String vsphereUrl;
    private String username;
    private String password;

    /**
     * Constructor.
     */
    public VsphereSessionAuthenticationInterceptor(String vsphereUrl, String username, String password,
                                                   Boolean useSelfSignedCertForVSphere,
                                                   KeyStore selfSignedCertKeyStore) {
        this.vsphereUrl = vsphereUrl;
        this.username = username;
        this.password = password;

        LoggingInterceptor loggingInterceptor = new LoggingInterceptor(
                LoggingInterceptor.ApiLogLevel.URL_STATUS_RESPONSE_FAILURE,
                ImmutableSet.of(),
                ImmutableSet.of("refresh_token", "token", "client_id", "client_secret", "csp-auth-token"),
                ImmutableSet.of());

        RestClientBuilder authRestClientBuilder = new RestClientBuilder();

        if (useSelfSignedCertForVSphere) {
            HttpComponentsClientHttpRequestFactory factory = OrchestratorUtils
                    .getHttpRequestFactoryGivenKeyStore(selfSignedCertKeyStore);

            // Utilizes above created factory using the selfSignedCertKeyStore
            authRestClientBuilder = authRestClientBuilder.withRequestFactory(factory);
        }

        this.vsphereRestTemplate = authRestClientBuilder.withBaseUrl(vsphereUrl)
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
        headers.add("vmware-api-session-id", authToken);
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
            String loginUri = VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath();

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.setBasicAuth(username, password);

            HttpEntity<String> loginReq = new HttpEntity<>(headers);
            ResponseEntity<VSphereAuthenticationResponse> resp =
                    vsphereRestTemplate.exchange(loginUri, HttpMethod.POST, loginReq,
                                          VSphereAuthenticationResponse.class);
            newAuthToken = resp.getBody().getValue();

        } catch (Exception e) {
            log.warn("Exception while generating auth token csp server {}: {}: {} ",
                        vsphereUrl, e.toString(), e.getMessage());
        }
        try {
            authTokenLock.writeLock().lock();
            this.authToken = newAuthToken;
        } finally {
            authTokenLock.writeLock().unlock();
        }
    }
}
