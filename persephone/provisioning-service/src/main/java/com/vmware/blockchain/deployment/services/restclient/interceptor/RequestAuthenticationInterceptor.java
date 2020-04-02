/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient.interceptor;

import java.io.IOException;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;

/**
 * Intercept and inject authentication headers in a request.
 */
public abstract class RequestAuthenticationInterceptor implements ClientHttpRequestInterceptor {

    /**
     * Intercept the request and add auth headers provided.
     *
     * @param request   - The request that is to be forwarded.
     * @param body      - The request body.
     * @param execution - The request execution that takes the request to the next step.
     * @return - The result of the execution.
     * @throws IOException - If there is an IO error while executing the request.
     */
    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
        throws IOException {
        ClientHttpResponse response = execution.execute(request, body);
        if (authRetryErrorCode(response.getStatusCode())) {
            refreshCredential();
            request.getHeaders().addAll(getAuthHeaders());
            response = execution.execute(request, body);
        }
        return response;
    }

    /**
     * Method to create HttpHeaders with basic authentication for given credentials.
     *
     * @return HttpHeaders with authorization configured
     */
    protected abstract HttpHeaders getAuthHeaders();

    protected abstract boolean authRetryErrorCode(HttpStatus status);

    protected abstract void refreshCredential();
}
