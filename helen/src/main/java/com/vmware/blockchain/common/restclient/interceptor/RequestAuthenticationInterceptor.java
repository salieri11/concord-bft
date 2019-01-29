/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.restclient.interceptor;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.MediaType;
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
        //Check if we need to intercept.
        if (!shouldIntercept(request)) {
            return execution.execute(request, body);
        }
        injectHeaders(request);
        return execution.execute(request, body);
    }

    /**
     * Inject auth headers provided by {@link #getAuthHeaders()}.Specify if headers can be overwritten by overriding
     * {@link #overwriteExisting()}
     *
     * @param request - {@link HttpRequest} that is intercepted.
     */
    private void injectHeaders(HttpRequest request) {
        HttpHeaders headers = request.getHeaders();
        HttpHeaders authHeaders = getAuthHeaders();
        //add default headers
        authHeaders.add(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
        authHeaders.add(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE);
        //Add headers - if overrideExisting is true, the header is replaced.ß
        if (overwriteExisting()) {
            headers.putAll(authHeaders);
        } else {
            for (Map.Entry<String, List<String>> entry : authHeaders.entrySet()) {
                headers.putIfAbsent(entry.getKey(), entry.getValue());
            }
        }
        //Should we add the SS OP ID as a header? Does csp support logging of a correlation id?
    }

    /**
     * Method to create HttpHeaders with basic authentication for given credentials.
     *
     * @return HttpHeaders with authorization configured
     */
    protected abstract HttpHeaders getAuthHeaders();

    /**
     * True, if existing headers can be overridden. (
     */
    protected boolean overwriteExisting() {
        return false;
    }

    /**
     * Check if the request needs to be intercepted.
     * @param request - The {@link HttpRequest} object
     * @return - True if the request should be intercepted.
     */
    protected abstract boolean shouldIntercept(HttpRequest request);
}
