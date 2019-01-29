/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.restclient.interceptor.retry;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.http.HttpMethod;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.retry.RetryPolicy;
import org.springframework.retry.backoff.BackOffPolicy;
import org.springframework.retry.support.RetryTemplate;

/**
 * Interceptor to retry a request. Retry policy is defined via @{@link RetryTemplate}.
 */
public class DefaultHttpRequestRetryInterceptor extends BaseRetryInterceptor {

    private List<HttpStatus> successStatuses;

    private List<HttpMethod> httpMethods;

    /**
     * Create an instance of a retry interceptor with {@link RetryTemplate}.
     * Will retry for all Server Error status ({@link HttpStatus.Series#is5xxServerError()}
     * and for GET, PUT and OPTIONS http methods.
     * @param retryPolicy The retry policy to apply. {@link RetryPolicy}
     * @param backOffPolicy The back off to apply between retries {@link BackOffPolicy}
     * @see org.springframework.retry.backoff.FixedBackOffPolicy
     * @see org.springframework.retry.backoff.ExponentialBackOffPolicy
     * @see org.springframework.retry.policy.SimpleRetryPolicy
     */
    public DefaultHttpRequestRetryInterceptor(RetryPolicy retryPolicy, BackOffPolicy backOffPolicy) {
        super(retryPolicy, backOffPolicy);
        this.successStatuses =
            Arrays.stream(HttpStatus.values()).filter(s -> !s.is5xxServerError()).collect(Collectors.toList());
        this.httpMethods = new ArrayList<HttpMethod>() {
            {
                add(HttpMethod.OPTIONS);
                add(HttpMethod.GET);
                add(HttpMethod.PUT); // debatable if we throw error
            }

        };
    }

    /**
     * Create an instance of the interceptor with the template and list of statuses that don't require a retry.
     * Retries are enabled from GET,PUT and OPTIONS http methods.
     * @param retryPolicy The retry policy to apply. {@link RetryPolicy}
     * @param backOffPolicy The back off to apply between retries {@link BackOffPolicy}
     * @param successStatuses List of {@link HttpStatus} that is considered a successful response.
     * @see org.springframework.retry.backoff.FixedBackOffPolicy
     * @see org.springframework.retry.backoff.ExponentialBackOffPolicy
     * @see org.springframework.retry.policy.SimpleRetryPolicy
     */
    public DefaultHttpRequestRetryInterceptor(RetryPolicy retryPolicy, BackOffPolicy backOffPolicy,
                                              List<HttpStatus> successStatuses) {
        super(retryPolicy, backOffPolicy);
        this.successStatuses = successStatuses;
        this.httpMethods = new ArrayList<HttpMethod>() {
            {
                add(HttpMethod.OPTIONS);
                add(HttpMethod.GET);
                add(HttpMethod.PUT);
            }
        };
    }

    /**
     * Create an instance of the interceptor with the given options.
     *
     * @param retryPolicy     The retry policy to apply. {@link RetryPolicy}
     * @param backOffPolicy   The back off to apply between retries {@link BackOffPolicy}
     * @param successStatuses List of {@link HttpStatus} that is considered a successful response.
     * @param methodsToRetry  List of {@link HttpMethod}s to retry for.
     * @see org.springframework.retry.backoff.FixedBackOffPolicy
     * @see org.springframework.retry.backoff.ExponentialBackOffPolicy
     * @see org.springframework.retry.policy.SimpleRetryPolicy
     */
    public DefaultHttpRequestRetryInterceptor(RetryPolicy retryPolicy, BackOffPolicy backOffPolicy,
                                              List<HttpStatus> successStatuses, List<HttpMethod> methodsToRetry) {
        super(retryPolicy, backOffPolicy);
        this.successStatuses = successStatuses;
        this.httpMethods = methodsToRetry;
    }

    @Override
    protected boolean isSuccessful(ClientHttpResponse response) throws IOException {
        return this.successStatuses.contains(response.getStatusCode());
    }

    @Override
    protected boolean shouldRetry(HttpRequest request) {
        return httpMethods.contains(request.getMethod());
    }
}
