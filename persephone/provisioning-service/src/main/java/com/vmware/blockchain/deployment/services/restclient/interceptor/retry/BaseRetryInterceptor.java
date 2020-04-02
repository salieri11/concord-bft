/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient.interceptor.retry;

import java.io.IOException;
import java.net.URI;
import java.util.Objects;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.retry.RetryPolicy;
import org.springframework.retry.backoff.BackOffPolicy;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.restclient.RestClientException;


/**
 * Interceptor to retry a request. Retry policy is defined via @{@link RetryTemplate}.
 */
public abstract class BaseRetryInterceptor implements ClientHttpRequestInterceptor {

    private RetryTemplate retryTemplate;

    private RetryPolicy retryPolicy;

    private final Logger logger = LogManager.getLogger(DefaultHttpRequestRetryInterceptor.class);

    /**
     * Construct a retry interceptor.
     *
     * @param retryPolicy - RetryPolicy
     * @param backOffPolicy - Backoff policy
     */
    public BaseRetryInterceptor(RetryPolicy retryPolicy, BackOffPolicy backOffPolicy) {
        this.retryPolicy = retryPolicy;
        this.retryTemplate = new RetryTemplate();
        this.retryTemplate.setRetryPolicy(retryPolicy);
        this.retryTemplate.setBackOffPolicy(backOffPolicy);
    }

    /**
     * Intercepts the request and retries based on the {@link RetryTemplate}.
     *
     * @param request - Request.
     * @param body - Request body.
     * @param execution - Execution.
     * @return ClientHttpResponse
     * @throws IOException - In case of an IO error.
     */
    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
            throws IOException {
        ClientHttpResponse clientHttpResponse = retryTemplate.execute(context -> {
            logger.debug("intercept for endpoint {} retry {}", request.getURI(), context.getRetryCount());
            ClientHttpResponse response;
            try {
                response = execution.execute(request, body);
            } catch (Exception ex) {
                logger.info("failure on {} status {}", cleanQueryParams(request.getURI()), ex.getMessage());
                throw new RestClientException(ErrorCode.INTERNAL_ERROR);
            }
            if (isSuccessful(response)) {
                // Log only if the call succeeded after retries.
                if (context.getRetryCount() > 0) {
                    logger.info("{} on endpoint {} succeeded after {} retries", request.getMethod(),
                            cleanQueryParams(request.getURI()), context.getRetryCount());
                }
                return response;
            }
            if (retryPolicy.canRetry(context) && shouldRetry(request)) {
                logger.info("{} on endpoint {} failing with {} after {} retries", request.getMethod(),
                        cleanQueryParams(request.getURI()), response.getStatusCode(), context.getRetryCount());
                // closing response releases connection back to pool
                response.close();
                throw new RestClientException(ErrorCode.INTERNAL_ERROR);
            }
            logger.warn("{} on endpoint {} failed permanently after {} retries with status {}, returning last response",
                    request.getMethod(), cleanQueryParams(request.getURI()), context.getRetryCount(),
                    response.getStatusCode());
            return response;
        });
        return clientHttpResponse;
    }

    protected abstract boolean shouldRetry(HttpRequest request);

    protected abstract boolean isSuccessful(ClientHttpResponse response) throws IOException;

    /**
     * Sanitizes the url and removes all query params. For example if the following url
     * <code>/this/is/a/url?nonSensitiveParam1=aValue&nonSensitiveParam2=aValue2&sensitiveParam3=aSensitiveValue</code>
     * needs to be sanitized, the output will be <code>/this/is/a/url?***</code>
     *
     * @param uri - The URI whose query params need to be replaced.
     */
    public static String cleanQueryParams(URI uri) {
        Objects.requireNonNull(uri);
        // Clean any request parameters.
        UriComponentsBuilder clean = UriComponentsBuilder.fromUri(uri);
        // set all query params to null
        clean.replaceQuery("*****");
        return clean.build().toString();
    }
}
