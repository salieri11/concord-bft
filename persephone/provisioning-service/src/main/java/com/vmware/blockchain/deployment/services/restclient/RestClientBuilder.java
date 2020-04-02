/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.retry.RetryPolicy;
import org.springframework.retry.backoff.BackOffPolicy;
import org.springframework.retry.backoff.ExponentialBackOffPolicy;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.util.StringUtils;
import org.springframework.web.client.ResponseErrorHandler;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.vmware.blockchain.deployment.services.restclient.interceptor.RequestAuthenticationInterceptor;
import com.vmware.blockchain.deployment.services.restclient.interceptor.retry.DefaultHttpRequestRetryInterceptor;

/**
 * Builder to create a {@link RestTemplate}.You can build a client with the following
 * <ol>
 * <li>An authetication mechanism, that's an implementation of {@link RequestAuthenticationInterceptor}</li>
 * <li>A base url <b>This would be prepended to all requests.</b></li>
 * <li>A retry policy defined via a {@link RetryPolicy}</li>
 * <li>A backoff policy for retry defined via a {@link BackOffPolicy}</li>
 * <li>A response error handler of type {@link ResponseErrorHandler}</li>
 * <li>An {@link ObjectMapper} for the {@link RestTemplate}</li>
 * <li>A list of {@link ClientHttpRequestInterceptor Request Interceptors}</li>
 * </ol>
 * If these are not provided, they will be defaulted as per {@link #setDefaults()}
 */
public class RestClientBuilder {

    private RetryPolicy retryPolicy;
    private BackOffPolicy backOffPolicy;
    private List<ClientHttpRequestInterceptor> requestInterceptors;
    private ResponseErrorHandler errorHandler;
    private ObjectMapper objectMapper;
    private String baseUrl;
    private RequestAuthenticationInterceptor authenticationInterceptor;
    private ClientHttpRequestFactory requestFactory;
    // Do we include an object mapper or not
    private boolean useMapper;

    public RestClientBuilder() {
        this.requestInterceptors = new ArrayList<>();
        useMapper = true;
    }

    /**
     * Set the csp base url.
     *
     * @param cspUrl Base Url that points to CSP.
     * @return - The builder.
     */
    public RestClientBuilder withBaseUrl(String cspUrl) {
        this.baseUrl = cspUrl;
        return this;
    }

    /**
     * Set the retry policy for the client. The api calls will be wrapped in a {@link RetryTemplate} via the
     * {@link DefaultHttpRequestRetryInterceptor}
     *
     * @param retryPolicy - An instance of a retry policy.
     * @return The builder.
     * @see RetryTemplate
     * @see DefaultHttpRequestRetryInterceptor
     */
    public RestClientBuilder withRetryPolicy(RetryPolicy retryPolicy) {
        this.retryPolicy = retryPolicy;
        return this;
    }

    /**
     * Set the backoff policy for the retry. This will be set on the {@link RetryTemplate} which wraps the api calls.
     *
     * @param backoffPolicy - An instance of {@link BackOffPolicy}
     * @return The builder.
     */
    public RestClientBuilder withBackoffPolicy(BackOffPolicy backoffPolicy) {
        this.backOffPolicy = backoffPolicy;
        return this;
    }

    /**
     * A convenience method to set retries without defining a retry policy. Lets you specify a fixed number of retries
     * before the api call is marked as failed. This method basically creates an instance of {@link SimpleRetryPolicy}
     *
     * @param maxRetries - The number of retries before failing.
     * @return The builder.
     */
    public RestClientBuilder withMaxRetries(int maxRetries) {
        SimpleRetryPolicy retryPolicy = new SimpleRetryPolicy();
        retryPolicy.setMaxAttempts(maxRetries);
        this.retryPolicy = retryPolicy;
        return this;
    }

    /**
     * A convenience method to define a simple backoff policy. This inserts a fixed delay between retries for the
     * specified interval.
     *
     * @param interval - How long to wait between retries.
     * @param intervalUnit - The unit to wait in (ms,seconds,minute etc).
     * @return The builder.
     */
    public RestClientBuilder andRetryEvery(long interval, TimeUnit intervalUnit) {
        FixedBackOffPolicy backOffPolicy = new FixedBackOffPolicy();
        backOffPolicy.setBackOffPeriod(intervalUnit.toMillis(interval));
        this.backOffPolicy = backOffPolicy;
        return this;
    }

    /**
     * Convenience method to define an exponential backoff. Uses default of min 100mS, multiplier 2x.
     *
     * @param maxInterval Maximum to wait
     * @param intervalUnit Unit that maxInterval is defined in.
     */
    public RestClientBuilder andRetryEveryExponential(long maxInterval, TimeUnit intervalUnit) {
        ExponentialBackOffPolicy backOffPolicy = new ExponentialBackOffPolicy();
        backOffPolicy.setMaxInterval(intervalUnit.toMillis(maxInterval));
        this.backOffPolicy = backOffPolicy;
        return this;
    }

    /**
     * Attach an error handler.
     *
     * @param errorHandler - An instance of {@link ResponseErrorHandler}
     * @return This.
     * @see ResponseErrorHandler
     */
    public RestClientBuilder withErrorHandler(ResponseErrorHandler errorHandler) {
        this.errorHandler = errorHandler;
        return this;
    }

    /**
     * Add a request interceptor.
     *
     * @param interceptor - An instance of {@link ClientHttpRequestInterceptor}.
     * @return this.
     */
    public RestClientBuilder withInterceptor(ClientHttpRequestInterceptor interceptor) {
        this.requestInterceptors.add(interceptor);
        return this;
    }

    public RestClientBuilder withAuthentication(
            RequestAuthenticationInterceptor requestAuthenticationInterceptor) {
        this.authenticationInterceptor = requestAuthenticationInterceptor;
        return this;
    }

    public RestClientBuilder withObjectMapper(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
        return this;
    }

    public RestClientBuilder withNoObjectMapper() {
        this.useMapper = false;
        return this;
    }

    public RestClientBuilder withRequestFactory(ClientHttpRequestFactory requestFactory) {
        this.requestFactory = requestFactory;
        return this;
    }

    /**
     * Builds the {@link RestTemplate}.
     *
     * @return RestTemplate.
     */
    public RestTemplate build() {
        setDefaults();
        // The apache request factory is needed to support PATCH operation.
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        // Set the ObjectMapper in the RestTemplate.
        if (useMapper) {
            MappingJackson2HttpMessageConverter jacksonMessageConverter = new MappingJackson2HttpMessageConverter();
            jacksonMessageConverter.setObjectMapper(objectMapper);
            restTemplate.getMessageConverters()
                    .removeIf(m -> m.getClass().equals(MappingJackson2HttpMessageConverter.class));
            restTemplate.getMessageConverters().add(0, jacksonMessageConverter);
        }
        // Add interceptors.

        // Add auth.
        if (authenticationInterceptor != null) {
            restTemplate.getInterceptors().add(0, authenticationInterceptor);
        }

        if (requestInterceptors != null && !requestInterceptors.isEmpty()) {
            restTemplate.getInterceptors().addAll(requestInterceptors);
        }

        if (errorHandler != null) {
            restTemplate.setErrorHandler(errorHandler);
        }
        if (needsRetrySupport()) {
            restTemplate.getInterceptors()
                    .add(new DefaultHttpRequestRetryInterceptor(retryPolicy, backOffPolicy));
        }

        if (!StringUtils.isEmpty(baseUrl)) {
            // set base url
            DefaultUriBuilderFactory uriTemplateHandler = new DefaultUriBuilderFactory(baseUrl);
            restTemplate.setUriTemplateHandler(uriTemplateHandler);
        }

        if (requestFactory != null) {
            restTemplate.setRequestFactory(requestFactory);
        }
        return restTemplate;
    }

    public ObjectMapper getObjectMapper() {
        return objectMapper;
    }

    /**
     * Set default values for optional parameters.
     * <ol>
     * <li>A new instance of {@link ObjectMapper } to {@link #objectMapper}</li>
     * </ol>
     */
    private void setDefaults() {
        if (objectMapper == null) {
            objectMapper = new ObjectMapper();
            objectMapper.registerModule(new JavaTimeModule());
        }
    }


    private boolean needsRetrySupport() {
        return retryPolicy != null && backOffPolicy != null;
    }

}
