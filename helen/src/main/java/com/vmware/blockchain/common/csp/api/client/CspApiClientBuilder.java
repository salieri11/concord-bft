/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.api.client;

import static com.vmware.blockchain.common.csp.CspConstants.CSP_LOGIN_API;
import static com.vmware.blockchain.common.csp.CspConstants.CSP_LOGIN_API_KEY;
import static com.vmware.blockchain.common.csp.CspConstants.CSP_LOGIN_REFRESH_TOKEN;
import static com.vmware.blockchain.common.csp.CspConstants.CSP_OAUTH_TOKEN;

import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.springframework.http.client.BufferingClientHttpRequestFactory;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.ResponseErrorHandler;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriTemplate;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.vmware.blockchain.common.csp.CspAuthenticationInterceptor;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.api.client.errorhandler.CspResponseErrorHandler;
import com.vmware.blockchain.common.restclient.RestClientBuilder;
import com.vmware.blockchain.common.restclient.interceptor.LoggingInterceptor;

/**
 * Builder to create a {@link CspApiClient}. To build a client, at least cspBaseUrl is required.
 * The following defaults are applied when building, if not overridden.
 * <ol>
 *  <li>
 *      An authentication mechanism. Currently inject auth token via {@link CspAuthenticationInterceptor}.
 *  </li>
 *  <li>
 *      Default {@link ObjectMapper} for csp.
 *  </li>
 *  <li>
 *      Default CSP ErrorHandler @{@link CspResponseErrorHandler}
 *  </li>
 * </ol>
 */
public class CspApiClientBuilder {

    private RestClientBuilder builder;

    private String cspBaseUrl;

    private ObjectMapper objectMapper;

    private ResponseErrorHandler errorHandler;

    private CspAuthenticationInterceptor serviceOwnerAuthProvider;

    private CspAuthenticationInterceptor clientCredentialsAuthProvider;

    /**
     * Start building a client with the csp base url.
     * @param cspBaseUrl - The base url for csp api. DO NOT INCLUDE /csp/gateway.
     */
    public CspApiClientBuilder(String cspBaseUrl) {
        this.cspBaseUrl = cspBaseUrl;
        builder = new RestClientBuilder().withBaseUrl(cspBaseUrl);
        this.objectMapper = new ObjectMapper();
        this.objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        this.errorHandler = new CspResponseErrorHandler(objectMapper);
    }

    /**
     * Enable refresh token based auth (vidm).
     * TODO - shouldn't need these List of APIs to not inject - those should never be used. Verify.
     *
     * @param refreshToken - The refresh token for a CSP account.
     * @return the builder.
     */
    public CspApiClientBuilder withRefreshTokenAuth(String refreshToken) {
        this.serviceOwnerAuthProvider =
                new CspAuthenticationInterceptor(cspBaseUrl, refreshToken,
                                                 ImmutableList.of(new UriTemplate(CSP_LOGIN_API),
                                                                  new UriTemplate(CSP_LOGIN_API_KEY)));
        return this;
    }


    /**
     * Enable authentication with client credentials auth.
     * @param clientId - The client id.
     * @param clientSecret - The client secret.
     * @return this builder.
     */
    public CspApiClientBuilder withClientCredentialsAuth(String clientId, String clientSecret, String orgId) {
        this.clientCredentialsAuthProvider =
                new CspAuthenticationInterceptor(
                        cspBaseUrl,
                        new CspAuthenticationInterceptor.OauthClientCredentials(clientId, clientSecret, orgId),
                        ImmutableList.of(new UriTemplate(CSP_LOGIN_API),
                                         new UriTemplate(CSP_LOGIN_API_KEY)));
        return this;
    }

    /**
     * Enables request and response logging.
     * @see LoggingInterceptor
     * @return the builder.
     */
    public CspApiClientBuilder enableRequestResponseLogging() {
        return enableLogging(LoggingInterceptor.ApiLogLevel.URL_STATUS);
    }

    /**
     * Enable logging within the log level specified. This masks the following URLs' path params.
     * <ul>
     *      <li>CSP_LOGIN_API</li>
     *      <li>CSP_LOGIN_API_KEY</li>
     *      <li>CSP_LOGIN_CLIENT_CREDENTIALS</li>
     *      <li>CSP_LOGIN_REFRESH_TOKEN</li>
     *      <li>CSP_VALIDATE_TOKEN</li>
     * </ul>
     * And the following query parameters won't be printed
     * <ul>
     *      <li>refresh_token</li>
     *      <li>token</li>
     *      <li>client_id</li>
     *      <li>client_secret</li>
     * </ul>
     * Also look at {@link #enableLogging(LoggingInterceptor)} and
     * {@link LoggingInterceptor#LoggingInterceptor(LoggingInterceptor.ApiLogLevel, Set, Set)}if you need more
     * customized logging.
     * @param logLevel The log level to be used when logging.
     */
    public CspApiClientBuilder enableLogging(LoggingInterceptor.ApiLogLevel logLevel) {
        builder.withInterceptor(
                new LoggingInterceptor(
                        logLevel,
                        ImmutableSet.of(
                                CSP_LOGIN_API,
                                CSP_LOGIN_API_KEY,
                                CSP_OAUTH_TOKEN,
                                CSP_LOGIN_REFRESH_TOKEN),
                        ImmutableSet.of("refresh_token", "token", "client_id", "client_secret"), ImmutableSet.of(
                        CspConstants.HEADER_CSP_REQUEST_ID)));
        builder.withRequestFactory(new BufferingClientHttpRequestFactory(new HttpComponentsClientHttpRequestFactory()));
        return this;
    }


    /**
     * Enable logging with the logging interceptor specified.
     *
     * @param logger - an instance of {@link LoggingInterceptor}
     */
    public CspApiClientBuilder enableLogging(LoggingInterceptor logger) {
        builder.withInterceptor(logger);
        builder.withRequestFactory(new BufferingClientHttpRequestFactory(new HttpComponentsClientHttpRequestFactory()));
        return this;
    }

    /**
     * Enable automatic retry of requests for non 2xx statuses.
     * @param times - No. of times to retry.
     * @param maxInterval - maximum time for exponential backoff.
     * @param timeUnit - Unit of maxInterval.
     * @return the builder.
     */
    public CspApiClientBuilder enableRetry(int times, int maxInterval, TimeUnit timeUnit) {
        builder.withMaxRetries(times).andRetryEveryExponential(maxInterval, timeUnit);
        return this;
    }

    /**
     * Specify a custom {@link ObjectMapper} if needed.
     * @param objectMapper an instance of {@link ObjectMapper}
     * @return the builder.
     */
    public CspApiClientBuilder withObjectMapper(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
        return this;
    }

    /**
     * Override and create a client with the {@link RestTemplate} provided.
     * Use {@link RestClientBuilder} if needed.
     *
     * @return an instance of {@link CspApiClient}.
     */
    @Deprecated
    public CspApiClient withRestTemplate(RestTemplate restTemplate) {
        return new CspApiClient(restTemplate);
    }

    /**
     * Builds the client with specified options. The following defaults are applied if not overridden.
     * <ol>
     *     <li>An instance of {@link CspResponseErrorHandler}</li>
     *     <li>Default {@link ObjectMapper}</li>
     * </ol>
     * @return an Instance of {@link CspApiClient}.
     */
    public CspApiClient build() {
        builder.withObjectMapper(objectMapper).withErrorHandler(errorHandler);
        return new CspApiClient(builder, serviceOwnerAuthProvider, clientCredentialsAuthProvider);
    }

    /**
     * Add an interceptor.
     * @param interceptor - The interceptor to add to the chain.
     * @return - the builder.
     */
    public CspApiClientBuilder withInterceptor(ClientHttpRequestInterceptor interceptor) {
        builder.withInterceptor(interceptor);
        return this;
    }
}
