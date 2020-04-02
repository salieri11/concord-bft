/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient.interceptor;

import static com.google.common.base.Preconditions.checkNotNull;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.util.StreamUtils;
import org.springframework.web.util.UriTemplate;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Intercept and log requests.
 * Ref:http://stackoverflow.com/questions/7952154/
 * spring-resttemplate-how-to-enable-full-debugging-logging-of-requests-responses
 */
public class LoggingInterceptor implements ClientHttpRequestInterceptor {

    private static Logger logger = LogManager.getLogger(LoggingInterceptor.class);
    public static final String BASE_MESSAGE = "The {} request to url {} completed with status {} "
                                              + "and response headers {}";

    private ApiLogger apiLogger;

    private Set<UriTemplate> noOpUrls; // list of urls that don't require authentication.

    private Set<String> queryParamsToRedact;

    private Set<String> headersToLog;

    /**
     * Class to hold request and response per api call.
     */
    @AllArgsConstructor
    @Data
    public static class RequestAndResponse {

        private String uri;
        private HttpRequest request;
        private byte[] requestBody;
        private ClientHttpResponse response;
        private Logger logger;
        private Map<String, String> responseHeaders;
    }

    /**
     * Default constructor. Initializes logging to log URL and
     */
    public LoggingInterceptor() {
        this(ApiLogLevel.URL_STATUS, Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
    }

    /**
     * Create an instance of a logging interceptor with specified log level and list of urls to not log.
     *
     * @param logLevel             - An instance of #ApiLogLevel
     * @param urlsToMaskPathParams - List of UriTemplates for which path params should be masked.
     */
    public LoggingInterceptor(ApiLogLevel logLevel, Set<String> urlsToMaskPathParams,
                              Set<String> queryParamsToRedact, Set<String> responseHeadersToLog) {
        checkNotNull(urlsToMaskPathParams);
        this.apiLogger = logLevel.apiLogger;
        this.noOpUrls = urlsToMaskPathParams.stream().map(url -> new UriTemplate(url)).collect(Collectors.toSet());
        this.queryParamsToRedact = queryParamsToRedact;
        this.headersToLog = responseHeadersToLog;
        logger.info(
                "API logging initialized with custom logging function and no logging urls {} and sensitive "
                + "query params {} ", urlsToMaskPathParams, queryParamsToRedact);
        logger.info(
                "API logging initialized with log level {} and no logging urls {} and sensitive query params {} "
                + "response headers {} ",
                logLevel,
                urlsToMaskPathParams,
                queryParamsToRedact, responseHeadersToLog);
    }

    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
            throws IOException {
        ClientHttpResponse response = execution.execute(request, body);
        //Wrap it in try-catch so that we don't propagate the error.
        try {
            Map<String, String> headers = new HashMap<>();
            //Be defensive!!!
            if (response.getHeaders() != null && headersToLog != null) {
                headers = headersToLog.stream().filter(response.getHeaders()::containsKey)
                        .collect(Collectors.toMap(Function.identity(), response.getHeaders()::getFirst));
            }
            apiLogger
                    .log(new RequestAndResponse(SanitizeUtils.cleanUri(request.getURI(), noOpUrls, queryParamsToRedact),
                                                request, body, response, this.logger, headers));
        } catch (Exception e) {
            logger.info("Unable to log due to error {}", e.getMessage());
        }
        return response;
    }

    /**
     * An enum specifying the verbosity of api logging.
     */
    public enum ApiLogLevel {
        ALL(LoggingInterceptor::logAll),
        URL_STATUS(LoggingInterceptor::logUrlAndStatus),
        URL_STATUS_REQUEST(LoggingInterceptor::logUrlStatusAndRequestBody),
        URL_STATUS_RESPONSE(LoggingInterceptor::logUrlStatusAndResponseBody),
        URL_STATUS_RESPONSE_FAILURE(LoggingInterceptor::logResponseBodyForFailure);
        private ApiLogger apiLogger;

        ApiLogLevel(ApiLogger apiLogger) {
            this.apiLogger = apiLogger;
        }
    }

    private static String getResponseBody(ClientHttpResponse response) throws IOException {
        try {
            return StreamUtils.copyToString(response.getBody(), Charset.defaultCharset());
        } catch (Exception e) {
            logger.debug("Exception while reading stream");
            return "Unreadable Response Stream";
        }
    }

    /**
     * A functional interface to hold the logging function.
     */
    @FunctionalInterface
    public interface ApiLogger {

        /**
         * Common method for all logging patterns.
         */
        void log(RequestAndResponse requestAndResponse);
    }

    /**
     * Logs the URL and status, with the log pattern {@link #BASE_MESSAGE}. Default pattern.
     */
    private static void logUrlAndStatus(RequestAndResponse requestAndResponse) {
        try {
            logger.info(BASE_MESSAGE + ".", requestAndResponse.getRequest().getMethod(),
                        requestAndResponse.getUri(),
                        requestAndResponse.getResponse().getRawStatusCode(),
                        requestAndResponse.getResponseHeaders());
        } catch (IOException e) {
            logIoException(requestAndResponse.getRequest(), e);
        }
    }

    /**
     * Logs the URL status and response body with the log pattern {@link #BASE_MESSAGE}.
     * Please be aware that you might end up logging sensitive stuff. Recommended only for debugging.
     */
    private static void logUrlStatusAndResponseBody(RequestAndResponse requestAndResponse) {
        try {
            logger.info(BASE_MESSAGE + " and response body {}.",
                        requestAndResponse.getRequest().getMethod(),
                        requestAndResponse.getUri(),
                        requestAndResponse.getResponse().getRawStatusCode(),
                        requestAndResponse.getResponseHeaders(),
                        getResponseBody(requestAndResponse.getResponse()));
        } catch (IOException e) {
            logIoException(requestAndResponse.getRequest(), e);
        }
    }

    /**
     * Logs the URL status and request body with the log pattern {@link #BASE_MESSAGE}.
     * Please be aware that you might end up logging sensitive stuff. Recommended only for debugging.
     */
    private static void logUrlStatusAndRequestBody(RequestAndResponse requestAndResponse) {
        try {
            logger.info(BASE_MESSAGE + " with request body {} .",
                        requestAndResponse.getRequest().getMethod(),
                        requestAndResponse.getUri(),
                        requestAndResponse.getResponse().getRawStatusCode(),
                        requestAndResponse.getResponseHeaders(),
                        new String(requestAndResponse.getRequestBody(), Charset.defaultCharset()));
        } catch (IOException e) {
            logIoException(requestAndResponse.getRequest(), e);
        }
    }

    /**
     * Logs the URL status and response body with the log pattern {@link #BASE_MESSAGE} for a client error (4xx)
     * response. Logs only the URL status for all other cases.
     */
    private static void logResponseBodyForFailure(RequestAndResponse requestAndResponse) {
        try {
            if (requestAndResponse.getResponse().getStatusCode().is4xxClientError()) {
                logger.info(BASE_MESSAGE + " and response body {}.",
                            requestAndResponse.getRequest().getMethod(),
                            requestAndResponse.getUri(),
                            requestAndResponse.getResponse().getRawStatusCode(),
                            requestAndResponse.getResponseHeaders(),
                            getResponseBody(requestAndResponse.getResponse()));
            } else {
                logger.info(BASE_MESSAGE + ".", requestAndResponse.getRequest().getMethod(),
                            requestAndResponse.getUri(),
                            requestAndResponse.getResponse().getRawStatusCode(),
                            requestAndResponse.getResponseHeaders());
            }
        } catch (IOException e) {
            logIoException(requestAndResponse.getRequest(), e);
        }
    }

    /**
     * Logs the URL, status,request body and response body with the log pattern {@link #BASE_MESSAGE}.
     * Please be aware that you might end up logging sensitive stuff. Recommended only for debugging.
     * @param requestAndResponse - The request and response.
     */
    private static void logAll(RequestAndResponse requestAndResponse) {
        try {
            logger.info(BASE_MESSAGE + " with request body {} and response body {}.",
                        requestAndResponse.getRequest().getMethod(),
                        requestAndResponse.getUri(),
                        requestAndResponse.getResponse().getRawStatusCode(),
                        requestAndResponse.getResponseHeaders(),
                        new String(requestAndResponse.getRequestBody(), Charset.defaultCharset()),
                        getResponseBody(requestAndResponse.getResponse()));
        } catch (IOException e) {
            logIoException(requestAndResponse.getRequest(), e);
        }
    }

    private static void logIoException(HttpRequest request, IOException e) {
        logger.info(BASE_MESSAGE + "failed with IO Error", request.getMethod(), request.getURI(), e);
    }

    /**
     * Update the log level of this logger, dynamically.
     * @param logLevel - The new log level.
     */
    public void updateLogLevel(ApiLogLevel logLevel) {
        this.apiLogger = logLevel.apiLogger;
    }

}