/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.api.client.errorhandler;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.Optional;

import org.slf4j.Logger;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.util.StreamUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.client.DefaultResponseErrorHandler;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.common.csp.api.response.CspErrorResponse;
import com.vmware.blockchain.common.csp.exception.CspApiAuthenticationException;
import com.vmware.blockchain.common.csp.exception.CspApiClientErrorException;
import com.vmware.blockchain.common.csp.exception.CspApiServerErrorException;

/**
 * Custom error handler to handle CSP errors.
 * This will parse error response to {@link CspErrorResponse} object and add it to the exception.
 * The {@link CspErrorResponse} includes more debugging information like requestId that can be used to trace csp logs.
 */
public class CspResponseErrorHandler extends DefaultResponseErrorHandler {

    private final Logger logger = org.slf4j.LoggerFactory.getLogger(DefaultResponseErrorHandler.class);

    // TODO define global mapper
    private ObjectMapper cspMapper;

    /**
     * Default constructor.
     */
    public CspResponseErrorHandler() {
    }

    /**
     * Create an {@link org.springframework.web.client.ResponseErrorHandler} that's aware of {@link CspErrorResponse}.
     * @param cspMapper - A mapper instance that can read csp api responses.
     */
    public CspResponseErrorHandler(ObjectMapper cspMapper) {
        this.cspMapper = cspMapper;
    }

    /**
     * Try and parse the response to see if the body can be parsed as a {@link CspErrorResponse} object.
     * @param response - The {@link ClientHttpResponse} object.
     * @throws IOException - If reading the {@link InputStream} from {@link ClientHttpResponse} fails.
     * @exception IOException - Will contain {@link CspErrorResponse} if the body can be parsed.
     */
    @Override
    public void handleError(ClientHttpResponse response) throws IOException {
        String responseBody = getResponseBodyString(response);
        HttpStatus statusCode = response.getStatusCode();
        HttpHeaders responseHeaders = response.getHeaders();
        switch (statusCode.series()) {
            case CLIENT_ERROR:
                if (HttpStatus.UNAUTHORIZED.equals(statusCode) || HttpStatus.FORBIDDEN
                    .equals(statusCode)) {
                    handleAuthenticationError(statusCode, responseBody, responseHeaders);
                }
                handleClientError(statusCode, responseBody, responseHeaders);
                break;
            case SERVER_ERROR:
                handleServerError(statusCode, responseBody, responseHeaders);
                break;
            default:
                Optional<CspErrorResponse> cspErrorResponse = parseResponse(responseBody, statusCode);
                CspApiClientErrorException exception =
                        cspErrorResponse.isPresent() ? new CspApiClientErrorException(cspErrorResponse.get(),
                                                                                      responseHeaders) :
                        new CspApiClientErrorException(statusCode, responseBody, responseHeaders);
                throw exception;
        }
    }

    private void handleClientError(HttpStatus status, String responseBody, HttpHeaders httpHeaders) {
        Optional<CspErrorResponse> cspErrorResponse = parseResponse(responseBody, status);
        CspApiClientErrorException exception =
                cspErrorResponse.isPresent() ? new CspApiClientErrorException(cspErrorResponse.get(), httpHeaders) :
                new CspApiClientErrorException(status, responseBody, httpHeaders);
        throw exception;
    }

    private void handleServerError(HttpStatus status, String responseBody, HttpHeaders headers) {
        Optional<CspErrorResponse> cspErrorResponse = parseResponse(responseBody, status);
        CspApiServerErrorException exception =
                cspErrorResponse.isPresent() ? new CspApiServerErrorException(cspErrorResponse.get(), headers) :
                new CspApiServerErrorException(status, responseBody, headers);
        throw exception;
    }

    private void handleAuthenticationError(HttpStatus status, String responseBody, HttpHeaders headers) {
        Optional<CspErrorResponse> cspErrorResponse = parseResponse(responseBody, status);
        CspApiAuthenticationException exception =
                cspErrorResponse.isPresent() ? new CspApiAuthenticationException(cspErrorResponse.get(), headers) :
                new CspApiAuthenticationException(status, responseBody, headers);
        throw exception;
    }

    private Optional<CspErrorResponse> parseResponse(String responseBody, HttpStatus status) {
        try {
            if (StringUtils.isEmpty(responseBody)) {
                return Optional.empty();
            }
            CspErrorResponse cspErrorResponse = cspMapper.readValue(responseBody, CspErrorResponse.class);
            cspErrorResponse.setStatusCode(status.value());
            return Optional.of(cspErrorResponse);
        } catch (Exception e) {
            logger.info("Exception while parsing csp error response {} ", responseBody, e);
            return Optional.empty();
        }
    }

    private String getResponseBodyString(ClientHttpResponse response) throws IOException {
        InputStream responseBody = response.getBody();
        try {
            if (responseBody != null) {
                return StreamUtils.copyToString(responseBody, Charset.defaultCharset());
            }
        } catch (Exception e) {
            logger.trace("Exception while reading body for response", e);
        }
        return null;
    }

}