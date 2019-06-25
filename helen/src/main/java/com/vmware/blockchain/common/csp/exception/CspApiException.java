/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.exception;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.api.response.CspErrorResponse;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Exception class to hold any api exceptions from CSP.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class CspApiException extends CspException {

    /**
     * Raw response data.
     */
    private HttpStatus httpStatus;
    private String httpBody;

    /**
     * Parsed error data, if any.
     */
    private String cspErrorCode;
    private String failureMessage;
    private CspErrorResponse cspErrorResponse;
    private String cspRequestId;
    private HttpHeaders responseHeaders;

    private static final String ERROR_MESSAGE = "The request failed with http status %s and response body %s";

    private static final String MESSAGE_FORMAT =
            "The csp api call failed with http status %s and csp failure message %s";
    private static final String MESSAGE_FORMAT_ERROR_CODE = MESSAGE_FORMAT + " and errorCode %s";

    private static final String MESSAGE_FORMAT_ERROR_REQUEST_ID = MESSAGE_FORMAT + " and csp-request-id %s";

    /**
     * Create an exception from csp error.
     *
     * @param httpStatus     - Http Status.
     * @param cspErrorCode   - ErrorCode from csp.
     * @param failureMessage ErrorMessage returned from csp.
     * @deprecated Use {@link #CspApiException(HttpStatus, String, String, HttpHeaders)}
     */
    @Deprecated
    public CspApiException(HttpStatus httpStatus, String cspErrorCode, String failureMessage) {
        this(httpStatus, cspErrorCode, failureMessage, null);
    }

    /**
     * Create an exception from csp error.
     *
     * @param httpStatus     - Http Status.
     * @param cspErrorCode   - ErrorCode from csp.
     * @param failureMessage ErrorMessage returned from csp.
     */
    public CspApiException(HttpStatus httpStatus, String cspErrorCode, String failureMessage,
                           HttpHeaders responseHeaders) {
        super(String.format(MESSAGE_FORMAT_ERROR_CODE, httpStatus.value(), failureMessage, cspErrorCode));
        this.httpStatus = httpStatus;
        this.cspErrorCode = cspErrorCode;
        this.failureMessage = failureMessage;
        processHeaders(responseHeaders);
    }

    /**
     * Overloaded constructor.
     *
     * @param httpStatus     - Http Status.
     * @param failureMessage - Error message indicating the failure.
     * @param e              - Exception that needs to be propagated.
     */
    @Deprecated
    public CspApiException(HttpStatus httpStatus, String failureMessage, Throwable e) {
        this(httpStatus, failureMessage, e, null);
    }

    /**
     * Overloaded constructor.
     *
     * @param httpStatus     - Http Status.
     * @param failureMessage - Error message indicating the failure.
     * @param e              - Exception that needs to be propagated.
     */
    public CspApiException(HttpStatus httpStatus, String failureMessage, Throwable e, HttpHeaders responseHeaders) {
        super(String.format(MESSAGE_FORMAT, httpStatus.value(), failureMessage), e);
        this.httpStatus = httpStatus;
        processHeaders(responseHeaders);
    }

    /**
     * Create an api error from csp error response.
     * @param cspErrorResponse Csp error response returned.
     * @deprecated - use {@link #CspApiException(HttpStatus, String, Throwable, HttpHeaders)} instead.
     */
    @Deprecated
    public CspApiException(CspErrorResponse cspErrorResponse) {
        this(cspErrorResponse, null);
    }

    /**
     * Create an api error from csp error response.
     *
     * @param cspErrorResponse Csp error response returned.
     * @param responseHeaders  - Http response headers.
     */
    public CspApiException(CspErrorResponse cspErrorResponse, HttpHeaders responseHeaders) {
        super(String.format(MESSAGE_FORMAT_ERROR_REQUEST_ID, cspErrorResponse.getStatusCode(),
                            cspErrorResponse.getMessage(),
                            cspErrorResponse.getRequestId()));
        this.httpStatus = HttpStatus.valueOf(cspErrorResponse.getStatusCode());
        this.cspErrorCode = cspErrorResponse.getError();
        this.failureMessage = cspErrorResponse.getMessage();
        this.cspErrorResponse = cspErrorResponse;
        processHeaders(responseHeaders);
    }

    /**
     * Create an exception from status and response body.
     *
     * @param httpStatus - Http Status.
     * @param httpBody   - The raw response body.
     */
    @Deprecated
    public CspApiException(HttpStatus httpStatus, String httpBody) {
        this(null, httpStatus, httpBody);
    }

    /**
     * Create an exception from status and response body.
     *
     * @param httpStatus - Http Status.
     * @param httpBody   - The raw response body.
     */
    public CspApiException(HttpHeaders responseHeaders, HttpStatus httpStatus, String httpBody) {
        super(String.format(ERROR_MESSAGE, httpStatus, httpBody));
        this.httpStatus = httpStatus;
        this.httpBody = httpBody;
        processHeaders(responseHeaders);
    }

    private void processHeaders(HttpHeaders headers) {
        if (headers == null) {
            return;
        }
        this.responseHeaders = headers;
        this.cspRequestId = headers.getFirst(CspConstants.HEADER_CSP_REQUEST_ID);
    }
}
