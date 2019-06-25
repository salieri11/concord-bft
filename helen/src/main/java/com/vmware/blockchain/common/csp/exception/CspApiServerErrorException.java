/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.exception;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.csp.api.response.CspErrorResponse;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Exception class to hold any api exceptions from CSP.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class CspApiServerErrorException extends CspApiException {

    /**
     * Create an api error from csp error response.
     *
     * @param cspErrorResponse Csp error response returned.
     */
    public CspApiServerErrorException(CspErrorResponse cspErrorResponse, HttpHeaders headers) {
        super(cspErrorResponse, headers);
    }

    /**
     * Create an exception from status and response body.
     *CspApiClientErrorException
     * @param httpStatus - Http Status.
     * @param httpBody   - The raw response body.
     */
    public CspApiServerErrorException(HttpStatus httpStatus, String httpBody, HttpHeaders headers) {
        super(headers, httpStatus, httpBody);
    }
}
