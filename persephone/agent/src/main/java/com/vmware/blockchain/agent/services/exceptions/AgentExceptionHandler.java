/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Agent Component Exception Handler, implemented using the standard Spring global exception handling mechanism.
 */
@ControllerAdvice
//TODO ? extends ResponseEntityExceptionHandler?
public class AgentExceptionHandler extends ResponseEntityExceptionHandler {

    /**
     * Holds the information of interest in case of exception.
     */
    @AllArgsConstructor
    @Getter
    private static class ErrorResponse {

        //the HTTP status
        private HttpStatus status;
        //the exception class name
        private String excCause;
        //the exception message
        private String details;
        //URI
        private String path;
    }

    @ExceptionHandler(AgentException.class)
    protected ResponseEntity<ErrorResponse> handleAgentException(final AgentException ex, HttpServletRequest request) {
        ErrorResponse errResp = createAgentExceptionErrorResponse(ex, request);
        return new ResponseEntity<>(errResp, errResp.getStatus());
    }

    private ErrorResponse createAgentExceptionErrorResponse(AgentException ex, HttpServletRequest request) {
        ErrorCode error = ex.getErrorCode();
        if (error == null) {
            return createAnyOtherErrorResponse(ex, request);
        }
        return new ErrorResponse(error.getHttpStatus(), ex.getClass().toString(),
                                 error.getMessage() + ": " + ex.getDetails(), request.getRequestURI());
    }

    @ExceptionHandler(Throwable.class)
    protected ResponseEntity<ErrorResponse> handleAnyOtherException(final Throwable ex, HttpServletRequest request) {
        ErrorResponse errResp = createAnyOtherErrorResponse(ex, request);
        return new ResponseEntity<>(errResp, errResp.getStatus());
    }

    private ErrorResponse createAnyOtherErrorResponse(Throwable ex, HttpServletRequest request) {
        ErrorCode error = ErrorCode.findByException(ex);
        return new ErrorResponse(error.getHttpStatus(), ex.getClass().toString(),
                                 error.getMessage() + ": " + ex.getMessage(),
                                 request.getRequestURI());
    }

}
