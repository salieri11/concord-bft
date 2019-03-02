/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.util.Collections;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

/**
 * Handle Helen exceptions, and retrun the proper error response.
 */
@RestControllerAdvice(basePackages = "com.vmware.blockchain")
public class HelenExceptionHandler {

    @Getter
    @Setter
    @AllArgsConstructor
    private static class ErrorMessage {
        String errorCode;
        List<String> errorMessages;
        int status;
        String path;
    }

    /**
     * Handle Helen Exceptions, and return an error message.
     */
    @ExceptionHandler
    @ResponseBody
    ResponseEntity<ErrorMessage> handleException(HelenException ex, HttpServletRequest request) {
        ErrorMessage errorMessage = new ErrorMessage(ex.getClass().getSimpleName(),
                Collections.singletonList(ex.getMessage()), ex.getHttpStatus().value(), request.getRequestURI());
        return new ResponseEntity<>(errorMessage, HttpStatus.valueOf(errorMessage.getStatus()));

    }


}
