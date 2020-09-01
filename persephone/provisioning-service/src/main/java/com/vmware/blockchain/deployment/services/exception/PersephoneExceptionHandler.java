/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Handle Persephone exceptions, and return the proper error response.
 */
@ControllerAdvice(basePackages = "com.vmware.blockchain.deployment")
public class PersephoneExceptionHandler {

    private static final Logger logger = LogManager.getLogger(PersephoneExceptionHandler.class);

    /**
     * Handle all Exceptions, and return an error message.
     */
    @ExceptionHandler
    @ResponseBody
    ResponseEntity<PersephoneExceptionHandlerUtil.ErrorMessage> handleException(Throwable ex,
                                                                                HttpServletRequest request) {
        PersephoneExceptionHandlerUtil.ErrorMessage errorMessage =
                PersephoneExceptionHandlerUtil.getErrorMessage(ex, request.getRequestURI());
        logger.info("Error code {}, message {}, status {}", errorMessage.getErrorCode(), ex.getMessage(),
                    errorMessage.getStatus(), ex);
        return new ResponseEntity<>(errorMessage, errorMessage.getStatus().getKey());
    }
}
