/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Some internal failure, such as DB failure, has occured.
 */
public class InternalFailureException extends VmbcException {

    private static final long serialVersionUID = 1L;

    public InternalFailureException(String message, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR, message, args);
    }

    public InternalFailureException(Throwable cause, String message, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR, cause, message, args);
    }

    public InternalFailureException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
              HttpStatus.INTERNAL_SERVER_ERROR, args));
    }

    public InternalFailureException(ErrorCodeType codeType, Throwable cause, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
             HttpStatus.INTERNAL_SERVER_ERROR, cause, args));
    }
}
