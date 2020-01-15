/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Return not found, 404.
 */
public class NotFoundException extends VmbcException {

    private static final long serialVersionUID = 1L;

    public NotFoundException(String message, Object... args) {
        super(HttpStatus.NOT_FOUND, message, args);
    }

    public NotFoundException(Throwable cause, String message, Object... args) {
        super(HttpStatus.NOT_FOUND, cause, message, args);
    }

    public NotFoundException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
             HttpStatus.NOT_FOUND, args));
    }

    public NotFoundException(ErrorCodeType codeType, Throwable cause, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
             HttpStatus.NOT_FOUND, cause, args));
    }
}
