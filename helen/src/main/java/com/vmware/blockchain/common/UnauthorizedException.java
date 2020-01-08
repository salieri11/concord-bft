/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Call has no authorization (status 401).
 */
public class UnauthorizedException extends HelenException {

    private static final long serialVersionUID = 1L;

    public UnauthorizedException(String message, Object... args) {
        super(HttpStatus.UNAUTHORIZED, message, args);
    }

    public UnauthorizedException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.UNAUTHORIZED, ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
                args));
    }
}
