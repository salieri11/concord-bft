/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Conflict exception.
 */
public class ConflictException extends VmbcException {
    private static final long serialVersionUID = 1L;

    public ConflictException(String message, Object... args) {
        super(HttpStatus.CONFLICT, message, args);
    }

    public ConflictException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
              HttpStatus.CONFLICT, args));
    }
}
