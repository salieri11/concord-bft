/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */
public class ConflictException extends HelenException {
    private static final long serialVersionUID = 1L;

    public ConflictException(String message, Object... args) {
        super(HttpStatus.CONFLICT, message, args);
    }

    public ConflictException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.CONFLICT, ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
              args));
    }
}
