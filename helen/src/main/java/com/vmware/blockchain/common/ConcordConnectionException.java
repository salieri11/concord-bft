/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Error on Concord connection.
 */
public class ConcordConnectionException extends HelenException {
    private static final long serialVersionUID = 1L;

    public ConcordConnectionException(String message, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR, message, args);
    }

    public ConcordConnectionException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR,
                ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args));
    }
}
