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
public class ServiceUnavailableException extends HelenException {
    private static final long serialVersionUID = 1L;

    public ServiceUnavailableException(String message, Object... args) {
        super(HttpStatus.SERVICE_UNAVAILABLE, message, args);
    }

    public ServiceUnavailableException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.SERVICE_UNAVAILABLE,
                ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args));
    }
}
