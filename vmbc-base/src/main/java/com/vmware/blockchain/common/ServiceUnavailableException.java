/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Service is not available.
 */
public class ServiceUnavailableException extends VmbcException {
    private static final long serialVersionUID = 1L;

    public ServiceUnavailableException(String message, Object... args) {
        super(HttpStatus.SERVICE_UNAVAILABLE, message, args);
    }

    public ServiceUnavailableException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
              HttpStatus.SERVICE_UNAVAILABLE, args));
    }
}
