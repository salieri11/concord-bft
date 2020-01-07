/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * There was an error trying to modify the user.
 */
public class EntityModificationException extends HelenException {
    private static final long serialVersionUID = 1L;

    public EntityModificationException(String message, Object... args) {
        super(HttpStatus.BAD_REQUEST, message, args);
    }

    public EntityModificationException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.BAD_REQUEST, ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
                args));
    }
}
