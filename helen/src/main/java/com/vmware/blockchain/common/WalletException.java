/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * There was an error trying to create the wallet for user.
 */
public class WalletException extends HelenException {
    private static final long serialVersionUID = 1L;

    public WalletException(String message, Object... args) {
        super(HttpStatus.BAD_REQUEST, message, args);
    }

    public WalletException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.BAD_REQUEST, ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
                args));
    }
}
