/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.ExceptionMessageHelper;
import com.vmware.blockchain.common.VmbcException;


/**
 * ConcurrentUpdateException is thrown by DB internals when there is an optimitic lock failure.
 * This situation can be handled at various levels, potentially in other service layers.
 * If it isn't handled anywhere else, report it as a conflict (409).
 */
public class ConcurrentUpdateException extends VmbcException {

    private static final long serialVersionUID = 1L;

    public ConcurrentUpdateException(String message, Object... args) {
        super(HttpStatus.CONFLICT, message, args);
    }

    public ConcurrentUpdateException(Throwable cause, String message, Object... args) {
        super(HttpStatus.CONFLICT, cause, message, args);
    }

    public ConcurrentUpdateException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
                                                           HttpStatus.CONFLICT, args));
    }

    public ConcurrentUpdateException(ErrorCodeType codeType, Throwable cause, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(),
              HttpStatus.CONFLICT, cause, args));
    }

}
