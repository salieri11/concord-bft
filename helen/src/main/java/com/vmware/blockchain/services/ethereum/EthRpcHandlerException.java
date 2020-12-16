/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.ExceptionMessageHelper;
import com.vmware.blockchain.common.HelenException;

/**
 * This class serves as a common exception type thrown by all EthRPC handlers.
 */
public class EthRpcHandlerException extends HelenException {
    private static final long serialVersionUID = 1L;

    public EthRpcHandlerException(String message, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR, message, args);
    }

    /**
     * Handles Exception with Helen ErrorCodeType.
     * @param codeType ErrorCodeType object with error message.
     * @param args args.
     */
    public EthRpcHandlerException(ErrorCodeType codeType, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR,
                ExceptionMessageHelper.getMessageOrErrorCode(
                        codeType.getErrorCodeTypeValue(),
                        args
                )
        );
    }
}
