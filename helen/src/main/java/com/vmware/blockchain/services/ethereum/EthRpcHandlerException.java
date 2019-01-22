/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.ethereum;

import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.HelenException;

/**
 * This class serves as a common exception type thrown by all EthRPC handlers.
 */
public class EthRpcHandlerException extends HelenException {
    private static final long serialVersionUID = 1L;

    public EthRpcHandlerException(String message) {
        super(message, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
