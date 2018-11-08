/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Error on Athena connection.
 */
public class AthenaConnectionException extends HelenException {
    private static final long serialVersionUID = 1L;

    public AthenaConnectionException(String message) {
        super(message, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
