/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Error on Concord connection.
 */
public class ConcordConnectionException extends HelenException {
    private static final long serialVersionUID = 1L;

    public ConcordConnectionException(String message) {
        super(message, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
