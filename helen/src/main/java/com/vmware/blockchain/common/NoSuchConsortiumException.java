/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * There was an error when the user does not exist.
 */
public class NoSuchConsortiumException extends HelenException {
    private static final long serialVersionUID = 1L;

    public NoSuchConsortiumException(String message) {
        super(message, HttpStatus.NOT_FOUND);
    }
}
