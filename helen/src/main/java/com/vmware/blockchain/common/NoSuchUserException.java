/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * There was an error when the user does not exist.
 */
public class NoSuchUserException extends HelenException {
    private static final long serialVersionUID = 1L;

    public NoSuchUserException(String message) {
        super(message, HttpStatus.BAD_REQUEST);
    }
}
