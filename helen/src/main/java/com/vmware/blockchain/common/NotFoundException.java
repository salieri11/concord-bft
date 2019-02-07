/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Return not found, 404.
 */
public class NotFoundException extends HelenException {

    private static final long serialVersionUID = 1L;

    public NotFoundException(String message) {
        super(message, HttpStatus.NOT_FOUND);
    }
}
