/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Return access forbidden, 403.
 */
public class AccessForbiddenException extends HelenException {

    private static final long serialVersionUID = 1L;

    public AccessForbiddenException(String message) {
        super(message, HttpStatus.FORBIDDEN);
    }
}
