/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * There was an error trying to modify the user.
 */
public class EntityModificationException extends HelenException {
    private static final long serialVersionUID = 1L;

    public EntityModificationException(String message) {
        super(message, HttpStatus.BAD_REQUEST);
    }
}
