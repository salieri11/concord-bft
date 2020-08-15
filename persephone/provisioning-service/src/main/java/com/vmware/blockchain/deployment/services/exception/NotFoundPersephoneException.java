/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import org.springframework.http.HttpStatus;

/**
 * Return not found, 404.
 */
public class NotFoundPersephoneException extends PersephoneException {

    private static final long serialVersionUID = 1L;

    public NotFoundPersephoneException(String message, Object... args) {
        super(HttpStatus.NOT_FOUND, message, args);
    }

    public NotFoundPersephoneException(Throwable cause, String message, Object... args) {
        super(HttpStatus.NOT_FOUND, cause, message, args);
    }

}
