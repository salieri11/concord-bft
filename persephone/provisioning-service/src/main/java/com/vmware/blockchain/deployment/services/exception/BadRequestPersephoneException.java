/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import org.springframework.http.HttpStatus;

/**
 * Class for BadRequest Persephone exceptions.
 */
public class BadRequestPersephoneException extends PersephoneException {

    private static final long serialVersionUID = 1L;

    public BadRequestPersephoneException(String message, Object... args) {
        super(HttpStatus.BAD_REQUEST, message, args);
    }

    /**
     * Create a BadRequestPersephoneException, and note the original cause.
     */
    public BadRequestPersephoneException(Throwable cause, String message, Object... args) {
        super(HttpStatus.BAD_REQUEST, cause, message, args);
    }

}
