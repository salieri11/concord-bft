/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import java.text.MessageFormat;

import org.springframework.http.HttpStatus;

/**
 * Base class for Persephone exceptions.
 */
public class BadRequestPersephoneException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final Object[] args;
    private final HttpStatus httpStatus;

    /**
     * Create a new Helen Exception.
     */
    public BadRequestPersephoneException(HttpStatus httpStatus, String message, Object... args) {
        super(MessageFormat.format(message, args));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public BadRequestPersephoneException(String message, Object... args) {
        this(HttpStatus.valueOf(400), message, args);
    }

    /**
     * Create a PersephoneException Exception, and note the original cause.
     */
    public BadRequestPersephoneException(Throwable cause, String message, Object... args) {
        super(message, cause);
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(400);
    }

    /**
     * Create a PersephoneException Exception with a specific status, and note the original cause.
     */
    public BadRequestPersephoneException(HttpStatus httpStatus, Throwable cause, String message, Object... args) {
        super(message, cause);
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public HttpStatus getHttpStatus() {
        return httpStatus;
    }

}
