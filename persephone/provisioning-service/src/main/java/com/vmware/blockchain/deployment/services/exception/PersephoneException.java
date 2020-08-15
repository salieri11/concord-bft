/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import java.text.MessageFormat;

import org.springframework.http.HttpStatus;

/**
 * Base class for Persephone exceptions.
 */
public class PersephoneException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final Object[] args;
    private final HttpStatus httpStatus;

    /**
     * Create a new Persephone Exception.
     */
    public PersephoneException(HttpStatus httpStatus, String message, Object... args) {
        super(MessageFormat.format(message, args));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public PersephoneException(String message, Object... args) {
        this(HttpStatus.valueOf(500), message, args);
    }

    /**
     * Create a PersephoneException Exception, and note the original cause.
     */
    public PersephoneException(Throwable cause, String message, Object... args) {
        super(MessageFormat.format(message, args), cause);
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /**
     * Create a PersephoneException Exception with a specific status, and note the original cause.
     */
    public PersephoneException(HttpStatus httpStatus, Throwable cause, String message, Object... args) {
        super(MessageFormat.format(message, args), cause);
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public HttpStatus getHttpStatus() {
        return httpStatus;
    }

}
