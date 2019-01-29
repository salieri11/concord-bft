/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import org.springframework.http.HttpStatus;

/**
 * Base class for Helen exceptions.  Can be thrown as is.
 */
public class HelenException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final String message;
    private final HttpStatus httpStatus;

    /**
     * Create a new Helen Exception.
     */
    public HelenException(String message, HttpStatus httpStatus) {
        super(message);
        this.message = message;
        this.httpStatus = httpStatus;
    }

    public HelenException(String message) {
        this(message, HttpStatus.valueOf(500));
    }

    /**
     * Create a Helen Exception, and not the original cause.
     */
    public HelenException(String message, Throwable cause) {
        super(message, cause);
        this.message = message;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    @Override
    public String getMessage() {
        return message;
    }

    public HttpStatus getHttpStatus() {
        return httpStatus;
    }

}
