/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.exceptions;

/**
 * Configuration Service specific exception base class.
 * Intended to be used as is, or to be subclasses into more specific exceptions.
 */
public class ConfigServiceException extends RuntimeException {

    private ErrorCode errorCode;
    /**
     * The aim is to human readable.
     * Usually consists of human readable explanation + originalExc.getMessage(), but can contain only
     * originalExc.getMessage() or just human readable message.
    */
    private String details;

    /**
     * CS exception created, when causes are known.
     */
    public ConfigServiceException(ErrorCode errorCode, String details, Throwable exc) {
        super(exc);
        this.errorCode = errorCode;
        this.details = details;
    }

    /*
    * Add constructors suited to your needs.
    * */

    /**
     * Gets the ErrorCode of CS Exception.
     */
    public ErrorCode getErrorCode() {
        return errorCode;
    }

    /**
     * Gets the details of CS Exception.
     * Usually consists of human readable explanation + originalExc.getMessage(), but can contain only
     * originalExc.getMessage() or just human readable message. The aim is to human readable.
     */
    public String getDetails() {
        return details;
    }

}
