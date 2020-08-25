/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

/**
 * Agent component specific exception base class.
 * Intended to be used as is, or to be subclasses into more specific exceptions.
 */
public class AgentException extends RuntimeException {

    private ErrorCode errorCode;
    private String details;
    private Throwable originalException;

    /**
     * Agent exception created from cause.
     * */
    public AgentException(Throwable e) {
        super(e);
        this.originalException = e;
        this.errorCode = ErrorCode.findByException(e);
        this.details = e.getMessage();
    }

    /**
     * Agent exception created, when causes are known.
     */
    public AgentException(ErrorCode errorCode, String details, Throwable exc) {
        super(exc);
        this.errorCode = errorCode;
        this.details = details;
    }

    /**
     * Agent exception created, when causes are not well-known.
     */
    public AgentException(ErrorCode errorCode, Throwable exception) {
        super(exception);
        this.errorCode = errorCode;
        //TODO maybe remove this
        this.errorCode.setExceptionType(exception.getClass());
        this.originalException = exception;
    }

    /**
     * Gets the ErrorCode of Agent Exception.
     */
    public ErrorCode getErrorCode() {
        return errorCode;
    }

    /**
     * Gets the details of Agent Exception.
     */
    public String getDetails() {
        return details;
    }

}
