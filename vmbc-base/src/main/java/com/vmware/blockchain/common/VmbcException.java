/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.text.MessageFormat;

import org.springframework.http.HttpStatus;

/**
 * Base class for Helen exceptions.  The message is a MessageFormat string, to aid in transition
 * to localization latter on.
 */
public class VmbcException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final ErrorCodeType errorCodeType = null;
    private final Object[] args;
    private final HttpStatus httpStatus;

    /**
     * Create a new Helen Exception.
     */
    public VmbcException(HttpStatus httpStatus, String message, Object... args) {
        super(MessageFormat.format(message, args));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    /**
     * Creating new constructors with ErrorCodeType.
     */
    public VmbcException(HttpStatus httpStatus, ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public VmbcException(String message, Object... args) {
        this(HttpStatus.valueOf(500), message, args);
    }

    /** Create Helen Exception with new ErrorCode type introduced.
     */
    public VmbcException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args));
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /**
     * Create a Helen Exception, and note the original cause.
     */
    public VmbcException(Throwable cause, String message, Object... args) {
        super(message, cause);
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /** Create Helen Exception with new ErrorCode type introduced.
     */

    public VmbcException(Throwable cause, ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args, cause));
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /**
     * Create a Helen Exception with a specific status, and note the original cause.
     */
    public VmbcException(HttpStatus httpStatus, Throwable cause, String message, Object... args) {
        super(message, cause);
        this.args = args;
        this.httpStatus = httpStatus;
    }

    /** Create Helen Exception with new ErrorCode type introduced.
     */
    public VmbcException(HttpStatus httpStatus, Throwable cause, ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args, cause));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public HttpStatus getHttpStatus() {
        return httpStatus;
    }

}
