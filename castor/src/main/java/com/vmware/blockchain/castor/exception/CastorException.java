/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.exception;

import java.text.MessageFormat;

import org.springframework.http.HttpStatus;

import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.ExceptionMessageHelper;
import com.vmware.blockchain.common.VmbcException;

/**
 * Base class for Castor exceptions.  The message is a MessageFormat string, to aid in transition
 * to localization latter on.
 */
public class CastorException extends VmbcException {

    private static final long serialVersionUID = 1L;

    private final ErrorCodeType errorCodeType = null;
    private final Object[] args;
    private final HttpStatus httpStatus;

    /**
     * Create a new Castor Exception.
     */
    public CastorException(HttpStatus httpStatus, String message, Object... args) {
        super(MessageFormat.format(message, args));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    /**
     * Creating new constructors with ErrorCodeType.
     */
    public CastorException(HttpStatus httpStatus, ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    public CastorException(String message, Object... args) {
        this(HttpStatus.valueOf(500), message, args);
    }

    /** Create Castor Exception with new ErrorCode type introduced.
     */
    public CastorException(ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args));
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /**
     * Create a Castor Exception, and note the original cause.
     */
    public CastorException(Throwable cause, String message, Object... args) {
        super(message, cause);
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /** Create Castor Exception with new ErrorCode type introduced.
     */

    public CastorException(Throwable cause, ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args, cause));
        this.args = args;
        this.httpStatus = HttpStatus.valueOf(500);
    }

    /**
     * Create a Castor Exception with a specific status, and note the original cause.
     */
    public CastorException(HttpStatus httpStatus, Throwable cause, String message, Object... args) {
        super(message, cause);
        this.args = args;
        this.httpStatus = httpStatus;
    }

    /** Create Castor Exception with new ErrorCode type introduced.
     */
    public CastorException(HttpStatus httpStatus, Throwable cause, ErrorCodeType codeType, Object... args) {
        super(ExceptionMessageHelper.getMessageOrErrorCode(codeType.getErrorCodeTypeValue(), args, cause));
        this.args = args;
        this.httpStatus = httpStatus;
    }

    /**
     * Get the http status.
     * @return http status
     */
    public HttpStatus getHttpStatus() {
        return httpStatus;
    }

}
