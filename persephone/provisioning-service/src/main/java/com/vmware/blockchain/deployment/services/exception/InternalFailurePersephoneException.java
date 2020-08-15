/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exception;

import org.springframework.http.HttpStatus;

/**
 * Base exception class for Oauth2 common utility exceptions.
 * This class is for external - and requires an errorCode as a string.
 */
public class InternalFailurePersephoneException extends PersephoneException {

    private static final long serialVersionUID = 1L;

    /**
     * Constructor taking errorCodeValue.
     * @param message which is a string.
     *
     */
    public InternalFailurePersephoneException(String message, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR, message, args);
    }

    /**
     * Constructor taking errorCode and cause.
     *
     * @param message {@link String}
     * @param cause Originating exception
     */
    public InternalFailurePersephoneException(Throwable cause, String message, Object... args) {
        super(HttpStatus.INTERNAL_SERVER_ERROR, cause, message, args);
    }
}
