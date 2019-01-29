/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.restclient;

import com.vmware.blockchain.common.HelenException;

import lombok.Getter;

/**
 * Base exception class for Oauth2 common utility exceptions.
 * This class is for external - and requires an errorCode as a string.
 */
public class RestClientException extends HelenException {

    private static final long serialVersionUID = 1L;

    @Getter
    protected String errorCodeValue;

    @Getter
    protected String[] errorParams;

    /**
     * Constructor taking errorCodeValue.
     * @param errorCodeValue which is a string.
     *
     */
    public RestClientException(String errorCodeValue, String... args) {
        // Set the parsed error message (if possible) or at least the error code.
        super(errorCodeValue);
        this.errorCodeValue = errorCodeValue;
        this.errorParams = args;
    }

    /**
     * Constructor taking errorCode and cause.
     *
     * @param errorCodeValue {@link String}
     * @param cause Originating exception
     * @param args params to be inserted in messages.
     */
    public RestClientException(String errorCodeValue, Throwable cause, String... args) {
        super(errorCodeValue, cause);
        this.errorCodeValue = errorCodeValue;
        this.errorParams = args;
    }
}
