/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient;

import com.vmware.blockchain.deployment.services.exception.PersephoneException;

/**
 * Base exception class for Oauth2 common utility exceptions.
 * This class is for external - and requires an errorCode as a string.
 */
public class RestClientException extends PersephoneException {

    private static final long serialVersionUID = 1L;

    /**
     * Constructor taking errorCodeValue.
     * @param errorCodeValue which is a string.
     *
     */
    public RestClientException(String errorCodeValue, Object... args) {
        // Set the parsed error message (if possible) or at least the error code.
        super(errorCodeValue, args);
    }

    /**
     * Constructor taking errorCode and cause.
     *
     * @param errorCodeValue {@link String}
     * @param cause Originating exception
     */
    public RestClientException(String errorCodeValue, Throwable cause, Object... args) {
        super(cause, errorCodeValue, args);
    }
}
