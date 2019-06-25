/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Base csp exception.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class CspException extends RuntimeException {

    /**
     * Pass the exception and message on to {@link RuntimeException#RuntimeException(String, Throwable)}.
     *
     * @param message - The error message.
     * @param cause   - Exception cause.
     */
    public CspException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Pass the exception and message on to {@link RuntimeException#RuntimeException(String)}.
     *
     * @param message - The error message.
     */
    public CspException(String message) {
        super(message);
    }

    public CspException() {
        super();
    }
}
