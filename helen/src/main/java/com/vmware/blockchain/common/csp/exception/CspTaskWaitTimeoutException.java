/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.exception;

import java.util.concurrent.TimeUnit;

/**
 * Timeout exception while waiting for CSP.
 */
public class CspTaskWaitTimeoutException extends CspException {

    private static final String EXCEPTION_MESSAGE_TEMPLATE = "The task url %s timed out";
    private static final String EXCEPTION_MESSAGE_TEMPLATE_EXTENDED = EXCEPTION_MESSAGE_TEMPLATE + " after %d $%s";

    /**
     * Throws an exception with formatted error message using {@link #EXCEPTION_MESSAGE_TEMPLATE}.
     *
     * @param taskUrl The task url that timed out.
     */
    public CspTaskWaitTimeoutException(String taskUrl) {
        super(String.format(EXCEPTION_MESSAGE_TEMPLATE, taskUrl));
    }

    /**
     * Throws an exception with extended error message using {@link #EXCEPTION_MESSAGE_TEMPLATE_EXTENDED}.
     *
     * @param taskUrl The task url that timed out.
     * @param timeout The timeout.
     * @param unit    The {@link TimeUnit} after which timeout happened.
     */
    public CspTaskWaitTimeoutException(String taskUrl, long timeout, TimeUnit unit) {
        super(String.format(EXCEPTION_MESSAGE_TEMPLATE_EXTENDED, taskUrl, timeout, unit));
    }

    /**
     * Throws an exception with extended error message using {@link #EXCEPTION_MESSAGE_TEMPLATE_EXTENDED} and a
     * throwable.
     *
     * @param taskUrl - The task url that timed out.
     * @param e       Any exception/error that needs to be propagated.
     */
    public CspTaskWaitTimeoutException(String taskUrl, Throwable e) {
        super(String.format(EXCEPTION_MESSAGE_TEMPLATE, taskUrl), e);
    }

    /**
     * Throws an exception with extended error message using {@link #EXCEPTION_MESSAGE_TEMPLATE_EXTENDED}.
     *
     * @param taskUrl The task url that timed out.
     * @param timeout The timeout.
     * @param unit    The {@link TimeUnit} after which timeout happened.
     * @param e       Any exception/error that needs to be propagated.
     */
    public CspTaskWaitTimeoutException(String taskUrl, long timeout, TimeUnit unit, Throwable e) {
        super(String.format(EXCEPTION_MESSAGE_TEMPLATE_EXTENDED, taskUrl, timeout, unit), e);
    }
}
