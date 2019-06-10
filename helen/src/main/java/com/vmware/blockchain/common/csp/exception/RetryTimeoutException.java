/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.exception;

/**
 * Exception to throw when the retries timeout. This helps retries from going on a long wait.
 */
public class RetryTimeoutException extends RuntimeException {

    private int retryCount;
    private long maxTimeout;

    private static final String MESSAGE_FORMAT = "Retry timed out after %d retries and %d seconds";

    /**
     * An exception class to specify that a retry loop has timed out. This is useful when we want to time box a retry.
     *
     * @param retryCount          - The number of times retry happened before timing out.
     * @param maxTimeoutInSeconds - Seconds waited after timeout.
     */
    public RetryTimeoutException(int retryCount, long maxTimeoutInSeconds) {
        super(String.format(MESSAGE_FORMAT, retryCount, maxTimeoutInSeconds));
        this.retryCount = retryCount;
        this.maxTimeout = maxTimeoutInSeconds;
    }

    public int getRetryCount() {
        return retryCount;
    }

    public long getMaxTimeout() {
        return maxTimeout;
    }
}
