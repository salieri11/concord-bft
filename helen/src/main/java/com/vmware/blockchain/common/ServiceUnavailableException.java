/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */
public class ServiceUnavailableException extends Exception {
    private static final long serialVersionUID = 1L;

    public ServiceUnavailableException(String message) {
        super(message);
    }
}
