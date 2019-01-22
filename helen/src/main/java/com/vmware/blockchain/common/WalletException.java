/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

/**
 * There was an error trying to create the wallet for user.
 */
public class WalletException extends Exception {
    private static final long serialVersionUID = 1L;

    public WalletException(String message) {
        super(message);
    }
}
