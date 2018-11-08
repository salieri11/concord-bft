/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

public class AthenaConnectionException extends Exception {
    private static final long serialVersionUID = 1L;

    public AthenaConnectionException(String message) {
        super(message);
    }
}
