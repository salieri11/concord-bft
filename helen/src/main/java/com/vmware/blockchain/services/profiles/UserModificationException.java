/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

public class UserModificationException extends Exception {
    private static final long serialVersionUID = 1L;

    public UserModificationException(String message) {
        super(message);
    }
}
