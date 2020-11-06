/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

/**
 * Invalid Configuration generic exception class.
 */
public class InvalidConfigurationException extends Exception {

    /**
     * Default constructor.
     * @param errMsg error
     */
    public InvalidConfigurationException(String errMsg) {
        super(errMsg);
    }
}
