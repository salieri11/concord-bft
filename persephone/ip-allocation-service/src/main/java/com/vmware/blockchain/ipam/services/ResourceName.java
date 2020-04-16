/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services;

/**
 * Data class for resource name.
 */
public class ResourceName {
    private String value;

    /**
     * Constructor used for default AddressBlock values.
     */
    public ResourceName() {
        this.value = "";
    }

    /**
     * Constructor used for specified AddressBlock values.
     */
    public ResourceName(String value) {
        this.value = value;
    }

    public String getValue() {
        return this.value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}