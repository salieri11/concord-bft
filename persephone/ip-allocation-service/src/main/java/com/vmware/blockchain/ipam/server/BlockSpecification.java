/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.server;

/**
 * Class representing BlockSpecification.
 */
public class BlockSpecification {
    int prefix;
    int subnet;

    public BlockSpecification() {
        this.prefix = 0;
        this.subnet = 0;
    }

    public BlockSpecification(int prefix, int subnet) {
        this.prefix = prefix;
        this.subnet = subnet;
    }
}