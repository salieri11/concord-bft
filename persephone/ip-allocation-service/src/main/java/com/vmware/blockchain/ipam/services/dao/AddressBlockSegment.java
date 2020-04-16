/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services.dao;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

/**
 * Entity class to store segments.
 */
@EntityColumnName("persephone.addressblocksegment")
public class AddressBlockSegment extends AbstractEntity {
    String name;
    int segment;
    byte[] allocations;

    /**
     * Default constructor.
     */
    public AddressBlockSegment() {
        this.name = "";
        this.segment = 0;
        this.allocations = new byte[0];
    }

    /**
     * Specific constructor.
     */
    public AddressBlockSegment(String name, int segment, byte[] allocations) {
        this.name = name;
        this.segment = segment;
        this.allocations = allocations;
    }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getSegment() {
        return this.segment;
    }

    public void setSegment(int segment) {
        this.segment = segment;
    }

    public byte[] getAllocations() {
        return this.allocations;
    }

    public void putAllocations(byte[] allocations) {
        this.allocations = allocations;
    }
}
