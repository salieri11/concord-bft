/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services.dao;

import com.google.protobuf.ByteString;
import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

import lombok.AllArgsConstructor;

/**
 * Entity class to store segments.
 */
@EntityColumnName("persephone.addressblocksegment")
@AllArgsConstructor
public class AddressBlockSegment extends AbstractEntity {
    String name;
    int segment;
    ByteString allocations;

    /**
     * Default.
     */
    public AddressBlockSegment() {
        this.name = "";
        this.segment = 0;
        this.allocations = ByteString.EMPTY;
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

    public ByteString getAllocations() {
        return this.allocations;
    }

    public void setAllocations(ByteString allocations) {
        this.allocations = allocations;
    }
}
