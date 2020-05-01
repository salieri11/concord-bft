/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services.dao;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entity class to store segments.
 */
@NoArgsConstructor
@AllArgsConstructor
@Data
@EntityColumnName("persephone.addressblocksegment")
public class AddressBlockSegment extends AbstractEntity {

    String name;
    int segment;
    byte[] allocations;
}