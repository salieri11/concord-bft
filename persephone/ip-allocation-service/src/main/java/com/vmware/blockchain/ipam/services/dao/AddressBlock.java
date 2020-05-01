/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.services.dao;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.ipam.services.BlockSpecification;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Class representing Address Blocks.
 */
@NoArgsConstructor
@AllArgsConstructor
@Data
@EntityColumnName("persephone.addressblock")
public class AddressBlock extends AbstractEntity {

    String name;
    BlockSpecification specification;
    State state;

    /**
     * Enum to determine state.
     */
    public enum State {
        ACTIVE,
        CREATING,
        DELETING
    }
}
