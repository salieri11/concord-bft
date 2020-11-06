/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.util;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * Blockchain Replica class.
 */
@ToString
@Getter
@Setter
public class BlockchainReplica extends BlockchainNode {

    /**
     * Default constructor.
     * @param id node id
     * @param ip node ip
     */
    public BlockchainReplica(String id, String ip) {
        super(id, ip);
    }

}




