/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.util;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * Blockchain Client node class.
 */
@Getter
@Setter
@ToString
public class BlockchainClient extends BlockchainNode {

    /**
     * Client constructor.
     * @param id node id
     * @param ip node ip
     */
    public BlockchainClient(String id, String ip) {
        super(id, ip);
    }
}