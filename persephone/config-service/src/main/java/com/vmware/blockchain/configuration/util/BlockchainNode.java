/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.util;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

/**
 * Blockchain Node class.
 */
@Getter
@Setter
@AllArgsConstructor
public abstract class BlockchainNode {
    String id;
    String ip;
}
