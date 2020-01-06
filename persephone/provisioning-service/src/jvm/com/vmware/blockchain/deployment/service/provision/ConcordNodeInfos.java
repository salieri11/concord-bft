/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.Collections;

import org.jetbrains.annotations.NotNull;

import com.vmware.blockchain.deployment.v1.ConcordModelIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConcordNodeInfo;

/**
 * Helper class for @ConcordNodeInfo.
 */
public final class ConcordNodeInfos {

    /**
     * Copy object
     * @param blockchainType type.
     * @return nodeInfo
     */
    public static final ConcordNodeInfo toConcordNode(
            @NotNull ConcordModelSpecification.BlockchainType blockchainType) {
        // TODO Change this to builders once moved off custom bindings
        return new ConcordNodeInfo(new ConcordModelIdentifier(),
                                   Collections.emptyMap(),
                                   blockchainType);
    }
}
