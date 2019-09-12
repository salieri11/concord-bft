/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.v1.ConcordModelSpecification
import com.vmware.blockchain.deployment.v1.ConcordNodeInfo

/**
 * This class helps in addition of default metadata in a given orchestration site.
 */
class ConcordNodeInfos {

    companion object {
        fun toConcordNode(
            blockchainType: ConcordModelSpecification.BlockchainType
        ): ConcordNodeInfo {
            return ConcordNodeInfo.defaultValue.copy(blockchainType = blockchainType)
        }
    }
}
