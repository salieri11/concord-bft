/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.deployment.v1.BlockchainType;

/**
 * Utility class for vaidation methods.
 */
public class ValidationUtil {
    /**
     * Validate if the object is non null.
     * @param obj Input object
     * @return True if the object is not null, false otherwise.
     */
    public static boolean isValid(Object obj) {
        if (obj == null) {
            return false;
        }
        return true;
    }

    /**
     * Validate the node count based on the Blockchain type.
     * @param nodeList List of nodes
     * @param blockchainType BLockchain type
     * @return True if node count is valid, false otherwise.
     */
    public static boolean isValidNodeCount(BlockchainNodeList nodeList, BlockchainType blockchainType) {
        if (nodeList == null || blockchainType == null) {
            return false;
        }
        switch (blockchainType) {
            case DAML:
                if (nodeList.getReplicaSize() <= 0 || nodeList.getClientSize() <= 0) {
                    return false;
                }
                break;
            case ETHEREUM:
            case HLF:
                if (nodeList.getReplicaSize() <= 0) {
                    return false;
                }
                break;
            default:
                break;
        }
        return true;
    }

}
