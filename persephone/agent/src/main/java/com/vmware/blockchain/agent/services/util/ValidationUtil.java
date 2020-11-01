/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.util;

import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification.NodeType;


/**
 * Utility class for validation methods.
 */
public class ValidationUtil {
    /**
     * Validate if the object is non null and valid.
     * @param bcType Blockchain type
     * @return True if bcType is not null and valid, false otherwise.
     */
    public static boolean isValidBlockchainType(BlockchainType bcType) {
        if (bcType == null) {
            return false;
        }
        // Is it valid?
        if (bcType.equals(BlockchainType.UNRECOGNIZED)) {
            return false;
        }
        return true;
    }

    /**
     * Validate if node type is non null and valid.
     * @param nodeType blockchain node type
     * @return True if nodeType is not null and valid, false otherwise.
     */
    public static boolean isValidNodeType(NodeType nodeType) {
        if (nodeType == null) {
            return false;
        }
        // Is it valid?
        /**
         * Check if the the node type is valid.
         * ETH case: We need ot allow NONE as valid type, otherwise we should reject 'NONE' as well.
         */
        if (nodeType.equals(NodeType.UNRECOGNIZED)) {
            return false;
        }
        return true;
    }

    /**
     * Validate if service type is non null or invalid.
     * @param serviceType service type
     * @return True if service type is not null and valid, false otherwise.
     */
    public static boolean isValidServiceType(ServiceType serviceType) {
        if (serviceType == null) {
            return false;
        }
        // Is it valid?
        if (serviceType.equals(ServiceType.UNRECOGNIZED)) {
            return false;
        }
        return true;
    }
}
