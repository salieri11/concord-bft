/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.List;
import java.util.Map;
import java.util.UUID;

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
                if (nodeList.getReplicaSize() <= 0) {
                    return false;
                }
                break;
            default:
                break;
        }
        return true;
    }

    /**
     * Validate that the node list is not null and not empty, making sure that it has some nodes.
     * @param nodeList List of nodes in the deployment
     * @return True if nodes are available, false otherwise.
     */
    public static boolean isValidNodeList(BlockchainNodeList nodeList) {
        if (nodeList == null || nodeList.getAllNodeIds().isEmpty()) {
            return false;
        }
        return true;
    }

    /**
     * Validate nodeId.
     * @param nodeId node id
     * @return True if the node Id is valid, false otherwise.
     */
    public static boolean isValidNodeId(String nodeId) {
        if (nodeId == null || nodeId.isEmpty()) {
            return false;
        }
        try {
            UUID.fromString(nodeId);
        } catch (IllegalArgumentException iae) {
            return false;
        }
        return true;
    }

    /**
     * Validate a list.
     * @param list of objects
     * @return True if the list is valid, false otherwise.
     */
    public static boolean isValidList(List list) {
        if (list == null || list.isEmpty()) {
            return false;
        }
        return true;
    }

    /**
     * Validate a map.
     * @param map of objects
     * @return True if the map is valid, false otherwise.
     */
    public static boolean isValidMap(Map map) {
        if (map == null || map.isEmpty()) {
            return false;
        }
        return true;
    }

}
