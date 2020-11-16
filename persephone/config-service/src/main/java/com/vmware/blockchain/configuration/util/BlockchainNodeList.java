/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.util;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import lombok.Builder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * Holder for list of nodes in the Blockchain.
 */
@Builder
@Getter
@Slf4j
public class BlockchainNodeList {
    private List<BlockchainReplica> replicas;
    private List<BlockchainReadReplica> readReplicas;
    private List<BlockchainClient> clients;

    /**
     * Get a list of Ids for all nodes in node list.
     * @return List of node Ids.
     */
    public List<String> getAllNodeIds() {
        var nodeIds = new ArrayList<String>();
        // This order is important, because we generate principal Ids based on this order.
        nodeIds.addAll(getIds(replicas));
        nodeIds.addAll(getIds(readReplicas));
        nodeIds.addAll(getIds(clients));
        return nodeIds;
    }

    /**
     * Get a list of Ids for all Replicas (BFT and read replica nodes) in node list.
     * @return List of node Ids.
     */
    public List<String> getAllReplicaNodeIds() {
        var nodeIds = new ArrayList<String>();
        // This order is important, because we generate principal Ids based on this order.
        nodeIds.addAll(getIds(replicas));
        nodeIds.addAll(getIds(readReplicas));
        return nodeIds;
    }

    /**
     * Get a list of Ids for a given list of nodes.
     * @param nodes A list of nodes.
     * @return A list of Ids.
     */
    private List<String> getIds(List<? extends BlockchainNode> nodes) {
        if (nodes == null) {
            return new ArrayList<>();
        }
        var nodeIdList = new ArrayList<String>();
        nodeIdList.addAll(nodes.stream().filter(node -> node != null).map(node -> node.getId())
                                  .collect(Collectors.toList()));
        return nodeIdList;
    }

    /**
     * Get number of clients available.
     * @return client size.
     */
    public int getClientSize() {
        return (clients != null) ? clients.size() : 0;
    }

    /**
     * Get number of replicas available.
     * @return replica size.
     */
    public int getReplicaSize() {
        return (replicas != null) ? replicas.size() : 0;
    }

    /**
     * Get number of read replicas available.
     * @return read replica size.
     */
    public int getReadReplicaSize() {
        return (readReplicas != null) ? readReplicas.size() : 0;
    }

    /**
     * Get number of replica and read replicas.
     * @return count of replica and read replicas.
     */
    public int getAllReplicasSize() {
        return getReadReplicaSize() + getReplicaSize();
    }

    /**
     * Get a list of Ids for all Read Replicas in node list.
     * @return List of node Ids.
     */
    public List<String> getReadReplicaNodeIds() {
        var nodeIds = new ArrayList<String>();
        nodeIds.addAll(getIds(readReplicas));
        return nodeIds;
    }

    /**
     * Get a list of Ids for all Clients in node list.
     * @return List of node Ids.
     */
    public List<String> getClientNodeIds() {
        var nodeIds = new ArrayList<String>();
        nodeIds.addAll(getIds(clients));
        return nodeIds;
    }

    /**
     * Get a list of Ids for all Replicas in node list.
     * @return List of node Ids.
     */
    public List<String> getReplicaNodeIds() {
        var nodeIds = new ArrayList<String>();
        nodeIds.addAll(getIds(replicas));
        return nodeIds;
    }
}
