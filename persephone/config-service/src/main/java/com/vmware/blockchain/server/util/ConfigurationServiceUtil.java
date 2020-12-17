/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.util;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.vmware.blockchain.configuration.util.BlockchainClient;
import com.vmware.blockchain.configuration.util.BlockchainNode;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReadReplica;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

import lombok.extern.slf4j.Slf4j;


/**
 * Utilities for configuration service server.
 */
@Slf4j
public class ConfigurationServiceUtil {

    public static final String OPID = "OpId";

    /**
     * Create and return a list of Node type objects. NodeType values are mapped to a subclass of Node class. For
     * example: NodeType.REPLICA is mapped to Replica NodeType.READ_REPLICA is mapped to ReadReplica NodeType.CLIENT is
     * mapped to Client
     *
     * @param nodesInfoMap Map of input nodeInfo
     * @param nodeType     Type of node
     * @param clazz        Class type of the supplied node type
     * @param <T>          Caller specified return type
     * @return A list of generic T objects. Caller specifies the class type.
     * @throws ConfigServiceException during an error
     */
    protected static <T extends BlockchainNode> List<T> getNodeListOfType(Map<String, NodesInfo> nodesInfoMap,
                                                                          NodeType nodeType, Class<T> clazz,
                                                                          UUID sessionId)
            throws ConfigServiceException {
        if (nodesInfoMap == null || nodesInfoMap.isEmpty() || nodeType == null) {
            log.error("NodesInfo map is empty or Invalid node type.");
            throw new ConfigServiceException(ErrorCode.CONFIGURATION_SERVICE_INPUT_MISSING_NODES,
                                             "Invalid node data in the request : " + sessionId);
        }
        List<T> list = new ArrayList<>();
        if (nodesInfoMap.containsKey(nodeType.name()) && nodesInfoMap.get(nodeType.name()).getEntriesList() != null) {
            nodesInfoMap.get(nodeType.name()).getEntriesList().forEach(each -> {
                try {
                    T obj = clazz.cast(clazz.getConstructor(String.class, String.class)
                                               .newInstance(each.getId(), each.getNodeIp()));
                    Map<String, String> valueMap = each.getProperties().getValuesMap();
                    if (obj.getClass().isAssignableFrom(BlockchainReadReplica.class)) {
                        BlockchainReadReplica readReplica = BlockchainReadReplica.class.cast(obj);
                        readReplica.setObjStoreBucketName(
                                valueMap.get(BlockchainReadReplica.OBJ_STORE_BUCKET_NAME_KEY_FOR_API));
                        readReplica
                                .setObjStoreAccessKey(valueMap.get(BlockchainReadReplica.OBJ_STORE_ACCESS_KEY_FOR_API));
                        readReplica.setObjStoreProtocol(
                                valueMap.get(BlockchainReadReplica.OBJ_STORE_PROTOCOL_KEY_FOR_API));
                        readReplica.setObjStoreUrl(valueMap.get(BlockchainReadReplica.OBJ_STORE_URL_KEY_FOR_API));
                        readReplica.setObjStoreSecret(valueMap.get(BlockchainReadReplica.OBJ_STORE_SECRET_KEY_FOR_API));
                    }
                    if (obj.getClass().isAssignableFrom(BlockchainClient.class)) {
                        BlockchainClient blockchainClient = BlockchainClient.class.cast(obj);
                        blockchainClient.setClientGroupId(valueMap.getOrDefault(
                                NodeProperty.Name.CLIENT_GROUP_ID.name(), each.getId()));
                    }
                    list.add(obj);
                } catch (InstantiationException | IllegalAccessException | InvocationTargetException
                        | NoSuchMethodException e) {
                    e.printStackTrace();
                    log.error("Error while creating node objects", e);
                    throw new ConfigServiceException(ErrorCode.CONFIGURATION_SERVICE_INPUT_MISSING_NODES,
                                                     "Invalid node data in the request : " + sessionId, e);
                }
            });
        }
        return list;
    }

    /**
     * Get List of nodes via NodeList.
     *
     * @param nodesInfoMap Node map from request.
     * @return NodeList
     */
    public static BlockchainNodeList getNodeList(Map<String, NodesInfo> nodesInfoMap, UUID sessionId)
            throws ConfigServiceException {
        if (nodesInfoMap == null || nodesInfoMap.isEmpty()) {
            log.error("Nodes info map is empty.");
            throw new ConfigServiceException(ErrorCode.CONFIGURATION_SERVICE_INPUT_MISSING_NODES,
                                             "Invalid node data in the request : " + sessionId);
        }
        List<BlockchainReplica> replicaNodes =
                getNodeListOfType(nodesInfoMap, NodeType.REPLICA, BlockchainReplica.class, sessionId);
        List<BlockchainClient> clientNodes =
                getNodeListOfType(nodesInfoMap, NodeType.CLIENT, BlockchainClient.class, sessionId);
        List<BlockchainReadReplica> readReplicaNodes =
                getNodeListOfType(nodesInfoMap, NodeType.READ_REPLICA, BlockchainReadReplica.class,
                                  sessionId);
        if (replicaNodes == null || replicaNodes.isEmpty()) {
            log.error("Required nodes replica are missing.");
            throw new ConfigServiceException(ErrorCode.CONFIGURATION_SERVICE_INPUT_MISSING_NODES,
                                             "Invalid node data in the request : " + sessionId);
        }

        return BlockchainNodeList.builder().replicas(replicaNodes).clients(clientNodes).readReplicas(readReplicaNodes)
                .build();
    }

    /**
     * Generate a new {@link UUID} based on a given request {@link MessageHeader}, or generate a
     * random value as ID if request does not contain sufficient parametric data.
     *
     */
    public static UUID extractOrGenerateId(String idString) {
        UUID uuid;
        if (idString.isEmpty()) {
            uuid = UUID.randomUUID();
        } else {
            // Hash the string value into an UUID.
            uuid = UUID.fromString(idString);
        }
        return uuid;
    }

}
