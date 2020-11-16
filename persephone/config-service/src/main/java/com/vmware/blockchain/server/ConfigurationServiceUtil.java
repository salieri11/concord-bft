/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.ConfigUtilHelpers;
import com.vmware.blockchain.configuration.generateconfig.ValidationUtil;
import com.vmware.blockchain.configuration.util.BlockchainClient;
import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNode;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReadReplica;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

import lombok.extern.slf4j.Slf4j;

/**
 * Implementation of ConfigurationService server.
 */
@Slf4j
public class ConfigurationServiceUtil {

    /**
     * Generate a new {@link ConfigurationSessionIdentifier}.
     *
     * @return
     *   a new {@link ConfigurationSessionIdentifier} instance.
     */

    static ConfigurationSessionIdentifier newSessionUId() {
        return ConfigurationSessionIdentifier.newBuilder().setId(UUID.randomUUID().toString()).build();
    }

    /**
     * Get TLS node identities of all nodes in the BLockchain.
     * @param concordNodePrincipals Principals for concord nodes (Replica, Read replica)
     * @param bftClientNodePrincipals Principals for Clients
     * @param certGen Certificate generator
     * @param nodeList List of all nodes in the Blockchain
     * @param bcFeatures Blockchain features
     * @param clientProxyPerParticipant Client proxies per participant
     * @return A map of Node ids and its list of TLS identities.
     * @throws ConfigServiceException during an error during the cert generation.
     */
    static Map<String, List<IdentityComponent>> getTlsNodeIdentities(Map<Integer, List<Integer>> concordNodePrincipals,
                                                        Map<Integer, List<Integer>> bftClientNodePrincipals,
                                                        ConcordEcCertificatesGenerator certGen,
                                                        BlockchainNodeList nodeList,
                                                        BlockchainFeatures bcFeatures,
                                                        int clientProxyPerParticipant)
            throws ConfigServiceException {
        if (!ValidationUtil.isValid(concordNodePrincipals) || !ValidationUtil.isValid(bftClientNodePrincipals)
            || !ValidationUtil.isValid(certGen) || !ValidationUtil.isValid(nodeList)
            || !ValidationUtil.isValid(bcFeatures)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                                             "Invalid input parameters.");
        }

        // Calculate max principalId, number of clients etc., based on the node list.
        /**
         * Client config.
         */
        var totalReplicas =
                bcFeatures.isObjectStoreEnabled() ? nodeList.getAllReplicasSize() : nodeList.getReplicaSize();
        var maxCommitterPrincipalId = (totalReplicas + ConfigUtilHelpers.CLIENT_PROXY_PER_COMMITTER
                                                       * nodeList.getReplicaSize()) - 1;
        var maxPrincipalId = maxCommitterPrincipalId + ((nodeList.getClientSize() + clientProxyPerParticipant
                                                                                    * nodeList.getClientSize()) - 1);
        var bftClientNumPrincipals = maxPrincipalId;

        /**
         * Concord config.
         * Replicas and Read replicas principal id max number.
         */
        int numPrincipals = maxCommitterPrincipalId + 1;

        Map<Integer, List<Integer>> nodePrincipal = new HashMap<>(concordNodePrincipals);
        // We will consider clients only when they are available.
        // When Blockchain type is NOT DAML, then no clients are available.
        boolean considerClients = !bftClientNodePrincipals.isEmpty();

        if (considerClients) {
            numPrincipals = bftClientNumPrincipals + 1;
            bftClientNodePrincipals.forEach((key, value) ->
                    nodePrincipal.put(concordNodePrincipals.size() + key, value));
        }

        List<Identity> tlsIdentityList =
                certGen.generateSelfSignedCertificates(numPrincipals,
                        ConcordComponent.ServiceType.CONCORD);

        return buildTlsIdentity(
                nodeList,
                tlsIdentityList,
                nodePrincipal,
                numPrincipals,
                bcFeatures,
                considerClients);
    }

    /**
     * Generate TLS identities based on nodetypes and principal Ids.
     * @param nodeList List of nodes
     * @param identities TLS identities
     * @param principals Principal nodes
     * @param numCerts Number of certificates generated
     * @param bcFeatures Features of this Blockchain
     * @return A map of TLS node identities for all nodes.
     */
    static Map<String, List<IdentityComponent>> buildTlsIdentity(BlockchainNodeList nodeList, List<Identity> identities,
                                                                 Map<Integer, List<Integer>> principals, int numCerts,
                                                                 BlockchainFeatures bcFeatures,
                                                                 boolean considerClients)
            throws ConfigServiceException {
        if (!ValidationUtil.isValid(identities) || !ValidationUtil.isValid(principals) || !ValidationUtil
                .isValidNodeList(nodeList)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                                             "Invalid input parameters.");
        }
        // Principals are always available, so there is no need to check if 'principals' is empty.
        Map<String, List<IdentityComponent>> result = new HashMap<>();
        // All applicable node Ids in this deployment, the order of nodes is very important due to the following code.
        var allNodeIds = getApplicableNodeIds(nodeList, bcFeatures, considerClients);
        log.info("All node Ids in this deployment {}", allNodeIds);
        // Also check to see if there are enough nodes as principals.
        if (allNodeIds.size() < principals.keySet().size()) {
            log.error("Available nodes are less than principals.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                                             "Not enough nodes are available in the node list.");
        }
        for (int node : principals.keySet()) {
            // We assume that if clients are available, then this must be a DAML Blockchain.
            boolean isThisClientNode =
                    nodeList.getClientNodeIds() != null && nodeList.getClientNodeIds().contains(allNodeIds.get(node));

            // If this node is Client node and BFT is enabled, then convert the URL.
            boolean replaceUrl = isThisClientNode;

            List<IdentityComponent> nodeIdentities = new ArrayList<>();

            List<Integer> notPrincipal = IntStream.range(0, numCerts)
                    .boxed().collect(Collectors.toList());
            notPrincipal.removeAll(principals.get(node));

            List<Identity> serverList = new ArrayList<>(identities.subList(0, identities.size() / 2));
            List<Identity> clientList = new ArrayList<>(identities.subList(identities.size() / 2, identities.size()));

            notPrincipal.forEach(entry -> {
                nodeIdentities.add(replaceClientUrl(serverList.get(entry).getCertificate(), replaceUrl));
                nodeIdentities.add(replaceClientUrl(clientList.get(entry).getCertificate(), replaceUrl));
            });

            // add self keys
            nodeIdentities.add(replaceClientUrl(serverList.get(node).getKey(), replaceUrl));
            nodeIdentities.add(replaceClientUrl(clientList.get(node).getKey(), replaceUrl));

            principals.get(node).forEach(entry -> {
                nodeIdentities.add(replaceClientUrl(serverList.get(entry).getCertificate(), replaceUrl));
                nodeIdentities.add(replaceClientUrl(serverList.get(entry).getKey(), replaceUrl));
                nodeIdentities.add(replaceClientUrl(clientList.get(entry).getCertificate(), replaceUrl));
                nodeIdentities.add(replaceClientUrl(clientList.get(entry).getKey(), replaceUrl));
            });
            // Add the Identities for the node.
            result.putIfAbsent(allNodeIds.get(node), nodeIdentities);
        }
        log.info("Total node mappings available {}", result.size());
        return result;
    }


    /**
     * Replace Identity URL, essentially for client nodes.
     * @param identityComponent Identity component
     * @param replaceUrl Identifies whether replacement is needed
     * @return If replaceUrl is true, then replace and return the Identity component, otherwise return as-is.
     */
    private static IdentityComponent replaceClientUrl(IdentityComponent identityComponent, boolean replaceUrl) {
        if (identityComponent == null || identityComponent.getUrl() == null) {
            log.error("Identity component or it's URL is missing.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                                             "Invalid Identity component.");
        }
        if (!replaceUrl) {
            log.info("No need to replace Identity component URL.");
            return identityComponent;
        }
        String newUrl = identityComponent.getUrl().replace(
                CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH,
                CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
        try {
            return IdentityComponent.newBuilder()
                    .setType(identityComponent.getType())
                    .setBase64Value(identityComponent.getBase64Value())
                    .setUrl(newUrl)
                    .build();
        } catch (NullPointerException npe) {
            log.error("Failed to build an Identity component with replaced URL.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                                             "Failed to build a new Identity component with replaced URL.");
        }
    }

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
                                                                          String sessionId)
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
                    if (obj.getClass().isAssignableFrom(BlockchainReadReplica.class)) {
                        BlockchainReadReplica readReplica = BlockchainReadReplica.class.cast(obj);
                        Map<String, String> valueMap = each.getProperties().getValuesMap();
                        readReplica.setObjStoreBucketName(
                                valueMap.get(BlockchainReadReplica.OBJ_STORE_BUCKET_NAME_KEY_FOR_API));
                        readReplica
                                .setObjStoreAccessKey(valueMap.get(BlockchainReadReplica.OBJ_STORE_ACCESS_KEY_FOR_API));
                        readReplica.setObjStoreProtocol(
                                valueMap.get(BlockchainReadReplica.OBJ_STORE_PROTOCOL_KEY_FOR_API));
                        readReplica.setObjStoreUrl(valueMap.get(BlockchainReadReplica.OBJ_STORE_URL_KEY_FOR_API));
                        readReplica.setObjStoreSecret(valueMap.get(BlockchainReadReplica.OBJ_STORE_SECRET_KEY_FOR_API));
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
    public static BlockchainNodeList getNodeList(Map<String, NodesInfo> nodesInfoMap, String sessionId)
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
            log.error("Required replica nodes are missing.");
            throw new ConfigServiceException(ErrorCode.CONFIGURATION_SERVICE_INPUT_MISSING_NODES,
                                             "Invalid node data in the request : " + sessionId);
        }

        return BlockchainNodeList.builder().replicas(replicaNodes).clients(clientNodes).readReplicas(readReplicaNodes)
                .build();
    }

    /**
     * Return a list of applicable nodes, based on Blockchain features.
     * @param bcFeatures Blockchain features
     * @return A list of applicable node Ids.
     */
    private static List<String> getApplicableNodeIds(BlockchainNodeList nodeList, BlockchainFeatures bcFeatures,
                                                     boolean considerClients) {
        // Prepare a list of applicable node Ids to be sent to build method.
        List<String> applicableNodeIds = new ArrayList<>();
        if (bcFeatures.isObjectStoreEnabled()) {
            applicableNodeIds.addAll(nodeList.getAllReplicaNodeIds());
        } else {
            applicableNodeIds.addAll(nodeList.getReplicaNodeIds());
        }
        if (considerClients) {
            applicableNodeIds.addAll(nodeList.getClientNodeIds());
        }
        return applicableNodeIds;
    }

}
