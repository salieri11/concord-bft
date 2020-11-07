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
     * Get tls node identities.
     * TODO: better refactoring when deprecated code cleaned up
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

        if (bcFeatures.isBftEnabled()) {
            numPrincipals = bftClientNumPrincipals + 1;
            bftClientNodePrincipals.forEach((key, value) ->
                    nodePrincipal.put(concordNodePrincipals.size() + key, value));
        }

        List<Identity> tlsIdentityList =
                certGen.generateSelfSignedCertificates(numPrincipals,
                        ConcordComponent.ServiceType.CONCORD);

        Map<String, List<IdentityComponent>> tlsNodeIdentities = buildTlsIdentity(
                nodeList.getAllNodeIds(),
                tlsIdentityList,
                nodePrincipal,
                numPrincipals);
        return tlsNodeIdentities;
    }

    /**
     * Filter tls identities based on nodes and principal ids.
     */
    static Map<String, List<IdentityComponent>> buildTlsIdentity(List<String> nodeIds,
                                                          List<Identity> identities,
                                                          Map<Integer, List<Integer>> principals,
                                                          int numCerts) {
        if (!ValidationUtil.isValid(nodeIds) || !ValidationUtil.isValid(identities)
            || !ValidationUtil.isValid(principals)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                                             "Invalid input parameters.");
        }


        Map<String, List<IdentityComponent>> result = new HashMap<>();

        // TODO: May remove logic once principals are available
        if (principals.size() == 0) {
            IntStream.range(0, nodeIds.size()).forEach(node -> {
                List<IdentityComponent> identityComponents = new ArrayList<>();
                identities.forEach(identity -> {
                    identityComponents.add(identity.getCertificate());
                    identityComponents.add(identity.getKey());
                });
                result.put(nodeIds.get(node), identityComponents);
            });
            return result;
        }

        for (int node : principals.keySet()) {
            List<IdentityComponent> nodeIdentities = new ArrayList<>();

            List<Integer> notPrincipal = IntStream.range(0, numCerts)
                    .boxed().collect(Collectors.toList());
            notPrincipal.removeAll(principals.get(node));

            List<Identity> serverList = new ArrayList<>(identities.subList(0, identities.size() / 2));
            List<Identity> clientList = new ArrayList<>(identities.subList(identities.size() / 2, identities.size()));

            notPrincipal.forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getCertificate());
            });

            // add self keys
            nodeIdentities.add(serverList.get(node).getKey());
            nodeIdentities.add(clientList.get(node).getKey());

            principals.get(node).forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(serverList.get(entry).getKey());
                nodeIdentities.add(clientList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getKey());
            });
            result.putIfAbsent(nodeIds.get(node), nodeIdentities);
        }

        log.info("Filtered tls identities based on nodes and principal ids.");
        return result;
    }

    /**
     * transform concord node identities to bft node identities.
     */
    static Map<String, List<IdentityComponent>> convertToBftTlsNodeIdentities(
            Map<String, List<IdentityComponent>> tlsNodeIdentities, BlockchainNodeList nodeList) {
        if (tlsNodeIdentities == null || tlsNodeIdentities.isEmpty()) {
            log.error("No node identities are available to process.");
            return new HashMap<>();
        }
        Map<String, List<IdentityComponent>> bftIdentityComponents = new HashMap<>();

        tlsNodeIdentities.forEach((key, value) -> {
            // Do not convert when nodeList is available, and the nodeId is a Read Replica.
            if (nodeList != null && nodeList.getReadReplicaNodeIds().contains(key)) {
                // Keep it as-is in the case of a Read Replica.
                bftIdentityComponents.put(key, value);
            } else {
                List<IdentityComponent> bftIdentities = new ArrayList<>();
                value.forEach(val -> {
                    String newUrl = val.getUrl().replace(
                            CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH,
                            CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
                    IdentityComponent ident = IdentityComponent.newBuilder()
                            .setType(val.getType())
                            .setBase64Value(val.getBase64Value())
                            .setUrl(newUrl)
                            .build();
                    bftIdentities.add(ident);
                });
                bftIdentityComponents.put(key, bftIdentities);
            }
        });

        return bftIdentityComponents;
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
        if (replicaNodes == null || replicaNodes.isEmpty() || clientNodes == null || clientNodes.isEmpty()) {
            log.error("Required nodes replica or clients are missing.");
            throw new ConfigServiceException(ErrorCode.CONFIGURATION_SERVICE_INPUT_MISSING_NODES,
                                             "Invalid node data in the request : " + sessionId);
        }

        return BlockchainNodeList.builder().replicas(replicaNodes).clients(clientNodes).readReplicas(readReplicaNodes)
                .build();
    }

}
