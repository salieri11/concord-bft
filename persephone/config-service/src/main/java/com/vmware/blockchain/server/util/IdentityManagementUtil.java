/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.util;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.vmware.blockchain.configuration.eccerts.TrsTrcTlsSingleCertificateGenerator;
import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.ConfigUtilHelpers;
import com.vmware.blockchain.configuration.generateconfig.ValidationUtil;
import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

import lombok.extern.slf4j.Slf4j;

/**
 * Utilities to create identities.
 */
@Slf4j
public class IdentityManagementUtil {

    private final BlockchainNodeList nodeList;
    private final BlockchainFeatures bcFeatures;
    private final String blockchainId;

    /**
     * Constructor per blockchain.
     * @param nodeList {@link BlockchainNodeList} list of all nodes
     * @param bcFeatures {@link BlockchainFeatures} list of all features
     * @param blockchainId blockchain id
     */
    public IdentityManagementUtil(BlockchainNodeList nodeList, BlockchainFeatures bcFeatures, String blockchainId) {
        this.nodeList = nodeList;
        this.bcFeatures = bcFeatures;
        this.blockchainId = blockchainId;
    }

    /**
     * Get TLS node identities of all nodes in the BLockchain.
     * @param concordNodePrincipals Principals for concord nodes (Replica, Read replica)
     * @param bftClientNodePrincipals Principals for Clients
     * @param certGen Certificate generator
     * @param clientProxyPerParticipant Client proxies per participant
     * @return {@link IdentityComponentsLists} list of all identity components
     * @throws ConfigServiceException during an error during the cert generation.
     */
    public IdentityComponentsLists getAllTlsNodeIdentities(Map<Integer, List<Integer>> concordNodePrincipals,
                                        Map<Integer, List<Integer>> bftClientNodePrincipals,
                                        CertificatesGenerator certGen,
                                        int clientProxyPerParticipant)
            throws ConfigServiceException {
        if (!ValidationUtil.isValid(concordNodePrincipals)
                || !ValidationUtil.isValid(certGen) || !ValidationUtil.isValid(nodeList)
                || !ValidationUtil.isValid(this.bcFeatures)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                    "Invalid input parameters.");
        }

        IdentityComponentsLists identityComponentsLists = IdentityComponentsLists.builder().build();

        boolean considerClients = true;
        if (this.nodeList.getClientSize() == 0) {
            considerClients = false;
            if (!bftClientNodePrincipals.isEmpty()) {
                throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                        "Blockchain has no clients. It can not have bft principals.");
            }
        }

        // Calculate max principalId, number of clients etc., based on the node list.
        /**
         * Client config.
         */
        var totalReplicas =
                this.bcFeatures.isObjectStoreEnabled() ? nodeList.getAllReplicasSize() : nodeList.getReplicaSize();
        var maxCommitterPrincipalId = (totalReplicas + ConfigUtilHelpers.CLIENT_PROXY_PER_COMMITTER
                * this.nodeList.getReplicaSize()) - 1;

        var bftClientNumPrincipals = 0;

        /**
         * Concord config.
         * Replicas and Read replicas principal id max number.
         */
        int numPrincipals = maxCommitterPrincipalId + 1;

        Map<Integer, List<Integer>> nodePrincipal = new HashMap<>(concordNodePrincipals);

        if (considerClients) {
            bftClientNumPrincipals = maxCommitterPrincipalId + ((nodeList.getClientSize() + clientProxyPerParticipant
                    * this.nodeList.getClientSize()) - 1);
            numPrincipals = bftClientNumPrincipals + 1;
            bftClientNodePrincipals.forEach((key, value) ->
                    nodePrincipal.put(concordNodePrincipals.size() + key, value));
        }

        List<Identity> tlsIdentityList =
                certGen.generateSelfSignedCertificates(numPrincipals,
                        ConcordComponent.ServiceType.CONCORD);

        buildConcordAndBftClientTlsIdentity(tlsIdentityList, nodePrincipal,
                numPrincipals, considerClients, identityComponentsLists);

        if (considerClients) {
            buildTrsTrcTlsIdentities(identityComponentsLists);
        } else {
            identityComponentsLists.setTrsIdentityComponents(new HashMap<>());
            identityComponentsLists.setTrcIdentityComponents(new HashMap<>());
        }

        return identityComponentsLists;
    }

    /**
     * build identity list for trs trc tls connections.
     */
    void buildTrsTrcTlsIdentities(IdentityComponentsLists identityComponentsLists) {

        Map<String, List<IdentityComponent>> trsIdentityComponents = new HashMap<>();
        Map<String, List<IdentityComponent>> trcIdentityComponents = new HashMap<>();

        if (!this.bcFeatures.isTrcTlsEnabled()) {
            log.info("Trs-Trc-Tls not enabled.");
            identityComponentsLists.setTrsIdentityComponents(trsIdentityComponents);
            identityComponentsLists.setTrcIdentityComponents(trcIdentityComponents);
            return;
        }
        List<CompletableFuture<Map.Entry<String, Identity>>> futures = new ArrayList<>();
        Map<String, String> clientIdMap = new HashMap<>();
        this.nodeList.getReplicas().forEach(replica -> {
            futures.add(CompletableFuture.supplyAsync(() -> getTrsTrcIdentityPerNode(replica.getId(),
                    ServiceType.CONCORD,
                    replica.getIp(),
                    this.blockchainId)));
        });
        this.nodeList.getClients().forEach(client -> {
            futures.add(CompletableFuture.supplyAsync(() -> getTrsTrcIdentityPerNode(client.getId(),
                    ServiceType.DAML_LEDGER_API,
                    client.getClientGroupId(),
                    this.blockchainId)));
            clientIdMap.put(client.getId(), client.getClientGroupId());
        });

        List<Map.Entry<String, Identity>> futRes = futures.stream()
                .map(CompletableFuture::join).collect(Collectors.toList());

        List<Identity> replicaIdentities = new ArrayList<>();
        List<Identity> clientIdentities = new ArrayList<>();
        List<String> replicaIds = this.nodeList.getReplicaNodeIds();
        List<String> clientIds = this.nodeList.getClientNodeIds();
        futRes.forEach(res -> {
            if (replicaIds.contains(res.getKey())) {
                replicaIdentities.add(res.getValue());
            }
            if (clientIds.contains(res.getKey())) {
                clientIdentities.add(res.getValue());
            }
        });
        String concatReplica = concatCerts(replicaIdentities);
        String concatClient = concatCerts(clientIdentities);
        Map<String, String> nodesMap = this.nodeList.getAllNodesIdsAndIps();
        futRes.forEach(res -> {
            List<IdentityComponent> components = new ArrayList<>();
            components.add(res.getValue().getCertificate());
            components.add(res.getValue().getKey());
            if (replicaIds.contains(res.getKey())) {
                components.add(getCertIdentity(concatClient,
                        CertificatesGenerator.FILE_PREFIX
                                + CertificatesGenerator.TRS_TLS_IDENTITY_PATH
                                + "/" + nodesMap.get(res.getKey()) + "/" + "client.cert"));
                trsIdentityComponents.put(res.getKey(), components);
            }
            if (clientIds.contains(res.getKey())) {
                components.add(getCertIdentity(concatReplica,
                        CertificatesGenerator.FILE_PREFIX
                                + CertificatesGenerator.TRC_TLS_IDENTITY_PATH
                                + "/" + clientIdMap.get(res.getKey()) + "/" + "server.cert"));
                trcIdentityComponents.put(res.getKey(), components);
            }
        });
        identityComponentsLists.setTrsIdentityComponents(trsIdentityComponents);
        identityComponentsLists.setTrcIdentityComponents(trcIdentityComponents);
    }

    /**
     * Generate TLS identities based on nodetypes and principal Ids.
     */
    private void buildConcordAndBftClientTlsIdentity(List<Identity> identities,
                                                     Map<Integer, List<Integer>> principals,
                                                     int numCerts, boolean considerClients,
                                                     IdentityComponentsLists identityComponentsLists)
            throws ConfigServiceException {
        if (!ValidationUtil.isValid(identities) || !ValidationUtil.isValid(principals) || !ValidationUtil
                .isValidNodeList(this.nodeList)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                    "Invalid input parameters.");
        }

        Map<String, List<IdentityComponent>> concordIdentityComponents = new HashMap<>();
        Map<String, List<IdentityComponent>> bftIdentityComponents = new HashMap<>();

        // All applicable node Ids in this deployment, the order of nodes is very important due to the following code.
        var allNodeIds = getApplicableNodeIds(this.nodeList, this.bcFeatures, considerClients);
        log.info("All node Ids in this deployment {}", allNodeIds);
        // Also check to see if there are enough nodes as principals.
        if (allNodeIds.size() < principals.keySet().size()) {
            log.error("Available nodes are less than principals.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                    "Not enough nodes are available in the node list.");
        }
        for (int node : principals.keySet()) {
            // We assume that if clients are available, then this must be a DAML Blockchain.

            // If this node is Client node and BFT is enabled, then convert the URL.
            // Current assumption is all client nodes have BFT client feature enabled.
            boolean isBftClient = this.nodeList.getClientNodeIds() != null && nodeList.getClientNodeIds()
                    .contains(allNodeIds.get(node));

            List<IdentityComponent> nodeIdentities = new ArrayList<>();

            List<Integer> notPrincipal = IntStream.range(0, numCerts)
                    .boxed().collect(Collectors.toList());
            notPrincipal.removeAll(principals.get(node));

            List<Identity> serverList = new ArrayList<>(identities.subList(0, identities.size() / 2));
            List<Identity> clientList = new ArrayList<>(identities.subList(identities.size() / 2, identities.size()));

            notPrincipal.forEach(entry -> {
                nodeIdentities.add(replaceClientUrl(serverList.get(entry).getCertificate(), isBftClient));
                nodeIdentities.add(replaceClientUrl(clientList.get(entry).getCertificate(), isBftClient));
            });

            // add self keys
            nodeIdentities.add(replaceClientUrl(serverList.get(node).getKey(), isBftClient));
            nodeIdentities.add(replaceClientUrl(clientList.get(node).getKey(), isBftClient));

            principals.get(node).forEach(entry -> {
                nodeIdentities.add(replaceClientUrl(serverList.get(entry).getCertificate(), isBftClient));
                nodeIdentities.add(replaceClientUrl(serverList.get(entry).getKey(), isBftClient));
                nodeIdentities.add(replaceClientUrl(clientList.get(entry).getCertificate(), isBftClient));
                nodeIdentities.add(replaceClientUrl(clientList.get(entry).getKey(), isBftClient));
            });
            // Add the Identities for the node.
            if (isBftClient) {
                bftIdentityComponents.putIfAbsent(allNodeIds.get(node), nodeIdentities);
                log.info("Total node mappings available for node {} is {}", node, bftIdentityComponents.size());
            } else {
                concordIdentityComponents.putIfAbsent(allNodeIds.get(node), nodeIdentities);
                log.info("Total node mappings available for node {} is {}", node, concordIdentityComponents.size());
            }
        }
        identityComponentsLists.setBftIdentityComponents(bftIdentityComponents);
        identityComponentsLists.setConcordIdentityComponents(concordIdentityComponents);
    }


    /**
     * Replace Identity URL, essentially for client nodes.
     * @param identityComponent Identity component
     * @param replaceUrl Identifies whether replacement is needed
     * @return If replaceUrl is true, then replace and return the Identity component, otherwise return as-is
     */
    private IdentityComponent replaceClientUrl(IdentityComponent identityComponent, boolean replaceUrl) {
        if (identityComponent == null || identityComponent.getUrl() == null) {
            log.error("Identity component or it's URL is missing.");
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_INVALID_INPUT_FAILURE,
                    "Invalid Identity component.");
        }
        if (!replaceUrl) {
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
     * Return a list of applicable nodes, based on Blockchain features.
     * @param bcFeatures Blockchain features
     * @return A list of applicable node Ids.
     */
    private List<String> getApplicableNodeIds(BlockchainNodeList nodeList, BlockchainFeatures bcFeatures,
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

    /**
     * get per node identity element.
     */
    private Map.Entry<String, Identity> getTrsTrcIdentityPerNode(String id, ServiceType serviceType,
                                                                        String identifier, String blockchainId) {
        CertificatesGenerator certificatesGenerator = new TrsTrcTlsSingleCertificateGenerator(identifier);
        List<Identity> identity = certificatesGenerator
                .generateSelfSignedCertificates(1, serviceType, identifier, blockchainId);

        return new AbstractMap.SimpleEntry<>(id, identity.get(0));
    }

    /**
     * build concatenation of certs.
     */
    private String concatCerts(List<Identity> identities) {
        StringBuilder concatCertificates = new StringBuilder();
        for (Identity identity:identities) {
            concatCertificates.append(identity.getCertificate().getBase64Value());
        }
        return concatCertificates.toString();
    }

    /**
     * build concatenation of certs.
     */
    private IdentityComponent getCertIdentity(String cert, String url) {
        return  IdentityComponent.newBuilder()
                .setType(IdentityComponent.Type.CERTIFICATE)
                .setBase64Value(cert)
                .setUrl(url)
                .build();
    }
}