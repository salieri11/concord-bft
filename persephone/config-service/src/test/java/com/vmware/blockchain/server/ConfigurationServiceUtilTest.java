/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.security.Security;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.configuration.util.BlockchainClient;
import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReadReplica;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;

/**
 * Unit test for config service helper methods.
 */
public class ConfigurationServiceUtilTest {

    static int numConcordCerts = 20;
    static int numBftCerts = 35;

    static int numConcordCertsRo = 24;
    ConcordEcCertificatesGenerator certGen = new ConcordEcCertificatesGenerator();
    static Map<Integer, List<Integer>> concordPrincipals = new HashMap<>();
    static Map<Integer, List<Integer>> bftPrincipals = new HashMap<>();
    static Map<Integer, List<Integer>> concordPrincipalsRo = new HashMap<>();
    static Map<Integer, List<Integer>> bftPrincipalsRo = new HashMap<>();

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());

        concordPrincipals.put(0, Arrays.asList(4, 8, 12, 16, 0));
        concordPrincipals.put(1, Arrays.asList(5, 9, 13, 17, 1));
        concordPrincipals.put(2, Arrays.asList(6, 10, 14, 18, 2));
        concordPrincipals.put(3, Arrays.asList(7, 11, 15, 19, 3));

        bftPrincipals.put(0, Arrays.asList(20, 21));

        // variables for RO cases.
        concordPrincipalsRo.put(0, Arrays.asList(6, 10, 14, 18, 0));
        concordPrincipalsRo.put(1, Arrays.asList(7, 11, 15, 19, 1));
        concordPrincipalsRo.put(2, Arrays.asList(8, 12, 16, 20, 2));
        concordPrincipalsRo.put(3, Arrays.asList(9, 13, 17, 21, 3));
        concordPrincipalsRo.put(4, List.of());
        concordPrincipalsRo.put(5, List.of());

        bftPrincipalsRo.put(0, Arrays.asList(20, 21));

    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    void testGetTlsNodeIdentitiesConcord() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().clients(clients).replicas(replicas).build();

        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().build();

        var actual = ConfigurationServiceUtil.getTlsNodeIdentities(concordPrincipals,
                                                                                                    bftPrincipals,
                                                                                                    certGen,
                                                                                                    nodeList,
                                                                                                    bcFeatures,
                                                                                                    2);
        Assertions.assertFalse(actual.isEmpty());
        checkValues(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    /**
     * Test with RO replica. This is a non-DAML case.
     */
    @Test
    void testGetTlsNodeIdentitiesConcordNonDamlWithRo() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        List<BlockchainReadReplica> readReplicas =
                List.of(new BlockchainReadReplica("ronode1", "addr1"),
                        new BlockchainReadReplica("ronode2", "addr2"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().clients(clients).replicas(replicas).readReplicas(readReplicas)
                        .build();

        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(true).build();

        var actual = ConfigurationServiceUtil.getTlsNodeIdentities(concordPrincipalsRo,
                                                                                                    bftPrincipalsRo,
                                                                                                    certGen,
                                                                                                    nodeList,
                                                                                                    bcFeatures, 2);
        Assertions.assertFalse(actual.isEmpty());
        checkValuesRo(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    @Test
    void testGetTlsNodeIdentitiesBftClient() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().clients(clients).replicas(replicas).build();
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().build();
        Map<String, List<IdentityComponent>> actual = ConfigurationServiceUtil.getTlsNodeIdentities(
                concordPrincipals,
                bftPrincipals,
                certGen,
                nodeList,
                bcFeatures, 15);

        Assertions.assertFalse(actual.isEmpty());
        checkValues(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    /**
     * This case is for DAML and Object store is enabled.
     */
    @Test
    void testGetTlsNodeIdentitiesBftClientRo() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("ronode1", "addr1"),
                                                           new BlockchainReadReplica("ronode2", "addr1"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().clients(clients).replicas(replicas).readReplicas(readReplicas).build();
        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(true).build();
        Map<String, List<IdentityComponent>> actual = ConfigurationServiceUtil.getTlsNodeIdentities(
                concordPrincipalsRo,
                bftPrincipalsRo,
                certGen,
                nodeList,
                bcFeatures, 2);
        Assertions.assertFalse(actual.isEmpty());
        checkValuesRo(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    @Test
    void testBuildTlsIdentity() {
        List<String> nodeIds = Arrays.asList("node1", "node2", "node3", "node4");
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().replicas(replicas).build();
        List<Identity> identities = certGen.generateSelfSignedCertificates(numConcordCerts,
                ConcordComponent.ServiceType.CONCORD);
        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(false).build();
        // Do not consider clients, because this is non DAML case.
        boolean considerClients = false;

        var actual = ConfigurationServiceUtil
                .buildTlsIdentity(nodeList, identities, concordPrincipals, numConcordCerts, bcFeatures,
                                  considerClients);

        Assertions.assertFalse(actual.isEmpty());
        checkValues(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    @Test
    void testBuildTlsIdentityRoDaml() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("ronode1", "addr1"),
                                                           new BlockchainReadReplica("ronode2", "addr1"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().clients(clients).replicas(replicas).readReplicas(readReplicas).build();
        List<Identity> identities = certGen.generateSelfSignedCertificates(numConcordCertsRo,
                                                                           ConcordComponent.ServiceType.CONCORD);
        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(true).build();
        // Consider clients, because this is DAML case.
        boolean considerClients = true;
        var actual = ConfigurationServiceUtil
                .buildTlsIdentity(nodeList, identities, concordPrincipalsRo, numConcordCertsRo, bcFeatures,
                                  considerClients);

        Assertions.assertFalse(actual.isEmpty());
        checkValuesRo(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    @Test
    void testBuildTlsIdentityRoNonDaml() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("ronode1", "addr1"),
                                                           new BlockchainReadReplica("ronode2", "addr1"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().clients(clients).replicas(replicas).readReplicas(readReplicas).build();
        List<Identity> identities = certGen.generateSelfSignedCertificates(numConcordCertsRo,
                                                                           ConcordComponent.ServiceType.CONCORD);
        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(true).build();
        // Do not consider clients, because this is non DAML case.
        boolean considerClients = false;

        var actual = ConfigurationServiceUtil
                .buildTlsIdentity(nodeList, identities, concordPrincipalsRo, numConcordCertsRo, bcFeatures,
                                  considerClients);

        Assertions.assertFalse(actual.isEmpty());
        checkValuesRo(actual);
        checkClientUrl(actual, nodeList.getClientNodeIds());
    }

    @Test
    void testGetNodeListOfTypePositive() {
        Map<String, NodesInfo> nodesInfoMap = new HashMap<>();
        NodesInfo.Entry entry = NodesInfo.Entry.newBuilder().setType(NodeType.REPLICA).setId("id1").setNodeIp("ip1")
                .build();
        nodesInfoMap.put(NodeType.REPLICA.name(), NodesInfo.newBuilder().addEntries(entry).build());
        List<BlockchainReplica> replicas = ConfigurationServiceUtil.getNodeListOfType(nodesInfoMap, NodeType.REPLICA,
                                                                            BlockchainReplica.class, "id");
        Assertions.assertTrue(!replicas.isEmpty());
    }


    @Test
    void testGetNodeListOfTypeNegative() {
        org.assertj.core.api.Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> ConfigurationServiceUtil.getNodeListOfType(null, NodeType.READ_REPLICA,
                                                                             BlockchainReplica.class, "id"))
                .withMessage("Invalid node data in the request : id");
    }

    @Test
    void testGetNodeListPositive() {
        Map<String, NodesInfo> nodesInfoMap = new HashMap<>();
        NodesInfo.Entry entry1 = NodesInfo.Entry.newBuilder().setType(NodeType.REPLICA).setId("id1").setNodeIp("ip1")
                .build();
        NodesInfo.Entry entry2 = NodesInfo.Entry.newBuilder().setType(NodeType.READ_REPLICA).setId("id2")
                .setNodeIp("ip2").build();
        NodesInfo.Entry entry3 = NodesInfo.Entry.newBuilder().setType(NodeType.CLIENT).setId("id3").setNodeIp("ip3")
                .build();
        nodesInfoMap.put(NodeType.REPLICA.name(), NodesInfo.newBuilder().addEntries(entry1).build());
        nodesInfoMap.put(NodeType.READ_REPLICA.name(), NodesInfo.newBuilder().addEntries(entry2).build());
        nodesInfoMap.put(NodeType.CLIENT.name(), NodesInfo.newBuilder().addEntries(entry3).build());

        BlockchainNodeList nodeList = ConfigurationServiceUtil.getNodeList(nodesInfoMap, "id");
        Assertions.assertEquals(1, nodeList.getReplicaSize(), "Replica size is not correct.");
        Assertions.assertEquals(1, nodeList.getClientSize(), "Client size is not correct.");
        Assertions.assertEquals(1, nodeList.getReadReplicaSize(),
                                "Read Replica size is not correct.");
        Assertions.assertEquals(3, nodeList.getAllNodeIds().size(), "Total node count is incorrect.");
        Assertions.assertEquals(2, nodeList.getAllReplicaNodeIds().size(),
                                "Total node count is incorrect.");
    }

    @Test
    void testGetNodeListNegative() {
        org.assertj.core.api.Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> ConfigurationServiceUtil.getNodeList(null, "id"))
                .withMessage("Invalid node data in the request : id");
    }


    private void checkValues(Map<String, List<IdentityComponent>> actual) {
        actual.forEach((key, value) -> {
            value.forEach(identity -> {
                switch (key) {
                    case "node1":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/0/")
                                                  || identity.getUrl().contains("/4/") || identity.getUrl()
                                                          .contains("/8/") || identity.getUrl().contains("/12/")
                                                  || identity.getUrl().contains("/16/"));
                        }
                        break;
                    case "node2":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/1/")
                                                  || identity.getUrl().contains("/5/") || identity.getUrl()
                                                          .contains("/9/") || identity.getUrl().contains("/13/")
                                                  || identity.getUrl().contains("/17/"));
                        }
                        break;
                    case "node3":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/2/")
                                                  || identity.getUrl().contains("/6/") || identity.getUrl()
                                                          .contains("/10/") || identity.getUrl().contains("/14/")
                                                  || identity.getUrl().contains("/18/"));
                        }
                        break;
                    case "node4":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/3/")
                                                  || identity.getUrl().contains("/7/") || identity.getUrl()
                                                          .contains("/11/") || identity.getUrl().contains("/15/")
                                                  || identity.getUrl().contains("/19/"));
                        }
                        break;
                    case "ronode":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/4/")
                                                  || identity.getUrl().contains("/13/"));
                        }
                        break;
                    case "participant0":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/4/")
                                                  || identity.getUrl().contains("/2") || identity.getUrl()
                                                          .contains("/3"));
                        }
                        break;
                    default:
                        Assertions.assertFalse(identity.getType().equals(IdentityComponent.Type.KEY));
                }
            });
        });
    }

    private void checkValuesRo(Map<String, List<IdentityComponent>> actual) {
        actual.forEach((key, value) -> {
            value.forEach(identity -> {
                switch (key) {
                    case "node1":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/0/")
                                                  || identity.getUrl().contains("/6/") || identity.getUrl()
                                                          .contains("/10/") || identity.getUrl().contains("/14/")
                                                  || identity.getUrl().contains("/18/"));
                        }
                        break;
                    case "node2":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/1/")
                                                  || identity.getUrl().contains("/7/") || identity.getUrl()
                                                          .contains("/11/") || identity.getUrl().contains("/15/")
                                                  || identity.getUrl().contains("/19/"));
                        }
                        break;
                    case "node3":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/2/")
                                                  || identity.getUrl().contains("/8/") || identity.getUrl()
                                                          .contains("/12/") || identity.getUrl().contains("/16/")
                                                  || identity.getUrl().contains("/20/"));
                        }
                        break;
                    case "node4":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/3/")
                                                  || identity.getUrl().contains("/9/") || identity.getUrl()
                                                          .contains("/13/") || identity.getUrl().contains("/17/")
                                                  || identity.getUrl().contains("/21/"));
                        }
                        break;
                    case "ronode1":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/4/")
                                                  || identity.getUrl().contains("/13/"));
                        }
                        break;
                    case "ronode2":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/5/")
                                                  || identity.getUrl().contains("/2"));
                        }
                        break;
                    case "participant0":
                        if (identity.getType().equals(IdentityComponent.Type.KEY)) {
                            Assertions.assertTrue(identity.getUrl().contains("/4/")
                                                  || identity.getUrl().contains("/2") || identity.getUrl()
                                                          .contains("/3") || identity.getUrl().contains("/6"));
                        }
                        break;
                    default:
                        Assertions.assertFalse(identity.getType().equals(IdentityComponent.Type.KEY));
                }
            });
        });
    }

    private void checkClientUrl(Map<String, List<IdentityComponent>> actual, List<String> clientNodeIds) {
        actual.forEach((key, value) -> {
            if (clientNodeIds.contains(key)) {
                value.forEach(ident -> {
                    Assertions.assertTrue(ident.getUrl().contains(
                                               CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH));
                });
            } else {
                value.forEach(ident -> {
                    Assertions.assertTrue(
                            ident.getUrl().contains(CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH));
                });
            }
        });
    }

}
