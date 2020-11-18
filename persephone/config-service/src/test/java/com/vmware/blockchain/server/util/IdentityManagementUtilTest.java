/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.util;

import java.security.Security;
import java.util.ArrayList;
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
import com.vmware.blockchain.deployment.v1.IdentityComponent;

/**
 * Unit test for identity management util methods.
 */
public class IdentityManagementUtilTest {

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

        bftPrincipalsRo.put(0, Arrays.asList(20, 21));

    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    void testBuildTlsNodeIdentitiesConcord() {
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder()
                .clients(new ArrayList<>()).replicas(replicas).build();

        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().build();

        IdentityManagementUtil identityManagementUtil = new IdentityManagementUtil(nodeList, bcFeatures,
                "myBlockchain");
        var res = identityManagementUtil.getAllTlsNodeIdentities(concordPrincipals,
                new HashMap<>(), certGen, 2);
        Assertions.assertFalse(res.getConcordIdentityComponents().isEmpty());
        Assertions.assertTrue(res.getBftIdentityComponents().isEmpty());
        testConcordIdentities(res.getConcordIdentityComponents());
    }

    /**
     * Test with RO replica.
     */
    @Test
    void testBuildTlsNodeIdentitiesConcordWithRo() {
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        List<BlockchainReadReplica> readReplicas =
                List.of(new BlockchainReadReplica("ronode1", "addr1"),
                        new BlockchainReadReplica("ronode2", "addr2"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().clients(new ArrayList<>()).replicas(replicas).readReplicas(readReplicas)
                        .build();

        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(true).build();

        IdentityManagementUtil identityManagementUtil = new IdentityManagementUtil(nodeList, bcFeatures,
                "myBlockchain");
        var res = identityManagementUtil.getAllTlsNodeIdentities(concordPrincipalsRo,
                new HashMap<>(), certGen, 2);
        Assertions.assertFalse(res.getConcordIdentityComponents().isEmpty());
        Assertions.assertTrue(res.getBftIdentityComponents().isEmpty());
        testRoReplicaIdentities(res.getConcordIdentityComponents());
    }

    @Test
    void testBuildTlsNodeIdentitiesBftClient() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().clients(clients).replicas(replicas).build();
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().build();
        IdentityManagementUtil identityManagementUtil = new IdentityManagementUtil(nodeList, bcFeatures,
                "myBlockchain");
        var res = identityManagementUtil.getAllTlsNodeIdentities(
                concordPrincipals, bftPrincipals, certGen, 5);

        Assertions.assertFalse(res.getBftIdentityComponents().isEmpty());
        Assertions.assertFalse(res.getConcordIdentityComponents().isEmpty());
        testConcordIdentities(res.getConcordIdentityComponents());
        testConcordIdentities(res.getBftIdentityComponents());
    }

    @Test
    void testBuildTlsNodeIdentitiesBftClientRo() {
        List<BlockchainClient> clients = List.of(new BlockchainClient("participant0", "addr1"));
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("node1", "addr1"), new BlockchainReplica("node2", "addr1"),
                        new BlockchainReplica("node3", "addr1"), new BlockchainReplica("node4", "addr1"));
        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("ronode1", "addr1"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().clients(clients).replicas(replicas).readReplicas(readReplicas).build();
        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isObjectStoreEnabled(true).build();
        IdentityManagementUtil identityManagementUtil = new IdentityManagementUtil(nodeList, bcFeatures,
                "myBlockchain");
        var res = identityManagementUtil.getAllTlsNodeIdentities(
                concordPrincipalsRo, bftPrincipalsRo, certGen, 2);

        Assertions.assertFalse(res.getBftIdentityComponents().isEmpty());
        Assertions.assertFalse(res.getConcordIdentityComponents().isEmpty());
        testRoReplicaIdentities(res.getConcordIdentityComponents());
        testRoReplicaIdentities(res.getBftIdentityComponents());
    }

    @Test
    void testBuildTrsTrcTlsIdentities() {
        List<String> clientIdList = List.of("participant0", "participant1", "participant2");
        List<String> addrList = List.of("addr1", "addr2", "addr3", "addr4");
        List<String> clientGroupIdList = List.of("id1", "id2");
        BlockchainClient client1 = new BlockchainClient(clientIdList.get(0), addrList.get(0));
        BlockchainClient client2 = new BlockchainClient(clientIdList.get(1), addrList.get(1));
        BlockchainClient client3 = new BlockchainClient(clientIdList.get(2), addrList.get(2));
        client1.setClientGroupId(clientGroupIdList.get(0));
        client2.setClientGroupId(clientGroupIdList.get(1));
        client3.setClientGroupId(clientGroupIdList.get(1));
        List<BlockchainClient> clients = List.of(client1, client2, client3);

        List<String> serverIdList = List.of("node1", "node2", "node3", "node4");
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica(serverIdList.get(0), addrList.get(0)),
                        new BlockchainReplica(serverIdList.get(1), addrList.get(1)),
                        new BlockchainReplica(serverIdList.get(2), addrList.get(2)),
                        new BlockchainReplica(serverIdList.get(3), addrList.get(3)));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().clients(clients).replicas(replicas).build();
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isTrcTlsEnabled(true).build();
        IdentityManagementUtil identityManagementUtil = new IdentityManagementUtil(nodeList, bcFeatures,
                "myBlockchain");
        IdentityComponentsLists identityComponentsLists = IdentityComponentsLists.builder().build();
        identityManagementUtil.buildTrsTrcTlsIdentities(identityComponentsLists);

        Assertions.assertEquals(4, identityComponentsLists.getTrsIdentityComponents().size());
        Assertions.assertEquals(3, identityComponentsLists.getTrcIdentityComponents().size());
        identityComponentsLists.getTrsIdentityComponents().forEach((key, value) -> {
            Assertions.assertEquals(3, value.size());
            value.forEach(val -> {
                testTrsTrcTlsIdentities(key, val.getUrl(), clientGroupIdList, addrList);
            });
        });

        identityComponentsLists.getTrcIdentityComponents().forEach((key, value) -> {
            Assertions.assertEquals(3, value.size());
            value.forEach(val -> {
                testTrsTrcTlsIdentities(key, val.getUrl(), clientGroupIdList, addrList);
            });
        });

    }

    private void testTrsTrcTlsIdentities(String key, String url,
                                         List<String> clientGroupIdList, List<String> addrList) {
        switch (key) {
            case "node1":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRS_TLS_IDENTITY_PATH + "/" + addrList.get(0)));
                break;
            case "node2":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRS_TLS_IDENTITY_PATH + "/" + addrList.get(1)));
                break;
            case "node3":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRS_TLS_IDENTITY_PATH + "/" + addrList.get(2)));
                break;
            case "node4":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRS_TLS_IDENTITY_PATH + "/" + addrList.get(3)));
                break;
            case "participant0":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRC_TLS_IDENTITY_PATH + "/" + clientGroupIdList.get(0)));
                break;
            case "participant1":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRC_TLS_IDENTITY_PATH + "/" + clientGroupIdList.get(1)));
                break;
            case "participant2":
                Assertions.assertTrue(url.contains(CertificatesGenerator.FILE_PREFIX
                        + CertificatesGenerator.TRC_TLS_IDENTITY_PATH + "/" + clientGroupIdList.get(1)));
                break;
            default:
                Assertions.assertTrue(false, "No other path structure should be allowed.");
        }
    }

    /**
     * test values.
     * @param actual values to test
     */
    private void testConcordIdentities(Map<String, List<IdentityComponent>> actual) {
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

    /**
     * test values for read only replicas.
     * @param actual values to test
     */
    private void testRoReplicaIdentities(Map<String, List<IdentityComponent>> actual) {
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
                                    || identity.getUrl().contains("/2")
                                    || identity.getUrl().contains("/3")
                                    || identity.getUrl().contains("/5"));
                        }
                        break;
                    default:
                        Assertions.assertFalse(identity.getType().equals(IdentityComponent.Type.KEY));
                }
            });
        });
    }
}