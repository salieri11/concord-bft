/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.util;

import java.security.Security;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;

/**
 * Unit test for config service helper methods.
 */
public class ConfigurationServiceUtilTest {

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
        concordPrincipalsRo.put(4, List.of());

        bftPrincipalsRo.put(0, Arrays.asList(20, 21));

    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    void testGetNodeListOfTypePositive() {
        Map<String, NodesInfo> nodesInfoMap = new HashMap<>();
        NodesInfo.Entry entry = NodesInfo.Entry.newBuilder().setType(NodeType.REPLICA).setId("id1").setNodeIp("ip1")
                .build();
        nodesInfoMap.put(NodeType.REPLICA.name(), NodesInfo.newBuilder().addEntries(entry).build());
        List<BlockchainReplica> replicas = ConfigurationServiceUtil.getNodeListOfType(nodesInfoMap, NodeType.REPLICA,
                                                                                      BlockchainReplica.class,
                                                                                      UUID.randomUUID());
        Assertions.assertTrue(!replicas.isEmpty());
    }


    @Test
    void testGetNodeListOfTypeNegative() {
        var id = UUID.randomUUID();
        org.assertj.core.api.Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> ConfigurationServiceUtil.getNodeListOfType(null, NodeType.READ_REPLICA,
                                                                             BlockchainReplica.class, id))
                .withMessage("Invalid node data in the request : " + id);
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

        BlockchainNodeList nodeList = ConfigurationServiceUtil.getNodeList(nodesInfoMap, UUID.randomUUID());
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
        var id = UUID.randomUUID();
        org.assertj.core.api.Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> ConfigurationServiceUtil.getNodeList(null, id))
                .withMessage("Invalid node data in the request : " + id);
    }

}
