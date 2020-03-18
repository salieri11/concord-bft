/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.v1.NodeProperty;

/**
 * Test for GenericConfigUtil.
 */
public class GenericConfigUtilTest {

    private GenericConfigUtil genericConfigUtil;

    @BeforeEach
    public void createObject() {
        this.genericConfigUtil = new GenericConfigUtil();
    }

    @Test
    public void testClientNodeProvidedHappyPath() {
        Map<Integer, String> nodeMap = new HashMap<>();
        nodeMap.put(0, "TEST-NODE0");
        nodeMap.put(1, "TEST-NODE1");
        nodeMap.put(2, "TEST-NODE2");
        nodeMap.put(3, "TEST-NODE3");

        Map<Integer, String> clientMap = new HashMap<>();
        clientMap.put(0, "CLIENT-NODE0");
        clientMap.put(1, "CLIENT-NODE0");
        clientMap.put(2, "CLIENT-NODE1");
        clientMap.put(3, "CLIENT-NODE1");

        List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_ID)
                        .putAllValue(nodeMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.CLIENT_GROUP_ID)
                        .putAllValue(clientMap).build());

        var expected = new HashMap<Integer, String>();
        expected.put(0, "NODE_UUID=TEST-NODE0\nCLIENT_GROUP_ID=CLIENT-NODE0");
        expected.put(1, "NODE_UUID=TEST-NODE1\nCLIENT_GROUP_ID=CLIENT-NODE0");
        expected.put(2, "NODE_UUID=TEST-NODE2\nCLIENT_GROUP_ID=CLIENT-NODE1");
        expected.put(3, "NODE_UUID=TEST-NODE3\nCLIENT_GROUP_ID=CLIENT-NODE1");

        var actual = this.genericConfigUtil.getGenericConfig(nodePropertyList);
        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testClientNodeNotProvided() {
        Map<Integer, String> nodeMap = new HashMap<>();
        nodeMap.put(0, "TEST-NODE0");
        nodeMap.put(1, "TEST-NODE1");
        nodeMap.put(2, "TEST-NODE2");
        nodeMap.put(3, "TEST-NODE3");

        List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_ID)
                        .putAllValue(nodeMap).build());

        var expected = new HashMap<Integer, String>();
        expected.put(0, "NODE_UUID=TEST-NODE0\nCLIENT_GROUP_ID=TEST-NODE0");
        expected.put(1, "NODE_UUID=TEST-NODE1\nCLIENT_GROUP_ID=TEST-NODE1");
        expected.put(2, "NODE_UUID=TEST-NODE2\nCLIENT_GROUP_ID=TEST-NODE2");
        expected.put(3, "NODE_UUID=TEST-NODE3\nCLIENT_GROUP_ID=TEST-NODE3");

        var actual = this.genericConfigUtil.getGenericConfig(nodePropertyList);
        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testNodeIdNotProvided() throws IOException {

        Map<Integer, String> clientMap = new HashMap<>();
        clientMap.put(0, "CLIENT-NODE0");
        clientMap.put(1, "CLIENT-NODE0");
        clientMap.put(2, "CLIENT-NODE1");
        clientMap.put(3, "CLIENT-NODE1");

        List<NodeProperty> nodePropertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.CLIENT_GROUP_ID)
                        .putAllValue(clientMap).build());

        var actual = this.genericConfigUtil.getGenericConfig(nodePropertyList);

        Assertions.assertThat(actual.size() == 0).isTrue();
    }

    @Test
    public void testRelevantPropertiesNotProvided() throws IOException {

        List<NodeProperty> nodePropertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.BLOCKCHAIN_ID)
                        .putAllValue(new HashMap<>()).build());

        var actual = this.genericConfigUtil.getGenericConfig(nodePropertyList);

        Assertions.assertThat(actual.isEmpty()).isTrue();
    }

    @Test
    public void testPath() {
        Assertions.assertThat(GenericConfigUtil.configPath.equals("/generic/identifiers.env")).isTrue();
    }
}
