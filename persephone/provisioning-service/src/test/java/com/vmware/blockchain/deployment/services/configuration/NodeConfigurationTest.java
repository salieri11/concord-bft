/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configuration;

import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.rules.ExpectedException;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Tests for the class that generates User Data.
 */
@ExtendWith(SpringExtension.class)
public class NodeConfigurationTest {

    NodeConfiguration nodeConfiguration;

    private String dockerImageBaseVersion = "latest";

    private BlockchainType blockchainType;

    private Properties globalProperties;

    private Map<NodeType, Properties> nodeTypeEntries;

    @Rule
    ExpectedException e = ExpectedException.none();

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    void init() {
        globalProperties = Properties.newBuilder().build();
        nodeTypeEntries = ImmutableMap.of(NodeType.CLIENT, Properties.newBuilder().build(),
                                          NodeType.REPLICA, Properties.newBuilder().build());
        nodeConfiguration = new NodeConfiguration(dockerImageBaseVersion);
    }

    @Test
    void testDefault() {
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeTypeEntries);
        Assert.assertEquals(nodeTypeEntries.size(), output.size());
        for (var eachNodeType : nodeTypeEntries.entrySet()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(eachNodeType.getKey()));
            assertResponse(output.get(eachNodeType.getKey()), dockerImageBaseVersion);
        }
    }

    @Test
    void testDefaultEtheruem() {
        nodeTypeEntries = ImmutableMap.of(NodeType.REPLICA, Properties.newBuilder().build());

        var output = nodeConfiguration.generateModelSpec(BlockchainType.ETHEREUM, nodeTypeEntries);
        Assert.assertEquals(nodeTypeEntries.size(), output.size());
        for (var eachNodeType : nodeTypeEntries.entrySet()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(eachNodeType.getKey()));
            assertResponse(output.get(eachNodeType.getKey()), dockerImageBaseVersion);
        }

    }

    @Test
    void testUnsupportedNodeType() {
        e.expect(BadRequestPersephoneException.class);
        nodeTypeEntries = ImmutableMap.of(NodeType.READ_REPLICA, Properties.newBuilder().build());
    }

    @Test
    void testDamlWithTag() {
        globalProperties = Properties.newBuilder()
                .putValues("docker.image.base.version", "test").build();
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeTypeEntries);
        Assert.assertEquals(nodeTypeEntries.size(), output.size());
        for (var eachNodeType : nodeTypeEntries.entrySet()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(eachNodeType.getKey()));
            assertResponse(output.get(eachNodeType.getKey()), "test");
        }
    }

    private void assertResponse(List<ConcordComponent> output, String dockerImageTag) {
        output.stream().forEach(k -> {
            if (NodeConfiguration.STATIC_TAG_LIST.containsKey(k.getServiceType())) {
                Assert.assertTrue(k.getName().endsWith(NodeConfiguration.STATIC_TAG_LIST.get(k.getServiceType())));
            } else {
                Assert.assertTrue(k.getName().endsWith(dockerImageTag));
            }
        });
    }
}
