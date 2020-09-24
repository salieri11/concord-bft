/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configuration;

import java.util.List;
import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Tests for the class that generates User Data.
 */
@ExtendWith(SpringExtension.class)
public class NodeConfigurationTest {

    NodeConfiguration nodeConfiguration;

    private String dockerImageBaseVersion = "latest";

    private NodeAssignment nodeAssignment;

    private static final UUID NODE_ID_1 = UUID.randomUUID();
    private static final UUID NODE_ID_2 = UUID.randomUUID();

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    void init() {

        nodeAssignment = NodeAssignment.newBuilder()
                .addEntries(NodeAssignment.Entry.newBuilder().setNodeId(NODE_ID_1.toString())
                                    .setType(NodeType.CLIENT).build())
                .addEntries(NodeAssignment.Entry.newBuilder().setNodeId(NODE_ID_2.toString())
                                    .setType(NodeType.REPLICA).build())
                .build();
        nodeConfiguration = new NodeConfiguration(dockerImageBaseVersion, "daml");
    }

    @Test
    void testDefault() {
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeAssignment);

        Assert.assertEquals(nodeAssignment.getEntriesList().size(), output.size());
        for (var eachNodeType : nodeAssignment.getEntriesList()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(UUID.fromString(eachNodeType.getNodeId())));
            assertResponse(output.get(UUID.fromString(eachNodeType.getNodeId())), dockerImageBaseVersion);
        }
    }

    @Test
    void testDefaultEtheruem() {

        nodeAssignment = NodeAssignment.newBuilder()
                .addEntries(NodeAssignment.Entry.newBuilder().setNodeId(NODE_ID_2.toString())
                                    .setType(NodeType.REPLICA).build())
                .build();

        var output = nodeConfiguration.generateModelSpec(BlockchainType.ETHEREUM, nodeAssignment);
        Assert.assertEquals(nodeAssignment.getEntriesList().size(), output.size());
        for (var eachNodeType : nodeAssignment.getEntriesList()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(UUID.fromString(eachNodeType.getNodeId())));
            assertResponse(output.get(UUID.fromString(eachNodeType.getNodeId())), dockerImageBaseVersion);
        }

    }

    @Test
    void testUnsupportedNodeType() {
        nodeAssignment = NodeAssignment.newBuilder()
                .addEntries(NodeAssignment.Entry.newBuilder().setNodeId(NODE_ID_1.toString())
                                    .setType(NodeType.READ_REPLICA).build())
                .build();
        Assertions.assertThrows(BadRequestPersephoneException.class, () -> {
            nodeConfiguration.generateModelSpec(BlockchainType.ETHEREUM, nodeAssignment);
        });
    }

    @Test
    void testDamlWithTag() {
        nodeAssignment = NodeAssignment.newBuilder()
                .addEntries(NodeAssignment.Entry.newBuilder().setNodeId(NODE_ID_1.toString())
                                    .setType(NodeType.CLIENT).setProperties(Properties.newBuilder()
                                .putValues(DeploymentAttributes.IMAGE_TAG.name(), "dummy")
                                                                                    .build()).build())
                .build();
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeAssignment);
        Assert.assertEquals(nodeAssignment.getEntriesList().size(), output.size());
        for (var eachNodeType : nodeAssignment.getEntriesList()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(UUID.fromString(eachNodeType.getNodeId())));
            assertResponse(output.get(UUID.fromString(eachNodeType.getNodeId())), "dummy");
        }
    }

    // Tests the presence of NotaryVerificationRequired in each component for each node
    @Test
    void testNotaryVerificationRequirementField() {
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeAssignment);

        Assert.assertEquals(nodeAssignment.getEntriesList().size(), output.size());

        for (var eachNode : nodeAssignment.getEntriesList()) {
            output.get(UUID.fromString(eachNode.getNodeId())).forEach(component -> {
                Assert.assertNotNull("NotaryVerificationRequired field missing for component",
                                     component.getNotaryVerificationRequired());
            });
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
