/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configuration;

import java.util.List;
import java.util.Map;
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
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Wavefront;

/**
 * Tests for the class that generates User Data.
 */
@ExtendWith(SpringExtension.class)
public class NodeConfigurationTest {

    NodeConfiguration nodeConfiguration;

    private String dockerImageBaseVersion = "latest";

    private NodeAssignment nodeAssignment;
    private Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> siteMap;

    private static final UUID NODE_ID_1 = UUID.randomUUID();
    private static final UUID NODE_ID_2 = UUID.randomUUID();

    private static final UUID STR_SITE_ID = UUID.randomUUID();
    private static final OrchestrationSiteIdentifier SITE_ID = OrchestrationSiteIdentifier
            .newBuilder().setId(STR_SITE_ID.toString()).build();

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    void init() {

        OrchestrationSiteInfo siteInfo = OrchestrationSiteInfo.newBuilder()
                .setType(OrchestrationSiteInfo.Type.VMC)
                .setVmc(VmcOrchestrationSiteInfo.newBuilder()
                        .setWavefront(Wavefront.newBuilder()
                                .setUrl("test.wavefront.com")
                                .setToken("testtoken").build())
                        .build())
                .build();

        siteMap = Map.of(SITE_ID, siteInfo);

        nodeAssignment = NodeAssignment.newBuilder()
                .addEntries(NodeAssignment.Entry.newBuilder()
                        .setNodeId(NODE_ID_1.toString())
                        .setSite(SITE_ID)
                        .setType(NodeType.CLIENT).build())
                .addEntries(NodeAssignment.Entry.newBuilder()
                        .setNodeId(NODE_ID_2.toString())
                        .setSite(SITE_ID)
                        .setType(NodeType.REPLICA).build())
                .build();
        nodeConfiguration = new NodeConfiguration(dockerImageBaseVersion, "daml");
    }

    @Test
    void testNoWavefrontProvided() {
        OrchestrationSiteInfo siteInfo = OrchestrationSiteInfo.newBuilder()
                .setType(OrchestrationSiteInfo.Type.VMC)
                .setVmc(VmcOrchestrationSiteInfo.newBuilder()
                        .setWavefront(Wavefront.newBuilder().build())
                        .build())
                .build();

        siteMap = Map.of(SITE_ID, siteInfo);

        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeAssignment, siteMap);
        output.forEach((nodeId, nodeOutput) -> {
            nodeOutput.forEach(nodeConfiguration -> {
                Assertions.assertNotEquals(nodeConfiguration.getServiceType(),
                        ConcordComponent.ServiceType.WAVEFRONT_PROXY);
            });
        });
    }

    @Test
    void testDefault() {
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeAssignment, siteMap);

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
                .addEntries(NodeAssignment.Entry.newBuilder()
                        .setNodeId(NODE_ID_2.toString())
                        .setSite(SITE_ID)
                        .setType(NodeType.REPLICA).build())
                .build();

        var output = nodeConfiguration.generateModelSpec(BlockchainType.ETHEREUM, nodeAssignment, siteMap);
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
                .addEntries(NodeAssignment.Entry.newBuilder()
                        .setNodeId(NODE_ID_1.toString())
                        .setSite(SITE_ID)
                        .setType(NodeType.READ_REPLICA).build())
                .build();
        Assertions.assertThrows(BadRequestPersephoneException.class, () -> {
            nodeConfiguration.generateModelSpec(BlockchainType.ETHEREUM, nodeAssignment, siteMap);
        });
    }

    @Test
    void testDamlWithTag() {
        nodeAssignment = NodeAssignment.newBuilder()
                .addEntries(NodeAssignment.Entry.newBuilder()
                        .setNodeId(NODE_ID_1.toString())
                        .setSite(SITE_ID)
                        .setType(NodeType.CLIENT).setProperties(Properties.newBuilder()
                                .putValues(DeploymentAttributes.IMAGE_TAG.name(), "dummy").build()).build())
                .build();
        var output = nodeConfiguration.generateModelSpec(BlockchainType.DAML, nodeAssignment, siteMap);
        Assert.assertEquals(nodeAssignment.getEntriesList().size(), output.size());
        for (var eachNodeType : nodeAssignment.getEntriesList()) {

            Assert.assertNotNull("ServiceType component missing for node type",
                                 output.get(UUID.fromString(eachNodeType.getNodeId())));
            assertResponse(output.get(UUID.fromString(eachNodeType.getNodeId())), "dummy");
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
