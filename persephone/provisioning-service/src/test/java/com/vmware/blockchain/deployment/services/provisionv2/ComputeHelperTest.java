/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;

/**
 * Tests for the compute helper functions.
 */
@ExtendWith(SpringExtension.class)
public class ComputeHelperTest {

    ComputeHelper computeHelper;

    @Mock
    UUID nodeId;

    @Mock
    UUID blockchainId;

    BlockchainType blockchainType;
    NodeAssignment.Entry node;

    @Mock
    DeploymentExecutionContext.LocalNodeDetails nodeDetails;

    @Mock
    Orchestrator orchestrator;

    List<ConcordComponent> model;

    @Mock
    ConfigurationSessionIdentifier configGenId;

    @Mock
    BootstrapComponent bootstrapComponent;

    @Mock
    Endpoint registry;

    @Mock
    Endpoint notaryServer;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        computeHelper = new ComputeHelper(bootstrapComponent);

        bootstrapComponent.template = "dummy-template";

        model = Arrays.asList(ConcordComponent.newBuilder().build());
        blockchainType = BlockchainType.DAML;
        node = NodeAssignment.Entry.newBuilder().setType(NodeType.REPLICA).build();

        when(orchestrator.createDeploymentV2(any())).thenReturn(mock(OrchestratorData.ComputeResourceEventCreatedV2
                                                                             .class));
    }

    @Test
    void testDeployNode() {

        var output = computeHelper.deployNode(nodeId,
                                              blockchainId,
                                              blockchainType,
                                              node,
                                              nodeDetails,
                                              orchestrator,
                                              model,
                                              configGenId,
                                              registry,
                                              notaryServer);
        Assert.assertNotNull(output);

        verify(orchestrator, times(1)).createDeploymentV2(any());
        verifyNoMoreInteractions(orchestrator);

        verifyNoMoreInteractions(bootstrapComponent);
        verifyNoMoreInteractions(nodeDetails);
    }

    @Test
    void testDeployReadReplicaNode() {
        blockchainType = BlockchainType.DAML;
        node = NodeAssignment.Entry.newBuilder().setType(NodeType.READ_REPLICA).build();

        when(orchestrator.createDeploymentV2(any())).thenReturn(mock(OrchestratorData.ComputeResourceEventCreatedV2
                                                                             .class));

        var output = computeHelper.deployNode(nodeId,
                                              blockchainId,
                                              blockchainType,
                                              node,
                                              nodeDetails,
                                              orchestrator,
                                              model,
                                              configGenId,
                                              registry,
                                              notaryServer);
        Assert.assertNotNull(output);

        verify(orchestrator, times(1)).createDeploymentV2(any());
        verifyNoMoreInteractions(orchestrator);

        verifyNoMoreInteractions(bootstrapComponent);
        verifyNoMoreInteractions(nodeDetails);
    }

    @Test
    void testDeployInvalidNode() {
        blockchainType = BlockchainType.UNRECOGNIZED;
        node = NodeAssignment.Entry.newBuilder().setType(NodeType.CLIENT).build();

        when(orchestrator.createDeploymentV2(any())).thenReturn(mock(OrchestratorData.ComputeResourceEventCreatedV2
                                                                             .class));
        Assert.assertThrows(IllegalArgumentException.class, () -> {
            var output = computeHelper.deployNode(nodeId,
                                                  blockchainId,
                                                  blockchainType,
                                                  node,
                                                  nodeDetails,
                                                  orchestrator,
                                                  model,
                                                  configGenId,
                                                  registry,
                                                  notaryServer);
        });
    }
}
