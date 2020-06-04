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

import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;

/**
 * Tests for the network helper functions.
 */
@ExtendWith(SpringExtension.class)
public class NetworkHelperTest {

    NetworkHelper networkHelper;

    private static final UUID NODE_ID1 = UUID.randomUUID();
    private static final UUID NODE_ID2 = UUID.randomUUID();

    private static final UUID SITE_ID1 = UUID.randomUUID();
    private static final UUID SITE_ID2 = UUID.randomUUID();

    Orchestrator orchestrator1 = mock(Orchestrator.class);
    Orchestrator orchestrator2 = mock(Orchestrator.class);

    NodeAssignment nodes;

    Map<OrchestrationSiteIdentifier, Orchestrator> sessionOrchestrators;

    @Mock
    ConcurrentHashMap.KeySetView<DeployedResource, Boolean> results;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        networkHelper = new NetworkHelper();

        var id1 = NODE_ID1.toString();
        var entry1 = NodeAssignment.Entry.newBuilder().setNodeId(id1)
                .setSite(OrchestrationSiteIdentifier.newBuilder().setId(SITE_ID1.toString()).build()).build();

        var id2 = NODE_ID2.toString();
        var entry2 = NodeAssignment.Entry.newBuilder().setNodeId(id2)
                .setSite(OrchestrationSiteIdentifier.newBuilder().setId(SITE_ID2.toString()).build()).build();

        nodes = NodeAssignment.newBuilder()
                .addAllEntries(Arrays.asList(entry1, entry2))
                .build();

        sessionOrchestrators = new HashMap<>();
        sessionOrchestrators
                .put(OrchestrationSiteIdentifier.newBuilder().setId(SITE_ID1.toString()).build(), orchestrator1);
        sessionOrchestrators
                .put(OrchestrationSiteIdentifier.newBuilder().setId(SITE_ID2.toString()).build(), orchestrator2);
    }

    @Test
    void testCreatePrivateIp() {

        when(orchestrator1.createPrivateNetworkAddress(any()))
                .thenReturn(OrchestratorData.NetworkResourceEventCreated.builder()
                                    .resource(mock(URI.class)).address("10.1.1.1").build());
        when(orchestrator2.createPrivateNetworkAddress(any()))
                .thenReturn(OrchestratorData.NetworkResourceEventCreated.builder()
                                    .resource(mock(URI.class)).address("10.1.1.2").build());

        var output = networkHelper.createPrivateIpMap(nodes, sessionOrchestrators, results);
        Assert.assertNotNull(output);

        Assert.assertTrue(output.containsKey(NODE_ID1));
        Assert.assertTrue(output.containsKey(NODE_ID2));

        for (var value : sessionOrchestrators.values()) {
            verify(value, times(1)).createPrivateNetworkAddress(any());
            verifyNoMoreInteractions(value);

            verify(results, times(2)).add(any());
        }
    }

    @Test
    void testAssignPublicIp() {
        var privateIp = new HashMap<UUID, DeploymentExecutionContext.LocalNodeDetails>();

        privateIp.put(NODE_ID1, DeploymentExecutionContext.LocalNodeDetails.builder().privateIp("10.1.1.1").build());
        privateIp.put(NODE_ID2, DeploymentExecutionContext.LocalNodeDetails.builder().privateIp("10.1.1.2").build());

        when(orchestrator1.createPublicNetworkAddress(any()))
                .thenReturn(OrchestratorData.NetworkResourceEventCreated.builder()
                                    .resource(mock(URI.class)).address("255.255.255.1").build());
        when(orchestrator2.createPublicNetworkAddress(any()))
                .thenReturn(OrchestratorData.NetworkResourceEventCreated.builder()
                                    .resource(mock(URI.class)).address("255.255.255.2").build());

        var alloc1 = mock(OrchestratorData.NetworkAllocationEvent.class);
        var alloc2 = mock(OrchestratorData.NetworkAllocationEvent.class);
        when(orchestrator1.createVmcNetworkAllocation(any())).thenReturn(alloc1);
        when(orchestrator2.createVmcNetworkAllocation(any())).thenReturn(alloc2);

        networkHelper.applyPublicNetworkAddress(nodes, sessionOrchestrators, privateIp, results);

        for (var value : sessionOrchestrators.values()) {
            verify(value, times(1)).createPublicNetworkAddress(any());
            verify(value, times(1)).createVmcNetworkAllocation(any());
            verifyNoMoreInteractions(value);

            verify(results, times(2)).add(any());
        }
    }
}
