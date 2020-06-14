/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.doReturn;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.verifyNoMoreInteractions;
import static org.powermock.api.mockito.PowerMockito.when;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;

import com.google.common.cache.Cache;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.configservice.ConfigServiceInvoker;
import com.vmware.blockchain.deployment.services.configuration.NodeConfiguration;
import com.vmware.blockchain.deployment.services.futureutil.ReactiveStream;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

/**
 * Tests for the compute helper functions.
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({NetworkHelper.class, ComputeHelper.class, NodeConfiguration.class, OrchestratorProvider.class,
                 Cache.class, ConfigServiceInvoker.class, ProvisioningServiceV2.class})
public class ProvisioningServiceTest {

    BootstrapComponent bootstrapComponent;
    NetworkHelper networkHelper;
    ComputeHelper computeHelper;
    ConfigHelper configHelper;
    NodeConfiguration nodeConfiguration;
    OrchestratorProvider orchestratorProvider;
    Cache<UUID, CompletableFuture<DeploymentExecutionContext>> deploymentLogCache;

    ProvisioningServiceV2 provisioningServiceV2;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        bootstrapComponent = mock(BootstrapComponent.class);
        Endpoint configService = Endpoint.newBuilder().setAddress("test.com")
                .setTransportSecurity(TransportSecurity.newBuilder()
                                              .setType(TransportSecurity.Type.TLSv1_2).build()).build();
        bootstrapComponent.configService = configService;

        // Would have loved to use expectNew. but not taking effect.
        provisioningServiceV2 = new ProvisioningServiceV2(bootstrapComponent, nodeConfiguration);

        networkHelper = mock(NetworkHelper.class);
        computeHelper = mock(ComputeHelper.class);
        configHelper = mock(ConfigHelper.class);
        nodeConfiguration = mock(NodeConfiguration.class);
        orchestratorProvider = mock(OrchestratorProvider.class);
        deploymentLogCache = mock(Cache.class);

        Whitebox.setInternalState(provisioningServiceV2, NetworkHelper.class, networkHelper);
        Whitebox.setInternalState(provisioningServiceV2, ComputeHelper.class, computeHelper);
        Whitebox.setInternalState(provisioningServiceV2, NodeConfiguration.class, nodeConfiguration);
        Whitebox.setInternalState(provisioningServiceV2, OrchestratorProvider.class, orchestratorProvider);
        Whitebox.setInternalState(provisioningServiceV2, ConfigHelper.class, configHelper);
        Whitebox.setInternalState(provisioningServiceV2, Cache.class, deploymentLogCache);

        when(nodeConfiguration.getDamlSdkVersion()).thenReturn("daml");
        when(nodeConfiguration.getDockerImageBaseVersion()).thenReturn("blockchain-version");
    }

    @Test
    void testCreateDeploymentDefault() throws Exception {
        DeploymentRequest request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .build();
        var promise = new CompletableFuture<DeploymentRequestResponse>();
        when(deploymentLogCache.asMap()).thenReturn(mock(ConcurrentMap.class));
        provisioningServiceV2.createDeployment(request, ReactiveStream.blockedResultObserver(promise));

        Assert.assertNotNull(promise.get().getId());
    }

    @Test
    void testCreateDeploymentWithSession() throws Exception {
        doReturn(mock(Orchestrator.class)).when(orchestratorProvider).newOrchestrator(any(), any());

        String sessionId = UUID.randomUUID().toString();
        DeploymentRequest request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().setId(sessionId).build())
                .build();
        var promise = new CompletableFuture<DeploymentRequestResponse>();
        when(deploymentLogCache.asMap()).thenReturn(mock(ConcurrentMap.class));
        provisioningServiceV2.createDeployment(request, ReactiveStream.blockedResultObserver(promise));

        Assert.assertEquals(promise.get().getId(), sessionId);
    }

    @Test
    void testDeployBlockchainEmptyRun() {

        when(configHelper.generateConfigurationId(any())).thenReturn(mock(ConfigurationSessionIdentifier.class));
        ConcurrentMap<UUID, CompletableFuture<DeploymentExecutionContext>> mockMap = mock(ConcurrentMap.class);
        when(mockMap.get(any())).thenReturn(new CompletableFuture<>());
        when(deploymentLogCache.asMap()).thenReturn(mockMap);

        DeploymentExecutionContext executionContext = DeploymentExecutionContext.builder()
                .id(UUID.randomUUID())
                .blockchainId(UUID.randomUUID())
                .nodeAssignment(NodeAssignment.newBuilder().build())
                .blockchainId(UUID.randomUUID())
                .consortiumId(UUID.randomUUID())
                .blockchainType(BlockchainType.DAML).build();
        provisioningServiceV2.deployBlockchain(executionContext);

        verify(configHelper, times(1)).generateConfigurationId(any());
        verifyNoMoreInteractions(configHelper);
    }

    @Test
    void testDeployBlockchainSingleNode() {
        var nodeUid = UUID.randomUUID();
        OrchestrationSiteIdentifier siteId = OrchestrationSiteIdentifier.newBuilder()
                .setId(UUID.randomUUID().toString()).build();

        when(configHelper.generateConfigurationId(any())).thenReturn(mock(ConfigurationSessionIdentifier.class));

        when(networkHelper.createPrivateIpMap(any(), any(), any()))
                .thenReturn(ImmutableMap.of(nodeUid,
                                            DeploymentExecutionContext
                                                    .LocalNodeDetails
                                                    .builder()
                                                    .build()));
        ConcurrentMap<UUID, CompletableFuture<DeploymentExecutionContext>> mockMap = mock(ConcurrentMap.class);
        when(mockMap.get(any())).thenReturn(new CompletableFuture<>());
        when(deploymentLogCache.asMap()).thenReturn(mockMap);

        DeploymentExecutionContext executionContext = DeploymentExecutionContext.builder()
                .id(UUID.randomUUID())
                .blockchainId(UUID.randomUUID())
                .nodeAssignment(NodeAssignment.newBuilder()
                                        .addEntries(NodeAssignment.Entry
                                                            .newBuilder()
                                                            .setSite(siteId)
                                                            .setType(NodeType.REPLICA)
                                                            .setNodeId(nodeUid.toString())).build())
                .deploymentType(OrchestrationSiteInfo.Type.VMC)
                .blockchainId(UUID.randomUUID())
                .consortiumId(UUID.randomUUID())
                .orchestrators(ImmutableMap.of(siteId, mock(Orchestrator.class)))
                //.nodeTypeComponent(ImmutableMap.of(NodeType.REPLICA,
                //                                   Arrays.asList(ConcordComponent.newBuilder().build())))
                .blockchainType(BlockchainType.DAML).build();
        provisioningServiceV2.deployBlockchain(executionContext);

        verify(configHelper, times(1)).generateConfigurationId(any());
        verifyNoMoreInteractions(configHelper);

        verify(networkHelper, times(1)).createPrivateIpMap(any(), any(), any());
        verify(networkHelper, times(1)).applyPublicNetworkAddress(any(), any(), any(), any());
        verifyNoMoreInteractions(networkHelper);

        verify(computeHelper, times(2)).getComputeNodes(any(), any(), any());
        verifyNoMoreInteractions(computeHelper);
    }

    @org.junit.Test
    public void dummyTest() {
    }
}
