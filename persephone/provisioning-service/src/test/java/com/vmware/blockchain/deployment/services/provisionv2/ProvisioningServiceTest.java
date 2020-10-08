/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import static com.vmware.blockchain.deployment.services.provisionv2.ProvisioningServiceUtil.generateEvent;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.doReturn;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.verifyNoMoreInteractions;
import static org.powermock.api.mockito.PowerMockito.when;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.reflect.Whitebox;

import com.google.common.cache.Cache;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.configservice.ConfigServiceInvoker;
import com.vmware.blockchain.deployment.services.configuration.NodeConfiguration;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.futureutil.ReactiveStream;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentResponse;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import io.grpc.stub.StreamObserver;

/**
 * Tests for the compute helper functions.
 */
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
    UUID consortiumId = UUID.randomUUID();
    UUID blockchainId = UUID.randomUUID();

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
        bootstrapComponent.pathToCerts = "src/test/resources/certs";

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

    @AfterEach
    public void tearDown() {
        provisioningServiceV2 = null;
        deploymentLogCache = null;
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
    void testStreamDeploymentSessionEvents() throws InterruptedException {
        UUID sessionId = UUID.randomUUID();
        DeploymentExecutionContext session = DeploymentExecutionContext.builder()
                .id(sessionId)
                .consortiumId(consortiumId)
                .blockchainId(blockchainId)
                .blockchainType(BlockchainType.DAML)
                .sitesById(Maps.newHashMap())
                .componentsByNode(Maps.newHashMap())
                .nodeAssignment(NodeAssignment.newBuilder().build())
                .orchestrators(Maps.newHashMap())
                .deploymentType(OrchestrationSiteInfo.Type.VMC)
                .build();
        session.events.add(generateEvent(session, DeploymentExecutionEvent.Type.ACKNOWLEDGED,
                DeploymentExecutionEvent.Status.ACTIVE).build());
        session.events.add(generateEvent(session, DeploymentExecutionEvent.Type.COMPLETED,
                DeploymentExecutionEvent.Status.SUCCESS).build());

        CompletableFuture<DeploymentExecutionContext> futureSession = CompletableFuture.completedFuture(session);
        ConcurrentMap<UUID, CompletableFuture<DeploymentExecutionContext>> mockMap = mock(ConcurrentMap.class);
        futureSession.join();
        Thread.sleep(2000);

        when(mockMap.get(sessionId)).thenReturn(futureSession);
        when(deploymentLogCache.asMap()).thenReturn(mockMap);

        String sessionIdStr = sessionId.toString();
        StreamDeploymentSessionEventRequest message = StreamDeploymentSessionEventRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().setId(sessionIdStr).build()).setSessionId(sessionIdStr).build();
        StreamObserver<DeploymentExecutionEvent> observer = mock(StreamObserver.class);
        provisioningServiceV2.streamDeploymentSessionEvents(message, observer);

        ArgumentCaptor<DeploymentExecutionEvent> captor = ArgumentCaptor.forClass(DeploymentExecutionEvent.class);
        TimeUnit.SECONDS.sleep(1);
        verify(observer, times(2)).onNext(captor.capture());
        List<DeploymentExecutionEvent> response = captor.getAllValues();
        Assert.assertEquals(2, response.size());
        Assert.assertEquals(DeploymentExecutionEvent.Type.ACKNOWLEDGED, response.get(0).getType());
        Assert.assertEquals(sessionIdStr, response.get(0).getSessionId());
        Assert.assertEquals(DeploymentExecutionEvent.Type.COMPLETED, response.get(1).getType());
    }

    @Test
    void testStreamDeploymentSessionEventsError() throws InterruptedException {
        UUID sessionId = UUID.randomUUID();
        DeploymentExecutionContext session = DeploymentExecutionContext.builder().build();
        CompletableFuture<DeploymentExecutionContext> futureSession = CompletableFuture.completedFuture(session);
        ConcurrentMap<UUID, CompletableFuture<DeploymentExecutionContext>> mockMap = mock(ConcurrentMap.class);
        futureSession.join();
        Thread.sleep(1000);

        when(mockMap.get(sessionId)).thenReturn(null);
        when(deploymentLogCache.asMap()).thenReturn(mockMap);

        String sessionIdStr = sessionId.toString();
        StreamDeploymentSessionEventRequest message = StreamDeploymentSessionEventRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().setId(sessionIdStr).build()).setSessionId(sessionIdStr).build();
        StreamObserver<DeploymentExecutionEvent> observer = mock(StreamObserver.class);
        provisioningServiceV2.streamDeploymentSessionEvents(message, observer);
        TimeUnit.SECONDS.sleep(1);
        verify(observer, times(1)).onError(any(IllegalStateException.class));
    }

    // @Test
    void testDeployBlockchainEmptyRun() {

        when(configHelper.generateConfigurationId(any(), any())).thenReturn(mock(ConfigurationSessionIdentifier.class));
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
        provisioningServiceV2.deployBlockchain(executionContext, any());

        verify(configHelper, times(1)).generateConfigurationId(any(), any());
        verifyNoMoreInteractions(configHelper);
    }

    // @Test
    void testDeployBlockchainSingleNode() {
        var nodeUid = UUID.randomUUID();
        OrchestrationSiteIdentifier siteId = OrchestrationSiteIdentifier.newBuilder()
                .setId(UUID.randomUUID().toString()).build();

        when(configHelper.generateConfigurationId(any(), any())).thenReturn(mock(ConfigurationSessionIdentifier.class));

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
        provisioningServiceV2.deployBlockchain(executionContext, any());

        verify(configHelper, times(1)).generateConfigurationId(any(), any());
        verifyNoMoreInteractions(configHelper);

        verify(networkHelper, times(1)).createPrivateIpMap(any(), any(), any());
        verify(networkHelper, times(1)).applyPublicNetworkAddress(any(), any(), any(), any());
        verifyNoMoreInteractions(networkHelper);

        verify(computeHelper, times(2)).getComputeNodes(any(), any(), any());
        verifyNoMoreInteractions(computeHelper);
    }

    @Test
    void testDeprovision() throws InterruptedException {
        UUID sessionId = UUID.fromString("cca32e37-d952-4612-a536-f1b4bb83941e");
        DeploymentExecutionContext session = DeploymentExecutionContext.builder()
                .id(sessionId)
                .build();
        String nodeId = "d0990bca-54aa-4d0c-bca2-a994087a9fbd";
        DeployedResource resCompute = DeployedResource.newBuilder()
                .setNodeId(nodeId)
                .setType(DeployedResource.Type.COMPUTE_RESOURCE)
                .setName("vmwarevmc.com")
                .build();
        DeployedResource resNetAlloc = DeployedResource.newBuilder()
                .setNodeId(nodeId)
                .setType(DeployedResource.Type.NETWORK_ALLOCATION)
                .setName("vmwarevmc.com")
                .build();
        DeployedResource resNetRes = DeployedResource.newBuilder()
                .setNodeId(nodeId)
                .setType(DeployedResource.Type.NETWORK_RESOURCE)
                .setName("vmwarevmc.com")
                .build();
        session.results.add(resCompute);
        session.results.add(resNetAlloc);
        session.results.add(resNetRes);

        CompletableFuture<DeploymentExecutionContext> futureSession = CompletableFuture.completedFuture(session);
        futureSession.join();
        // Thread.sleep(2000);
        ConcurrentMap<UUID, CompletableFuture<DeploymentExecutionContext>> mockMap = Maps.newConcurrentMap();
        mockMap.put(sessionId, futureSession);
        when(deploymentLogCache.asMap()).thenReturn(mockMap);
        String sessionIdStr = sessionId.toString();
        DeprovisionDeploymentRequest request = DeprovisionDeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSessionId(sessionIdStr)
                .build();
        StreamObserver<DeprovisionDeploymentResponse> observer = mock(StreamObserver.class);
        provisioningServiceV2.deprovisionDeployment(request, observer);
        TimeUnit.SECONDS.sleep(1);
        ArgumentCaptor<DeprovisionDeploymentResponse> captor =
                ArgumentCaptor.forClass(DeprovisionDeploymentResponse.class);
        verify(observer, times(1)).onNext(captor.capture());
        verify(observer, never()).onError(any(Throwable.class));
        DeprovisionDeploymentResponse response = captor.getValue();
        Assert.assertNotNull(response);
    }

    @Test
    void testDeprovisionBadInput() throws InterruptedException {
        UUID sessionId = UUID.fromString("cca32e37-d952-4612-a536-f1b4bb83941e");
        DeploymentExecutionContext session = DeploymentExecutionContext.builder()
                .build();

        CompletableFuture<DeploymentExecutionContext> futureSession = CompletableFuture.completedFuture(session);
        futureSession.join();
        ConcurrentMap<UUID, CompletableFuture<DeploymentExecutionContext>> mockMap = Maps.newConcurrentMap();
        // Don't put sessionId in mockMap like happy path
        when(deploymentLogCache.asMap()).thenReturn(mockMap);
        String sessionIdStr = sessionId.toString();
        DeprovisionDeploymentRequest request = DeprovisionDeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSessionId(sessionIdStr)
                .build();
        StreamObserver<DeprovisionDeploymentResponse> observer = mock(StreamObserver.class);
        provisioningServiceV2.deprovisionDeployment(request, observer);
        TimeUnit.SECONDS.sleep(1);
        verify(observer, times(1)).onError(any(BadRequestPersephoneException.class));
    }

}
