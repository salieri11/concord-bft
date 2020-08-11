/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.powermock.reflect.Whitebox;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.service.CastorDeploymentStatus;
import com.vmware.blockchain.castor.service.ProvisionerService;
import com.vmware.blockchain.castor.service.ProvisioningServiceTestImpl;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;

import io.grpc.ManagedChannel;
import io.grpc.Server;
import io.grpc.inprocess.InProcessChannelBuilder;
import io.grpc.inprocess.InProcessServerBuilder;
import io.grpc.stub.StreamObserver;
import io.grpc.testing.GrpcCleanupRule;

/**
 * Test RPC round trip via all Castor services.
 */
// Needs Junit4 for rules
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CastorTestConfiguration.class)
public class CastorDeploymentTest {
    @Rule public final GrpcCleanupRule grpcCleanupRule = new GrpcCleanupRule();
    @Rule public final TemporaryFolder folder = new TemporaryFolder();

    public static final String CASTOR_GRPC_TEST = "castor-grpc-test";

    private Server inProcessServer;
    private ManagedChannel channel;
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingStub;
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningStub;
    private InfrastructureDescriptorModel infrastructureDescriptorModel;
    private DeploymentDescriptorModel deploymentDescriptorModel;
    ProvisioningServiceTestImpl provisioningServiceTest;
    private CompletableFuture<CastorDeploymentStatus> completableFuture;

    @Autowired
    private ProvisionerService provisionerService;

    /**
     * Comment to keep #$#$%%$^ checkstyle happy.
     * @throws IOException exception
     */
    @Before
    public void init() throws IOException {
        infrastructureDescriptorModel = DescriptorTestUtills.buildInfraDescriptorModel();
        deploymentDescriptorModel = DescriptorTestUtills.buildDeploymentDescriptorModel();

        completableFuture = new CompletableFuture<>();
        provisioningServiceTest = new ProvisioningServiceTestImpl(completableFuture);
        // spy so we can verify that the gRPC skeleton was actually invoked
        provisioningServiceTest = spy(provisioningServiceTest);

        inProcessServer = InProcessServerBuilder
                .forName(CASTOR_GRPC_TEST)
                .directExecutor()
                .addService(provisioningServiceTest)
                .build();

        channel = InProcessChannelBuilder
                .forName(CASTOR_GRPC_TEST)
                .directExecutor()
                .build();

        blockingStub = ProvisioningServiceV2Grpc.newBlockingStub(channel);
        asyncProvisioningStub = ProvisioningServiceV2Grpc.newStub(channel);

        grpcCleanupRule.register(inProcessServer);
        grpcCleanupRule.register(channel);

        Whitebox.setInternalState(provisionerService, "blockingProvisioningClient", blockingStub);
        Whitebox.setInternalState(provisionerService, "asyncProvisioningClient", asyncProvisioningStub);
        inProcessServer.start();
    }

    @Test
    public void testProvisioningCallSuccess()
            throws InterruptedException, ExecutionException, TimeoutException, IOException {
        DeploymentRequest deploymentRequest = DeploymentRequest.newBuilder().build();

        // Sanity test without going through Castor services
        DeploymentRequestResponse deploymentResponse =
                blockingStub.createDeployment(deploymentRequest);
        deploymentResponse = spy(deploymentResponse);
        assertEquals(ProvisioningServiceTestImpl.REQUEST_ID, UUID.fromString(deploymentResponse.getId()));

        File outputFile = folder.newFile("provision-success.out");
        PrintWriter printWriter = new PrintWriter(outputFile);

        // Test path through Castor services
        provisionerService.provisioningHandoff(
                printWriter, infrastructureDescriptorModel, deploymentDescriptorModel, completableFuture);
        // The future should be set, TimeoutException is unexpected for successful test.
        CastorDeploymentStatus result = completableFuture.get(5, TimeUnit.SECONDS);
        assertEquals(CastorDeploymentStatus.SUCCESS, result);
        // the createDeployment() call should have been called twice: once explicitly as above, and one
        // via provisionerService.
        verify(provisioningServiceTest, times(2))
                .createDeployment(any(DeploymentRequest.class), any(StreamObserver.class));

        // Also verify that this server call was invoked. This one is called only once, indirectly via
        // provisionerService
        verify(provisioningServiceTest, times(1))
                .streamDeploymentSessionEvents(
                        any(StreamDeploymentSessionEventRequest.class), any(StreamObserver.class));

        // Verify that deprovisionDeployment was NOT called - this is a successful deployment
        verify(provisioningServiceTest, never())
                .deprovisionDeployment(any(DeprovisionDeploymentRequest.class), any(StreamObserver.class));

        printWriter.close();
        List<String> lines = Files.readAllLines(outputFile.toPath());
        // Match Blockchain ID
        Optional<String> bid = lines.stream()
                .filter(l -> l.contains("Blockchain Id"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.BLOCKCHAIN_ID))
                .findFirst();
        assertTrue(bid.isPresent());
        // Match Deployment Request ID
        Optional<String> drid = lines.stream()
                .filter(l -> l.contains("Deployment Request Id"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.REQUEST_ID.toString()))
                .findFirst();
        assertTrue(drid.isPresent());
        // Match (node id + ip) #1
        Optional<String> nodeIdIp1 = lines.stream()
                .filter(l -> l.contains("Node Id"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.NODE_ID_1.toString()))
                .filter(l -> l.contains("key: PUBLIC_IP"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.PUBLIC_IP_1))
                .findFirst();
        assertTrue(nodeIdIp1.isPresent());

        // Match (node id + ip) #2
        Optional<String> nodeIdIp2 = lines.stream()
                .filter(l -> l.contains("Node Id"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.NODE_ID_2.toString()))
                .filter(l -> l.contains("key: PUBLIC_IP"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.PUBLIC_IP_2))
                .findFirst();
        assertTrue(nodeIdIp2.isPresent());

        // Mismatch node id #1 + ip address #2
        Optional<String> nodeIdIpWrong = lines.stream()
                .filter(l -> l.contains("Node Id"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.NODE_ID_1.toString()))
                .filter(l -> l.contains("key: PUBLIC_IP"))
                .filter(l -> l.contains(ProvisioningServiceTestImpl.PUBLIC_IP_2))
                .findFirst();
        assertTrue(nodeIdIpWrong.isEmpty());
    }
}
