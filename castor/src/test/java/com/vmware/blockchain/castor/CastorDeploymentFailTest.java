/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
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
import com.vmware.blockchain.castor.service.ProvisioningServiceFailureTestImpl;
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
 * Test RPC round trip via all Castor services for FAILED deployments.
 * Make sure the deprovisionDeployment() callback is called. This is a separate test because the
 * failure service (skeleton impl) needs to be registered with the InProcess GRPC server.
 */
// Needs Junit4 for rules
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CastorTestConfiguration.class)
public class CastorDeploymentFailTest {
    @Rule public final GrpcCleanupRule grpcCleanupRule = new GrpcCleanupRule();
    @Rule public final TemporaryFolder folder = new TemporaryFolder();

    public static final String CASTOR_GRPC_TEST = "castor-grpc-test";

    private Server inProcessServer;
    private ManagedChannel channel;
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingStub;
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningStub;
    private InfrastructureDescriptorModel infrastructureDescriptorModel;
    private DeploymentDescriptorModel deploymentDescriptorModel;
    private ProvisioningServiceFailureTestImpl provisioningServiceFailureTest;
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
        provisioningServiceFailureTest = new ProvisioningServiceFailureTestImpl(completableFuture);
        // spy so we can verify that the gRPC skeleton was actually invoked
        provisioningServiceFailureTest = spy(provisioningServiceFailureTest);

        inProcessServer = InProcessServerBuilder
                .forName(CASTOR_GRPC_TEST)
                .directExecutor()
                .addService(provisioningServiceFailureTest)
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
    public void testDeprovisioningCallFailure()
            throws InterruptedException, ExecutionException, TimeoutException, IOException {
        DeploymentRequest deploymentRequest = DeploymentRequest.newBuilder().build();

        // Sanity test without going through Castor services
        DeploymentRequestResponse deploymentResponse =
                blockingStub.createDeployment(deploymentRequest);
        deploymentResponse = spy(deploymentResponse);
        assertEquals(ProvisioningServiceFailureTestImpl.REQUEST_ID, UUID.fromString(deploymentResponse.getId()));

        File outputFile = folder.newFile("provision-fail.out");
        PrintWriter printWriter = new PrintWriter(outputFile);

        // Test path through Castor services
        provisionerService.provisioningHandoff(
                printWriter, infrastructureDescriptorModel, deploymentDescriptorModel, completableFuture);
        // The future should be set, TimeoutException is unexpected for successful test.
        CastorDeploymentStatus result = completableFuture.get(5, TimeUnit.SECONDS);
        assertEquals(CastorDeploymentStatus.FAILURE, result);
        // the createDeployment() call should have been called twice: once explicitly as above, and one
        // via provisionerService.
        verify(provisioningServiceFailureTest, times(2))
                .createDeployment(any(DeploymentRequest.class), any(StreamObserver.class));

        // Also verify that this server call was invoked. This one is called only once, indirectly via
        // provisionerService
        verify(provisioningServiceFailureTest)
                .streamDeploymentSessionEvents(
                        any(StreamDeploymentSessionEventRequest.class), any(StreamObserver.class));

        // Since this is a failure case, the deprovisionDeployment() call should have been made by the client,
        // and it should have reached the gRPC server.
        verify(provisioningServiceFailureTest)
                .deprovisionDeployment(
                        any(DeprovisionDeploymentRequest.class), any(StreamObserver.class));
    }
}
