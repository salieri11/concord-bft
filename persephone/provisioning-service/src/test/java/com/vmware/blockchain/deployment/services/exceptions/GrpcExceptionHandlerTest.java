/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.exceptions;

import static com.vmware.blockchain.deployment.services.exceptions.ProvisioningServiceFailureTestImpl.ERR_MSG_BAD_REQ;
import static com.vmware.blockchain.deployment.services.exceptions.ProvisioningServiceFailureTestImpl.ERR_MSG_NOT_FOUND;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;

import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.vmware.blockchain.deployment.services.exception.GrpcExceptionHandler;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

import io.grpc.ManagedChannel;
import io.grpc.Server;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import io.grpc.inprocess.InProcessChannelBuilder;
import io.grpc.inprocess.InProcessServerBuilder;
import io.grpc.testing.GrpcCleanupRule;

/**
 * Tests if GrpcExceptionHandler is working.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ExtendWith(SpringExtension.class)
public class GrpcExceptionHandlerTest {

    @Rule
    public final GrpcCleanupRule grpcCleanupRule = new GrpcCleanupRule();
    @Rule
    public final TemporaryFolder folder = new TemporaryFolder();

    public static final String GRPC_ERROR_MSG_TEST = "grpc-error-msg-test";

    private Server inProcessServer;
    private ManagedChannel channel;
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingStub;
    private ProvisioningServiceFailureTestImpl provisioningServiceFailureTest;
    private CompletableFuture<DeploymentStatus> completableFuture;


    /**
     * Sets the necessary elements for the tests.
     * */
    @Before
    public void init() throws IOException {

        completableFuture = new CompletableFuture<>();
        provisioningServiceFailureTest = new ProvisioningServiceFailureTestImpl(completableFuture);

        inProcessServer = InProcessServerBuilder
                .forName(GRPC_ERROR_MSG_TEST)
                .directExecutor()
                .addService(provisioningServiceFailureTest)
                .intercept(new GrpcExceptionHandler())
                .build();

        channel = InProcessChannelBuilder
                .forName(GRPC_ERROR_MSG_TEST)
                .directExecutor()
                .build();

        blockingStub = ProvisioningServiceV2Grpc.newBlockingStub(channel);

        grpcCleanupRule.register(inProcessServer);
        grpcCleanupRule.register(channel);

        inProcessServer.start();
    }

    @Test
    public void testBadRequestErrorMsg() {
        try {
            blockingStub.createDeployment(DeploymentRequest.newBuilder().build());
        } catch (StatusRuntimeException ex) {
            Status status = ex.getStatus();
            assertEquals(status.getCode(), Status.INVALID_ARGUMENT.getCode());
            assertEquals(status.getDescription(), ERR_MSG_BAD_REQ);
        }
    }

    @Test
    public void testNotFoundErrorMsg() {
        try {
            blockingStub.deprovisionDeployment(
                    DeprovisionDeploymentRequest.newBuilder().build());
        } catch (StatusRuntimeException ex) {
            Status status = ex.getStatus();
            assertEquals(status.getCode(), Status.NOT_FOUND.getCode());
            assertEquals(status.getDescription(), ERR_MSG_NOT_FOUND);
        }
    }

}
