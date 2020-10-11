/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.exceptions;

import static com.vmware.blockchain.server.exceptions.ConfigServiceFailureTestImpl.ERR_MSG_BFT_NO_CONFIG;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequestV2;

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

    public static final String GRPC_EXC_HANDLER_TEST = "grpc-exception-handler-test";
    public static final String GRPC_ERROR_TEST = "grpc-error-test";

    private Server inProcessServer;
    private ManagedChannel channel;
    private ConfigServiceFailureTestImpl configServiceFailureTest;
    private ConfigurationServiceGrpc.ConfigurationServiceBlockingStub stub;

    /**
     * Tests and if successful proves an error enhancement is done by the gRPC exception handler.
     * */
    @Test
    public void testExceptionHandlerWorking() throws IOException {
        init(Boolean.TRUE);
        try {
            stub.createConfigurationV2(ConfigurationServiceRequestV2.getDefaultInstance());
        } catch (StatusRuntimeException ex) {
            Status status = ex.getStatus();
            assertEquals(status.getCode(), Status.INTERNAL.getCode());
            assertEquals(status.getDescription(), ERR_MSG_BFT_NO_CONFIG);
        }
    }

    /**
     * Tests and if successful proves the error IS NOT enhanced (i.e. client gets default error)
     * if no gRPC exception handler is used.
     * */
    @Test
    public void testNoExceptionHandlerWorking() throws IOException {
        init(Boolean.FALSE);
        try {
            stub.createConfigurationV2(ConfigurationServiceRequestV2.getDefaultInstance());
        } catch (StatusRuntimeException ex) {
            Status status = ex.getStatus();
            assertEquals(status.getCode(), Status.UNKNOWN.getCode());
            assertNull(status.getDescription());
        }
    }

    private void init(boolean isErrorHandled) throws IOException {
        configServiceFailureTest = new ConfigServiceFailureTestImpl();
        if (isErrorHandled) {
            inProcessServer = InProcessServerBuilder
                    .forName(GRPC_EXC_HANDLER_TEST)
                    .directExecutor()
                    .addService(configServiceFailureTest)
                    .intercept(new GrpcExceptionHandler())
                    .build();

            channel = InProcessChannelBuilder
                    .forName(GRPC_EXC_HANDLER_TEST)
                    .usePlaintext()
                    .directExecutor()
                    .build();
        } else {
            inProcessServer = InProcessServerBuilder
                    .forName(GRPC_ERROR_TEST)
                    .directExecutor()
                    .addService(configServiceFailureTest)
                    .build();

            channel = InProcessChannelBuilder
                    .forName(GRPC_ERROR_TEST)
                    .usePlaintext()
                    .directExecutor()
                    .build();
        }

        stub = ConfigurationServiceGrpc.newBlockingStub(channel);

        grpcCleanupRule.register(inProcessServer);
        grpcCleanupRule.register(channel);

        inProcessServer.start();

    }
}
