/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.util.List;
import java.util.Optional;
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

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;
import com.vmware.blockchain.castor.service.CastorDeploymentStatus;
import com.vmware.blockchain.castor.service.SiteValidationServiceTestImpl;
import com.vmware.blockchain.castor.service.SiteValidatorService;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;

import io.grpc.ManagedChannel;
import io.grpc.Server;
import io.grpc.inprocess.InProcessChannelBuilder;
import io.grpc.inprocess.InProcessServerBuilder;
import io.grpc.stub.StreamObserver;
import io.grpc.testing.GrpcCleanupRule;

/**
 * Test Site validation RPC round-trip, success case.
 */
// Needs Junit4 for rules
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = CastorTestConfiguration.class)
public class CastorSiteValidationSuccessTest {
    @Rule public final GrpcCleanupRule grpcCleanupRule = new GrpcCleanupRule();
    @Rule public final TemporaryFolder folder = new TemporaryFolder();

    public static final String CASTOR_GRPC_TEST = "castor-grpc-test";

    private Server inProcessServer;
    private ManagedChannel channel;
    private OrchestrationSiteServiceGrpc.OrchestrationSiteServiceBlockingStub blockingStub;
    private InfrastructureDescriptorModel infrastructureDescriptorModel;
    private ProvisionDescriptorDescriptorModel provisioningDescriptorModel;
    SiteValidationServiceTestImpl siteValidationServiceTest;
    private CompletableFuture<CastorDeploymentStatus> completableFuture;

    @Autowired
    private SiteValidatorService siteValidatorService;

    /**
     * Comment to keep #$#$%%$^ checkstyle happy.
     * @throws IOException exception
     */
    @Before
    public void init() throws IOException {
        infrastructureDescriptorModel = DescriptorTestUtills.buildInfraDescriptorModel();
        provisioningDescriptorModel = DescriptorTestUtills.buildDeploymentDescriptorModel();

        completableFuture = new CompletableFuture<>();
        siteValidationServiceTest = new SiteValidationServiceTestImpl(true, completableFuture);
        // spy so we can verify that the gRPC skeleton was actually invoked
        siteValidationServiceTest = spy(siteValidationServiceTest);

        inProcessServer = InProcessServerBuilder
                .forName(CASTOR_GRPC_TEST)
                .directExecutor()
                .addService(siteValidationServiceTest)
                .build();

        channel = InProcessChannelBuilder
                .forName(CASTOR_GRPC_TEST)
                .directExecutor()
                .build();

        blockingStub = OrchestrationSiteServiceGrpc.newBlockingStub(channel);

        grpcCleanupRule.register(inProcessServer);
        grpcCleanupRule.register(channel);

        Whitebox.setInternalState(siteValidatorService, "orchestrationSiteClient", blockingStub);
        inProcessServer.start();
    }

    @Test
    public void testSiteValidationCallSuccess()
            throws InterruptedException, ExecutionException, TimeoutException, IOException {

        File outputFile = folder.newFile("site-validation-success.out");
        PrintWriter printWriter = new PrintWriter(outputFile);
        siteValidatorService.siteValidationHandoff(
                printWriter, infrastructureDescriptorModel, provisioningDescriptorModel, completableFuture);

        // The future should be set, TimeoutException is unexpected for successful test.
        CastorDeploymentStatus result = completableFuture.get(5, TimeUnit.SECONDS);
        assertEquals(CastorDeploymentStatus.SUCCESS, result);
        // the validateOrchestrationSite() call should have been called once via siteValidationHandoff().
        verify(siteValidationServiceTest, times(1))
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class), any(StreamObserver.class));

        printWriter.close();
        List<String> lines = Files.readAllLines(outputFile.toPath());
        // Match success
        Optional<String> bid = lines.stream()
                .filter(l -> l.contains("Site validation succeeded for zone: test-zone-1 - A"))
                .findFirst();
        assertTrue(bid.isPresent());
    }
}
