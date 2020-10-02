/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import static com.vmware.blockchain.castor.service.DeploymentHelper.PROVISIONING_TIMEOUT_MINUTES_KEY;

import java.io.PrintWriter;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSite;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.Sites;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;

import lombok.extern.log4j.Log4j2;

/**
 * Provisioner service implementation.
 */
@Service
@Log4j2
public class ProvisionerServiceImpl implements ProvisionerService {

    private final Environment environment;
    private final ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient;
    private final ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningClient;

    @Autowired
    public ProvisionerServiceImpl(
            Environment environment,
            ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient,
            ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningClient) {
        this.environment = environment;
        this.blockingProvisioningClient = blockingProvisioningClient;
        this.asyncProvisioningClient = asyncProvisioningClient;
    }

    @Override
    public void provisioningHandoff(
            PrintWriter printWriter, InfrastructureDescriptorModel infrastructureDescriptorModel,
            ProvisionDescriptorDescriptorModel provisioningDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) {

        DeploymentRequest deploymentRequest = DeploymentHelper.constructDeploymentRequest(
                infrastructureDescriptorModel, provisioningDescriptorModel);

        // Request a deployment from the provisioning service
        log.debug("Requesting a deployment with request info: {}", deploymentRequest);
        String deploymentRequestId = submitDeploymentRequest(deploymentRequest, deploymentCompletionFuture);
        printWriter.printf("Deployment Request Id: %s\n", deploymentRequestId);

        // Process callbacks from the provisioning service
        CastorDeploymentStatus status = provisionAndComplete(printWriter, deploymentRequestId);
        if (CastorDeploymentStatus.FAILURE == status) {
            deprovisionDeployment(infrastructureDescriptorModel, provisioningDescriptorModel, deploymentRequestId);
            deploymentCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        } else {
            deploymentCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
        }
    }

    private String submitDeploymentRequest(
            DeploymentRequest deploymentRequest,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) throws CastorException {
        try {
            DeploymentRequestResponse response = blockingProvisioningClient.createDeployment(deploymentRequest);
            String requestId = response.getId();
            log.info("Deployment submitted, request id {}", requestId);
            return requestId;
        } catch (Throwable e) {
            deploymentCompletionFuture.completeExceptionally(e);
            log.error("Error in submitting a deployment request", e);
            throw new CastorException(ErrorCode.DEPL_REQUEST_SUBMIT_ERROR, e);
        }
    }

    private CastorDeploymentStatus provisionAndComplete(PrintWriter printWriter, String deploymentRequestId) {

        // This future is for completion of provisioning itself, since those callbacks are asynchronous
        CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture = new CompletableFuture<>();

        // Request callbacks from Persephone to the previously issued deployment request.
        streamDeploymentSessionEvents(printWriter, deploymentRequestId, provisioningCompletionFuture);

        // Wait until the deployment session is completed.
        long provisioningTimeoutMinutes = environment.getProperty(PROVISIONING_TIMEOUT_MINUTES_KEY, Long.class, 30L);
        try {
            CastorDeploymentStatus provisioningStatus =
                    provisioningCompletionFuture.get(provisioningTimeoutMinutes, TimeUnit.MINUTES);
            return provisioningStatus;
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            log.error("Encountered error during provisioning call", e);
            return CastorDeploymentStatus.FAILURE;
        }
    }

    private void streamDeploymentSessionEvents(PrintWriter printWriter, String deploymentRequestId,
                                               CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture) {
        DeploymentExecutionEventResponseObserver deero =
                new DeploymentExecutionEventResponseObserver(
                        printWriter, deploymentRequestId, provisioningCompletionFuture);

        StreamDeploymentSessionEventRequest request =
                StreamDeploymentSessionEventRequest.newBuilder()
                        .setHeader(MessageHeader.newBuilder().build())
                        .setSessionId(deero.getRequestId())
                        .build();

        asyncProvisioningClient.streamDeploymentSessionEvents(request, deero);
    }

    private void deprovisionDeployment(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            ProvisionDescriptorDescriptorModel provisioningDescriptorModel,
            String requestId) {
        log.info("Initiating deprovisioning for deployment with request id: {}", requestId);
        List<OrchestrationSite> orchestrationSites =
                DeploymentHelper.buildSites(infrastructureDescriptorModel,
                                            provisioningDescriptorModel.getCommitters(),
                                            provisioningDescriptorModel.getClients());
        Sites sites = Sites.newBuilder()
                .addAllInfoList(orchestrationSites)
                .build();

        DeprovisionDeploymentRequest deprovisionDeploymentRequest =
                DeprovisionDeploymentRequest.newBuilder()
                        .setSessionId(requestId)
                        .setSites(sites)
                        .build();

        // Use the blocking API since DeprovisionDeploymentResponse has no useful info.
        blockingProvisioningClient.deprovisionDeployment(deprovisionDeploymentRequest);
        log.info("Deprovisioning completed for deployment with request id: {}", requestId);
    }
}
