/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.springframework.stereotype.Service;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ReconfigurationDescriptorModel;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

/**
 * Cloning service implementation.
 */
@Service
@Log4j2
@RequiredArgsConstructor
public class CloningServiceImpl implements CloningService {

    private final ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient;

    private final ProvisionerService provisionerService;

    @Override
    public void cloningHandoff(
            PrintWriter printWriter,
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            ReconfigurationDescriptorModel reconfigurationDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) {

        DeploymentRequest cloningRequest = DeploymentHelper.constructDeploymentRequest(
                infrastructureDescriptorModel, reconfigurationDescriptorModel);

        // Additionally, the reconfigure request must be provided a blockchain ID from a previous
        // PROVISION deployment.
        UUID blockchainId = reconfigurationDescriptorModel.getBlockchain().getBlockchainId();
        DeploymentSpec originalSpec = cloningRequest.getSpec();
        var originalProps = originalSpec.getProperties();
        var finalPros = Properties.newBuilder(originalProps)
                .putValues(DeploymentAttributes.NO_LAUNCH.name(), "True");
        DeploymentSpec.Builder newSpec =
                DeploymentSpec.newBuilder(originalSpec)
                        .setBlockchainId(blockchainId.toString())
                .setProperties(finalPros);

        // Request a deployment from the provisioning service
        String deploymentRequestId = provisionerService
                .submitDeploymentRequest(DeploymentRequest.newBuilder(cloningRequest).setSpec(newSpec).build(),
                                         deploymentCompletionFuture);
        printWriter.printf("Deployment Request Id: %s\n", deploymentRequestId);

        // Process callbacks from the provisioning service
        CastorDeploymentStatus status = provisionerService.provisionAndComplete(printWriter, deploymentRequestId);
        if (CastorDeploymentStatus.FAILURE == status) {
            // TODO introduce auto cleanup if needed.
            deploymentCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        } else {
            deploymentCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
        }
    }

    @Override
    public void scalingHandoff(
            PrintWriter printWriter,
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            ReconfigurationDescriptorModel reconfigurationDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) {

        DeploymentRequest cloningRequest = DeploymentHelper.constructDeploymentRequest(
                infrastructureDescriptorModel, reconfigurationDescriptorModel);

        // Additionally, the reconfigure request must be provided a blockchain ID from a previous
        // PROVISION deployment.
        UUID blockchainId = reconfigurationDescriptorModel.getBlockchain().getBlockchainId();
        DeploymentSpec originalSpec = cloningRequest.getSpec();
        var originalProps = originalSpec.getProperties();
        var finalPros = Properties.newBuilder(originalProps)
                .putValues(DeploymentAttributes.NO_LAUNCH.name(), "True")
                .putValues(DeploymentAttributes.SKIP_CONFIG_SERVICE.name(), "True");
        DeploymentSpec.Builder newSpec =
                DeploymentSpec.newBuilder(originalSpec)
                        .setBlockchainId(blockchainId.toString())
                        .setProperties(finalPros);

        // Request a deployment from the provisioning service
        String deploymentRequestId = provisionerService
                .submitDeploymentRequest(DeploymentRequest.newBuilder(cloningRequest).setSpec(newSpec).build(),
                                         deploymentCompletionFuture);
        printWriter.printf("Deployment Request Id: %s\n", deploymentRequestId);

        // Process callbacks from the provisioning service
        CastorDeploymentStatus status = provisionerService.provisionAndComplete(printWriter, deploymentRequestId);
        if (CastorDeploymentStatus.FAILURE == status) {
            // TODO introduce auto cleanup if needed.
            deploymentCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        } else {
            deploymentCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
        }
    }

}
