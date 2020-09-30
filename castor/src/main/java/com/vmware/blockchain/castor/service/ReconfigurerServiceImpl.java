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
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
import com.vmware.blockchain.deployment.v1.GenerateConfigurationResponse;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

/**
 * Reconfiguration service implementation.
 */
@Service
@Log4j2
@RequiredArgsConstructor
public class ReconfigurerServiceImpl implements ReconfigurerService {

    private final ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient;

    @Override
    public void reconfigureHandoff(
            PrintWriter printWriter,
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            ReconfigurationDescriptorModel reconfigurationDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) {

        DeploymentRequest originalReconfigRequest = DeploymentHelper.constructDeploymentRequest(
                infrastructureDescriptorModel, reconfigurationDescriptorModel);

        // Additionally, the reconfigure request must be provided a blockchain ID from a previous
        // PROVISION deployment.
        UUID blockchainId = reconfigurationDescriptorModel.getBlockchain().getBlockchainId();
        DeploymentSpec originalSpec = originalReconfigRequest.getSpec();
        DeploymentSpec.Builder newSpec =
                DeploymentSpec.newBuilder(originalSpec).setBlockchainId(blockchainId.toString());

        DeploymentRequest.Builder newReconfigRequestBuilder =
                DeploymentRequest.newBuilder(originalReconfigRequest).setSpec(newSpec);
        DeploymentRequest newReconfigRequest = newReconfigRequestBuilder.build();

        // Request a deployment from the provisioning service
        log.debug("Requesting a deployment with request info: {}", newReconfigRequest);
        deploymentCompletionFuture.completeAsync(() -> {
            try {
                GenerateConfigurationResponse response =
                        blockingProvisioningClient.generateConfiguration(newReconfigRequest);
                String configurationId = response.getId();
                printWriter.printf("Reconfiguration Id: %s\n", configurationId);
                return CastorDeploymentStatus.SUCCESS;
            } catch (Exception e) {
                log.error("Reconfiguration failed with exception: {}", e);
                return CastorDeploymentStatus.FAILURE;
            }
        });
    }
}
