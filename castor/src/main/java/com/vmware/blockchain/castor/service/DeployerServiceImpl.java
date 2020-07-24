/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

/**
 * Deployer servvice implementation.
 */
@Service
@Log4j2
@RequiredArgsConstructor
public class DeployerServiceImpl implements DeployerService {

    private static final String DEPL_DESC_LOC_KEY = "castor.deployment.descriptor.location";
    private static final String INFRA_DESC_LOC_KEY = "castor.infrastructure.descriptor.location";
    private static final String DEPLOYMENT_TIMEOUT_MINUTES_KEY = "castor.deployment.timeout.minutes";

    private final Environment environment;
    private final DescriptorService descriptorService;
    private final ProvisionerService provisionerService;

    /**
     * Deployer service entrypoint.
     */
    public void start() {
        log.info("Starting Castor deployer service");

        String infrastructureDescriptorLocation = environment.getProperty(INFRA_DESC_LOC_KEY);
        if (!StringUtils.hasText(infrastructureDescriptorLocation)) {
            log.error("Required infrastructure descriptor property {} is not set", INFRA_DESC_LOC_KEY);
            throw new CastorException(ErrorCode.INFRA_DESC_PROPERTY_NOT_SET, INFRA_DESC_LOC_KEY);
        }

        InfrastructureDescriptorModel infrastructureDescriptor =
                descriptorService.readInfrastructureDescriptorSpec(infrastructureDescriptorLocation);

        String deploymentDescriptorLocation = environment.getProperty(DEPL_DESC_LOC_KEY);
        if (!StringUtils.hasText(deploymentDescriptorLocation)) {
            log.error("Required deployment descriptor property {} is not set", DEPL_DESC_LOC_KEY);
            throw new CastorException(ErrorCode.DEPL_DESC_PROPERTY_NOT_SET, DEPL_DESC_LOC_KEY);
        }

        DeploymentDescriptorModel deploymentDescriptor =
                descriptorService.readDeploymentDescriptorSpec(deploymentDescriptorLocation);

        // Validate properties for correctness
        validate(infrastructureDescriptor, deploymentDescriptor);

        // This is used so the application will wait until the async deployment process finishes
        CompletableFuture<String> deploymentCompletionFuture = new CompletableFuture<>();

        int deploymentTimeoutMinutes = environment.getProperty(DEPLOYMENT_TIMEOUT_MINUTES_KEY, Integer.class, 30);

        // Process deployment
        try {
            provisionerService.provisioningHandoff(
                    infrastructureDescriptor, deploymentDescriptor, deploymentCompletionFuture);
        } catch (Throwable t) {
            // caught by ExecutionException below
            deploymentCompletionFuture.completeExceptionally(t);
        } finally {
            try {
                String result = deploymentCompletionFuture.get(deploymentTimeoutMinutes, TimeUnit.MINUTES);
            } catch (InterruptedException e) {
                log.error("Deployment failure. Provisioning was interrupted", e);
            } catch (ExecutionException e) {
                log.error("Deployment failure. Provisioning encountered an error", e);
            } catch (TimeoutException e) {
                log.error("Deployment failure: timed out in {} minutes", deploymentTimeoutMinutes, e);
            }
        }
    }

    private void validate(InfrastructureDescriptorModel infrastructureDescriptor,
                          DeploymentDescriptorModel deploymentDescriptor) {
        // DINKARTODO: Move this to a service. Validate entries. See BlockchainController::createDeployment()
    }
}
