/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.List;
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

    private static final String DEPLOYMENT_TIMEOUT_MINUTES_KEY = "castor.deployment.timeout.minutes";

    private final Environment environment;
    private final DescriptorService descriptorService;
    private final ProvisionerService provisionerService;
    private final ValidatorService validatorService;

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
        List<ValidationError> errors = validatorService.validate(infrastructureDescriptor, deploymentDescriptor);
        if (errors.size() > 0) {
            log.error("Infrastructure and/or deployment descriptor specification has errors");
            throw new CastorException(ErrorCode.VALIDATION_ERRORS, INFRA_DESC_LOC_KEY);
        }

        // This is used so the application will wait until the async deployment process finishes
        CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture = new CompletableFuture<>();

        long deploymentTimeoutMinutes = environment.getProperty(DEPLOYMENT_TIMEOUT_MINUTES_KEY, Long.class, 30L);

        // Process deployment
        try {
            provisionerService.provisioningHandoff(
                    infrastructureDescriptor, deploymentDescriptor, deploymentCompletionFuture);
        } finally {
            try {
                CastorDeploymentStatus result =
                        deploymentCompletionFuture.get(deploymentTimeoutMinutes, TimeUnit.MINUTES);
                log.info("Deployment completed with status: {}", result);
            } catch (InterruptedException e) {
                log.error("Deployment failure. Provisioning was interrupted", e);
            } catch (ExecutionException e) {
                log.error("Deployment failure. Provisioning encountered an error", e);
            } catch (TimeoutException e) {
                log.error("Deployment failure: timed out in {} minutes", deploymentTimeoutMinutes, e);
            }
        }
    }
}
