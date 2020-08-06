/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

/**
 * Provisioner service.
 */
public interface ProvisionerService {
    void provisioningHandoff(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture);
}
