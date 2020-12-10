/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;

/**
 * Provisioner service.
 */
public interface ProvisionerService {
    /**
     * Reconfiguration entrypoint.
     * @param printWriter the writer to which the reconfiguration status is logged
     * @param infrastructureDescriptorModel the infra descriptor
     * @param provisioningDescriptorModel the provisioning deployment descriptor
     * @param deploymentCompletionFuture the completion future used to signal the caller about the status of the request
     */
    void provisioningHandoff(
            PrintWriter printWriter, InfrastructureDescriptorModel infrastructureDescriptorModel,
            ProvisionDescriptorDescriptorModel provisioningDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture);

    /**
     * Internal API to trigger deployment.
     */
    String submitDeploymentRequest(
            DeploymentRequest deploymentRequest,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture);

    /**
     * Internal API to track deployment.
     */
    CastorDeploymentStatus provisionAndComplete(PrintWriter printWriter, String deploymentRequestId);
}
