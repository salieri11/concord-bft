/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;

/**
 * Site Validation service.
 */
public interface SiteValidatorService {
    /**
     * Site validation entrypoint.
     * @param printWriter the writer to which the validation status is logged
     * @param infrastructureDescriptorModel the infra descriptor
     * @param provisioningDescriptorModel the provisioning deployment descriptor
     * @param deploymentCompletionFuture the completion future used to signal the caller about the status of the request
     */
    void siteValidationHandoff(
            PrintWriter printWriter, InfrastructureDescriptorModel infrastructureDescriptorModel,
            ProvisionDescriptorDescriptorModel provisioningDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture);
}
