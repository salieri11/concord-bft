/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ReconfigurationDescriptorModel;

/**
 * Defines the reconfiguration service contract.
 */
public interface ReconfigurerService {

    /**
     * Reconfiguration entrypoint.
     * @param printWriter the writer to which the reconfiguration status is logged
     * @param infrastructureDescriptorModel the infra descriptor
     * @param reconfigurationDescriptorModel the reconfiguration deployment descriptor
     * @param deploymentCompletionFuture the completion future used to signal the caller about the status of the request
     */
    void reconfigureHandoff(
            PrintWriter printWriter,
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            ReconfigurationDescriptorModel reconfigurationDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture);

}
