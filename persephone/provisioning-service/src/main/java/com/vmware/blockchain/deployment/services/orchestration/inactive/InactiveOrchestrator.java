/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.orchestration.inactive;

import java.util.concurrent.Flow;

import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

/**
 * Deployment orchestration driver for an unsupported [OrchestrationSiteInfo] type.
 */
public class InactiveOrchestrator implements Orchestrator {

    private OrchestrationSiteInfo site;

    /** Error message when caller attempts to invoke orchestration actions. */
    private String errorMessage = "Orchestrator is not active, type(${site.type})";

    public InactiveOrchestrator(OrchestrationSiteInfo site) {
        this.site = site;
    }

    @Override
    public OrchestratorData.ComputeResourceEvent createDeployment(
            OrchestratorData.CreateComputeResourceRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public OrchestratorData.ComputeResourceEvent deleteDeployment(
            OrchestratorData.DeleteComputeResourceRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkResourceEvent> createNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public OrchestratorData.NetworkResourceEvent createPrivateNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkResourceEvent> deleteNetworkAddress(
            OrchestratorData.DeleteNetworkResourceRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkAllocationEvent> createNetworkAllocation(
            OrchestratorData.CreateNetworkAllocationRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }

    @Override
    public Flow.Publisher<OrchestratorData.NetworkAllocationEvent> deleteNetworkAllocation(
            OrchestratorData.DeleteNetworkAllocationRequest request) {
        throw new UnsupportedOperationException(errorMessage);
    }
}
