/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision;

import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;

/**
 * Test implementation of {@link OrchestratorProvider}.
 */
class TestOrchestratorFactory implements OrchestratorProvider {

    @Override
    public CompletableFuture<Orchestrator> newOrchestrator(OrchestrationSiteInfo site) {
        return CompletableFuture.completedFuture(new BookKeepingStubOrchestrator(site));
    }
}
