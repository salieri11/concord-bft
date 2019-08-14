/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import com.vmware.blockchain.deployment.model.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider;

/**
 * Test implementation of {@link OrchestratorProvider}.
 */
class TestOrchestratorFactory implements OrchestratorProvider {

    @Override
    public Orchestrator newOrchestrator(OrchestrationSiteInfo site) {
        return new BookKeepingStubOrchestrator(site);
    }
}
