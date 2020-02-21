/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.services.provision

import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo

/**
 * Test implementation of [OrchestratorProvider].
 */
internal class TestOrchestratorFactory(
    private val validator: com.vmware.blockchain.services.provision.OrchestrationSiteValidator
) : OrchestratorProvider {

    override fun newOrchestrator(site: OrchestrationSiteInfo): Orchestrator {
        return com.vmware.blockchain.services.provision.BookKeepingStubOrchestrator(site, validator)
    }
}
