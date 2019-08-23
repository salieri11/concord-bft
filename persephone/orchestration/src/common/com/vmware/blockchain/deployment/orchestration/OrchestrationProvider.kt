/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.orchestration

import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo

/**
 * Contract factory interface to construct a new [Orchestrator] instance.
 */
interface OrchestratorProvider {

    /**
     * Create a new [Orchestrator] based on the input [OrchestrationSiteInfo].
     *
     * @param site
     *   information pertaining to the orchestration site to create orchestrator driver for.
     *
     * @return
     *   a new concrete implementation of an [Orchestrator] instance.
     */
    fun newOrchestrator(site: OrchestrationSiteInfo): Orchestrator
}
