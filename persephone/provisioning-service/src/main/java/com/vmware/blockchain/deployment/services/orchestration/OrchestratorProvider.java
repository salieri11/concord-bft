/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

/**
 * Contract factory interface to construct a new [Orchestrator] instance.
 */
public interface OrchestratorProvider {

    /**
     * Create a new [Orchestrator] based on the input [OrchestrationSiteInfo].
     *
     * @param site
     *   information pertaining to the orchestration site to create orchestrator driver for.
     *
     * @return
     *   a new concrete implementation of an [Orchestrator] instance.
     */
    Orchestrator newOrchestrator(OrchestrationSiteInfo site, IpamClient ipamClient);
}
