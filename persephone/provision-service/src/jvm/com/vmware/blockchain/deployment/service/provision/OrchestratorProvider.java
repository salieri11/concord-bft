/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;

/**
 * Contract factory interface to construct a new {@link Orchestrator} instance.
 */
public interface OrchestratorProvider {

    /**
     * Create a new {@link Orchestrator} based on the input {@link OrchestrationSiteInfo}.
     *
     * @param site
     *   information pertaining to the orchestration site to create orchestrator driver for.
     *
     * @return
     *   a new concrete implementation of an {@link Orchestrator} instance.
     */
    CompletableFuture<Orchestrator> newOrchestrator(OrchestrationSiteInfo site);
}
