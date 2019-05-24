/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.InactiveOrchestrator;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;
import com.vmware.blockchain.deployment.vmc.VmcOrchestrator;

import kotlinx.coroutines.ExecutorsKt;

/**
 * Concrete implementation of {@link OrchestratorProvider}.
 */
public class OrchestratorFactory implements OrchestratorProvider {

    /** Logger instance. */
    private static final Logger log = LoggerFactory.getLogger(OrchestratorFactory.class);

    /** Executor to use for all async operations. */
    private final ExecutorService executor;

    OrchestratorFactory(ExecutorService executor) {
        this.executor = executor;
    }

    @Override
    public CompletableFuture<Orchestrator> newOrchestrator(OrchestrationSiteInfo site) {
        if (site.getType() == OrchestrationSiteInfo.Type.VMC) {
            var publisher = VmcOrchestrator.newOrchestrator(site, ExecutorsKt.from(executor));

            // Subscribe to publisher and return a future.
            return ReactiveStream.toFuture(publisher)
                    .handle((result, error) -> {
                        if (error != null) {
                            log.info("Cannot initialize orchestrator, site({})", site);

                            // Return an inactive orchestrator instance.
                            return new InactiveOrchestrator(site);
                        }
                        else {
                            // No problems, just return the same result to next stage.
                            return result;
                        }
                    });
        } else {
            return CompletableFuture.completedFuture(new InactiveOrchestrator(site));
        }
    }
}
