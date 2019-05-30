/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.provision.common;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.InactiveOrchestrator;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;
import com.vmware.blockchain.deployment.vmc.VmcOrchestrator;

import kotlinx.coroutines.ExecutorsKt;


/**
 *  Provides Orchestrator objects to work with.
 */
public class OrchestrationProviders {

    private static final Logger log = LoggerFactory.getLogger(OrchestrationProviders.class);

    private final ExecutorService executor;

    public OrchestrationProviders(ExecutorService executor) {
        this.executor = executor;
    }

    /**
     * Creates and returns a new orchestrator based on 'Type' in OrchestrationInfo.
     * @param site OrchestrationSiteInfo
     * @return Orchestrator as a future
     * TODO: Make interface impls and package when multiple orchestrator types are implemented
     */
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

    /*public OrchestrationSiteInfo getOrchestrationSiteInfo(String info) {
        OrchestrationSiteInfo siteInfo = new VmcOrchestrationSiteInfo();
    }*/


}
