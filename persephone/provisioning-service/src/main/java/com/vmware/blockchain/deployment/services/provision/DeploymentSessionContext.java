/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.provision;

import java.util.Map;

import javax.validation.constraints.NotNull;

import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;

/**
 * Data object which stored session.
 */
@Deprecated
public class DeploymentSessionContext {

    @NotNull
    private final Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators;

    @NotNull
    public final Map<OrchestrationSiteIdentifier, Orchestrator> getOrchestrators() {
        return this.orchestrators;
    }

    public DeploymentSessionContext(@NotNull Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators) {
        this.orchestrators = orchestrators;
    }
}