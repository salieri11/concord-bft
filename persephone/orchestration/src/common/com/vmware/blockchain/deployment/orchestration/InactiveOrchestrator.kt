/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.orchestration

import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.reactive.ErrorPublisher
import com.vmware.blockchain.deployment.reactive.Publisher

/**
 * Deployment orchestration driver for an unsupported [OrchestrationSiteInfo] type.
 */
class InactiveOrchestrator(site: OrchestrationSiteInfo) : Orchestrator {

    /** Error message when caller attempts to invoke orchestration actions. */
    private val errorMessage: String = "Orchestrator is not active, type(${site.type})"

    override fun close() {}

    override fun createDeployment(
        request: Orchestrator.CreateComputeResourceRequest
    ): Publisher<Orchestrator.ComputeResourceEvent> {
        return ErrorPublisher(UnsupportedOperationException(errorMessage))
    }

    override fun deleteDeployment(
        request: Orchestrator.DeleteComputeResourceRequest
    ): Publisher<Orchestrator.ComputeResourceEvent> {
        return ErrorPublisher(UnsupportedOperationException(errorMessage))
    }

    override fun createNetworkAddress(
        request: Orchestrator.CreateNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        return ErrorPublisher(UnsupportedOperationException(errorMessage))
    }

    override fun deleteNetworkAddress(
        request: Orchestrator.DeleteNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        return ErrorPublisher(UnsupportedOperationException(errorMessage))
    }

    override fun createNetworkAllocation(
        request: Orchestrator.NetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return ErrorPublisher(UnsupportedOperationException(errorMessage))
    }

    override fun deleteNetworkAllocation(
        request: Orchestrator.NetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return ErrorPublisher(UnsupportedOperationException(errorMessage))
    }
}
