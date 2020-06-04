/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import java.util.concurrent.Flow;

/**
 * Asynchronous deployment orchestration library interface.
 * Specific implementation may employ different strategies to satisfy the interface operations (e.g.
 * stateful, staged executions, cached resources, pre-allocations, etc).
 */
public interface Orchestrator {

    /**
     * Test connectivity/sanity of the orchestration site.
     *
     * @return a [Publisher] of [Boolean] corresponding to the result of validating the orchestration site associated
     * with this [Orchestrator] instance.
     */
    default boolean validate() {
        throw new UnsupportedOperationException("Validation operation is unsupported");
    }

    /**
     * Create a Concord deployment based on a given [CreateComputeResourceRequest].
     *
     * @return a [Publisher] of [ComputeResourceEvent] corresponding to side-effects engendered by the request.
     * @param[request] deployment creation request specification.
     */
    OrchestratorData.ComputeResourceEvent createDeployment(
            OrchestratorData.CreateComputeResourceRequest request);

    default OrchestratorData.ComputeResourceEventCreatedV2 createDeploymentV2(
            OrchestratorData.CreateComputeResourceRequestV2 request) {
        throw new UnsupportedOperationException("Validation operation is unsupported");
    }

    /**
     * Delete a Concord deployment based on a given [DeleteComputeResourceRequest].
     *
     * @return a [Publisher] of [ComputeResourceEvent] corresponding to side-effects engendered by the request.
     * @param[request] deployment deletion request specification.
     */
    OrchestratorData.ComputeResourceEvent deleteDeployment(
            OrchestratorData.DeleteComputeResourceRequest request);

    /**
     * Create a reachable network address based on a given [CreateNetworkResourceRequest].
     *
     * @return a [Publisher] of [NetworkResourceEvent] corresponding to side-effects engendered by the request.
     * @param[request] network address creation request specification.
     */
    @Deprecated
    Flow.Publisher<OrchestratorData.NetworkResourceEvent> createNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request);

    OrchestratorData.NetworkResourceEvent createPrivateNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request);

    default OrchestratorData.NetworkResourceEvent createPublicNetworkAddress(
            OrchestratorData.CreateNetworkResourceRequest request) {
        throw new UnsupportedOperationException("Validation operation is unsupported");
    }

    /**
     * Delete a reachable network address based on a given [DeleteNetworkResourceRequest].
     *
     * @return a [Publisher] of [NetworkResourceEvent] corresponding to side-effects engendered by the request.
     * @param[request] network address creation request specification.
     */
    Flow.Publisher<OrchestratorData.NetworkResourceEvent> deleteNetworkAddress(
            OrchestratorData.DeleteNetworkResourceRequest request);

    /**
     * Allocate a network address to a deployed compute resource.
     *
     * @param[request] network address allocation request specification.
     * @return [Publisher] of [NetworkAllocationEvent] corresponding to side-effects engendered by the request.
     */
    @Deprecated
    Flow.Publisher<OrchestratorData.NetworkAllocationEvent> createNetworkAllocation(
            OrchestratorData.CreateNetworkAllocationRequest request);

    //TODO this can be moved to VMC specific.
    default OrchestratorData.NetworkAllocationEvent createVmcNetworkAllocation(
            OrchestratorData.CreateNetworkAllocationRequestV2 request) {
        return null;
    }

    /**
     * Deallocate a network address to a deployed compute resource.
     *
     * @param[request] network address allocation request specification.
     * @return a [Publisher] of [NetworkAllocationEvent] corresponding to side-effects engendered by the request.
     */
    Flow.Publisher<OrchestratorData.NetworkAllocationEvent> deleteNetworkAllocation(
            OrchestratorData.DeleteNetworkAllocationRequest request);
}