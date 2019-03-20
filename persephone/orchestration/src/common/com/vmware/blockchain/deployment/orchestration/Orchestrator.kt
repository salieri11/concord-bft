/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.orchestration

import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.model.ConcordModelSpecification
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.reactive.Publisher

/**
 * Asynchronous deployment orchestration library interface.
 *
 * Specific implementation may employ different strategies to satisfy the interface operations (e.g.
 * stateful, staged executions, cached resources, pre-allocations, etc).
 */
interface Orchestrator {

    /**
     * Error denoting the condition that a resource creation requisition cannot be satisfied.
     *
     * @param[request]
     *   resource requisition request.
     */
    data class ResourceCreationFailedException(
        val request: Any
    ) : RuntimeException("Failed to create resource")

    /**
     * Compute resource deployment creation request specification.
     *
     * @param[concordModelSpecification]
     *   metadata specification of versioned Concord model template to deploy.
     */
    data class CreateComputeResourceRequest(
        val sessionIdentifier: DeploymentSessionIdentifier,
        val clusterIdentifier: ConcordClusterIdentifier,
        val concordModelSpecification: ConcordModelSpecification
    )

    /**
     * Compute resource deployment deletion request specification.
     *
     * @param[resource]
     *   deployment resource to be deleted.
     */
    data class DeleteComputeResourceRequest(val resource: URI)

    /**
     * Events corresponding to the execution of a deployment session.
     */
    sealed class DeploymentEvent {
        data class Created(val resourceIdentifier: URI) : DeploymentEvent()
        data class Started(val resourceIdentifier: URI) : DeploymentEvent()
        data class Deleted(val resourceIdentifier: URI) : DeploymentEvent()
    }

    /**
     * Network resource provisioning request specification.
     *
     * @param[public]
     *   whether network resource must be externally reachable outside of the orchestration site.
     */
    data class CreateNetworkResourceRequest(val public: Boolean)

    /**
     * Network resource de-provisioning request specification.
     *
     * @param[resource]
     *   network address resource to be de-provisioned.
     */
    data class DeleteNetworkResourceRequest(val resource: URI)

    /**
     * Events corresponding to the execution of a network address requisition workflow.
     */
    sealed class NetworkResourceEvent {
        data class Created(val networkIdentifier: URI) : NetworkResourceEvent()
        data class Deleted(val networkIdentifier: URI) : NetworkResourceEvent()
    }

    /**
     * Allocation request to assign a network resource to a given deployment.
     *
     * @param[resourceIdentifier]
     *   deployment unit to assign network resource to.
     * @param[networkIdentifier]
     *   network resource to be assigned.
     */
    data class NetworkAllocationRequest(val resourceIdentifier: URI, val networkIdentifier: URI)

    /**
     * Events corresponding to the execution of a network allocation workflow.
     */
    sealed class NetworkAllocationEvent {
        object Created : NetworkAllocationEvent()
        object Deleted : NetworkAllocationEvent()
    }

    /**
     * Shutdown the [Orchestrator] instance and closes all resources.
     */
    fun close()

    /**
     * Create a Concord deployment based on a given [CreateComputeResourceRequest].
     *
     * @param[request]
     *   deployment creation request specification.
     *
     * @return
     *   a [Publisher] of [DeploymentEvent] corresponding to side-effects engendered by the request.
     */
    fun createDeployment(request: CreateComputeResourceRequest): Publisher<DeploymentEvent>

    /**
     * Delete a Concord deployment based on a given [DeleteComputeResourceRequest].
     *
     * @param[request]
     *   deployment deletion request specification.
     *
     * @return
     *   a [Publisher] of [DeploymentEvent] corresponding to side-effects engendered by the request.
     */
    fun deleteDeployment(request: DeleteComputeResourceRequest): Publisher<DeploymentEvent>

    /**
     * Create a reachable network address based on a given [CreateNetworkResourceRequest].
     *
     * @param[request]
     *   network address creation request specification.
     *
     * @return
     *   a [Publisher] of [NetworkResourceEvent] corresponding to side-effects engendered by the
     *   request.
     */
    fun createNetworkAddress(request: CreateNetworkResourceRequest): Publisher<NetworkResourceEvent>

    /**
     * Delete a reachable network address based on a given [DeleteNetworkResourceRequest].
     *
     * @param[request]
     *   network address creation request specification.
     *
     * @return
     *   a [Publisher] of [NetworkResourceEvent] corresponding to side-effects engendered by the
     *   request.
     */
    fun deleteNetworkAddress(request: DeleteNetworkResourceRequest): Publisher<NetworkResourceEvent>

    /**
     * Allocate a network address to a deployed compute resource.
     *
     * @param[request]
     *   network address allocation request specification.
     *
     * @return
     *   a [Publisher] of [NetworkAllocationEvent] corresponding to side-effects engendered by the
     *   request.
     */
    fun createNetworkAllocation(request: NetworkAllocationRequest): Publisher<NetworkAllocationEvent>

    /**
     * Deallocate a network address to a deployed compute resource.
     *
     * @param[request]
     *   network address allocation request specification.
     *
     * @return
     *   a [Publisher] of [NetworkAllocationEvent] corresponding to side-effects engendered by the
     *   request.
     */
    fun deleteNetworkAllocation(request: NetworkAllocationRequest): Publisher<NetworkAllocationEvent>
}
