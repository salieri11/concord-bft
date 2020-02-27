/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.orchestration

import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.reactive.ErrorPublisher
import com.vmware.blockchain.deployment.reactive.IteratingPublisher
import com.vmware.blockchain.deployment.reactive.Publisher
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.ethereum.type.Genesis

/** Default orchestrator operation short timeout value. */
const val ORCHESTRATOR_SHORT_TIMEOUT_MILLIS = 10000L

/** Default orchestrator operation normal timeout value. */
const val ORCHESTRATOR_TIMEOUT_MILLIS = 60000L * 10

/** Default orchestrator operation long timeout value. */
const val ORCHESTRATOR_LONG_TIMEOUT_MILLIS = 60000L * 10

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
     * Error denoting the condition that a resource deletion requisition cannot be satisfied.
     *
     * @param[request]
     *   resource requisition request.
     */
    data class ResourceDeletionFailedException(
        val request: Any
    ) : RuntimeException("Failed to delete resource")

    /**
     * Error denoting the condition that an operation was not completed before timeout occurred.
     *
     * @param[timeout]
     *   deadline value (time unit not specified).
     */
    data class OperationTimeoutException(
        val timeout: Any
    ) : RuntimeException("Failed to complete the operation in time")

    /**
     * Compute resource deployment creation request specification.
     *
     * @param[cluster]
     *   identifier of the cluster the deployed resource belongs to.
     * @param[node]
     *   identifier of the member node.
     * @param[model]
     *   metadata specification of versioned Concord model template to deploy.
     * @param[genesis]
     *   common genesis block information to be deployed on the resource.
     * @param[privateNetworkAddress]
     *   network address to be statically assigned on the compute resource.
     */
    data class CreateComputeResourceRequest(
        val cluster: ConcordClusterIdentifier,
        val node: ConcordNodeIdentifier,
        val model: ConcordModelSpecification,
        val genesis: Genesis,
        val privateNetworkAddress: String = "",
        val configurationSessionIdentifier: ConfigurationSessionIdentifier,
        val concordId: Int,
        val configServiceEndpoint: Endpoint,
        val configServiceRestEndpoint: Endpoint,
        val vmProfile: String = "small"
    )

    /**
     * Compute resource deployment deletion request specification.
     *
     * @param[resource]
     *   deployment resource to be deleted.
     */
    data class DeleteComputeResourceRequest(val resource: URI)

    /**
     * Generic interface type denoting a deployment event.
     */
    interface OrchestrationEvent

    /**
     * Events corresponding to the execution of a deployment session.
     */
    sealed class ComputeResourceEvent : OrchestrationEvent {
        abstract val resource: URI

        data class Created(
            override val resource: URI,
            val node: ConcordNodeIdentifier
        ) : ComputeResourceEvent()
        data class Started(override val resource: URI) : ComputeResourceEvent()
        data class Deleted(override val resource: URI) : ComputeResourceEvent()
    }

    /**
     * Network resource provisioning request specification.
     *
     * @param[name]
     *   name of the network resource to create, for [Orchestrator] backends that associate network
     *   resource with additional naming identifiers or surrogate identifiers to each provisioned
     *   network resource.
     * @param[public]
     *   whether network resource must be externally reachable outside of the orchestration site.
     */
    data class CreateNetworkResourceRequest(
        val name: String,
        val public: Boolean
    )

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
    sealed class NetworkResourceEvent : OrchestrationEvent {
        abstract val resource: URI

        data class Created(
            override val resource: URI,
            val name: String,
            val address: String,
            val public: Boolean
        ) : NetworkResourceEvent()
        data class Deleted(override val resource: URI) : NetworkResourceEvent()
    }

    /**
     * Allocation request to assign a network resource to a given deployment.
     *
     * @param[name]
     *   name of the network allocation to create, for [Orchestrator] backends that associate
     *   network allocation with additional naming identifiers or surrogate identifiers to each
     *   provisioned network allocation.
     * @param[compute]
     *   compute resource to allocate network resource to.
     * @param[publicNetwork]
     *   network resource to be assigned.
     * @param[privateNetwork]
     *   network resource to be assigned.
     */
    data class CreateNetworkAllocationRequest(
        val name: String,
        val compute: URI,
        val publicNetwork: URI,
        val privateNetwork: URI
    )

    /**
     * Network allocation de-provisioning request specification.
     *
     * @param[resource]
     *   network allocation resource to be de-provisioned.
     */
    data class DeleteNetworkAllocationRequest(val resource: URI)

    /**
     * Events corresponding to the execution of a network allocation workflow.
     */
    sealed class NetworkAllocationEvent : OrchestrationEvent {
        abstract val resource: URI

        data class Created(
            override val resource: URI,
            val name: String,
            val compute: URI,
            val publicNetwork: URI,
            val privateNetwork: URI
        ) : NetworkAllocationEvent()
        data class Deleted(override val resource: URI) : NetworkAllocationEvent()
    }

    /**
     * Initialize the [Orchestrator] instance.
     *
     * Note: Specific [Orchestrator] implementation may choose to emit 0 or more `onNext()` signals.
     * The only contract between an [Orchestrator] and a calling subscriber of this method is that
     * either `onComplete()` or `onError()` will be signaled eventually to mark the completion of
     * the initialization process.
     *
     * @return
     *   a [Publisher] detailing the success or failure of the asynchronous initialization.
     */
    fun initialize(): Publisher<Any> = IteratingPublisher(emptyList())

    /**
     * Test connectivity/sanity of the orchestration site.
     *
     * @return
     *   a [Publisher] of [Boolean] corresponding to the result of validating the orchestration site
     *   associated with this [Orchestrator] instance.
     */
    fun validate(): Publisher<Boolean> {
        return ErrorPublisher(UnsupportedOperationException("Validation operation is unsupported"))
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
     *   a [Publisher] of [ComputeResourceEvent] corresponding to side-effects engendered by the
     *   request.
     */
    fun createDeployment(request: CreateComputeResourceRequest): Publisher<ComputeResourceEvent>

    /**
     * Delete a Concord deployment based on a given [DeleteComputeResourceRequest].
     *
     * @param[request]
     *   deployment deletion request specification.
     *
     * @return
     *   a [Publisher] of [ComputeResourceEvent] corresponding to side-effects engendered by the
     *   request.
     */
    fun deleteDeployment(request: DeleteComputeResourceRequest): Publisher<ComputeResourceEvent>

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
    fun createNetworkAllocation(
        request: CreateNetworkAllocationRequest
    ): Publisher<NetworkAllocationEvent>

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
    fun deleteNetworkAllocation(
        request: DeleteNetworkAllocationRequest
    ): Publisher<NetworkAllocationEvent>
}
