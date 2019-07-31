/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vsphere

import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.Address
import com.vmware.blockchain.deployment.model.AllocateAddressRequest
import com.vmware.blockchain.deployment.model.AllocateAddressResponse
import com.vmware.blockchain.deployment.model.IPAllocationServiceStub
import com.vmware.blockchain.deployment.model.IPv4Network
import com.vmware.blockchain.deployment.model.MessageHeader
import com.vmware.blockchain.deployment.model.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.model.VSphereOrchestrationSiteInfo
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import com.vmware.blockchain.deployment.reactive.Publisher
import com.vmware.blockchain.deployment.vm.CloudInitConfiguration
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import io.grpc.CallOptions
import io.grpc.ManagedChannelBuilder
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.reactive.publish
import kotlinx.coroutines.withTimeout
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext

/**
 * Deployment orchestration driver for VMware Cloud [orchestration site type]
 * [OrchestrationSiteInfo.Type.VSPHERE].
 *
 * @param[vSphere]
 *   vSphere API client handle.
 */
open class VSphereOrchestrator private constructor(
    private val info: VSphereOrchestrationSiteInfo,
    private val vSphere: VSphereClient,
    private val context: CoroutineContext = Dispatchers.Default
) : Orchestrator, CoroutineScope {

    companion object : CoroutineScope {

        /** [CoroutineContext] to launch all coroutines associated with this singleton object. */
        override val coroutineContext: CoroutineContext
            get() = EmptyCoroutineContext

        /** Default maximum orchestrator operation timeout value. */
        const val ORCHESTRATOR_TIMEOUT_MILLIS = 60000L * 10

        /** Default IPAM resource name prefix. */
        const val IPAM_RESOURCE_NAME_PREFIX = "blocks/"

        /** Blockchain Network name within deployment template in `ConcordModelSpecification`. */
        const val BLOCKCHAIN_NETWORK_NAME = "blockchain-network"

        /**
         * Create a new [VSphereOrchestrator] based on parameters from a given [OrchestrationSiteInfo].
         *
         * @param[site]
         *   orchestration information pertaining to a VMC-based orchestration site.
         *
         * @return
         *   a [VSphereOrchestrator] instance corresponding to the given input parameter.
         */
        @JvmStatic
        fun newOrchestrator(site: OrchestrationSiteInfo): VSphereOrchestrator {
            // Precondition.
            require(site.type == OrchestrationSiteInfo.Type.VSPHERE)
            val info = requireNotNull(site.vsphere)

            // Create new vSphere client.
            val context = VSphereHttpClient.Context(
                    endpoint = URI.create(info.api.address),
                    username = info.api.credential.passwordCredential.username,
                    password = info.api.credential.passwordCredential.password
            )
            val vSphereClient = VSphereClient(
                    VSphereHttpClient(
                            context,
                            VSphereModelSerializer,
                            allowInsecureConnection = true
                    )
            )

            return VSphereOrchestrator(info, vSphereClient)
        }
    }

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    override fun close() {
        job.cancel()
    }

    override fun createDeployment(
        request: Orchestrator.CreateComputeResourceRequest
    ): Publisher<Orchestrator.ComputeResourceEvent> {
        // Resolve the configuration mapping to find the compute network intended for blockchain.
        val network = requireNotNull(info.network)

        return publish(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val clusterId = UUID(request.cluster.high, request.cluster.low)
                    val nodeId = UUID(request.node.high, request.node.low)

                    val getFolder = async { vSphere.getFolder(name = info.folder) }
                    val getDatastore = async { vSphere.getDatastore(name = info.datastore) }
                    val getResourcePool = async { vSphere.getResourcePool(name = info.resourcePool) }
                    val getNetwork = async { vSphere.getNetwork(network.name, type = "") }
                    val getLibraryItem = async { vSphere.getLibraryItem(request.model.template) }

                    // Collect all information and deploy.
                    val folder = requireNotNull(getFolder.await())
                    val datastore = requireNotNull(getDatastore.await())
                    val resourcePool = requireNotNull(getResourcePool.await())
                    val controlNetwork = requireNotNull(getNetwork.await())
                    val libraryItem = requireNotNull(getLibraryItem.await())
                    val instance = vSphere.createVirtualMachine(
                            name = "$clusterId-$nodeId",
                            libraryItem = libraryItem,
                            datastore = datastore,
                            resourcePool = resourcePool,
                            folder = folder,
                            networks = listOf(
                                    BLOCKCHAIN_NETWORK_NAME to controlNetwork
                            ),
                            cloudInit = CloudInitConfiguration(
                                    info.containerRegistry,
                                    request.model,
                                    request.genesis,
                                    request.privateNetworkAddress,
                                    network.gateway.toIPv4Address(),
                                    network.subnet,
                                    request.cluster,
                                    request.concordId,
                                    request.configurationSessionIdentifier,
                                    request.configServiceEndpoint
                            )
                    )

                    // 1. If instance is created, send the created event signal.
                    // 2. Then start the instance.
                    // 3. If instance is started, send the started event signal.
                    // 4. If anything failed, send error signal with the request as the argument.
                    instance?.toComputeResource()
                            ?.apply {
                                send(Orchestrator.ComputeResourceEvent.Created(this, request.node))
                            }
                            ?.takeIf { vSphere.ensureVirtualMachinePowerStart(instance) }
                            ?.apply { send(Orchestrator.ComputeResourceEvent.Started(this)) }
                            ?: close(Orchestrator.ResourceCreationFailedException(request))
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceCreationFailedException(request))
                }
            }
        }
    }

    override fun deleteDeployment(
        request: Orchestrator.DeleteComputeResourceRequest
    ): Publisher<Orchestrator.ComputeResourceEvent> {
        return publish<Orchestrator.ComputeResourceEvent>(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    // Retrieve only the last portion of the URI to get the VM ID.
                    val id = request.resource.path.substringAfterLast("/")

                    // Ensure that the VM is powered off.
                    vSphere.ensureVirtualMachinePowerStop(id).takeIf { it }
                            ?.run { vSphere.deleteVirtualMachine(id) }?.takeIf { it }
                            ?.run {
                                send(Orchestrator.ComputeResourceEvent.Deleted(request.resource))
                            }
                            ?: close(Orchestrator.ResourceDeletionFailedException(request))
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceDeletionFailedException(request))
                }
            }
        }
    }

    override fun createNetworkAddress(
        request: Orchestrator.CreateNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        return publish<Orchestrator.NetworkResourceEvent>(coroutineContext) {
            val network = requireNotNull(info.network)

            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                // send(Orchestrator.NetworkResourceEvent.Created(
                //         URI.create(network.allocator.address + "/" + "FIXED"),
                //         request.name,
                //         "127.0.0.1",
                //         false
                // ))
                val privateIpAddress = allocatedPrivateIP(network)
                privateIpAddress
                        ?.apply {
                            send(Orchestrator.NetworkResourceEvent.Created(
                                    URI.create("${network.allocationServer.address}/$name"),
                                    request.name,
                                    privateIpAddress.value.toIPv4Address(),
                                    false))
                        }
                        ?: close(Orchestrator.ResourceCreationFailedException(request))

                log.info { "Assigned private IP($privateIpAddress)" }
            }
        }
    }

    override fun deleteNetworkAddress(
        request: Orchestrator.DeleteNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        return publish(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
            }
        }
    }

    override fun createNetworkAllocation(
        request: Orchestrator.CreateNetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return publish(coroutineContext) {}
    }

    override fun deleteNetworkAllocation(
        request: Orchestrator.DeleteNetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return publish(coroutineContext) {}
    }

    /**
     * Represent the [String] as a compute resource, as known by this [Orchestrator].
     *
     * @return
     *   compute resource as a [URI] instance.
     */
    private fun String.toComputeResource(): URI {
        return vSphere.resolveVirtualMachineIdentifier(this)
    }

    /**
     * Allocate a new private IP address for use from IP allocation service.
     *
     * @return
     *   allocated IP address resource as a instance of [Address], if created, `null` otherwise.
     */
    private suspend fun allocatedPrivateIP(network: IPv4Network): Address? {
        val requestAllocateAddress = AllocateAddressRequest(
                header = MessageHeader(),
                parent = IPAM_RESOURCE_NAME_PREFIX + network.name
        )

        val observer = ChannelStreamObserver<AllocateAddressResponse>(1)

        // IP allocation service client.
        val ipAllocation = IPAllocationServiceStub(
                ManagedChannelBuilder.forTarget(network.allocationServer.address)
                        .usePlaintext()
                        .build(),
                CallOptions.DEFAULT
        )
        ipAllocation.allocateAddress(requestAllocateAddress, observer)
        val response = observer.asReceiveChannel().receive()

        return response
                .takeIf { it.status == AllocateAddressResponse.Status.OK }
                ?.let { it.address }
    }
}