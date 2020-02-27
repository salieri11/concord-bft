/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vsphere

import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.orchestration.ORCHESTRATOR_LONG_TIMEOUT_MILLIS
import com.vmware.blockchain.deployment.orchestration.ORCHESTRATOR_SHORT_TIMEOUT_MILLIS
import com.vmware.blockchain.deployment.orchestration.ORCHESTRATOR_TIMEOUT_MILLIS
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import com.vmware.blockchain.deployment.reactive.Publisher
import com.vmware.blockchain.deployment.v1.Address
import com.vmware.blockchain.deployment.v1.AllocateAddressRequest
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc.IPAllocationServiceStub
import com.vmware.blockchain.deployment.v1.IPv4Network
import com.vmware.blockchain.deployment.v1.MessageHeader
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse
import com.vmware.blockchain.deployment.v1.TransportSecurity
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo
import com.vmware.blockchain.deployment.vm.CloudInitConfiguration
import io.grpc.ManagedChannel
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.reactive.publish
import kotlinx.coroutines.withTimeout
import java.util.concurrent.Executor
import java.util.concurrent.ForkJoinPool
import kotlin.coroutines.CoroutineContext

/**
 * Deployment orchestration driver for VMware vSphere environment.
 *
 * @property[info]
 *   environment context describing the target orchestration site environment.
 * @property[vSphere]
 *   vSphere client handle.
 * @property[context]
 *   coroutine execution context for coroutines launched by this instance.
 */
class VSphereOrchestrator constructor(
    private val info: VSphereOrchestrationSiteInfo,
    private val vSphere: VSphereClient,
    private val context: CoroutineContext = Dispatchers.Default
) : Orchestrator, CoroutineScope {

    /**
     * Mappings of [IPv4Network] name to service's client stub. (name corresponds to the specified
     * network names in `vsphere` property within [info].
     */
    private lateinit var networkAddressAllocationServers: Map<String, IPAllocationServiceGrpc.IPAllocationServiceStub>

    companion object {
        /** Default IPAM resource name prefix. */
        const val IPAM_RESOURCE_NAME_PREFIX = "blocks/"

        /** Blockchain Network name within deployment template in `ConcordModelSpecification`. */
        const val BLOCKCHAIN_NETWORK_NAME = "blockchain-network"
    }

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    override fun initialize(): Publisher<Any> {
        return publish {
            // There is only the default network right now. When more networks are introduced
            // (to be allocated for provisioned nodes as network interfaces), the mapping will
            // contain more than one entry.
            networkAddressAllocationServers = mapOf(
                    info.vsphere.network.let {
                        it.name to IPAllocationServiceGrpc.newStub(newClientRpcChannel(it.allocationServer))
                    }
            )
        }
    }

    override fun close() = job.cancel()

    override fun validate(): Publisher<Boolean> {
        return publish {
            try {
                withTimeout(ORCHESTRATOR_SHORT_TIMEOUT_MILLIS) {
                    val folder = vSphere.getFolder(name = info.vsphere.folder) != null
                    val compute = vSphere.getResourcePool(name = info.vsphere.resourcePool) != null

                    // Validation result is the conjunction of all validation actions taken.
                    (folder && compute).apply { send(this) }
                }
            } catch (error: Throwable) {
                close(error)
            }
        }
    }

    override fun createDeployment(
        request: Orchestrator.CreateComputeResourceRequest
    ): Publisher<Orchestrator.ComputeResourceEvent> {
        val compute = info.vsphere.resourcePool
        val storage = info.vsphere.datastore
        val network = info.vsphere.network

        @Suppress("DuplicatedCode")
        return publish {
            withTimeout(ORCHESTRATOR_LONG_TIMEOUT_MILLIS) {
                try {
                    val clusterId = UUID(request.cluster.high, request.cluster.low)
                    val nodeId = UUID(request.node.high, request.node.low)

                    val getFolder = async { vSphere.getFolder(name = info.vsphere.folder) }
                    val getDatastore = async { vSphere.getDatastore(name = storage) }
                    val getResourcePool = async { vSphere.getResourcePool(name = compute) }
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
                                    request.privateNetworkAddress,
                                    network.gateway.toIPv4Address(),
                                    network.nameServersList,
                                    network.subnet,
                                    request.cluster,
                                    request.concordId,
                                    request.configurationSessionIdentifier,
                                    request.configServiceEndpoint,
                                    request.configServiceRestEndpoint,
                                    info.vsphere.outboundProxy,
                                    info.wavefront
                            ),
                            vmProfile = request.vmProfile
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
        @Suppress("DuplicatedCode")
        return publish {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    // Retrieve only the last portion of the URI to get the VM ID.
                    val id = request.resource.path.substringAfterLast("/")

                    // Ensure that the VM is powered off.
                    vSphere.ensureVirtualMachinePowerStop(id).takeIf { it }
                            ?.run { vSphere.deleteVirtualMachine(id) }?.takeIf { it }
                            ?.run {
                                val event: Orchestrator.ComputeResourceEvent =
                                        Orchestrator.ComputeResourceEvent.Deleted(request.resource)
                                send(event)
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
        val network = info.vsphere.network

        return publish {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                val privateIpAddress = allocatedPrivateIP(network)
                privateIpAddress
                        ?.apply {
                            log.info { "Assigned private IP($privateIpAddress)" }

                            val event = Orchestrator.NetworkResourceEvent.Created(
                                    URI.create("//${network.allocationServer.address}/$name"),
                                    request.name,
                                    privateIpAddress.value.toIPv4Address(),
                                    public = false
                            )
                            send(event as Orchestrator.NetworkResourceEvent)

                            // If request asked for public IP provisioning as well, send an event.
                            if (request.public) {
                                val publicAddressEvent = Orchestrator.NetworkResourceEvent.Created(
                                        URI.create("//${network.allocationServer.address}/$name"),
                                        request.name,
                                        privateIpAddress.value.toIPv4Address(),
                                        public = true
                                )
                                send(publicAddressEvent as Orchestrator.NetworkResourceEvent)
                            }
                        }
                        ?: close(Orchestrator.ResourceCreationFailedException(request))
            }
        }
    }

    override fun deleteNetworkAddress(
        request: Orchestrator.DeleteNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        val network = info.vsphere.network

        return publish {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val released = releasePrivateIP(network, request.resource)
                    if (released) {
                        log.info { "Released private IP resource(${request.resource})" }

                        val event: Orchestrator.NetworkResourceEvent =
                                Orchestrator.NetworkResourceEvent.Deleted(request.resource)
                        send(event)
                    } else {
                        close(Orchestrator.ResourceDeletionFailedException(request))
                    }
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceDeletionFailedException(request))
                }
            }
        }
    }

    override fun createNetworkAllocation(
        request: Orchestrator.CreateNetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return publish {
            val event = Orchestrator.NetworkAllocationEvent.Created(
                    resource = URI.create("///network-allocations/${request.name}"),
                    name = request.name,
                    compute = request.compute,
                    publicNetwork = request.publicNetwork,
                    privateNetwork = request.privateNetwork
            )

            send(event as Orchestrator.NetworkAllocationEvent)
        }
    }

    override fun deleteNetworkAllocation(
        request: Orchestrator.DeleteNetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return publish {
            val event = Orchestrator.NetworkAllocationEvent.Deleted(request.resource)
            send(event as Orchestrator.NetworkAllocationEvent)
        }
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
     * Allocate a new private IP address for use from a given [IPv4Network].
     *
     * @param[network]
     *   network to allocate IP address resource from.
     *
     * @return
     *   allocated IP address resource as a instance of [Address], if created, `null` otherwise.
     */
    @Suppress("DuplicatedCode")
    private suspend fun allocatedPrivateIP(network: IPv4Network): Address? {
        val requestAllocateAddress = AllocateAddressRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setParent(IPAM_RESOURCE_NAME_PREFIX + network.name)
                .build()

        val observer = ChannelStreamObserver<AllocateAddressResponse>(1)
        val ipAllocationService = requireNotNull(networkAddressAllocationServers[network.name])
        ipAllocationService.allocateAddress(requestAllocateAddress, observer)
        val response = observer.asReceiveChannel().receive()

        return response.takeIf { it.status == AllocateAddressResponse.Status.OK }?.address
    }

    /**
     * Release an IP address resource from a given [IPv4Network].
     *
     * @param[network]
     *   network to release IP address resource from.
     * @param[resource]
     *   URI of the resource to be released.
     *
     * @return
     *   `true` if resource is successfully released, `false` otherwise.
     */
    @Suppress("DuplicatedCode")
    private suspend fun releasePrivateIP(network: IPv4Network, resource: URI): Boolean {
        val observer = ChannelStreamObserver<ReleaseAddressResponse>(1)
        val requestAllocateAddress = ReleaseAddressRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setName(resource.path.removePrefix("/"))
                .build()

        // IP allocation service client.
        val ipAllocationService = requireNotNull(networkAddressAllocationServers[network.name])
        ipAllocationService.releaseAddress(requestAllocateAddress, observer)

        return observer.asReceiveChannel().receive().status == ReleaseAddressResponse.Status.OK
    }

    fun newClientRpcChannel(endpoint: Endpoint,
                            executor: Executor = ForkJoinPool.commonPool()): ManagedChannel {
        return NettyChannelBuilder.forTarget(endpoint.address).apply {
            when (endpoint.transportSecurity.type) {
                TransportSecurity.Type.NONE -> usePlaintext()
                TransportSecurity.Type.TLSv1_2 -> {
                    // Trusted certificates (favor local data over URL).
                    val trustedCertificates = when {
                        endpoint.transportSecurity.trustedCertificatesData.isNotEmpty() ->
                            endpoint.transportSecurity.trustedCertificatesData.toByteArray()
                        endpoint.transportSecurity.trustedCertificatesUrl.isNotEmpty() ->
                            URI.create(endpoint.transportSecurity.trustedCertificatesUrl).toURL().readBytes()
                        else -> null
                    }?.inputStream()

                    // Key certificate chain (favor local data over URL).
                    val keyCertificateChain = when {
                        endpoint.transportSecurity.certificateData.isNotEmpty() ->
                            endpoint.transportSecurity.certificateData.toByteArray()
                        endpoint.transportSecurity.certificateUrl.isNotEmpty() ->
                            URI.create(endpoint.transportSecurity.certificateUrl).toURL().readBytes()
                        else -> null
                    }?.inputStream()

                    // Private key (favor local data over URL).
                    val privateKey = when {
                        endpoint.transportSecurity.privateKeyData.isNotEmpty() ->
                            endpoint.transportSecurity.privateKeyData.toByteArray()
                        endpoint.transportSecurity.privateKeyUrl.isNotEmpty() ->
                            URI.create(endpoint.transportSecurity.privateKeyUrl).toURL().readBytes()
                        else -> null
                    }?.inputStream()

                    // Setup SSL context and enable TLS.
                    sslContext(
                            GrpcSslContexts.forClient()
                                    .trustManager(trustedCertificates)
                                    .keyManager(keyCertificateChain, privateKey)
                                    .build()
                    )
                    useTransportSecurity()
                }
            }

            // Set the executor for background task execution.
            executor(executor)
        }.build()
    }
}
