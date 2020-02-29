/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.model.nsx.NatRule
import com.vmware.blockchain.deployment.model.nsx.PublicIP
import com.vmware.blockchain.deployment.orchestration.ORCHESTRATOR_TIMEOUT_MILLIS
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import com.vmware.blockchain.deployment.reactive.Publisher
import com.vmware.blockchain.deployment.v1.Address
import com.vmware.blockchain.deployment.v1.AllocateAddressRequest
import com.vmware.blockchain.deployment.v1.AllocateAddressResponse
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc.IPAllocationServiceStub
import com.vmware.blockchain.deployment.v1.IPAllocationServiceGrpc.newStub
import com.vmware.blockchain.deployment.v1.IPv4Network
import com.vmware.blockchain.deployment.v1.MessageHeader
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo
import com.vmware.blockchain.deployment.v1.ReleaseAddressRequest
import com.vmware.blockchain.deployment.v1.ReleaseAddressResponse
import com.vmware.blockchain.deployment.v1.TransportSecurity
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo
import com.vmware.blockchain.deployment.vm.CloudInitConfiguration
import com.vmware.blockchain.deployment.vsphere.ChannelStreamObserver
import com.vmware.blockchain.deployment.vsphere.VSphereClient
import com.vmware.blockchain.deployment.vsphere.VSphereHttpClient
import com.vmware.blockchain.deployment.vsphere.VSphereModelSerializer
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
 * Deployment orchestration driver for VMware Cloud [orchestration site type]
 * [OrchestrationSiteInfo.Type.VMC].
 *
 * @property[info]
 *   environment context describing the target orchestration site environment.
 * @property[vmc]
 *   VMware Cloud API client handle.
 * @property[context]
 *   coroutine execution context for coroutines launched by this instance.
 */
class VmcOrchestrator(
    private val info: VmcOrchestrationSiteInfo,
    private val vmc: VmcClient,
    private val context: CoroutineContext = Dispatchers.Default
) : Orchestrator, CoroutineScope {

    /** vSphere Client to utilize to exact orchestration actions. */
    private lateinit var vSphere: VSphereClient

    /** vSphere Client to utilize to exact orchestration actions. */
    private lateinit var nsx: VmcClient

    /**
     * Mappings of [IPv4Network] name to service's client stub. (name corresponds to the specified
     * network names in `vsphere` property within [info].
     */
    private lateinit var networkAddressAllocationServers: Map<String, IPAllocationServiceStub>

    /**
     * Global singleton companion to [VmcOrchestrator] that also provides a global-scoped process-
     * wide [CoroutineScope] to launch [VmcOrchestrator]-related coroutines.
     */
    companion object {
        /** Default IPAM resource name prefix. */
        const val IPAM_RESOURCE_NAME_PREFIX = "blocks/"
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
            vmc.getDataCenterInfo()?.apply {
                // Use VMC SDDC info to create NSX client.

                nsx = VmcHttpClient.Context(
                        endpoint = URI(resource_config.nsx_api_public_endpoint_url),
                        authenticationEndpoint = URI.create(info.authentication.address),
                        refreshToken = info.authentication.credential.tokenCredential.token,
                        organization = info.organization,
                        datacenter = info.datacenter
                ).let { VmcClient(VmcHttpClient(it, VmcModelSerializer)) }

                // Use VMC SDDC info to create vSphere client.
                vSphere = VSphereHttpClient.Context(
                        endpoint = URI(resource_config.vc_url),
                        username = resource_config.cloud_username,
                        password = resource_config.cloud_password
                ).let { VSphereClient(VSphereHttpClient(it, VSphereModelSerializer)) }

                // There is only the default network right now. When more networks are introduced
                // (to be allocated for provisioned nodes as network interfaces), the mapping will
                // contain more than one entry.
                networkAddressAllocationServers = mapOf(
                        info.vsphere.network.let {
                            it.name to newStub(newClientRpcChannel(it.allocationServer))
                        }
                )
            }
        }
    }

    override fun close() = job.cancel()

    override fun createDeployment(
        request: Orchestrator.CreateComputeResourceRequest
    ): Publisher<Orchestrator.ComputeResourceEvent> {
        val compute = info.vsphere.resourcePool
        val storage = info.vsphere.datastore
        val network = info.vsphere.network

        @Suppress("DuplicatedCode")
        return publish {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val clusterId = UUID(request.cluster.high, request.cluster.low)
                    val nodeId = UUID(request.node.high, request.node.low)

                    val getFolder = async { vSphere.getFolder(name = info.vsphere.folder) }
                    val getDatastore = async { vSphere.getDatastore(name = storage) }
                    val getResourcePool = async { vSphere.getResourcePool(name = compute) }
                    val getControlNetwork = async { vSphere.getNetwork(network.name) }
                    val getLibraryItem = async { vSphere.getLibraryItem(request.model.template) }

                    // Collect all information and deploy.
                    val folder = getFolder.await()
                    val datastore = getDatastore.await()
                    val resourcePool = getResourcePool.await()
                    val controlNetwork = requireNotNull(getControlNetwork.await())
                    val libraryItem = getLibraryItem.await()
                    val instance = vSphere.createVirtualMachine(
                            name = "$clusterId-$nodeId",
                            libraryItem = requireNotNull(libraryItem),
                            datastore = requireNotNull(datastore),
                            resourcePool = requireNotNull(resourcePool),
                            folder = requireNotNull(folder),
                            networks = listOf(
                                    "blockchain-network" to controlNetwork
                            ),
                            cloudInit = CloudInitConfiguration(
                                    info.containerRegistry,
                                    request.model,
                                    request.privateNetworkAddress,
                                    network.gateway.toIPv4Address(),
                                    network.nameServersList,
                                    network.subnet,
                                    request.cluster,
                                    request.node,
                                    request.concordId,
                                    request.configurationSessionIdentifier,
                                    request.configServiceEndpoint,
                                    request.configServiceRestEndpoint,
                                    OutboundProxyInfo.newBuilder().build(),
                                    info.logManagementsList,
                                    info.wavefront,
                                    request.consortium
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
        return publish<Orchestrator.ComputeResourceEvent> {
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
        val network = info.vsphere.network
        return publish<Orchestrator.NetworkResourceEvent> {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val privateIpAddress = allocatedPrivateIP(network)
                    privateIpAddress
                            ?.apply {
                                log.info { "Created private IP($privateIpAddress)" }

                                send(Orchestrator.NetworkResourceEvent.Created(
                                        URI.create("//${network.allocationServer.address}/$name"),
                                        request.name,
                                        privateIpAddress.value.toIPv4Address(),
                                        false))
                            }
                            ?: close(Orchestrator.ResourceCreationFailedException(request))

                    val publicIpAddress = nsx.createPublicIP(request.name)
                    publicIpAddress?.takeIf { it.ip != null }
                            ?.apply {
                                log.info { "Created public IP(${publicIpAddress.ip})" }

                                send(Orchestrator.NetworkResourceEvent.Created(
                                        this.toNetworkResource(),
                                        request.name,
                                        ip!!,
                                        true))
                                close()
                            }
                            ?: close(Orchestrator.ResourceCreationFailedException(request))

                } catch (error: CancellationException) {
                    log.info { "Network address creation request was cancelled" }

                    close(Orchestrator.ResourceCreationFailedException(request))
                } catch (error: Throwable) {
                    log.error("Unexpected error(${error.message})", error)

                    close(Orchestrator.ResourceCreationFailedException(request))
                }
            }
        }
    }

    override fun deleteNetworkAddress(
        request: Orchestrator.DeleteNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        val network = info.vsphere.network

        return publish<Orchestrator.NetworkResourceEvent> {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    // FIXME:
                    //   The orchestrator should group all the resource providers via its authority.
                    //   During resource event emission, the authority
                    //   (i.e. <scheme>://<authority>/...)
                    //   should determine "how" and "who" to contact to release the resource, be it
                    //   VC, nsx, or IPAM.
                    if (request.resource.scheme == null) {
                        releasePrivateIP(network, request.resource).takeIf { it }
                                ?.run {
                                    send(Orchestrator.NetworkResourceEvent.Deleted(request.resource))
                                }
                                ?: close(Orchestrator.ResourceDeletionFailedException(request))
                    } else {
                        // Issue DELETE against the IP resource via its URI.
                        nsx.deleteResource(request.resource).takeIf { it }
                                ?.run {
                                    send(Orchestrator.NetworkResourceEvent.Deleted(request.resource))
                                }
                                ?: close(Orchestrator.ResourceDeletionFailedException(request))
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
        return publish<Orchestrator.NetworkAllocationEvent> {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val privateIP: String? = request.privateNetwork.path.substringAfterLast("/")
                            .toInt(16)
                            .toIPv4Address()

                    // Obtain the public IP address from the IP resource.
                    val networkId = request.publicNetwork.path.substringAfterLast("/")
                    val publicIP = nsx.getPublicIP(networkId)?.ip

                    // Setup the NAT rule.
                    nsx.createNatRule(name = request.name,
                                  action = NatRule.Action.REFLEXIVE,
                                  sourceNetwork = checkNotNull(privateIP),
                                  translatedNetwork = checkNotNull(publicIP))
                            ?.toNetworkAllocationResource()
                            ?.apply {
                                send(Orchestrator.NetworkAllocationEvent.Created(
                                        resource = this,
                                        name = request.name,
                                        compute = request.compute,
                                        publicNetwork = request.publicNetwork,
                                        privateNetwork = request.privateNetwork
                                ))
                            }
                            ?: close(Orchestrator.ResourceCreationFailedException(request))
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceCreationFailedException(request))
                }
            }
        }
    }

    override fun deleteNetworkAllocation(
        request: Orchestrator.DeleteNetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return publish<Orchestrator.NetworkAllocationEvent> {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    nsx.deleteResource(request.resource).takeIf { it }
                            ?.run {
                                send(Orchestrator.NetworkAllocationEvent.Deleted(request.resource))
                            }
                            ?: close(Orchestrator.ResourceDeletionFailedException(request))
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceDeletionFailedException(request))
                }
            }
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
     * Represent the [PublicIP] as a network resource [URI], as known by this [Orchestrator].
     *
     * @return
     *   network resource as a [URI] instance.
     */
    private fun PublicIP.toNetworkResource(): URI {
        return nsx.resolvePublicIPIdentifier(checkNotNull(id))
    }

    /**
     * Represent the [NatRule] as an allocation resource [URI], as known by this [Orchestrator].
     *
     * @return
     *   allocation resource as a [URI] instance.
     */
    private fun NatRule.toNetworkAllocationResource(): URI {
        return nsx.resolveNsxResourceIdentifier(checkNotNull(path))
    }

    /**
     * Allocate a new private IP address for use from IP allocation service.
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
                .setParent(IPAM_RESOURCE_NAME_PREFIX + info.datacenter + "-" + network.name).build()

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
        val requestAllocateAddress = ReleaseAddressRequest.newBuilder().setHeader(MessageHeader.newBuilder().build())
                .setName(resource.path.removePrefix("/")).build()

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
