/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.vm.InitScript
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.core.UUID
import com.vmware.blockchain.deployment.model.nsx.NatRule
import com.vmware.blockchain.deployment.model.nsx.PublicIP
import com.vmware.blockchain.deployment.model.nsx.Segment
import com.vmware.blockchain.deployment.model.nsx.SegmentSubnet
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo
import com.vmware.blockchain.deployment.model.vsphere.GetDatastoreResponse
import com.vmware.blockchain.deployment.model.vsphere.GetFolderResponse
import com.vmware.blockchain.deployment.model.vsphere.GetNetworkResponse
import com.vmware.blockchain.deployment.model.vsphere.GetResourcePoolResponse
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeployRequest
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeployResponse
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeploymentSpec
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeploymentTarget
import com.vmware.blockchain.deployment.model.vsphere.NetworkMapping
import com.vmware.blockchain.deployment.model.vsphere.OvfParameter
import com.vmware.blockchain.deployment.model.vsphere.OvfParameterTypes
import com.vmware.blockchain.deployment.model.vsphere.OvfProperty
import com.vmware.blockchain.deployment.model.vmc.Sddc
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachineGuestIdentityInfo
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachineGuestIdentityResponse
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachinePowerResponse
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachinePowerState
import com.vmware.blockchain.deployment.orchestration.Orchestrator
import com.vmware.blockchain.deployment.orchestration.randomSubnet
import com.vmware.blockchain.deployment.orchestration.toIPv4Address
import com.vmware.blockchain.deployment.reactive.Publisher
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.delay
import kotlinx.coroutines.reactive.publish
import kotlinx.coroutines.withTimeout
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext

/**
 * Deployment orchestration driver for VMware Cloud [orchestration site type]
 * [OrchestrationSiteInfo.Type.VMC].
 *
 * @param[vmc]
 *   VMware Cloud API client handle.
 * @param[vSphere]
 *   vSphere API client handle.
 */
class VmcOrchestrator private constructor(
    private val vmc: VmcClient,
    private val vSphere: VSphereClient,
    private val nsx: VmcClient,
    private val context: CoroutineContext = Dispatchers.Default
) : Orchestrator, CoroutineScope {

    /**
     * Global singleton companion to [VmcOrchestrator] that also provides a global-scoped process-
     * wide [CoroutineScope] to launch [VmcOrchestrator]-related coroutines.
     */
    companion object : CoroutineScope {

        /** [CoroutineContext] to launch all coroutines associated with this singleton object. */
        override val coroutineContext: CoroutineContext
            get() = EmptyCoroutineContext

        /** Default internal retry interval for SDDC operations. */
        const val OPERATION_RETRY_INTERVAL_MILLIS = 500L

        /** Default maximum orchestrator operation timeout value. */
        const val ORCHESTRATOR_TIMEOUT_MILLIS = 60000L * 4

        /**
         * Get SDDC information associated with a given [VmcClient].
         *
         * @param[vmc]
         *   VMC API client handle.
         *
         * @return
         *   information pertaining to SDDC, as a [Sddc] instance.
         */
        @JvmStatic
        private suspend fun getDataCenterInfo(vmc: VmcClient): Sddc? {
            return vmc
                    .get<Sddc>(
                            Endpoints.VMC_SDDC
                                    .interpolate(pathVariables = listOf(
                                            "{org}" to vmc.context.organization,
                                            "{sddc}" to vmc.context.datacenter)
                                    ),
                            contentType = "application/json",
                            headers = emptyList()
                    )
                    .takeIf { it.statusCode() == 200 }
                    ?.let { it.body() }
        }

        /**
         * Create a new [VmcOrchestrator] based on parameters from a given [OrchestrationSiteInfo].
         *
         * @param[site]
         *   orchestration information pertaining to a VMC-based orchestration site.
         * @param[executionContext]
         *   execution context for asynchronous executions triggered by this operation.
         *
         * @return
         *   a [VmcOrchestrator] instance corresponding to the given input parameter.
         */
        @JvmStatic
        fun newOrchestrator(
            site: OrchestrationSiteInfo,
            executionContext: CoroutineContext = Dispatchers.Default
        ): Publisher<VmcOrchestrator> {
            // Precondition.
            require(site.type == OrchestrationSiteInfo.Type.VMC)

            val info = requireNotNull(site.vmc)
            val token = requireNotNull(info.authentication.credential.tokenCredential).token

            // Create new VMC client.
            val vmcContext = VmcClient.Context(
                    endpoint = info.api.address,
                    authenticationEndpoint = info.authentication.address,
                    refreshToken = token,
                    organization = info.organization,
                    datacenter = info.datacenter
            )
            val vmc = VmcClient(vmcContext, ModelSerializer)

            // Asynchronously create the orchestrator instance based on VMC operation results.
            return publish(executionContext) {
                // Use VMC client to obtain VMC SDDC information.
                getDataCenterInfo(vmc)?.apply {
                    // Use VMC SDDC info to create NSX client.
                    val nsxUrl = "${resource_config.nsx_api_public_endpoint_url}/sks-nsxt-manager"
                    val nsx = VmcClient.Context(
                            endpoint = URI(nsxUrl),
                            authenticationEndpoint = info.authentication.address,
                            refreshToken = token,
                            organization = info.organization,
                            datacenter = info.datacenter
                    ).let { VmcClient(it, ModelSerializer) } // Proxied NSX client.

                    // Use VMC SDDC info to create vSphere client.
                    val vsphere = VSphereClient.Context(
                            endpoint = URI(resource_config.vc_url),
                            username = resource_config.cloud_username,
                            password = resource_config.cloud_password
                    ).let { VSphereClient(it, ModelSerializer) } // vSphere client.

                    // New Orchestrator instance.
                    send(VmcOrchestrator(vmc, vsphere, nsx, executionContext))
                }?: close(RuntimeException("Cannot retrieve site authorization information"))
            }
        }
    }

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
        return publish(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val clusterId = UUID(request.cluster.high, request.cluster.low)
                    val nodeId = UUID(request.node.high, request.node.low)

                    val getFolder = async { getFolder() }
                    val getDatastore = async { getDatastore() }
                    val getResourcePool = async { getResourcePool() }
                    val ensureControlNetwork = async {
                        ensureLogicalNetwork("cgw", "blockchain-control", 0x0A010000, 16, 16)
                    }
                    val ensureReplicaNetwork = async {
                        ensureLogicalNetwork("cgw", "blockchain-data", 0x0AFF0000, 16)
                    }
                    val getLibraryItem = async { getLibraryItem(request.model.template) }

                    // Collect all information and deploy.
                    val folder = getFolder.await()
                    val datastore = getDatastore.await()
                    val resourcePool = getResourcePool.await()
                    val controlNetwork = ensureControlNetwork.await()
                    val dataNetwork = ensureReplicaNetwork.await()
                    val libraryItem = getLibraryItem.await()
                    val instance = createVirtualMachine(
                            name = "$clusterId-$nodeId",
                            libraryItem = requireNotNull(libraryItem),
                            datastore = requireNotNull(datastore),
                            resourcePool = requireNotNull(resourcePool),
                            folder = requireNotNull(folder),
                            controlNetwork = controlNetwork,
                            dataNetwork = dataNetwork,
                            initScript = InitScript(request.model, request.configuration)
                    )

                    // 1. If instance is created, send the created event signal.
                    // 2. Then start the instance.
                    // 3. If instance is started, send the started event signal.
                    // 4. If anything failed, send error signal with the request as the argument.
                    instance?.toComputeResource()
                            ?.apply {
                                send(Orchestrator.ComputeResourceEvent.Created(this, request.node))
                            }
                            ?.takeIf { ensureVirtualMachinePowerStart(instance) }
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
                    ensureVirtualMachinePowerStop(id)
                            .takeIf { it }
                            ?.run {
                                // Issue DELETE against the VM resource via its URI.
                                vSphere.delete<Unit>(
                                        request.resource.toString(),
                                        contentType = "application/json",
                                        headers = emptyList()
                                )
                            }
                            ?.takeIf { it.statusCode() == 200 }
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
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    val publicIp = createPublicIP(request.name)
                    publicIp?.takeIf { it.ip != null }
                            ?.apply {
                                send(Orchestrator.NetworkResourceEvent.Created(
                                        this.toNetworkResource(),
                                        request.name,
                                        ip!!))
                                close()
                            }
                            ?: close(Orchestrator.ResourceCreationFailedException(request))
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceCreationFailedException(request))
                }
            }
        }
    }

    override fun deleteNetworkAddress(
        request: Orchestrator.DeleteNetworkResourceRequest
    ): Publisher<Orchestrator.NetworkResourceEvent> {
        return publish<Orchestrator.NetworkResourceEvent>(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    // Issue DELETE against the IP resource via its URI.
                    val response = nsx.delete<Unit>(
                            request.resource.toString(),
                            contentType = "application/json",
                            headers = emptyList()
                    )
                    response.takeIf { it.statusCode() == 200 }
                            ?.run {
                                send(Orchestrator.NetworkResourceEvent.Deleted(request.resource))
                            }
                            ?: close(Orchestrator.ResourceDeletionFailedException(request))
                } catch (error: CancellationException) {
                    close(Orchestrator.ResourceDeletionFailedException(request))
                }
            }
        }
    }

    override fun createNetworkAllocation(
        request: Orchestrator.CreateNetworkAllocationRequest
    ): Publisher<Orchestrator.NetworkAllocationEvent> {
        return publish<Orchestrator.NetworkAllocationEvent>(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    // Retrieve only the last portion of the URI to get the VM ID.
                    val computeId = request.compute.path.substringAfterLast("/")

                    // Obtain current private IP.
                    // Note: This can take a while as guest needs to boot and start vmware-tools
                    // service and also have the service pick up and report network stack
                    // information back to SDDC.
                    //
                    // TODO: This is not the eventual workflow with agent in the picture.
                    // Current workflow involves polling the deployed entities for its current
                    // status, which does not scale well.
                    // The scalable approach involves the agent calling back to fleet management,
                    // which can *then* trigger the createNetworkAllocation workflow.
                    //
                    // Longer retry frequency to take into account of potentially long wait.
                    val retryFrequency = 5000L
                    var info: VirtualMachineGuestIdentityInfo? = null
                    while (info == null) {
                        delay(retryFrequency)
                        info = getVirtualMachineGuestIdentity(computeId)
                    }
                    val privateIP = info.ip_address

                    // Obtain the public IP address from the IP resource.
                    val publicIP = nsx
                            .get<PublicIP?>(
                                    request.network.toString(),
                                    contentType = "application/json",
                                    headers = emptyList()
                            )
                            .takeIf { it.statusCode() == 200 }
                            ?.body()
                            ?.ip

                    // Setup the NAT rule (use the same resource name as the public IP).
                    val networkId = request.network.path.substringAfterLast("/")
                    createNatRule(name = networkId,
                                  action = NatRule.Action.REFLEXIVE,
                                  sourceNetwork = checkNotNull(privateIP),
                                  translatedNetwork = checkNotNull(publicIP))
                            ?.toNetworkAllocationResource()
                            ?.apply { send(Orchestrator.NetworkAllocationEvent.Created(this)) }
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
        return publish<Orchestrator.NetworkAllocationEvent>(coroutineContext) {
            withTimeout(ORCHESTRATOR_TIMEOUT_MILLIS) {
                try {
                    // Issue DELETE against the NAT resource via its URI.
                    val response = nsx.delete<Unit>(
                            request.resource.toString(),
                            contentType = "application/json",
                            headers = emptyList()
                    )
                    response.takeIf { it.statusCode() == 200 }
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
        return Endpoints.VSPHERE_VM
                .resolve(vSphere.context.endpoint, pathVariables = listOf("{vm}" to this))
    }

    /**
     * Represent the [PublicIP] as a network resource [URI], as known by this [Orchestrator].
     *
     * @return
     *   network resource as a [URI] instance.
     */
    private fun PublicIP.toNetworkResource(): URI {
        val identifier = checkNotNull(id)
        return Endpoints.VMC_PUBLIC_IP
                .resolve(nsx.context.endpoint, pathVariables = listOf("{ip_id}" to identifier))
    }

    /**
     * Represent the [NatRule] as an allocation resource [URI], as known by this [Orchestrator].
     *
     * @return
     *   allocation resource as a [URI] instance.
     */
    private fun NatRule.toNetworkAllocationResource(): URI {
        val resource = checkNotNull(path)
        return Endpoints.NSX_API_ROOT
                .resolve(nsx.context.endpoint, pathVariables = listOf("{resource}" to resource))
    }

    /**
     * Get ID of a specified folder based on name and type.
     *
     * @param[name]
     *   name of the folder to look up.
     * @param[type]
     *   type of the folder to constrain the lookup by.
     *
     * @return
     *   ID of the folder as a [String], if found.
     */
    private suspend fun getFolder(
        name: String = "Workloads",
        type: String = "VIRTUAL_MACHINE"
    ): String? {
        return vSphere
                .get<GetFolderResponse>(
                    Endpoints.VSPHERE_FOLDERS
                            .interpolate(parameters = listOf("filter.type" to type,
                                                             "filter.names" to name)),
                    contentType = "application/json",
                    headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.takeIf { it.value.isNotEmpty() }
                ?.let { it.value[0] }
                ?.let { it.folder /* ID */ }
    }

    /**
     * Get ID of a specified resource pool based on name.
     *
     * @param[name]
     *   name of the resource pool to look up.
     *
     * @return
     *   ID of the resource pool as a [String], if found.
     */
    private suspend fun getResourcePool(name: String = "Compute-ResourcePool"): String? {
        return vSphere
                .get<GetResourcePoolResponse>(
                    path = Endpoints.VSPHERE_RESOURCE_POOLS
                            .interpolate(parameters = listOf("filter.names" to name)),
                    contentType = "application/json",
                    headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.takeIf { it.value.isNotEmpty() }
                ?.let { it.value[0] }
                ?.let { it.resource_pool /* ID */ }
    }

    /**
     * Get ID of a specified datastore based on name.
     *
     * @param[name]
     *   name of the datastore to look up.
     *
     * @return
     *   ID of the datastore as a [String], if found.
     */
    private suspend fun getDatastore(name: String = "WorkloadDatastore"): String? {
        return vSphere
                .get<GetDatastoreResponse>(
                    path = Endpoints.VSPHERE_DATASTORES
                            .interpolate(parameters = listOf("filter.names" to name)),
                    contentType = "application/json",
                    headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.takeIf { it.value.isNotEmpty() }
                ?.let { it.value[0] }
                ?.let { it.datastore /* ID */ }
    }

    /**
     * Get information regarding a specified tier-1 network segment based on name.
     *
     * @param[tier1]
     *   identifier of the tier-1 network.
     * @param[name]
     *   name of the network segment to look up.
     *
     * @return
     *   information regarding the network as a [Segment], if found.
     */
    private suspend fun getNetworkSegment(tier1: String = "cgw", name: String): Segment? {
        return nsx
                .get<Segment>(
                        Endpoints.NSX_NETWORK_SEGMENT
                                .interpolate(pathVariables = listOf("{tier1}" to tier1,
                                                                    "{segment}" to name)),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
    }

    /**
     * Create a routed NSX logical network segment with the given parameters.
     *
     * @param[tier1]
     *   tier-1 network to create the logical network segment under.
     * @param[name]
     *   name of the network segment to create.
     * @param[prefix]
     *   network prefix range to create the network segment under.
     * @param[prefixSubnet]
     *   subnet size for the network prefix range.
     * @param[subnetSize]
     *   subnet size for the network segment itself.
     *
     * @return
     *   ID of the network segment as a [String], if created.
     */
    private suspend fun createNetworkSegment(
        tier1: String = "cgw",
        name: String,
        prefix: Int,
        prefixSubnet: Int,
        subnetSize: Int = 28
    ): String? {
        // Generate a network model based on a randomly generated subnet.
        // Note (current implementation choices):
        // - Primary address is set to the first address in the subnet.
        // - DHCP pool can assign addresses from second address to subnet-broadcast (max) - 1.
        val subnet = randomSubnet(prefix, prefixSubnet, subnetSize)
        val subnetMax = subnet + (1 shl (Int.SIZE_BITS - subnetSize)) - 1
        val segmentSubnet = SegmentSubnet(
                gateway_address = "${toIPv4Address(subnet + 1)}/$subnetSize",
                dhcp_ranges = listOf("${toIPv4Address(subnet + 2)}-${toIPv4Address(subnetMax - 1)}")
        )
        val segment = Segment(subnets = listOf(segmentSubnet))

        val response = nsx
                .patch<Segment, Segment>(
                        Endpoints.NSX_NETWORK_SEGMENT
                                .interpolate(pathVariables = listOf("{tier1}" to tier1,
                                                                    "{segment}" to name)),
                        contentType = "application/json",
                        headers = emptyList(),
                        body = segment
                )

        return when (response.statusCode()) {
            200 -> {
                // The API does not return the resource ID from vSphere's perspective, so we have to
                // follow up w/ another GET.
                //
                // We need to be able to retrieve the ID by name. If not found, just return no
                // result and let caller try again.
                //
                // Note:
                // Given that getNetwork() and intent creation are via 2 separate endpoints, it is
                // possible that vSphere does not yet have the created entity even though the NSX
                // network intent has been created. In this case, status 200 will still result in
                // entity not found (i.e. null).
                getNetwork(name)
            }
            else -> null
        }
    }

    /**
     * Get ID of a specified logical network port-group based on name.
     *
     * @param[name]
     *   name of the network to look up.
     * @param[type]
     *   type of the network to constrain the lookup by.
     *
     * @return
     *   ID of the network as a [String], if found.
     */
    private suspend fun getNetwork(name: String, type: String = "OPAQUE_NETWORK"): String? {
        return vSphere
                .get<GetNetworkResponse>(
                    path = Endpoints.VSPHERE_NETWORKS
                            .interpolate(parameters = listOf("filter.types" to type,
                                                             "filter.names" to name)),
                    contentType = "application/json",
                    headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.takeIf { it.value.isNotEmpty() }
                ?.let { it.value[0] }
                ?.let { it.network /* ID */ }
    }

    /**
     * Ensure the existence of a named logical network by creating it according to the given
     * parameters if the network is not found.
     *
     * Note: This suspendable method does not terminate until the network exists. If necessary, it
     * is the caller's responsibility to constrain the call within a cancellable coroutine scope in
     * order to properly terminate and reclaim any system resources (background thread) engendered
     * by an invocation of this method.
     *
     * @param[tier1]
     *   ID of the gateway / up-link to attach the logical network.
     * @param[name]
     *   unique name of the logical network within the target SDDC.
     * @param[prefix]
     *   prefix address of the network range to create the sub-network CIDR within.
     * @param[prefixSubnet]
     *   prefix subnet (size) of the network range to create the sub-network CIDR within.
     * @param[subnetSize]
     *   size of the logical network subnet.
     *
     * @return
     *   ID of the logical network as a [String].
     */
    private suspend fun ensureLogicalNetwork(
        tier1: String = "cgw",
        name: String,
        prefix: Int,
        prefixSubnet: Int,
        subnetSize: Int = 28
    ): String {
        var result: String? = null
        while (result == null) {
            // Create the network segment and resolve its vSphere name. Due to realization delays,
            // the create() call may not immediately yield any result. In such cases, let the loop
            // take care of eventually getting a network segment that matches the desired name.
            result = getNetwork(name)
                    ?: createNetworkSegment(tier1, name, prefix, prefixSubnet, subnetSize)

            if (result == null) {
                // Do not engage next iteration immediately.
                delay(OPERATION_RETRY_INTERVAL_MILLIS)
            }
        }
        return result
    }

    /**
     * Get ID of the specified content library item based on name.
     *
     * @param[name]
     *   name of the library item.
     *
     * @return
     *   ID of the library item as a [String], if found.
     */
    private suspend fun getLibraryItem(name: String): String? {
        // TODO: Finish integration work with Content Library API.
        // return "f4ccb861-6716-4e08-b58d-323977f13aab"
        return "dad738c1-43d8-4138-8fcc-52a7d86ff52b"
    }

    /**
     * Create a housing instance based on the specified parameters.
     */
    private suspend fun createVirtualMachine(
        name: String,
        libraryItem: String,
        datastore: String,
        resourcePool: String,
        folder: String,
        controlNetwork: String,
        dataNetwork: String,
        initScript: InitScript
    ): String? {
        val deployRequest = LibraryItemDeployRequest(
                LibraryItemDeploymentSpec(
                        name = name,
                        accept_all_EULA = true,
                        default_datastore_id = datastore,
                        network_mappings = listOf(
                                NetworkMapping("control-network", controlNetwork),
                                NetworkMapping("data-network", dataNetwork)
                        ),
                        additional_parameters = listOf(
                                OvfParameter(
                                        `@class` = OvfParameterTypes.PROPERTY_PARAMS.classValue,
                                        type = OvfParameterTypes.PROPERTY_PARAMS.type,
                                        properties = listOf(
                                                OvfProperty("instance-id", name),
                                                OvfProperty("hostname", "replica"),
                                                OvfProperty("user-data", String(initScript.base64()))
                                        )
                                )
                        )
                ),
                LibraryItemDeploymentTarget(resource_pool_id = resourcePool, folder_id = folder)
        )

        val response = vSphere
                .post<LibraryItemDeployRequest, LibraryItemDeployResponse>(
                    path = Endpoints.VSPHERE_OVF_LIBRARY_ITEM
                            .interpolate(
                                     pathVariables = listOf("{library_item}" to libraryItem),
                                     parameters = listOf("~action" to "deploy")
                            ),
                    contentType = "application/json",
                    headers = emptyList(),
                    body = deployRequest
                )
        return when (response.statusCode()) {
            200 -> {
                response.body()
                        ?.value
                        ?.takeIf { it.succeeded }
                        ?.resource_id
                        ?.id
            }
            else -> null
        }
    }

    /**
     * Update the power state of a virtual machine specified by the given parameter.
     *
     * @param[name]
     *   identifier of the virtual machine.
     * @param[state]
     *   power state to update to.
     *
     * @return
     *   `true` if power operation is successfully executed for the specified virtual machine,
     *   `false` otherwise.
     */
    private suspend fun updateVirtualMachinePowerState(
        name: String,
        state: VirtualMachinePowerState
    ): Boolean {
        val endpoint = when (state) {
            VirtualMachinePowerState.POWERED_OFF -> Endpoints.VSPHERE_VM_POWER_STOP
            VirtualMachinePowerState.POWERED_ON -> Endpoints.VSPHERE_VM_POWER_START
            VirtualMachinePowerState.SUSPEND -> Endpoints.VSPHERE_VM_POWER_SUSPEND
        }

        return vSphere
                .post<Unit, Unit>(
                        path = endpoint.interpolate(pathVariables = listOf("{vm}" to name)),
                        contentType = "application/json",
                        headers = emptyList(),
                        body = null
                )
                .let { it.statusCode() == 200 }
    }

    /**
     * Get the power state of a virtual machine.
     *
     * @param[name]
     *   identifier of the virtual machine.
     *
     * @return
     *   the power state of the virtual machine expressed as [VirtualMachinePowerState], `null` if
     *   the operation cannot be executed or if the virtual machine does not exist.
     */
    private suspend fun getVirtualMachinePower(name: String): VirtualMachinePowerState? {
        return vSphere
                .get<VirtualMachinePowerResponse>(
                        path = Endpoints.VSPHERE_VM_POWER
                                .interpolate(pathVariables = listOf("{vm}" to name)),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.let { it.value }
                ?.let { it.state }
    }

    /**
     * Update the power state of a virtual machine's guest OS according to the given parameters.
     *
     * @param[name]
     *   identifier of the virtual machine.
     * @param[action]
     *   power action to invoke on guest.
     *
     * @return
     *   `true` if power operation is successfully executed for the specified virtual machine guest,
     *   `false` otherwise.
     */
    private suspend fun updateVirtualMachineGuestPower(name: String, action: String): Boolean {
        return vSphere.post<Unit, Unit>(
                path = Endpoints.VSPHERE_VM_GUEST_POWER
                        .interpolate(
                                parameters = listOf("action" to action),
                                pathVariables = listOf("{vm}" to name)
                        ),
                contentType = "application/json",
                headers = emptyList(),
                body = null
        )
                .let { it.statusCode() == 200 }
    }

    /**
     * Ensure the power-on state of the virtual machine specified by the given parameter.
     *
     * @param[name]
     *   identifier of the virtual machine.
     *
     * @return
     *   `true` if the power state of the virtual machine is on at some point during the execution
     *   of the function, `false` otherwise.
     */
    private suspend fun ensureVirtualMachinePowerStart(name: String): Boolean {
        var confirmed = false
        var iterating = true
        while (iterating) {
            // The typical case.
            updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_ON)
            when (getVirtualMachinePower(name)) {
                VirtualMachinePowerState.POWERED_OFF, VirtualMachinePowerState.SUSPEND -> {
                    updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_ON)

                    // Do not engage next iteration immediately.
                    delay(OPERATION_RETRY_INTERVAL_MILLIS)
                }
                VirtualMachinePowerState.POWERED_ON -> {
                    iterating = false
                    confirmed = true
                }
                else -> {
                    // If the VM doesn't exist or power-state cannot be retrieved.
                    iterating = false
                    confirmed = false
                }
            }
        }

        return confirmed
    }

    /**
     * Ensure the power-off state of the virtual machine specified by the given parameter.
     *
     * @param[name]
     *   identifier of the virtual machine.
     *
     * @return
     *   `true` if the power state of the virtual machine is off at some point during the execution
     *   of the function, `false` otherwise.
     */
    private suspend fun ensureVirtualMachinePowerStop(name: String): Boolean {
        var confirmed = false
        var iterating = true
        var attempts = 0
        while (iterating) {
            // Try guest-friendly power-off.
            updateVirtualMachineGuestPower(name, "shutdown")
            when (getVirtualMachinePower(name)) {
                VirtualMachinePowerState.POWERED_ON, VirtualMachinePowerState.SUSPEND -> {
                    // Start to force the issue after a while.
                    if (attempts > 20) {
                        updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_OFF)
                    } else {
                        attempts++
                    }

                    // Do not engage next iteration immediately.
                    delay(OPERATION_RETRY_INTERVAL_MILLIS)
                }
                VirtualMachinePowerState.POWERED_OFF -> {
                    iterating = false
                    confirmed = true
                }
                else -> {
                    // If the VM doesn't exist or power-state cannot be retrieved.
                    iterating = false
                    confirmed = false
                }
            }
        }

        return confirmed
    }

    /**
     * Get the guest identity information of a virtual machine.
     *
     * @param[name]
     *   identifier of the virtual machine.
     *
     * @return
     *   the guest identity information of the virtual machine expressed as
     *   [VirtualMachineGuestIdentityInfo], `null` if the operation cannot be executed or if the
     *   virtual machine does not exist.
     */
    private suspend fun getVirtualMachineGuestIdentity(
        name: String
    ): VirtualMachineGuestIdentityInfo? {
        return vSphere
                .get<VirtualMachineGuestIdentityResponse>(
                        path = Endpoints.VSPHERE_VM_GUEST_IDENTITY
                                .interpolate(pathVariables = listOf("{vm}" to name)),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.let { it.value }
    }

    /**
     * Allocate a new public IP address for use, with a given name as the identifier.
     *
     * @param[name]
     *   identifier of the IP address resource.
     *
     * @return
     *   allocated IP address resource as a instance of [PublicIP], if created, `null` otherwise.
     */
    private suspend fun createPublicIP(name: String): PublicIP? {
        return nsx
                .put<PublicIP, PublicIP>(
                        path = Endpoints.VMC_PUBLIC_IP
                                .interpolate(pathVariables = listOf("{ip_id}" to name)),
                        contentType = "application/json",
                        headers = emptyList(),
                        body = PublicIP(id = name, display_name = name)
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
    }

    /**
     * Create a NAT rule based on the given parameters.
     *
     * @param[tier1]
     *   tier-1 network that the NAT region is under for NAT rule addition.
     * @param[nat]
     *   NAT region to add rule to.
     * @param[name]
     *   identifier to assign to the NAT rule.
     * @param[action]
     *   type of NAT action.
     * @param[sourceNetwork]
     *   source address(es) to translate for SNAT and REFLEXIVE rules.
     * @param[destinationNetwork]
     *   destination address(es) to translate for DNAT rules.
     * @param[translatedNetwork]
     *   network address to apply translation into.
     * @param[translatedPorts]
     *   network ports to apply translation.
     */
    private suspend fun createNatRule(
        tier1: String = "cgw",
        nat: String = "USER",
        name: String,
        action: NatRule.Action,
        sourceNetwork: String? = null,
        destinationNetwork: String? = null,
        translatedNetwork: String? = null,
        translatedPorts: String? = null
    ): NatRule? {
        val endpoint = Endpoints.NSX_NAT_RULE.interpolate(
                pathVariables = listOf("{tier1}" to tier1, "{nat}" to nat, "{nat_rule}" to name)
        )
        val response = nsx
                .patch<NatRule, Unit>(
                        path = endpoint,
                        contentType = "application/json",
                        headers = emptyList(),
                        body = NatRule(
                                action = action,
                                source_network = sourceNetwork,
                                destination_network = destinationNetwork,
                                translated_network = translatedNetwork,
                                translated_ports = translatedPorts
                        )
                )

        // PATCH does not return a body, follow up with another GET to obtain metadata information
        // that is assigned post-creation (e.g. ID, infra path, etc).
        return when (response.statusCode()) {
            200 -> {
                nsx.get<NatRule>(path = endpoint,
                                 contentType = "application/json",
                                 headers = emptyList())
                        .takeIf { it.statusCode() == 200 }
                        ?.let { it.body() }
            }
            else -> null
        }
    }
}
