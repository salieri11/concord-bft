/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.serialization.JsonSerializer
import com.vmware.blockchain.deployment.vm.InitScript
import com.vmware.blockchain.model.core.URI
import com.vmware.blockchain.model.deployment.OrchestrationSite
import com.vmware.blockchain.model.nsx.Segment
import com.vmware.blockchain.model.nsx.SegmentSubnet
import com.vmware.blockchain.model.sddc.GetDatastoreResponse
import com.vmware.blockchain.model.sddc.GetFolderResponse
import com.vmware.blockchain.model.sddc.GetNetworkResponse
import com.vmware.blockchain.model.sddc.GetResourcePoolResponse
import com.vmware.blockchain.model.sddc.LibraryItemDeployRequest
import com.vmware.blockchain.model.sddc.LibraryItemDeployResponse
import com.vmware.blockchain.model.sddc.LibraryItemDeploymentSpec
import com.vmware.blockchain.model.sddc.LibraryItemDeploymentTarget
import com.vmware.blockchain.model.sddc.NetworkMapping
import com.vmware.blockchain.model.sddc.OvfParameter
import com.vmware.blockchain.model.sddc.OvfParameterTypes
import com.vmware.blockchain.model.sddc.OvfProperty
import com.vmware.blockchain.model.vmc.AddressGroup
import com.vmware.blockchain.model.vmc.DhcpConfig
import com.vmware.blockchain.model.vmc.DhcpIpPool
import com.vmware.blockchain.model.vmc.GetLogicalNetworkResponse
import com.vmware.blockchain.model.vmc.LogicalNetwork
import com.vmware.blockchain.model.vmc.Sddc
import com.vmware.blockchain.model.vmc.Subnets
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.delay
import kotlin.coroutines.CoroutineContext
import kotlin.random.Random

/**
 * Deployment orchestration driver for VMware Cloud-based [OrchestrationSite].
 *
 * @param[vmc]
 *   VMware Cloud API client handle.
 * @param[vSphere]
 *   vSphere API client handle.
 */
class Orchestrator private constructor(
    private val vmc: VmcClient,
    private val vSphere: VSphereClient,
    private val nsx: VmcClient
) {
    companion object {
        /**
         * Randomly generate an IPv4 address sub-network constrained within a prefix network range.
         *
         * @param[prefix]
         *   prefix of the network range to create a sub-network within.
         * @param[prefixSubnet]
         *   number of bits used for mask of the prefix network to constrain by.
         * @param[subnet]
         *   number of bits used for mask of the sub-network to generate.
         *
         * @return
         *   generated subnet CIDR as an [Int].
         */
        @JvmStatic
        private fun randomSubnet(prefix: Int, prefixSubnet: Int, subnet: Int): Int {
            return prefix + (Random.nextBits(subnet - prefixSubnet) shl (Int.SIZE_BITS - subnet))
        }

        /**
         * Convert an [Int] to the IPv4 address it represents in canonical format.
         *
         * @param[value]
         *   integer value to convert.
         *
         * @return
         *   canonical IPv4 address as a [String].
         */
        @JvmStatic
        private fun toIPv4Address(value: Int): String {
            val first = (value ushr 24)
            val second = (value and 0x00FF0000) ushr 16
            val third = (value and 0x0000FF00) ushr 8
            val fourth = (value and 0x000000FF)

            return "$first.$second.$third.$fourth"
        }

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
        private suspend fun getSddc(vmc: VmcClient): Sddc? {
            return vmc
                    .get<Sddc>(
                            Endpoints.VMC_SDDC.interpolate(pathVariables = listOf(
                                    Pair("{org}", vmc.context.organization),
                                    Pair("{sddc}", vmc.context.datacenter)
                            )),
                            contentType = "application/json",
                            headers = emptyList()
                    )
                    .takeIf { it.statusCode() == 200 }
                    ?.let { it.body() }
        }

        /**
         * Create a new [Orchestrator] based on parameter info from a given [OrchestrationSite].
         *
         * @param[site]
         *   orchestration information pertaining to a VMC-based orchestration site.
         * @param[executionContext]
         *   execution context for asynchronous executions triggered by this operation.
         *
         * @return
         *   a [Orchestrator] instance corresponding to the given input parameter.
         */
        @JvmStatic
        fun newOrchestrator(
            site: OrchestrationSite,
            executionContext: CoroutineContext = Dispatchers.IO
        ): Deferred<Orchestrator?> {
            // Precondition.
            require(site.type == OrchestrationSite.Type.VMC)
            require(site.info is OrchestrationSite.Info.Vmc)

            val info = (site.info as OrchestrationSite.Info.Vmc).entries
            val token = requireNotNull(info.authentication.credential.tokenCredential).token

            /** Create a shared common data model serializer. */
            val serializer = JsonSerializer()

            // Create new VMC client.
            val vmcContext = VmcClient.Context(
                    endpoint = info.api.address,
                    authenticationEndpoint = info.authentication.address,
                    refreshToken = token,
                    organization = info.organization,
                    datacenter = info.datacenter
            )
            val vmc = VmcClient(vmcContext, serializer)

            // Asynchronously create the orchestrator instance based on VMC operation results.
            return CoroutineScope(executionContext).async {
                // Use VMC client to obtain vSphere information.
                getSddc(vmc)
                        ?.let { sddc ->
                            val nsx = sddc
                                    .let {
                                        val base = sddc.resource_config.nsx_api_public_endpoint_url
                                        val url = "$base/sks-nsxt-manager"
                                        VmcClient.Context(
                                                endpoint = URI(url),
                                                authenticationEndpoint = info.authentication.address,
                                                refreshToken = token,
                                                organization = info.organization,
                                                datacenter = info.datacenter
                                        )
                                    }
                                    .let { VmcClient(it, serializer) } // New proxied NSX client.
                            val vsphere = sddc
                                    .let {
                                        // Transform SDDC info into vSphere client context.
                                        VSphereClient.Context(
                                                endpoint = URI(sddc.resource_config.vc_url),
                                                username = sddc.resource_config.cloud_username,
                                                password = sddc.resource_config.cloud_password
                                        )
                                    }
                                    .let { VSphereClient(it, serializer) } // New vSphere client.

                            // New Orchestrator instance.
                            Orchestrator(vmc, vsphere, nsx)
                        }
            }
        }
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
    suspend fun getFolder(name: String = "Workloads", type: String = "VIRTUAL_MACHINE"): String? {
        return vSphere
                .get<GetFolderResponse>(
                    Endpoints.VSPHERE_FOLDERS
                            .interpolate(parameters = listOf(Pair("filter.type", type),
                                                             Pair("filter.names", name))),
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
    suspend fun getResourcePool(name: String = "Compute-ResourcePool"): String? {
        return vSphere
                .get<GetResourcePoolResponse>(
                    path = Endpoints.VSPHERE_RESOURCE_POOLS
                            .interpolate(parameters = listOf(Pair("filter.names", name))),
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
    suspend fun getDatastore(name: String = "WorkloadDatastore"): String? {
        return vSphere
                .get<GetDatastoreResponse>(
                    path = Endpoints.VSPHERE_DATASTORES
                            .interpolate(parameters = listOf(Pair("filter.names", name))),
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
    suspend fun getNetworkSegment(tier1: String, name: String): Segment? {
        return nsx
                .get<Segment>(
                        Endpoints.NSX_NETWORK_SEGMENT
                                .interpolate(pathVariables = listOf(Pair("{tier1}", tier1),
                                                                    Pair("{segment}", name))),
                        contentType = "application/json",
                        headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
    }

    private suspend fun createNetworkSegment(
        tier1: String,
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
                .patch<Segment>(
                        Endpoints.NSX_NETWORK_SEGMENT
                                .interpolate(pathVariables = listOf(Pair("{tier1}", tier1),
                                                                    Pair("{segment}", name))),
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
     * Get ID of a specified NSX logical network based on name.
     *
     * @param[name]
     *   name of the network to look up.
     *
     * @return
     *   ID of the network as a [String], if found.
     */
    suspend fun getLogicalNetwork(name: String): String? {
        return vmc
                .get<GetLogicalNetworkResponse>(
                    path = Endpoints.VMC_LOGICAL_NETWORKS
                            .interpolate(pathVariables = listOf(
                                    Pair("{org}", vmc.context.organization),
                                    Pair("{sddc}", vmc.context.datacenter)
                            )),
                    contentType = "application/json",
                    headers = emptyList()
                )
                .takeIf { it.statusCode() == 200 }
                ?.let { it.body() }
                ?.takeIf { it.data.isNotEmpty() }
                ?.let { it.data.asSequence() }
                ?.firstOrNull { it.name == name }
                ?.id
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
                            .interpolate(parameters = listOf(Pair("filter.types", type),
                                                             Pair("filter.names", name))),
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
     * @param[attachedGateway]
     *   ID of the gateway / uplink to attach the logical network.
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
    suspend fun ensureLogicalNetwork(
        attachedGateway: String,
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
                    ?: createNetworkSegment(attachedGateway, name, prefix, prefixSubnet, subnetSize)

            if (result == null) {
                // Do not engage next iteration immediately.
                delay(500) // 500ms.
            }
        }
        return result
    }

    /**
     * Create a logical network based on the specified parameters.
     *
     * @param[attachedGateway]
     *   ID of the gateway / uplink to attach the logical network.
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
     *   ID of the logical network as a [String], if created.
     */
    private suspend fun createLogicalNetwork(
        attachedGateway: String,
        name: String,
        prefix: Int,
        prefixSubnet: Int,
        subnetSize: Int
    ): String? {
        // Generate a network model based on a randomly generated subnet.
        // Note (current implementation choices):
        // - Primary address is set to the first address in the subnet.
        // - DHCP pool can assign addresses from second address to subnet-broadcast (max) - 1.
        val subnet = randomSubnet(prefix, prefixSubnet, subnetSize)
        val subnetMax = subnet + (1 shl (Int.SIZE_BITS - subnetSize)) - 1
        val addressGroup = AddressGroup(toIPv4Address(subnet + 1), subnetSize.toString())
        val dhcpPool = DhcpIpPool(
                ipRange = "${toIPv4Address(subnet + 2)}-${toIPv4Address(subnetMax - 1)}",
                domainName = null
        )
        val network = LogicalNetwork(
                cgwId = attachedGateway,
                name = name,
                subnets = Subnets(listOf(addressGroup)),
                dhcpConfigs = DhcpConfig(listOf(dhcpPool))
        )

        val response = vmc
                .post<String>(
                    path = Endpoints.VMC_LOGICAL_NETWORKS
                            .interpolate(pathVariables = listOf(
                                    Pair("{org}", vmc.context.organization),
                                    Pair("{sddc}", vmc.context.datacenter)
                            )),
                    contentType = "application/json",
                    headers = emptyList(),
                    body = network
                )
        return when (response.statusCode()) {
            201 -> {
                // The API does not return the resource ID, so we have to follow up w/ another GET.
                // We need to be able to retrieve the ID by name. If not found, just return no
                // result and let caller try again.
                //
                // Note: This will likely change once the API client is pointing directly at NSX-T
                // rather than via VMC networking.
                getNetwork(name)
            }
            else -> null
        }
    }

    /**
     * Delete a logical network based on the specified parameters.
     *
     * @param[id]
     *   ID of the network to delete.
     *
     * @return
     *   `true` if request succeeded, `false` otherwise.
     */
    suspend fun deleteLogicalNetwork(id: String): Boolean {
        return vmc
                .delete<Unit>(
                    path = Endpoints.VMC_LOGICAL_NETWORK
                            .interpolate(pathVariables = listOf(
                                    Pair("{org}", vmc.context.organization),
                                    Pair("{sddc}", vmc.context.datacenter),
                                    Pair("{network}", id)
                            )),
                    contentType = "application/json",
                    headers = emptyList()
                )
                .let { it.statusCode() == 200 }
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
    suspend fun getLibraryItem(name: String): String? {
        // TODO(jameschang) - Finish integration work with Content Library API.
        // return "a1500e02-5cd7-4afd-8429-1695f2fd8d6c"
        return "f4ccb861-6716-4e08-b58d-323977f13aab"
    }

    /**
     * Create a housing instance based on the specified parameters.
     */
    suspend fun createInstance(
        instanceName: String,
        libraryItem: String,
        datastore: String,
        resourcePool: String,
        folder: String,
        controlNetwork: String,
        dataNetwork: String
    ): String? {
        val deployRequest = LibraryItemDeployRequest(
                LibraryItemDeploymentSpec(
                        name = instanceName,
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
                                                OvfProperty("instance-id", instanceName),
                                                OvfProperty("hostname", instanceName),
                                                OvfProperty("user-data", String(InitScript().base64()))
                                        )
                                )
                        )
                ),
                LibraryItemDeploymentTarget(
                        resource_pool_id = resourcePool,
                        folder_id = folder
                )
        )

        val response = vSphere
                .post<LibraryItemDeployResponse>(
                    path = Endpoints.VSPHERE_OVF_LIBRARY_ITEM
                            .interpolate(
                                     pathVariables = listOf(Pair("{library_item}", libraryItem)),
                                     parameters = listOf(Pair("~action", "deploy"))),
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
}
