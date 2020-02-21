/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vsphere

import com.google.protobuf.ByteString
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.model.vsphere.GetDatastoreResponse
import com.vmware.blockchain.deployment.model.vsphere.GetFolderResponse
import com.vmware.blockchain.deployment.model.vsphere.GetNetworkResponse
import com.vmware.blockchain.deployment.model.vsphere.GetResourcePoolResponse
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeployRequest
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeployResponse
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeploymentSpec
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemDeploymentTarget
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemFindRequest
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemFindResponse
import com.vmware.blockchain.deployment.model.vsphere.LibraryItemFindSpec
import com.vmware.blockchain.deployment.model.vsphere.NetworkMapping
import com.vmware.blockchain.deployment.model.vsphere.OvfParameter
import com.vmware.blockchain.deployment.model.vsphere.OvfParameterTypes
import com.vmware.blockchain.deployment.model.vsphere.OvfProperty
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachineGuestIdentityInfo
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachineGuestIdentityResponse
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachinePowerResponse
import com.vmware.blockchain.deployment.model.vsphere.VirtualMachinePowerState
import com.vmware.blockchain.deployment.vm.CloudInitConfiguration
import kotlinx.coroutines.delay
import java.util.*

/**
 * A client for issuing commands to a vSphere environment targeted by a given [VSphereHttpClient]..
 *
 * @property[client]
 *   underlying [VSphereHttpClient] to use for REST API communication.
 */
class VSphereClient(private val client: VSphereHttpClient) {

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
    suspend fun getFolder(
        name: String = "Workloads",
        type: String = "VIRTUAL_MACHINE"
    ): String? {
        return client
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
    suspend fun getResourcePool(name: String = "Compute-ResourcePool"): String? {
        return client
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
    suspend fun getDatastore(name: String = "WorkloadDatastore"): String? {
        return client
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
    suspend fun getNetwork(name: String, type: String = "OPAQUE_NETWORK"): String? {
        return client
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
     * Get ID of the specified content library item based on name.
     *
     * @param[sourceId]
     *   unique ID of the library item as declared by publishing source.
     *
     * @return
     *   ID of the library item as a [String], if found.
     */
    suspend fun getLibraryItem(sourceId: String): String? {
        return client
                .post<LibraryItemFindRequest, LibraryItemFindResponse>(
                        path = Endpoints.VSPHERE_CONTENT_LIBRARY_ITEM
                                .interpolate(parameters = listOf("~action" to "find")),
                        contentType = "application/json",
                        headers = emptyList(),
                        body = LibraryItemFindRequest(
                                spec = LibraryItemFindSpec(source_id = sourceId)
                        )
                )
                .takeIf { it.statusCode() == 200 }
                ?.body()
                ?.value
                ?.first()
    }

    /**
     * Create a housing instance based on the specified parameters.
     */
    suspend fun createVirtualMachine(
        name: String,
        libraryItem: String,
        datastore: String,
        resourcePool: String,
        folder: String,
        networks: List<Pair<String, String>>,
        cloudInit: CloudInitConfiguration
    ): String? {
        val encodedUserData = Base64.getEncoder().encodeToString(cloudInit.userData().toByteArray())
        val deployRequest = LibraryItemDeployRequest(
                LibraryItemDeploymentSpec(
                        name = name,
                        accept_all_EULA = true,
                        default_datastore_id = datastore,
                        network_mappings = networks.map { NetworkMapping(it.first, it.second) },
                        additional_parameters = listOf(
                                OvfParameter(
                                        `@class` = OvfParameterTypes.PROPERTY_PARAMS.classValue,
                                        type = OvfParameterTypes.PROPERTY_PARAMS.type,
                                        properties = listOf(
                                                OvfProperty("instance-id", name),
                                                OvfProperty("hostname", "replica"),
                                                OvfProperty(
                                                        "user-data",
                                                        encodedUserData
                                                )
                                        )
                                )
                        )
                ),
                LibraryItemDeploymentTarget(resource_pool_id = resourcePool, folder_id = folder)
        )

        val response = client
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
     * Delete the virtual machine referenced by the given identifier.
     *
     * @param[id]
     *   identifier of the virtual machine.
     *
     * @return
     *   `true` if deleted, `false` otherwise.
     */
    suspend fun deleteVirtualMachine(id: String): Boolean {
        // Issue DELETE against the VM resource via its URI.
        val response = client.delete<Unit>(
                resolveVirtualMachineIdentifier(id).toString(),
                contentType = "application/json",
                headers = emptyList()
        )

        return when (response.statusCode()) {
            200 -> true
            else -> false
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
    suspend fun updateVirtualMachinePowerState(
        name: String,
        state: VirtualMachinePowerState
    ): Boolean {
        val endpoint = when (state) {
            VirtualMachinePowerState.POWERED_OFF -> Endpoints.VSPHERE_VM_POWER_STOP
            VirtualMachinePowerState.POWERED_ON -> Endpoints.VSPHERE_VM_POWER_START
            VirtualMachinePowerState.SUSPEND -> Endpoints.VSPHERE_VM_POWER_SUSPEND
        }

        return client
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
    suspend fun getVirtualMachinePower(name: String): VirtualMachinePowerState? {
        return client
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
    suspend fun updateVirtualMachineGuestPower(name: String, action: String): Boolean {
        return client.post<Unit, Unit>(
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
     * @param[retryInterval]
     *   interval to retry checking for guest OS's power state, in milliseconds.
     *
     * @return
     *   `true` if the power state of the virtual machine is on at some point during the execution
     *   of the function, `false` otherwise.
     */
    suspend fun ensureVirtualMachinePowerStart(name: String, retryInterval: Long = 500): Boolean {
        var confirmed = false
        var iterating = true
        while (iterating) {
            // The typical case.
            updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_ON)
            when (getVirtualMachinePower(name)) {
                VirtualMachinePowerState.POWERED_OFF, VirtualMachinePowerState.SUSPEND -> {
                    updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_ON)

                    // Do not engage next iteration immediately.
                    delay(retryInterval)
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
     * @param[retryInterval]
     *   interval to retry checking for guest OS's power state, in milliseconds.
     *
     * @return
     *   `true` if the power state of the virtual machine is off at some point during the execution
     *   of the function, `false` otherwise.
     */
    suspend fun ensureVirtualMachinePowerStop(name: String, retryInterval: Long = 500): Boolean {
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
                    delay(retryInterval)
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
    suspend fun getVirtualMachineGuestIdentity(
        name: String
    ): VirtualMachineGuestIdentityInfo? {
        return client
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
     * Resolve a virtual machine identifier (managed object reference identifier) to its canonical
     * resource URI as perceived by the target vSphere server endpoint.
     *
     * @param[id]
     *   virtual machine identifier.
     *
     * @return
     *   virtual machine resource URL as an instance of [URI].
     */
    fun resolveVirtualMachineIdentifier(id: String): URI {
        return Endpoints.VSPHERE_VM
                .resolve(client.context.endpoint, pathVariables = listOf("{vm}" to id))
    }
}
