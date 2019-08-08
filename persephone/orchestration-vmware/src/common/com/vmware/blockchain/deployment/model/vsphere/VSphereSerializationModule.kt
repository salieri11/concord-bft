/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.modules.serializersModuleOf
import kotlinx.serialization.modules.SerialModule

/**
 * Enumeration of all serializable types related to vSphere data models.
 */
enum class VSphereSerializationModule(val module: SerialModule) {
    DATASTORE_SUMMARY(serializersModuleOf(DatastoreSummary::class, DatastoreSummary.serializer())),
    DEPLOYABLE_IDENTITY(serializersModuleOf(DeployableIdentity::class, DeployableIdentity.serializer())),
    FOLDER_SUMMARY(serializersModuleOf(FolderSummary::class, FolderSummary.serializer())),
    GET_DATASTORE_RESPONSE(serializersModuleOf(GetDatastoreResponse::class, GetDatastoreResponse.serializer())),
    GET_FOLDER_RESPONSE(serializersModuleOf(GetFolderResponse::class, GetFolderResponse.serializer())),
    GET_NETWORK_RESPONSE(serializersModuleOf(GetNetworkResponse::class, GetNetworkResponse.serializer())),
    GET_RESOURCE_POOL_RESPONSE(serializersModuleOf(GetResourcePoolResponse::class, GetResourcePoolResponse.serializer())),
    LIBRARY_ITEM_DEPLOYMENT_RESULT(serializersModuleOf(LibraryItemDeploymentResult::class, LibraryItemDeploymentResult.serializer())),
    LIBRARY_ITEM_DEPLOYMENT_SPEC(serializersModuleOf(LibraryItemDeploymentSpec::class, LibraryItemDeploymentSpec.serializer())),
    LIBRARY_ITEM_DEPLOYMENT_TARGET(serializersModuleOf(LibraryItemDeploymentTarget::class, LibraryItemDeploymentTarget.serializer())),
    LIBRARY_ITEM_DEPLOY_REQUEST(serializersModuleOf(LibraryItemDeployRequest::class, LibraryItemDeployRequest.serializer())),
    LIBRARY_ITEM_DEPLOY_RESPONSE(serializersModuleOf(LibraryItemDeployResponse::class, LibraryItemDeployResponse.serializer())),
    LIBRARY_ITEM_FIND_REQUEST(serializersModuleOf(LibraryItemFindRequest::class, LibraryItemFindRequest.serializer())),
    LIBRARY_ITEM_FIND_RESPONSE(serializersModuleOf(LibraryItemFindResponse::class, LibraryItemFindResponse.serializer())),
    LIBRARY_ITEM_FIND_SPEC(serializersModuleOf(LibraryItemFindSpec::class, LibraryItemFindSpec.serializer())),
    NETWORK_MAPPING(serializersModuleOf(NetworkMapping::class, NetworkMapping.serializer())),
    NETWORK_SUMMARY(serializersModuleOf(NetworkSummary::class, NetworkSummary.serializer())),
    OVF_PARAMETER(serializersModuleOf(OvfParameter::class, OvfParameter.serializer())),
    OVF_PROPERTY(serializersModuleOf(OvfProperty::class, OvfProperty.serializer())),
    RESOURCE_POOL_SUMMARY(serializersModuleOf(ResourcePoolSummary::class, ResourcePoolSummary.serializer())),
    VIRTUAL_MACHINE_GUEST_IDENTITY_INFO(serializersModuleOf(VirtualMachineGuestIdentityInfo::class, VirtualMachineGuestIdentityInfo.serializer())),
    VIRTUAL_MACHINE_GUEST_IDENTITY_RESPONSE(serializersModuleOf(VirtualMachineGuestIdentityResponse::class, VirtualMachineGuestIdentityResponse.serializer())),
    VIRTUAL_MACHINE_POWER_INFO(serializersModuleOf(VirtualMachinePowerInfo::class, VirtualMachinePowerInfo.serializer())),
    VIRTUAL_MACHINE_POWER_RESPONSE(serializersModuleOf(VirtualMachinePowerResponse::class, VirtualMachinePowerResponse.serializer())),
    VSPHERE_AUTHENTICATION_RESPONSE(serializersModuleOf(VSphereAuthenticationResponse::class, VSphereAuthenticationResponse.serializer()))
}
