/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.context.SimpleModule

/**
 * Enumeration of all serializable types related to vSphere data models.
 */
enum class VSphereSerializationModule(val module: SimpleModule<*>) {
    DATASTORE_SUMMARY(SimpleModule(DatastoreSummary::class, DatastoreSummary.serializer())),
    DEPLOYABLE_IDENTITY(SimpleModule(DeployableIdentity::class, DeployableIdentity.serializer())),
    FOLDER_SUMMARY(SimpleModule(FolderSummary::class, FolderSummary.serializer())),
    GET_DATASTORE_RESPONSE(SimpleModule(GetDatastoreResponse::class, GetDatastoreResponse.serializer())),
    GET_FOLDER_RESPONSE(SimpleModule(GetFolderResponse::class, GetFolderResponse.serializer())),
    GET_NETWORK_RESPONSE(SimpleModule(GetNetworkResponse::class, GetNetworkResponse.serializer())),
    GET_RESOURCE_POOL_RESPONSE(SimpleModule(GetResourcePoolResponse::class, GetResourcePoolResponse.serializer())),
    LIBRARY_ITEM_DEPLOYMENT_RESULT(SimpleModule(LibraryItemDeploymentResult::class, LibraryItemDeploymentResult.serializer())),
    LIBRARY_ITEM_DEPLOYMENT_SPEC(SimpleModule(LibraryItemDeploymentSpec::class, LibraryItemDeploymentSpec.serializer())),
    LIBRARY_ITEM_DEPLOYMENT_TARGET(SimpleModule(LibraryItemDeploymentTarget::class, LibraryItemDeploymentTarget.serializer())),
    LIBRARY_ITEM_DEPLOY_REQUEST(SimpleModule(LibraryItemDeployRequest::class, LibraryItemDeployRequest.serializer())),
    LIBRARY_ITEM_DEPLOY_RESPONSE(SimpleModule(LibraryItemDeployResponse::class, LibraryItemDeployResponse.serializer())),
    NETWORK_MAPPING(SimpleModule(NetworkMapping::class, NetworkMapping.serializer())),
    NETWORK_SUMMARY(SimpleModule(NetworkSummary::class, NetworkSummary.serializer())),
    OVF_PARAMETER(SimpleModule(OvfParameter::class, OvfParameter.serializer())),
    OVF_PROPERTY(SimpleModule(OvfProperty::class, OvfProperty.serializer())),
    RESOURCE_POOL_SUMMARY(SimpleModule(ResourcePoolSummary::class, ResourcePoolSummary.serializer())),
    VIRTUAL_MACHINE_GUEST_IDENTITY_INFO(SimpleModule(VirtualMachineGuestIdentityInfo::class, VirtualMachineGuestIdentityInfo.serializer())),
    VIRTUAL_MACHINE_GUEST_IDENTITY_RESPONSE(SimpleModule(VirtualMachineGuestIdentityResponse::class, VirtualMachineGuestIdentityResponse.serializer())),
    VIRTUAL_MACHINE_POWER_INFO(SimpleModule(VirtualMachinePowerInfo::class, VirtualMachinePowerInfo.serializer())),
    VIRTUAL_MACHINE_POWER_RESPONSE(SimpleModule(VirtualMachinePowerResponse::class, VirtualMachinePowerResponse.serializer())),
    VSPHERE_AUTHENTICATION_RESPONSE(SimpleModule(VSphereAuthenticationResponse::class, VSphereAuthenticationResponse.serializer()))
}
