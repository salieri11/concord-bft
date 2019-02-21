/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class LibraryItemDeploymentSpec(
    val accept_all_EULA: Boolean,
    val name: String? = null,
    val annotation: String? = null,
    val default_datastore_id: String? = null,
    val storage_provisioning: String? = "thin",
    val network_mappings: List<NetworkMapping> = emptyList(),
    val additional_parameters: List<OvfParameter> = emptyList()
)
