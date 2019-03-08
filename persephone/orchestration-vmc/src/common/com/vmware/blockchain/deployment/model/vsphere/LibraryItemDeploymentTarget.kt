/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class LibraryItemDeploymentTarget(
    val resource_pool_id: String,
    val host_id: String? = null,
    val folder_id: String? = null)
