/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class LibraryItemFindSpec(
    val name: String? = null,
    val library_id: String? = null,
    val source_id: String? = null,
    val type: String? = null,
    val cached: Boolean? = null
)
