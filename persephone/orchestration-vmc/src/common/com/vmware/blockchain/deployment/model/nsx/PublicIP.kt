/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.Serializable

@Serializable
data class PublicIP(
    val id: String? = null,
    val display_name: String? = null,
    val ip: String? = null
)
