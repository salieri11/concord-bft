/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.Optional
import kotlinx.serialization.Serializable

@Serializable
data class PublicIP(
    @Optional val id: String? = null,
    @Optional val display_name: String? = null,
    @Optional val ip: String? = null
)
