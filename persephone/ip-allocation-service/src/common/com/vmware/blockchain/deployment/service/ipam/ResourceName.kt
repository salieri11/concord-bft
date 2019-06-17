/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.ipam

import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable

@Serializable
data class ResourceName(
    @SerialId(1) val value: String = ""
)
