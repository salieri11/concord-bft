/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.Serializable

@Serializable
data class Segment(
    val id: String? = null,
    val path: String? = null,
    val type: Type? = null,
    val subnets: List<SegmentSubnet>? = null
) {
    enum class Type {
        ROUTED,
        EXTENDED,
        DISCONNECTED
    }
}
