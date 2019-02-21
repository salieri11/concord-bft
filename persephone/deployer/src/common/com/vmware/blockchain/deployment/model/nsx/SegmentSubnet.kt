/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.Serializable

@Serializable
data class SegmentSubnet(
    val dhcp_ranges: List<String>? = null,
    val gateway_address: String? = null,
    val network: String? = null
)
