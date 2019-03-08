/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vmc

import kotlinx.serialization.Serializable

@Serializable
data class LogicalNetwork(
    val name: String,
    val cgwId: String,
    val id: String? = null,
    val cgwName: String? = null,
    val subnets: Subnets? = null,
    val dhcpConfigs: DhcpConfig? = null,
    val l2Extension: L2Extension? = null
)
