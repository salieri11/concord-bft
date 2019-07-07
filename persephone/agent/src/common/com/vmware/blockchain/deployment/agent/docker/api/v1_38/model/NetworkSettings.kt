/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class NetworkSettings(
    val Bridge: String,
    val SandboxID: String,
    val HairpinMode: Boolean,
    val LinkLocalIPv6Address: String,
    val LinkLocalIPv6PrefixLen: Int,
    val Ports: PortMap,
    val SandboxKey: String,
    val SecondaryIPAddresses: List<Address>?,
    val SecondaryIPv6Addresses: List<Address>?,
    val EndpointID: String,
    val Gateway: String,
    val Networks: Map<String, EndpointSettings>? = null
)
