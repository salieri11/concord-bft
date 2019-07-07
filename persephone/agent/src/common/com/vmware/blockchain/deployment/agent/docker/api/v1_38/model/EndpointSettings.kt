/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class EndpointSettings(
    val IPAMConfig: EndpointIPAMConfig? = null,
    val Links: List<String>? = null,
    val Aliases: List<String>? = null,
    val NetworkID: String? = null,
    val EndpointID: String? = null,
    val Gateway: String? = null,
    val IPAddress: String? = null,
    val IPPrefixLen: Int? = null,
    val IPv6Gateway: String? = null,
    val GlobalIPv6Address: String? = null,
    val GlobalIPv6PrefixLen: Long? = null,
    val MacAddress: String? = null,
    val DriverOpts: Map<String, String>? = null
)
