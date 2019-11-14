/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class NetworkConfig(
    val Name: String? = "",
    val CheckDuplicate: Boolean = false,
    val Driver: String = "bridge",
    val Internal: Boolean = false,
    val Attachable: Boolean = false,
    val Ingress: Boolean = false,
    val IPAM: Ipam? = null,
    val EnableIPv6: Boolean = false,
    val Options: Map<String, String> = emptyMap(),
    val Labels: Map<String, String> = emptyMap()
)
