/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class Network(
    val Name: String? = null,
    val Id: String,
    val Created: String? = null,
    val Scope: String? = null,
    val Driver: String? = null,
    val EnableIPv6: Boolean = false,
    val IPAM: Ipam? = null,
    val Attachable: Boolean = false,
    val Ingress: Boolean = false,
    val Containers: Map<String, NetworkContainer> = emptyMap(),
    val Options: Map<String, String> = emptyMap(),
    val Labels: Map<String, String> = emptyMap()
)
