/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class Ipam(
    val Driver: String = "default",
    val Config: List<Map<String, String>> = emptyList(),
    val Options: Map<String, String>? = null
)
