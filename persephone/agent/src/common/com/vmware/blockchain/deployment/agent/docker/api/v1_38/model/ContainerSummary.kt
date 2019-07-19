/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class ContainerSummary(
    val Id: String,
    val Names: List<String>,
    val Image: String,
    val ImageID: String,
    val Command: String,
    val Created: Long,
    val Ports: List<Port>,
    val SizeRw: Long = 0,
    val SizeRootFs: Long = 0,
    val Labels: Map<String, String>,
    val State: String,
    val Status: String,
    val HostConfig: Map<String, String>,
    val NetworkSettings: ContainerNetworkSettings,
    val Mounts: List<Mount>
) {
    @Serializable
    data class ContainerNetworkSettings(
        val Networks: Map<String, EndpointSettings>? = null
    )
}
