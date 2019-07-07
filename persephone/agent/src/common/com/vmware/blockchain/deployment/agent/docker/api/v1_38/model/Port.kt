/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class Port(
    val Type: PortType,
    val PrivatePort: Int,
    val PublicPort: Int = 0,
    val IP: String? = null
) {
    enum class PortType {
        tcp,
        udp,
        sctp
    }
}
