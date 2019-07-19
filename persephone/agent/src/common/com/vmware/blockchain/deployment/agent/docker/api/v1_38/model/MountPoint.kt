/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class MountPoint(
    val Type: String,
    val Name: String,
    val Source: String,
    val Destination: String,
    val Driver: String,
    val Mode: String,
    val RW: Boolean,
    val Propagation: String
)
