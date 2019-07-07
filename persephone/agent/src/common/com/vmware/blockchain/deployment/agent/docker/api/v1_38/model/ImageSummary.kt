/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class ImageSummary(
    val Id: String,
    val ParentId: String? = null,
    val Created: Long = 0,
    val Size: Long = 0,
    val SharedSize: Long = 0,
    val VirtualSize: Long = 0,
    val Containers: Long = 0,
    val RepoTags: List<String>? = null,
    val RepoDigests: List<String>? = null,
    val Labels: Map<String, String>? = null
)
