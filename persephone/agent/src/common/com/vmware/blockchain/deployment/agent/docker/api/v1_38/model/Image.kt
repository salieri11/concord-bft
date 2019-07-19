/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class Image(
    val Id: String,
    val Parent: String,
    val Comment: String,
    val Created: String,
    val Container: String,
    val DockerVersion: String,
    val Author: String,
    val Architecture: String,
    val Os: String,
    val Size: Long,
    val VirtualSize: Long,
    val RootFS: RootFSInfo,
    val GraphDriver: GraphDriverData,
    val OsVersion: String? = null,
    val ContainerConfig: ContainerConfig? = null,
    val Config: ContainerConfig? = null,
    val RepoTags: List<String>? = null,
    val RepoDigests: List<String>? = null,
    val Metadata: MetadataInfo? = null
) {
    @Serializable
    data class RootFSInfo(
        val Type: String,
        val Layers: List<String>? = null,
        val BaseLayer: String? = null
    )

    @Serializable
    data class MetadataInfo(val LastTagTime: String)
}