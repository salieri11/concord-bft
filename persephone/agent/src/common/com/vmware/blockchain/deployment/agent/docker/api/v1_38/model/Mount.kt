/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class Mount(
    val Type: MountType,
    val Target: String? = null,
    val Source: String? = null,
    val ReadOnly: Boolean = false,
    val Consistency: String? = null,
    val BindOptions: BindTypeOptions? = null,
    val VolumeOptions: VolumeTypeOptions? = null,
    val TmpfsOptions: TmpfsTypeOptions? = null
) {
    enum class MountType {
        bind,
        volume,
        tmpfs,
        npipe
    }

    @Serializable
    data class BindTypeOptions(
        val Propagation: PropagationType,
        val NonRecursive: Boolean = false
    ) {
        enum class PropagationType {
            `private`,
            rprivate,
            shared,
            rshared,
            slave,
            rslave,
        }
    }

    @Serializable
    data class VolumeTypeOptions(
        val NoCopy: Boolean = false,
        val Labels: Map<String, String>? = null,
        val DriverConfig: VolumeDriverConfig? = null
    ) {
        @Serializable
        data class VolumeDriverConfig(
            val Name: String,
            val Options: Map<String, String>
        )
    }

    @Serializable
    data class TmpfsTypeOptions(
        val SizeBytes: Long,
        val Mode: Int
    )
}
