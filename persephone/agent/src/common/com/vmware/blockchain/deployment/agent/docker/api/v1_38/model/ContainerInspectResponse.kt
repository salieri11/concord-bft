/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class ContainerInspectResponse(
    val Id: String,
    val Created: String,
    val Path: String,
    val Args: List<String>,
    val State: ContainerState,
    val Image: String,
    val ResolvConfPath: String,
    val HostnamePath: String,
    val HostsPath: String,
    val LogPath: String,
    val Name: String,
    val RestartCount: Long,
    val Driver: String,
    val MountLabel: String,
    val ProcessLabel: String,
    val AppArmorProfile: String,
    val ExecIDs: List<String>?,
    val HostConfig: HostConfig,
    val GraphDriver: GraphDriverData,
    val SizeRw: Long = 0,
    val SizeRootFs: Long = 0,
    val Mount: List<MountPoint>? = null,
    val NetworkSettings: NetworkSettings,
    val Config: ContainerConfig,
    val Node: Map<String, String>? = null // Listed as "TO DO" on Swagger YAML.
) {
    @Serializable
    data class ContainerState(
        val Status: ContainerStatus,
        val Running: Boolean,
        val Paused: Boolean,
        val Restarting: Boolean,
        val OOMKilled: Boolean,
        val Dead: Boolean,
        val Pid: Int,
        val ExitCode: Int,
        val Error: String,
        val StartedAt: String,
        val FinishedAt: String
    ) {
        enum class ContainerStatus {
            created,
            running,
            paused,
            restarting,
            removing,
            exited,
            dead
        }
    }
}