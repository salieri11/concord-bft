/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.internal.CommonEnumSerializer

@Serializable
data class HostConfig(
    val Binds: List<String>? = null,
    val ContainerIDFile: String? = null,
    val LogConfig: LoggingConfig? = null,
    val NetworkMode: String? = null,
    val PortBindings: Map<String, List<PortBinding>?>? = null,
    val RestartPolicy: RestartPolicy? = null,
    val AutoRemove: Boolean = false,
    val VolumeDriver: String? = null,
    val VolumesFrom: List<String>? = null,
    val Mounts: List<Mount>? = null,
    val Capabilities: List<String>? = null,
    val CapAdd: List<String>? = null,
    val CapDrop: List<String>? = null,
    val CgroupnsMode: CgroupNamespaceMode? = null,
    val Dns: List<String>? = null,
    val DnsOption: List<String>? = null,
    val DnsSearch: List<String>? = null,
    val ExtraHosts: List<String>? = null,
    val GroupAdd: List<String>? = null,
    val IpcMode: String? = null,
    val Cgroup: String? = null,
    val Links: List<String>? = null,
    val OomScoreAdj: Int = 0,
    val PidMode: String? = null,
    val Privileged: Boolean = false,
    val PublishAllPorts: Boolean = false,
    val ReadonlyRootfs: Boolean = false,
    val SecurityOpt: List<String>? = null,
    val StorageOpt: Map<String, String>? = null,
    val Tmpfs: Map<String, String>? = null,
    val UTSMode: String? = null,
    val UsernsMode: String? = null,
    val ShmSize: Long = 64 * 1024 * 1204,
    val Sysctls: Map<String, String>? = null,
    val Runtime: String? = null,
    val ConsoleSize: List<Int>? = null,
    val Isolation: IsolationMode? = null,
    val MaskedPaths: List<String>? = null,
    val ReadonlyPaths: List<String>? = null
) {
    enum class CgroupNamespaceMode {
        private,
        host
    }

    @Serializable(with = IsolationModeSerializer::class)
    enum class IsolationMode {
        empty,
        default,
        process,
        hyperv
    }

    object IsolationModeSerializer : CommonEnumSerializer<IsolationMode>(
            "IsolationMode",
            arrayOf(
                    IsolationMode.empty,
                    IsolationMode.default,
                    IsolationMode.process,
                    IsolationMode.hyperv
            ),
            arrayOf("", "default", "process", "hyperv")
    )

    @Serializable
    data class LoggingConfig(
        val Type: LoggingType,
        val Config: Map<String, String>? = null
    ) {
        enum class LoggingType {
            `json-file`,
            syslog,
            journald,
            gelf,
            fluentd,
            awslogs,
            splunk,
            etwlogs,
            none
        }
    }
}
