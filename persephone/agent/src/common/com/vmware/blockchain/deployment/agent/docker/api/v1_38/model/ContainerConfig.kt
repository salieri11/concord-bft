/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38.model

import kotlinx.serialization.Serializable

@Serializable
data class ContainerConfig(
    val Image: String,
    val Hostname: String? = null,
    val Domainname: String? = null,
    val User: String? = null,
    val AttachStdin: Boolean = false,
    val AttachStdout: Boolean = true,
    val AttachStderr: Boolean = true,
    val ExposedPorts: Map<String, Empty>? = null,
    val Tty: Boolean = false,
    val OpenStdin: Boolean = false,
    val StdinOnce: Boolean = false,
    val Env: List<String>? = null,
    val Cmd: List<String>? = null,
    val ArgsEscaped: Boolean = false,
    val Volumes: Map<String, Empty>? = null,
    val WorkingDir: String? = null,
    val Entrypoint: List<String>? = null,
    val NetworkDisabled: Boolean = false,
    val MacAddress: String? = null,
    val OnBuild: List<String>? = null,
    val Labels: Map<String, String>? = null,
    val StopSignal: String = "SIGTERM",
    val StopTimeout: Int = 10,
    val Shell: List<String>? = null,
    val Healthcheck: HealthConfig? = null,
    val HostConfig: HostConfig? = null,
    val NetworkingConfig: ContainerNetworkingConfig? = null
) {
    @Serializable
    data class ContainerNetworkingConfig(
        val EndpointsConfig: Map<String, EndpointSettings>? = null
    )
}
