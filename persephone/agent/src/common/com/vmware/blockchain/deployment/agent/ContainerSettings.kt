/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerClient.ContainerPortBinding
import com.vmware.blockchain.deployment.agent.docker.DockerClient.HostPortBinding

/** Default component artifact target mount path. */
const val CONFIGURATION_MOUNT_PATH = "/config"

/**
 * Specification for customization settings of a container deployment.
 *
 * @property[imageRepository]
 *   image repository name (without registry portion).
 * @property[imageTag]
 *   image tag.
 * @property[containerName]
 *   name of the container to create/deploy for the the image.
 * @property[portBindings]
 *   container-to-host port bindings to use for the deployment.
 * @property[volumeBindings]
 *   volume mount bindings to use for the deployment.
 * @property[environment]
 *   key-value environment bindings for the deployment.
 */
interface ContainerSetting {
    val imageRepository: String?
    val imageTag: String?
    val containerName: String?
    val portBindings: Map<ContainerPortBinding, List<HostPortBinding>?>
    val volumeBindings: Map<String, String>
    val environment: Map<String, String?>
}

/**
 * Enumeration of [ContainerSetting] for known container images.
 *
 * @property[imageRepository]
 *   image repository name (without registry portion).
 * @property[imageTag]
 *   image tag.
 * @property[containerName]
 *   name of the container to create/deploy for the the image.
 * @property[portBindings]
 *   container-to-host port bindings to use for the deployment.
 * @property[volumeBindings]
 *   volume mount bindings to use for the deployment.
 * @property[environment]
 *   key-value environment bindings for the deployment.
 */
enum class ContainerSettings(
    override val imageRepository: String? = null,
    override val imageTag: String? = null,
    override val containerName: String? = null,
    override val portBindings: Map<ContainerPortBinding, List<HostPortBinding>?> = emptyMap(),
    override val volumeBindings: Map<String, String> = emptyMap(),
    override val environment: Map<String, String?> = emptyMap()
) : ContainerSetting {
    DEFAULT,
    CONCORD_CORE_UDP(
            imageRepository = "vmwblockchain/concord-core",
            containerName = "concord",
            portBindings = mapOf(
                    ContainerPortBinding(5458, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 5458)),
                    ContainerPortBinding(3501, ContainerPortBinding.Protocol.UDP) to
                            listOf(HostPortBinding(port = 3501)),
                    ContainerPortBinding(3502, ContainerPortBinding.Protocol.UDP) to
                            listOf(HostPortBinding(port = 3502)),
                    ContainerPortBinding(3503, ContainerPortBinding.Protocol.UDP) to
                            listOf(HostPortBinding(port = 3503)),
                    ContainerPortBinding(3504, ContainerPortBinding.Protocol.UDP) to
                            listOf(HostPortBinding(port = 3504)),
                    ContainerPortBinding(3505, ContainerPortBinding.Protocol.UDP) to
                            listOf(HostPortBinding(port = 3505))
            ),
            volumeBindings = mapOf(
                    "$CONFIGURATION_MOUNT_PATH/concord/config-local" to "/concord/config-local",
                    "$CONFIGURATION_MOUNT_PATH/concord/config-public" to "/concord/config-public"
            )
    ),
    CONCORD_CORE_TLS(
            imageRepository = "vmwblockchain/concord-core",
            containerName = "concord",
            portBindings = mapOf(
                    ContainerPortBinding(5458, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 5458)),
                    ContainerPortBinding(3501, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 3501)),
                    ContainerPortBinding(3502, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 3502)),
                    ContainerPortBinding(3503, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 3503)),
                    ContainerPortBinding(3504, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 3504)),
                    ContainerPortBinding(3505, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 3505))
            ),
            volumeBindings = mapOf(
                    "$CONFIGURATION_MOUNT_PATH/concord/config-local" to "/concord/config-local",
                    "$CONFIGURATION_MOUNT_PATH/concord/config-public" to "/concord/config-public"
            )
    ),
    ETHEREUM_RPC(
            imageRepository = "vmwblockchain/ethrpc",
            containerName = "ethrpc",
            portBindings = mapOf(
                    ContainerPortBinding(8545, ContainerPortBinding.Protocol.TCP) to
                            listOf(HostPortBinding(port = 8545))
            ),
            environment = mapOf("CONCORD_AUTHORITIES" to "concord:5458")
    );
}
