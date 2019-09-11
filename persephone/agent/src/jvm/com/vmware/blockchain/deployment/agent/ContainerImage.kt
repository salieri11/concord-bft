/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent

import com.vmware.blockchain.deployment.agent.docker.DockerHttpClient
import com.vmware.blockchain.deployment.logging.Logger
import com.vmware.blockchain.deployment.logging.logger
import java.util.regex.Pattern

/**
 * Representation of a container image in its tagged and named parts (registry, repository, tag).
 *
 * @property[registry]
 *   registry that this container image belongs to.
 * @property[repository]
 *   repository of this container image.
 * @property[tag]
 *   tag of this particular image instance within the repository.
 */
data class ContainerImage(
    val registry: String,
    val repository: String,
    val tag: String
) {
    companion object {
        private val log: Logger by logger()

        @JvmStatic
        private val IMAGE_NAME_PATTERN = Pattern.compile(
                "(?:(?<registry>[^:]+:?[0-9]+)/)?(?<repository>[^:]+)(?::(?<tag>.+))?"
        )

        @JvmStatic
        fun newContainerImage(name: String): ContainerImage {
            // Setup default values in case parsing was unsuccessful.
            var registry = DockerHttpClient.DEFAULT_CONTAINER_REGISTRY.address
            var repository = name
            var tag = "latest"

            try {
                val matcher = IMAGE_NAME_PATTERN.matcher(name)
                if (matcher.matches()) {
                    if (matcher.group("registry") != null) {
                        registry = matcher.group("registry")
                    }
                    if (matcher.group("repository") != null) {
                        repository = matcher.group("repository")
                    }
                    if (matcher.group("tag") != null) {
                        tag = matcher.group("tag")
                    }
                }
            } catch (error: Throwable) {
                log.error("Encountered error while parsing name, using default values", error)
            }

            return ContainerImage(registry, repository, tag)
        }
    }
}
