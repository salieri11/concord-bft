/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.deployment

/**
 * Model definition of a component within a deployment of Concord instance.
 *
 * @property[type]
 *   type of the component.
 * @property[name]
 *   name of the component.
 * @property[url]
 *   url of the artifact.
 */
data class ConcordComponent(
    val type: ConcordComponentType,
    val name: String,
    val url: String
)

/**
 * Denote the types of components that can be installed in a Concord deployment instance.
 */
enum class ConcordComponentType {
    DOCKER_IMAGE
}
