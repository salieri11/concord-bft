/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.agent.docker.api.v1_38

import com.vmware.blockchain.deployment.http.EndpointEnumeration

/* Constants. */
const val API_VERSION: String = "v1.38"

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
enum class Endpoints(
    override val path: String,
    override val parameterMappings: Set<String> = emptySet(),
    override val pathMappings: Set<String> = emptySet()
) : EndpointEnumeration {

    CONTAINERS_CREATE("/$API_VERSION/containers/create", setOf("name")),
    CONTAINERS_DELETE("/$API_VERSION/containers/{id}", setOf("v", "force", "link"), setOf("{id}")),
    CONTAINERS_INSPECT("/$API_VERSION/containers/{id}/json", setOf("size"), setOf("{id}")),
    CONTAINERS_LIST("/$API_VERSION/containers/json", setOf("all", "limit", "size", "filters")),
    CONTAINERS_START("/$API_VERSION/containers/{id}/start", setOf("detachKeys"), setOf("{id}")),
    CONTAINERS_STOP("/$API_VERSION/containers/{id}/stop", setOf("t"), setOf("{id}")),
    IMAGES_CREATE("/$API_VERSION/images/create", setOf("fromImage", "fromSrc", "repo", "tag", "platform")),
    IMAGES_INSPECT("/$API_VERSION/images/{name}/json", emptySet(), setOf("{name}")),
    IMAGES_LIST("/$API_VERSION/images/json", setOf("all", "filters", "digests")),
    NETWORKS_CREATE("/$API_VERSION/networks/create"),
    NETWORKS_LIST("/$API_VERSION/networks", setOf("filters"))
}
