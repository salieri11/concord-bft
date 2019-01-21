/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.deployment

import com.vmware.blockchain.model.core.UUID
import kotlinx.serialization.ContextualSerialization
import kotlinx.serialization.Serializable

/**
 * Denote the identifier of a [ConcordModel].
 */
typealias ConcordClusterIdentifier = UUID

/**
 * Specification of a deployment intent to deploy a cluster of Concord instances.
 *
 * @property[orchestrations]
 *   entries of orchestrator endpoints (failure domains) and credentials to utilize for deployment.
 * @property[name]
 *   unique name of the cluster deployment.
 * @property[image]
 *   identifier of the [ConcordModel] to use for deployment.
 * @property[size]
 *   size of the cluster deployment.
 */
@Serializable
data class ConcordClusterSpecification(
    val orchestrations: List<OrchestrationSiteIdentifier>,
    val name: String,
    @ContextualSerialization val image: ConcordModelIdentifier,
    val size: Short
)

/**
 * Model definition of a deployed Concord cluster.
 *
 * @property[instances]
 *   entries of [OrchestrationSiteIdentifier]s and the deployed [ConcordInstance]s hosted by it.
 */
@Serializable
data class ConcordCluster(
    val instances: Map<OrchestrationSiteIdentifier, Set<ConcordInstance>>
)
