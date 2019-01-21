/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.deployment

import com.vmware.blockchain.model.core.PublicKey
import com.vmware.blockchain.model.core.URI
import com.vmware.blockchain.model.core.UUID
import kotlinx.serialization.ContextualSerialization
import kotlinx.serialization.Serializable

/**
 * Denote the identifier of a [ConcordModel].
 */
typealias ConcordModelIdentifier = UUID

/**
 * Model definition of a versioned Concord template image.
 *
 * @property[version]
 *   version of the image.
 * @property[template]
 *   name of the template entity to use as housing.
 *
 *   Note: Deployment target (e.g. denoted instance of [OrchestrationSite]) is expected to
 *   translate this value into template entity to be used internally for deployment workflow.
 * @property[components]
 *   Positionally ordered list of components to be deployed within a deployment unit.
 *
 *   Note: Orchestration engine may utilize the sequence order during deployment workflow.
 */
@Serializable
data class ConcordModel(
    val version: String,
    val template: String,
    val components: List<ConcordComponent>
)

/**
 * Model definition of a deployed Concord instance.
 *
 * @property[resource]
 *   Concord instance's queryable resource (IP or DNS-resolvable hostname).
 * @property[publicKey]
 *   instance's public key.
 * @property[model]
 *   identifier of the [ConcordModel] used to create the instance.
 */
@Serializable
data class ConcordInstance(
    @ContextualSerialization val resource: URI,
    val publicKey: PublicKey,
    @ContextualSerialization val model: ConcordModelIdentifier
)
