/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.service

import com.vmware.blockchain.model.deployment.ConcordModel
import com.vmware.blockchain.model.deployment.ConcordModelIdentifier

/**
 * Denote the contract of the image repository service for Concord cluster deployment.
 */
interface Repository {
    /**
     * Create a deployment image based on a [ConcordModel].
     *
     * @param[model]
     *   model specification of the deployment image.
     *
     * @return
     *   identifier of the deployment image, if found.
     */
    fun create(model: ConcordModel): ConcordModelIdentifier?

    /**
     * Retrieve the deployment model associated with a given [ConcordModelIdentifier].
     *
     * @param[identifier]
     *   identifier of the deployment image.
     *
     * @return
     *   deployment model corresponding to the identifier, if found.
     */
    fun get(identifier: ConcordModelIdentifier): ConcordModel

    /**
     * Delete the deployment image associated with a given [ConcordModelIdentifier].
     *
     * @param[identifier]
     *   identifier of the deployment image.
     *
     * @return
     *   `true` if deleted, `false` otherwise.
     */
    fun delete(identifier: ConcordModelIdentifier): Boolean

    /**
     * List all known deployment images.
     *
     * @return
     *   [Map] entries of [ConcordModelIdentifier] to the associated [ConcordModel].
     */
    fun list(): Map<ConcordModelIdentifier, ConcordModel>
}
