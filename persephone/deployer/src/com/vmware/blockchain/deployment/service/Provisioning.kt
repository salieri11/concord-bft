/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.service

import com.vmware.blockchain.model.deployment.ConcordCluster
import com.vmware.blockchain.model.deployment.ConcordClusterIdentifier
import com.vmware.blockchain.model.deployment.ConcordClusterSpecification

/**
 * Denote the contract of the provisioning service for Concord cluster deployment.
 */
interface Provisioning {
    /**
     * Create a new Concord cluster deployment according the supplied specification.
     *
     * @param[specification]
     *   intended cluster specification to use for deployment.
     *
     * @return
     *   identifier of the deployed Concord cluster, if created.
     */
    fun create(specification: ConcordClusterSpecification): ConcordClusterIdentifier?

    /**
     * Retrieve the cluster deployment model associated with a given [ConcordClusterIdentifier].
     *
     * @param[identifier]
     *   identifier of the cluster deployment.
     *
     * @return
     *   cluster model corresponding to the identifier, if found.
     */
    fun get(identifier: ConcordClusterIdentifier): ConcordCluster

    // TODO(jameschang - 2018-12-18):
    // Need to decide on how to PUT/PATCH an existing deployment into management service(s).
    //fun update(
    //    identifier: ConcordClusterIdentifier,
    //    specification: ConcordClusterSpecification
    //): ConcordCluster
}
