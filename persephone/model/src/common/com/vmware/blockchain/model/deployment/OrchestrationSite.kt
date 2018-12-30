/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.deployment

import com.vmware.blockchain.model.core.Credential
import com.vmware.blockchain.model.core.URI

/**
 * Representation of an orchestration site.
 *
 * @param[type]
 *   type of the the endpoint.
 */
sealed class OrchestrationSite(val type: Type) {
    /**
     * Enumeration of possible [OrchestrationSite] types.
     */
    enum class Type { VMC }

    /**
     * Endpoint information.
     *
     * @param[address]
     *   address of the server endpoint as an [URI].
     * @param[credential]
     *   credential to present to the endpoint during connection.
     */
    data class Endpoint(val address: URI, val credential: Credential)
}

/**
 * Representation of a VMware Cloud-based orchestration site.
 *
 * @param[authentication]
 *   authentication endpoint information.
 * @param[api]
 *   API endpoint information.
 * @param[organization]
 *   VMC organization identifier.
 * @param[datacenter]
 *   SDDC datacenter identifier owned by the organization.
 */
data class VmcOrchestrationSite(
    val authentication: Endpoint,
    val api: Endpoint,
    val organization: String,
    val datacenter: String
) : OrchestrationSite(Type.VMC)
