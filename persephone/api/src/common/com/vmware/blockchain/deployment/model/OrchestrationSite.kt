/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model

import com.vmware.blockchain.deployment.model.core.Endpoint

/**
 * Representation of an orchestration site.
 *
 * @param[type]
 *   type of the the endpoint.
 * @param[info]
 *   info pertaining to the endpoint.
 */
class OrchestrationSite(val type: Type, val info: Info) {
    /**
     * Enumeration of possible [OrchestrationSite] types.
     */
    enum class Type { VMC }

    /**
     * Representation of all possible orchestration sites.
     */
    sealed class Info {
        data class Vmc(val entries: VmcOrchestrationSite): Info()
    }
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
)
