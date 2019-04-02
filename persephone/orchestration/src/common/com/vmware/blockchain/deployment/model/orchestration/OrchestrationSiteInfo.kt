/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.orchestration

import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier
import com.vmware.blockchain.deployment.model.core.Endpoint
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.map

/**
 * Representation of an orchestration site.
 *
 * @param[type]
 *   type of the the endpoint.
 * @param[vmc]
 *   information pertaining to a [Type.VMC] type endpoint.
 */
@Serializable
data class OrchestrationSiteInfo(val type: Type, val vmc: Vmc? = null) {
    /**
     * Enumeration of possible [OrchestrationSiteInfo] types.
     */
    enum class Type { VMC }

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
    @Serializable
    data class Vmc(
        val authentication: Endpoint,
        val api: Endpoint,
        val organization: String,
        val datacenter: String
    )
}

/**
 * Obtain a global serializer for an associative mapping of [OrchestrationSiteIdentifier] to
 * [OrchestrationSiteInfo].
 *
 * @return
 *   a [KSerializer] instance for two-way serialization of mapping entries.
 */
fun getOrchestrationSiteMapSerializer(
): KSerializer<Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo>> {
    return (OrchestrationSiteIdentifier.serializer() to OrchestrationSiteInfo.serializer()).map
}
