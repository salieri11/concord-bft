/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.orchestration

import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo
import kotlinx.serialization.KSerializer
import kotlinx.serialization.map

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
