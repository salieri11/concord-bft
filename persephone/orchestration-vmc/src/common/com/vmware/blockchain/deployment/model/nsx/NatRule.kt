/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.Optional
import kotlinx.serialization.Serializable

@Serializable
data class NatRule(
    val action: Action,
    @Optional val enabled: Boolean = true,
    @Optional val logging: Boolean = false,
    @Optional val source_network: String? = null,
    @Optional val destination_network: String? = null,
    @Optional val translated_network: String? = null,
    @Optional val translated_ports: String? = null,
    @Optional val sequence_number: Int = 0,
    @Optional val service: String? = null,
    @Optional val firewall_match: FirewallMatch? = null,
    @Optional val scope: List<String> = emptyList()
) {
    enum class Action {
        SNAT,
        DNAT,
        REFLEXIVE,
        NO_SNAT,
        NO_DNAT,
    }

    enum class FirewallMatch {
        MATCH_EXTERNAL_ADDRESS,
        MATCH_INTERNAL_ADDRESS,
        BYPASS
    }
}
