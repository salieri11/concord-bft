/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.Serializable

@Serializable
data class NatRule(
    val action: Action,
    val id: String? = null,
    val display_name: String? = null,
    val path: String? = null,
    val enabled: Boolean = true,
    val logging: Boolean = false,
    val source_network: String? = null,
    val destination_network: String? = null,
    val translated_network: String? = null,
    val translated_ports: String? = null,
    val sequence_number: Int = 0,
    val service: String? = null,
    val firewall_match: FirewallMatch? = null,
    val scope: List<String> = emptyList()
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
