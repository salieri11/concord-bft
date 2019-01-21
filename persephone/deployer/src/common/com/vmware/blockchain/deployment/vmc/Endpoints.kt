/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.model.core.URI

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
enum class Endpoints(
    private val path: String,
    private val parameterMappings: Set<String>,
    private val pathMappings: Set<String> = emptySet()
) {
    NSX_NETWORK_SEGMENT(
            "policy/api/v1/infra/tier-1s/{tier1}/segments/{segment}",
            emptySet(),
            setOf("{tier1}", "{segment}")
    ),
    VMC_AUTHENTICATION("/csp/gateway/am/api/auth/api-tokens/authorize", setOf("refresh_token")),
    VMC_LOGICAL_NETWORK(
            "/vmc/api/orgs/{org}/sddcs/{sddc}/networks/4.0/sddc/networks/{network}",
            emptySet(),
            setOf("{org}", "{sddc}", "{network}")
    ),
    VMC_LOGICAL_NETWORKS(
            "/vmc/api/orgs/{org}/sddcs/{sddc}/networks/4.0/sddc/networks",
            emptySet(),
            setOf("{org}", "{sddc}")
    ),
    VMC_SDDC(
            "/vmc/api/orgs/{org}/sddcs/{sddc}",
            emptySet(),
            setOf("{org}", "{sddc}")
    ),
    VSPHERE_AUTHENTICATION("/rest/com/vmware/cis/session", emptySet()),
    VSPHERE_DATASTORES("/rest/vcenter/datastore", setOf("filter.names")),
    VSPHERE_FOLDERS("/rest/vcenter/folder", setOf("filter.type", "filter.names")),
    VSPHERE_RESOURCE_POOLS("/rest/vcenter/resource-pool", setOf("filter.hosts", "filter.names")),
    VSPHERE_OVF_LIBRARY_ITEM(
            "/rest/com/vmware/vcenter/ovf/library-item/id:{library_item}",
            setOf("~action"),
            setOf("{library_item}")
    ),
    VSPHERE_NETWORKS("/rest/vcenter/network", setOf("filter.types", "filter.names"));

    /**
     * Interpolate the expected URL by associating the given set of parameters with values.
     *
     * @param[parameters]
     *   list of pairs of parameter name and values.
     * @param[pathVariables]
     *   list of pairs of path variable name and values.
     *
     * @return
     *   the expected URL, as a [String].
     */
    fun interpolate(
        parameters: List<Pair<String, String>> = emptyList(),
        pathVariables: List<Pair<String, String>> = emptyList()
    ): String {
        val unresolved = path
        val resolved = pathVariables
                .filter { pathMappings.contains(it.first) }
                .fold(unresolved) {
                    current, mapping -> current.replace(mapping.first, mapping.second)
                }
        return parameters
                .filter { parameterMappings.contains(it.first) }
                .takeIf { it.isNotEmpty() }
                ?.joinToString(prefix = "$resolved?", separator = "&",
                               transform = { (key, value) -> "$key=$value" })
                ?: resolved
    }

    /**
     * Resolve the endpoint.
     *
     * @param[base]
     *   base URI of the endpoint.
     * @param[parameters]
     *   list of pairs of parameter name and values.
     * @param[pathVariables]
     *   list of pairs of path variable name and values.
     *
     * @return
     *   a resolved URL, as a [URI].
     */
    fun resolve(
        base: URI,
        parameters: List<Pair<String, String>> = emptyList(),
        pathVariables: List<Pair<String, String>> = emptyList()
    ): URI {
        return base.resolve(interpolate(parameters, pathVariables))
    }
}