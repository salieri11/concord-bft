/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.model.core.URI

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
enum class Endpoints(
    private val path: String,
    private val parameterMappings: Set<String>,
    private val pathMappings: Set<String> = emptySet()
) {
    NSX_API_ROOT(
            "policy/api/v1/{resource}",
            emptySet(),
            setOf("{resource}")
    ),
    NSX_NAT_RULE(
            "policy/api/v1/infra/tier-1s/{tier1}/nat/{nat}/nat-rules/{nat_rule}",
            emptySet(),
            setOf("{tier1}", "{nat}", "{nat_rule}")
    ),
    NSX_NETWORK_SEGMENT(
            "policy/api/v1/infra/tier-1s/{tier1}/segments/{segment}",
            emptySet(),
            setOf("{tier1}", "{segment}")
    ),
    VMC_AUTHENTICATION("/csp/gateway/am/api/auth/api-tokens/authorize", setOf("refresh_token")),
    VMC_PUBLIC_IP(
            "cloud-service/api/v1/infra/public-ips/{ip_id}",
            emptySet(),
            setOf("{ip_id}")
    ),
    VMC_SDDC(
            "/vmc/api/orgs/{org}/sddcs/{sddc}",
            emptySet(),
            setOf("{org}", "{sddc}")
    ),
    VSPHERE_AUTHENTICATION("/rest/com/vmware/cis/session", emptySet()),
    VSPHERE_DATASTORES("/rest/vcenter/datastore", setOf("filter.names")),
    VSPHERE_FOLDERS("/rest/vcenter/folder", setOf("filter.type", "filter.names")),
    VSPHERE_CONTENT_LIBRARY_ITEM(
            "/rest/com/vmware/content/library/item",
            setOf("~action"),
            emptySet()
    ),
    VSPHERE_RESOURCE_POOLS("/rest/vcenter/resource-pool", setOf("filter.hosts", "filter.names")),
    VSPHERE_OVF_LIBRARY_ITEM(
            "/rest/com/vmware/vcenter/ovf/library-item/id:{library_item}",
            setOf("~action"),
            setOf("{library_item}")
    ),
    VSPHERE_NETWORKS("/rest/vcenter/network", setOf("filter.types", "filter.names")),
    VSPHERE_VM("/rest/vcenter/vm/{vm}", emptySet(), setOf("{vm}")),
    VSPHERE_VM_GUEST_IDENTITY("/rest/vcenter/vm/{vm}/guest/identity", emptySet(), setOf("{vm}")),
    VSPHERE_VM_GUEST_POWER("/rest/vcenter/vm/{vm}/guest/power", setOf("action"), setOf("{vm}")),
    VSPHERE_VM_POWER("/rest/vcenter/vm/{vm}/power", emptySet(), setOf("{vm}")),
    VSPHERE_VM_POWER_START("/rest/vcenter/vm/{vm}/power/start", emptySet(), setOf("{vm}")),
    VSPHERE_VM_POWER_STOP("/rest/vcenter/vm/{vm}/power/stop", emptySet(), setOf("{vm}")),
    VSPHERE_VM_POWER_SUSPEND("/rest/vcenter/vm/{vm}/power/suspend", emptySet(), setOf("{vm}")),
    VSPHERE_VMS("/rest/vcenter/vm", setOf("filter.vms"), emptySet());

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