/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vsphere

import com.vmware.blockchain.deployment.http.EndpointEnumeration

/**
 * Enumeration of URI endpoint with expected parameter names.
 */
enum class Endpoints(
    override val path: String,
    override val parameterMappings: Set<String>,
    override val pathMappings: Set<String> = emptySet()
) : EndpointEnumeration {

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
    VSPHERE_VM_POWER_SUSPEND("/rest/vcenter/vm/{vm}/power/suspend", emptySet(), setOf("{vm}"));
}
