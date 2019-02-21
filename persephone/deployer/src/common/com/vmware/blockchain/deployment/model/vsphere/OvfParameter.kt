/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class OvfParameter(
    val `@class`: String,
    val type: String,
    val properties: List<OvfProperty> = emptyList()
)

enum class OvfParameterTypes(val classValue: String, val type: String) {
    PROPERTY_PARAMS("com.vmware.vcenter.ovf.property_params", "PropertyParams")
}
