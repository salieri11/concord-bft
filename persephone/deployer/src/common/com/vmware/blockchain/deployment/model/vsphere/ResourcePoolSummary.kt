/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class ResourcePoolSummary(val resource_pool: String, val name: String)
