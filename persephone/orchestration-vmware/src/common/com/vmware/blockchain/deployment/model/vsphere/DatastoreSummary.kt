/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class DatastoreSummary(
    val datastore: String,
    val name: String,
    val type: String,
    val free_space: Long,
    val capacity: Long
)
