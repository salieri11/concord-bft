/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Serializable

@Serializable
data class LocalizableMessage(
    val id: String,
    val default_message: String,
    val args: List<String>
)
