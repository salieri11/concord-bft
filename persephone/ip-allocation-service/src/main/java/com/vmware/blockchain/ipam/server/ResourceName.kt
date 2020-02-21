/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/

package com.vmware.blockchain.ipam.server

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable

@Serializable
data class ResourceName (val value: String = "") {
    companion object {
        fun getSerializer(): KSerializer<ResourceName> = serializer()
    }
}