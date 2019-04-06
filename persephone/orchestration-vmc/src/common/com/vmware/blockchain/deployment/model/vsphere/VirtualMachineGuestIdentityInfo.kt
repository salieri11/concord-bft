/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vsphere

import kotlinx.serialization.Optional
import kotlinx.serialization.Serializable

@Serializable
data class VirtualMachineGuestIdentityInfo(
    val name: String,
    val family: GuestOSFamily,
    val host_name: String,
    val full_name: LocalizableMessage,
    @Optional val ip_address: String? = null
) {
    enum class GuestOSFamily {
        /** Windows operating system */
        WINDOWS,
        /** Linux operating system */
        LINUX,
        /** Novell Netware */
        NETWARE,
        /** Solaris operating system */
        SOLARIS,
        /** Mac OS operating system */
        DARWIN,
        /** Other operating systems */
        OTHER
    }
}
