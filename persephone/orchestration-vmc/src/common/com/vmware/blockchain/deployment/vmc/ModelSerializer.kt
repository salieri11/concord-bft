/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.nsx.NsxSerializationModule
import com.vmware.blockchain.deployment.model.vsphere.VSphereSerializationModule
import com.vmware.blockchain.deployment.model.vmc.VmcSerializationModule

/**
 * Bi-directional JSON serialization support for VMC data models.
 */
object ModelSerializer : JsonSerializer() {

    init {
        for (entry in NsxSerializationModule.values()) {
            install(entry.module)
        }
        for (entry in VSphereSerializationModule.values()) {
            install(entry.module)
        }
        for (entry in VmcSerializationModule.values()) {
            install(entry.module)
        }
    }
}
