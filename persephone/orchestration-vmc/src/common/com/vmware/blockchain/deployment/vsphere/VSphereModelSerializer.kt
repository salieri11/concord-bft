/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vsphere

import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.vsphere.VSphereSerializationModule
import kotlinx.serialization.modules.SerializersModule

/**
 * Bi-directional JSON serialization support for VMC data models.
 */
object VSphereModelSerializer : JsonSerializer(SerializersModule {
    for (entry in VSphereSerializationModule.values()) {
        include(entry.module)
    }
})