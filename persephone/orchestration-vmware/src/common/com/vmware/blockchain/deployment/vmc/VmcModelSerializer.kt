/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vmc

import com.vmware.blockchain.deployment.http.JsonSerializer
import com.vmware.blockchain.deployment.model.nsx.NsxSerializationModule
import com.vmware.blockchain.deployment.model.vmc.VmcSerializationModule
import kotlinx.serialization.modules.SerializersModule

/**
 * Bi-directional JSON serialization support for VMC data models.
 */
object VmcModelSerializer : JsonSerializer(SerializersModule {
    for (entry in NsxSerializationModule.values()) {
        include(entry.module)
    }
    for (entry in VmcSerializationModule.values()) {
        include(entry.module)
    }
})
