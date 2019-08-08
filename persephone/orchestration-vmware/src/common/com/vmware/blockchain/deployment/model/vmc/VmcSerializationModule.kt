/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vmc

import kotlinx.serialization.modules.serializersModuleOf
import kotlinx.serialization.modules.SerialModule

/**
 * Enumeration of all serializable types related to VMC data models.
 */
enum class VmcSerializationModule(val module: SerialModule) {
    SDDC(serializersModuleOf(Sddc::class, Sddc.serializer())),
    SDDC_RESOURCE_CONFIG(serializersModuleOf(SddcResourceConfig::class, SddcResourceConfig.serializer())),
    VMC_AUTHENTICATION_RESPONSE(serializersModuleOf(VmcAuthenticationResponse::class, VmcAuthenticationResponse.serializer()))
}
