/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vmc

import kotlinx.serialization.context.SimpleModule

/**
 * Enumeration of all serializable types related to VMC data models.
 */
enum class VmcSerializationModule(val module: SimpleModule<*>) {
    SDDC(SimpleModule(Sddc::class, Sddc.serializer())),
    SDDC_RESOURCE_CONFIG(SimpleModule(SddcResourceConfig::class, SddcResourceConfig.serializer())),
    VMC_AUTHENTICATION_RESPONSE(SimpleModule(VmcAuthenticationResponse::class, VmcAuthenticationResponse.serializer()))
}
