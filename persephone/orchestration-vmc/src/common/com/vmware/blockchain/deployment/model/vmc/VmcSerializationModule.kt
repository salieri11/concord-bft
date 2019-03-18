/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vmc

import kotlinx.serialization.context.SimpleModule

/**
 * Enumeration of all serializable types related to VMC data models.
 */
enum class VmcSerializationModule(val module: SimpleModule<*>) {
    ADDRESS_GROUP(SimpleModule(AddressGroup::class, AddressGroup.serializer())),
    DHCP_CONFIG(SimpleModule(DhcpConfig::class, DhcpConfig.serializer())),
    DHCP_IP_POOL(SimpleModule(DhcpIpPool::class, DhcpIpPool.serializer())),
    GET_LOGICAL_NETWORK_RESPONSE(SimpleModule(GetLogicalNetworkResponse::class, GetLogicalNetworkResponse.serializer())),
    L2_EXTENSION(SimpleModule(L2Extension::class, L2Extension.serializer())),
    LOGICAL_NETWORK(SimpleModule(LogicalNetwork::class, LogicalNetwork.serializer())),
    SDDC(SimpleModule(Sddc::class, Sddc.serializer())),
    SDDC_RESOURCE_CONFIG(SimpleModule(SddcResourceConfig::class, SddcResourceConfig.serializer())),
    SUBNETS(SimpleModule(Subnets::class, Subnets.serializer())),
    VMC_AUTHENTICATION_RESPONSE(SimpleModule(VmcAuthenticationResponse::class, VmcAuthenticationResponse.serializer()))
}
