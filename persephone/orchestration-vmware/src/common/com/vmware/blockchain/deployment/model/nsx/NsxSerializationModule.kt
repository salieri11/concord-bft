/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.modules.serializersModuleOf
import kotlinx.serialization.modules.SerialModule

/**
 * Enumeration of all serializable types related to NSX data models.
 */
enum class NsxSerializationModule(val module: SerialModule) {
    NAT_RULE(serializersModuleOf(NatRule::class, NatRule.serializer())),
    PUBLIC_IP(serializersModuleOf(PublicIP::class, PublicIP.serializer())),
    SEGMENT(serializersModuleOf(Segment::class, Segment.serializer())),
    SEGMENT_SUBNET(serializersModuleOf(SegmentSubnet::class, SegmentSubnet.serializer()))
}
