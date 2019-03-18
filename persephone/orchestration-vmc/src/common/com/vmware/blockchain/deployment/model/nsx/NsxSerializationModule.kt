/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.nsx

import kotlinx.serialization.context.SimpleModule

/**
 * Enumeration of all serializable types related to NSX data models.
 */
enum class NsxSerializationModule(val module: SimpleModule<*>) {
    SEGMENT(SimpleModule(Segment::class, Segment.serializer())),
    SEGMENT_SUBNET(SimpleModule(SegmentSubnet::class, SegmentSubnet.serializer()))
}
