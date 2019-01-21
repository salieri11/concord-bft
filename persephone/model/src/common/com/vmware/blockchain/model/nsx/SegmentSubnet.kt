/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.nsx

data class SegmentSubnet(
    val dhcp_ranges: List<String>? = null,
    val gateway_address: String? = null,
    val network: String? = null
)
