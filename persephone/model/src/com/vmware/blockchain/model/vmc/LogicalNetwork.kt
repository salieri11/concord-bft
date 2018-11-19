/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.vmc

data class LogicalNetwork(
    val name: String,
    val cgwId: String,
    val id: String? = null,
    val cgwName: String? = null,
    val subnets: Subnets? = null,
    val dhcpConfigs: DhcpConfig? = null,
    val l2Extension: L2Extension? = null
)
