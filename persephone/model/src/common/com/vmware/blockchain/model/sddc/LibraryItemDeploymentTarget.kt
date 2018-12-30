/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.sddc

data class LibraryItemDeploymentTarget(
    val resource_pool_id: String,
    val host_id: String? = null,
    val folder_id: String? = null)
