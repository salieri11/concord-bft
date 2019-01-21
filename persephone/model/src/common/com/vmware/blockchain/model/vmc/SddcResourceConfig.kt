/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.vmc

data class SddcResourceConfig(
    val provider: SddcResourceProvider,
    val vc_url: String,
    val cloud_username: String,
    val cloud_password: String,
    val nsx_mgr_url: String,
    val nsx_api_public_endpoint_url: String) {

    enum class SddcResourceProvider {
        AWS
    }
}
