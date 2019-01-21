/* **********************************************************************
 * Copyright 2018-2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.vmc

data class VmcAuthenticationResponse(
    val id_token: String,
    val token_type: String,
    val expires_in: Long,
    val scope: String,
    val access_token: String,
    val refresh_token: String
)
