/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.vmc

import kotlinx.serialization.Serializable

@Serializable
data class VmcAuthenticationResponse(
    val id_token: String? = null,
    val token_type: String,
    val expires_in: Long,
    val scope: String,
    val access_token: String,
    val refresh_token: String
)
