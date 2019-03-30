/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.core

import kotlinx.serialization.Serializable

/**
 * Endpoint information.
 *
 * @param[address]
 *   address of the server endpoint as an [URI].
 * @param[credential]
 *   credential to present to the endpoint during connection.
 */
@Serializable
data class Endpoint(
    @Serializable(with = URISerializer::class) val address: URI,
    val credential: Credential
)
