/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.core

import kotlinx.serialization.ContextualSerialization
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
    @ContextualSerialization val address: URI,
    val credential: Credential
)
