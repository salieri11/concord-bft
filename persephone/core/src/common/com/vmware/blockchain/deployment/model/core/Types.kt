/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.core

import kotlinx.serialization.KSerializer

/* ******************************************************************************************
 * Type definitions to allow a zero-cost, type-safe abstraction in case definition changes.
 *
 * If multi-platform build is utilized, `typealias` definitions need to switch to `expect`
 * and `actual` declarations instead.
 * *****************************************************************************************/

/**
 * Represent a Uniform Resource Identifier (URI) reference.
 *
 * @param[string]
 *   string-formatted URI.
 */
expect class URI(string: String) {
    /**
     * Constructs a new URI by parsing the given string and then resolving it
     * against this URI.
     *
     * @return
     *   resolved value as an [URI] instance.
     */
    fun resolve(uri: String): URI
}

/**
 * Singleton serializer object for [URI] that conforms to [KSerializer] contract.
 */
expect object URISerializer : KSerializer<URI>

/**
 * Represent an immutable 128-bit universally unique identifier (UUID).
 */
expect class UUID(high: Long, low: Long)

/**
 * Singleton serializer object for [UUID] that conforms to [KSerializer] contract.
 */
expect object UUIDSerializer : KSerializer<UUID>