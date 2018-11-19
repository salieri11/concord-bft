/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.core

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
 * Represent an immutable 128-bit universally unique identifier (UUID).
 */
expect class UUID
