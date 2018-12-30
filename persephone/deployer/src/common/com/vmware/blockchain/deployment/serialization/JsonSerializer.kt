/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.serialization

/**
 * Bi-directional serialization support for JSON.
 */
expect class JsonSerializer {

    /**
     * Convert an instance of type [T] to a JSON value.
     *
     * @return
     *   the JSON value as a [String].
     */
    inline fun <reified T> toJson(value: T): String

    /**
     * Parse a JSON value and map the result to an instance of type [T].
     *
     * @return
     *   an instance of type [T] corresponding to the JSON value.
     */
    inline fun <reified T> fromJson(json: String): T
}
