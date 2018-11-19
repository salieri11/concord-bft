/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.serialization

import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module
import com.fasterxml.jackson.module.kotlin.KotlinModule

/**
 * Bi-directional serialization support for JSON.
 *
 * @property[jackson]
 *   internal Jackson serializer.
 */
actual class JsonSerializer {

    /** Jackson serializer. */
    val jackson: ObjectMapper = ObjectMapper()
            .registerModule(Jdk8Module())
            .registerModule(KotlinModule())
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

    /**
     * Convert an instance of type [T] to a JSON value.
     *
     * @return
     *   the JSON value as a [String].
     */
    actual inline fun <reified T> toJson(value: T): String = jackson.writeValueAsString(value)

    /**
     * Parse a JSON value and map the result to an instance of type [T].
     *
     * @return
     *   an instance of type [T] corresponding to the JSON value.
     */
    actual inline fun <reified T> fromJson(json: String): T = jackson.readValue(json, T::class.java)
}
