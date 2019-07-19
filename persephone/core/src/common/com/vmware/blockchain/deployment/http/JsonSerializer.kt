/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.http

import kotlinx.serialization.KSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.list
import kotlinx.serialization.modules.SerialModule
import kotlin.reflect.KClass

/**
 * Bi-directional serialization support for JSON.
 *
 * @property[json]
 *   internal JSON serializer.
 */
open class JsonSerializer(private val context: SerialModule) {

    /** Internal serializer. */
    val json: Json = Json(
            JsonConfiguration.Stable.copy(encodeDefaults = false, strictMode = false),
            context
    )

    /**
     * Resolve the [KSerializer] for a given [KClass] type.
     *
     * @param[type]
     *   [KClass] type to look up.
     *
     * @return
     *   [KSerializer] corresponding to the type.
     */
    fun <T : Any> serializerForType(type: KClass<T>): KSerializer<T> {
        return requireNotNull(context.getContextual(type))
    }

    /**
     * Convert an instance of type [T] to a JSON value.
     *
     * @param[value]
     *   an instance of type [T] to serialize.
     *
     * @return
     *   the JSON value as a [String].
     */
    inline fun <reified T : Any> toJson(value: T): String {
        return json.stringify(serializerForType(T::class), value)
    }

    /**
     * Parse a JSON value and map the result to an instance of type [T].
     *
     * @param[value]
     *   UTF-8 encoded JSON string to deserialize.
     *
     * @return
     *   an instance of type [T] corresponding to the JSON value.
     */
    inline fun <reified T : Any> fromJson(value: String): T {
        return json.parse(serializerForType(T::class), value)
    }

    /**
     * Parse a JSON value and map the result to an array of type [T] instances.
     *
     * @param[value]
     *   UTF-8 encoded JSON string to deserialize.
     *
     * @return
     *   an array of type [T] instances corresponding to the JSON value.
     */
    inline fun <reified T : Any> fromJsonArray(value: String): List<T> {
        return json.parse(serializerForType(T::class).list, value)
    }
}
