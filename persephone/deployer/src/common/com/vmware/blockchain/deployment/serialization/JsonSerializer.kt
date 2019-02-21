/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.serialization

import com.vmware.blockchain.deployment.model.nsx.NsxSerializationModule
import com.vmware.blockchain.deployment.model.vsphere.VSphereSerializationModule
import com.vmware.blockchain.deployment.model.vmc.VmcSerializationModule
import kotlinx.serialization.KSerializer
import kotlinx.serialization.json.Json
import kotlin.reflect.KClass

/**
 * Bi-directional serialization support for JSON.
 *
 * @property[json]
 *   internal JSON serializer.
 */
class JsonSerializer {

    /** Internal serializer. */
    val json: Json = Json(strictMode = false)

    /** Type to serializer mapping. */
    private val types: MutableMap<KClass<*>, KSerializer<*>> = mutableMapOf()

    init {
        for (entry in NsxSerializationModule.values()) {
            Json.apply { install(entry.module) }
            types[entry.module.kClass] = entry.module.kSerializer
        }
        for (entry in VSphereSerializationModule.values()) {
            Json.apply { install(entry.module) }
            types[entry.module.kClass] = entry.module.kSerializer
        }
        for (entry in VmcSerializationModule.values()) {
            Json.apply { install(entry.module) }
            types[entry.module.kClass] = entry.module.kSerializer
        }
    }

    /**
     * Resolve the [KSerializer] for a given [KClass] type.
     *
     * @param[type]
     *   [KClass] type to look up.
     *
     * @return
     *   [KSerializer] corresponding to the type.
     */
    @Suppress("UNCHECKED_CAST")
    fun <T> serializerFor(type: KClass<*>): KSerializer<T> {
        return types[type] as KSerializer<T>
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
    inline fun <reified T> toJson(value: T): String {
        return json.stringify(serializerFor(T::class), value)
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
    inline fun <reified T> fromJson(value: String): T {
        return json.parse(serializerFor(T::class), value)
    }
}