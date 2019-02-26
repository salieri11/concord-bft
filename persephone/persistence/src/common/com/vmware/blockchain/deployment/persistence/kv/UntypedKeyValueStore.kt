/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Version
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.reactive.Publisher
import kotlinx.coroutines.channels.Channel

/**
 * Garbage-in / garbage-out untyped shim abstraction for key-value storage.
 *
 * @param[T]
 *   subtype of [Version] to use to version the key-value entries.
 */
interface UntypedKeyValueStore<T : Version<T>> : KeyValueStore<Value, Value, T> {

    /**
     * Possible request types to be supported by the underlying storage or storage driver.
     */
    sealed class Request<out T> {
        data class Get<T>(val response: Channel<Response<T>>, val key: Value) : Request<T>()
        data class Set<T>(
            val response: Channel<Response<T>>,
            val key: Value,
            val value: Value,
            val expected: Version<T>
        ) : Request<T>()
        data class Delete<T>(
            val response: Channel<Response<T>>,
            val key: Value,
            val expected: Version<T>
        ) : Request<T>()
        data class Subscribe<T>(
            val response: Channel<Response<T>>,
            val state: Boolean
        ) : Request<T>()
        data class Unsubscribe<T>(val subscription: Channel<*>) : Request<T>()
    }

    /**
     * Possible response types to be supported by the underlying storage or storage driver, for
     * [Request]s that expect a response.
     */
    sealed class Response<out T> {
        data class Get<T>(val versioned: Versioned<Value, T>) : Response<T>()
        data class Set<T>(val versioned: Versioned<Value, T>) : Response<T>()
        data class Delete<T>(val versioned: Versioned<Value, T>): Response<T>()
        data class Subscribe<T>(val subscription: Channel<Event<Value, Value, T>>) : Response<T>()
        data class Error<T>(val throwable: Throwable) : Response<T>()
    }

    override operator fun get(key: Value): Publisher<Versioned<Value, T>>

    override fun set(key: Value, expected: Version<T>, value: Value): Publisher<Versioned<Value, T>>

    override fun delete(key: Value, expected: Version<T>): Publisher<Versioned<Value, T>>

    override fun subscribe(capacity: Int, state: Boolean): Publisher<Event<Value, Value, T>>

    override fun unsubscribe(eventSink: Publisher<Event<Value, Value, T>>)
}
