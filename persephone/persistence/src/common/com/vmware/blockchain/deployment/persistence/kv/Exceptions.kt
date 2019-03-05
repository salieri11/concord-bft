/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.deployment.persistence.kv.UntypedKeyValueStore.Response
import kotlin.reflect.KClass

/**
 * Subtype of [IllegalStateException] denoting the error condition when [KeyValueStore] operations
 * cannot be completed successfully due to a version mismatch between the supplied expected version
 * and the actual version on the storage.
 *
 * @param[expected]
 *   expected version value.
 * @param[actual]
 *   actual version value.
 */
data class VersionMismatchException(
    val expected: KeyValueStore.Version<*>,
    val actual: KeyValueStore.Version<*>
) : IllegalStateException("Unexpected version($actual != $expected)")

/**
 * Error denoting the condition that a resource's buffer was full due to some downstream consumer
 * not consuming from the buffer fast enough before it reached full capacity.
 *
 * @param[capacity]
 *   capacity of the buffer that was full.
 */
data class BufferFullException(
    val capacity: Int
) : RuntimeException("Internal buffer is full, capacity($capacity).")

/**
 * Error denoting the condition when a received [Response] is not the expected response type.
 *
 * @param[expected]
 *   [KClass] of the expected response type.
 * @param[response]
 *   actual response instance received.
 */
data class UnexpectedResponseException(
    val expected: KClass<out Response<*>>,
    val response: UntypedKeyValueStore.Response<*>
) : RuntimeException("Unexpected response(${response::class} != $expected)")

/**
 * Error denoting the condition that a server instance is unable to continue a given event stream.
 */
object InterruptedEventStreamException: RuntimeException("Server stream is interrupted")

/**
 * Error denoting the condition when a request is made to a server instance that is closing or
 * already closed (shut down).
 */
object ServerClosedException: IllegalStateException("Server is closed")
