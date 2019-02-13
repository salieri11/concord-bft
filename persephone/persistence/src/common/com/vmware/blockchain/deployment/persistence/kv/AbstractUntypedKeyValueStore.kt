/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Version
import com.vmware.blockchain.deployment.reactive.BroadcastingPublisher
import com.vmware.blockchain.deployment.reactive.ErrorPublisher
import com.vmware.blockchain.deployment.reactive.Publisher
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.async
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import kotlinx.coroutines.reactive.publish
import kotlin.coroutines.AbstractCoroutineContextElement
import kotlin.coroutines.CoroutineContext

/**
 * Base prototype asynchronous implementation of [UntypedKeyValueStore] interface.
 *
 * Note: Subtypes can implement [newStorageServer] to facilitate the actual work of record storage
 * and retrieval.
 *
 * @param[T]
 *   subtype of [Version] to use to version the key-value entries.
 * @param[context]
 *   coroutine context to use as basis for underlying coroutine-based operations.
 */
abstract class AbstractUntypedKeyValueStore<T : Version<T>>(
    private val context: CoroutineContext = Dispatchers.Default
) : UntypedKeyValueStore<T>, CoroutineScope {

    /**
     * A [CoroutineContext.Element] that retains a settable reference to the [Channel] that
     * corresponds to the input source to an event sink.
     */
    class EventSinkContext: AbstractCoroutineContextElement(EventSinkContext) {

        /** Singleton companion object that also acts as a [CoroutineContext.Key]. */
        companion object : CoroutineContext.Key<EventSinkContext>

        /** Upstream [Channel] that the event sink receives elements from. */
        internal val channel = kotlinx.atomicfu.atomic<Channel<*>?>(null)
    }

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = Job()

    /** Request channel to send to the storage server. **/
    private val requestChannel by lazy { newStorageServer() }

    /**
     * Allocate a storage server or server adapter.
     *
     * @return
     *   a request [Channel] to send commands to the server with, closing the channel closes the
     *   communication.
     */
    protected abstract fun newStorageServer(): Channel<UntypedKeyValueStore.Request<T>>

    /**
     * Shutdown the [UntypedKeyValueStore] instance and closes all resources.
     */
    override fun close() {
        requestChannel.close()
        job.cancel()
    }

    override fun get(key: Value): Publisher<Versioned<Value, T>> {
        return try {
            // Setup request with a response channel buffer of 1 (expecting only 1 message back).
            val request = Channel<UntypedKeyValueStore.Response<T>>(Channel.CONFLATED)
                    .let { UntypedKeyValueStore.Request.Get(it, key) }

            // Send the request and block-receive in a suspendable coroutine.
            val response = async {
                requestChannel.send(request)
                request.response.receive()
            }

            // Setup a publisher such that every incoming subscriber awaits for the response.
            publish(coroutineContext) {
                when (val message = response.await()) {
                    is UntypedKeyValueStore.Response.Get -> send(message.versioned)
                    is UntypedKeyValueStore.Response.Error -> throw message.throwable
                    else ->
                        throw UnexpectedResponseException(
                                UntypedKeyValueStore.Response.Get::class,
                                message
                        )
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun put(key: Value, value: Value, expected: Version<T>): Publisher<Versioned<Value, T>> {
        return try {
            // Setup request with a response channel buffer of 1 (expecting only 1 message back).
            val request = Channel<UntypedKeyValueStore.Response<T>>(Channel.CONFLATED)
                    .let { UntypedKeyValueStore.Request.Put(it, key, value, expected) }

            // Send the request and block-receive in a suspendable coroutine.
            val response = async {
                requestChannel.send(request)
                request.response.receive()
            }

            // Setup a publisher such that every incoming subscriber awaits for the response.
            publish(coroutineContext) {
                when (val message = response.await()) {
                    is UntypedKeyValueStore.Response.Put -> send(message.versioned)
                    is UntypedKeyValueStore.Response.Error -> throw message.throwable
                    else ->
                        throw UnexpectedResponseException(
                                UntypedKeyValueStore.Response.Put::class,
                                message
                        )
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun delete(key: Value, expected: Version<T>): Publisher<Versioned<Value, T>> {
        return try {
            // Setup request with a response channel buffer of 1 (expecting only 1 message back).
            val request = Channel<UntypedKeyValueStore.Response<T>>(Channel.CONFLATED)
                    .let { UntypedKeyValueStore.Request.Delete(it, key, expected) }

            // Send the request and block-receive in a suspendable coroutine.
            val response = async {
                requestChannel.send(request)
                request.response.receive()
            }

            // Setup a publisher such that every incoming subscriber awaits for the response.
            publish(coroutineContext) {
                when (val message = response.await()) {
                    is UntypedKeyValueStore.Response.Delete -> send(message.versioned)
                    is UntypedKeyValueStore.Response.Error -> throw message.throwable
                    else ->
                        throw UnexpectedResponseException(
                                UntypedKeyValueStore.Response.Put::class,
                                message
                        )
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun subscribe(capacity: Int): Publisher<Event<Value, Value, T>> {
        return try {
            val request = Channel<UntypedKeyValueStore.Response<T>>(Channel.CONFLATED)
                    .let { UntypedKeyValueStore.Request.Subscribe(it) }

            // Send the request and block-receive in a suspendable coroutine.
            val response = async {
                requestChannel.send(request)
                request.response.receive()
            }

            // Pipeline the incoming into a local broadcast channel.
            val publishContext = coroutineContext + EventSinkContext()
            val publisher = BroadcastingPublisher<Event<Value, Value, T>>(capacity, publishContext)
            launch(publishContext) {
                when (val message = response.await()) {
                    is UntypedKeyValueStore.Response.Subscribe -> {
                        val upstreamChannel = message.subscription

                        // Save the upstream channel in the coroutine context.
                        // Since this coroutine's context shares a parent with the broadcast
                        // publisher's context, saving a "cookie" allows local unsubscribe to
                        // reverse-lookup the channel from a given publisher instance. Using the
                        // looked up channel, unsubscribe can then send the request to server to
                        // really unsubscribe.
                        //
                        // Note: To avoid any weird non-debuggable concurrency bugs, only set the
                        // context's channel if it wasn't already set, to avoid overwrite behavior.
                        // this.coroutineContext[EventSinkContext]
                        publishContext[EventSinkContext]?.apply {
                            channel.compareAndSet(null, upstreamChannel)
                        }

                        for (event in upstreamChannel) {
                            if (!publisher.broadcast(event)) {
                                // If broadcast() returns false, then channel was full.
                                // To simplify error handling we propagate error to all downstream
                                // subscribers (even if possibly only 1 subscriber was slow in
                                // consuming from buffer). All subscribers will end up with
                                // onError() signal indicating that broadcast buffer was full.
                                //
                                // This achieves non-blocking for the producer while allowing event
                                // linearity for all downstream subscribers since they are
                                // guaranteed to observe the sequence as intended by the producer
                                // or receive an error indicating that stream was interrupted due to
                                // un-sendable condition.
                                publisher
                                        .apply { close(BufferFullException(capacity)) }
                                        .also { unsubscribe(it) }
                            }
                        }
                    }
                    is UntypedKeyValueStore.Response.Error -> throw message.throwable
                    else ->
                        throw UnexpectedResponseException(
                                UntypedKeyValueStore.Response.Subscribe::class,
                                message
                        )
                }
            }.invokeOnCompletion { error: Throwable? ->
                // If the upstream sender of broadcast is completing, make sure to propagate the
                // signal to downstream as well. Publisher has its own coroutine scope, which is why
                // the cancellation propagation is manual and explicit.
                publisher.close(error)
            }

            // Before returning the new publisher back to caller, check if the publish coroutine
            // isn't already concurrently closed. If closed, just return a short-circuit publisher
            // that publishes an error.
            return if (publishContext.isActive) {
                publisher
            } else {
                ErrorPublisher(ServerClosedException)
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun unsubscribe(eventSink: Publisher<Event<Value, Value, T>>) {
        when (eventSink) {
            is BroadcastingPublisher -> {
                // Fish for a saved coroutine context if available.
                eventSink.coroutineContext[EventSinkContext]?.apply {
                    channel.value
                            ?.let { UntypedKeyValueStore.Request.Unsubscribe<T>(it) }
                            ?.let {
                                // Just one-shot send the request. If unsubscribe is successful,
                                // eventually the subscription stream would complete.
                                launch(coroutineContext) { requestChannel.send(it) }
                            }
                }
            }
        }
    }
}
