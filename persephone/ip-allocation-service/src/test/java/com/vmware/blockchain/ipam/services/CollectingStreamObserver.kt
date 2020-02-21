/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.ipam.services

import io.grpc.stub.StreamObserver
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.sync.Mutex

/**
 * Implementation of [StreamObserver] that collects observations such that observations can
 * be awaited on for results.
 */
class CollectingStreamObserver<T> : StreamObserver<T> {

    private val observations: MutableList<T> = mutableListOf()
    private val data: CompletableDeferred<List<T>> = CompletableDeferred()
    private val completion: Mutex = Mutex(locked = true)

    override fun onNext(value: T) {
        observations += value
    }

    override fun onError(t: Throwable) {
        data.completeExceptionally(t)
        completion.unlock()
    }

    override fun onCompleted() {
        data.complete(observations)
        completion.unlock()
    }

    suspend fun await(): List<T> = data.await()

    suspend fun awaitSingle(): T {
        completion.lock()

        return data.await().let {
            if (it.size != 1) {
                throw IllegalStateException("More than one element emitted")
            } else {
                it[0]
            }
        }
    }
}
