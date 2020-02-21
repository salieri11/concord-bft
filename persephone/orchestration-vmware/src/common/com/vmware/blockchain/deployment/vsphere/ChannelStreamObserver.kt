/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.vsphere

import io.grpc.stub.StreamObserver
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.ReceiveChannel

/**
 * An implementation of [StreamObserver] where all elements signaled from [onNext] is made available
 * as elements on a [ReceiveChannel], obtained from [ChannelStreamObserver.asReceiveChannel()].
 *
 * @property[T]
 *   type of element observed via [onNext].
 * @property[capacity]
 *   the maximum buffer / capacity on the internal [Channel] backing this instance.
 */
class ChannelStreamObserver<T>(
    private val capacity: Int = Channel.UNLIMITED
) : StreamObserver<T> {

    private val channel: Channel<T> = Channel(capacity)

    /**
     * Convert this [StreamObserver] instance to a [ReceiveChannel] where [onNext] signal can
     * be received from the [ReceiveChannel] instance.
     *
     * @return
     *   this instance as a [ReceiveChannel] of elements of type [T].
     */
    fun asReceiveChannel(): ReceiveChannel<T> = channel

    override fun onNext(value: T) {
        if (!channel.isClosedForSend) {
            if (!channel.offer(value)) {
                // Reached capacity => receiver is too slow at draining.
                onError(RuntimeException("Internal buffer is full, capacity($capacity)"))
            }
        }
    }

    override fun onError(t: Throwable) {
        channel.close(t)
    }

    override fun onCompleted() {
        channel.close()
    }
}
