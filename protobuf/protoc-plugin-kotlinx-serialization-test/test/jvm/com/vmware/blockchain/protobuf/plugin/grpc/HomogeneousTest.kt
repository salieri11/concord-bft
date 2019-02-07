/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.plugin.grpc

import io.grpc.inprocess.InProcessServerBuilder
import io.grpc.stub.StreamObserver
import io.grpc.inprocess.InProcessChannelBuilder
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Various tests verifying emitted code utilize underlying GRPC runtime correctly..
 */
class HomogeneousTest {

    /**
     * Simple implementation of [RpcServiceImplBase].
     */
    class SimpleRpcService : RpcServiceImplBase() {

        /**
         * Echos the message content of the [EchoRequest] back to caller as a [EchoResponse].
         */
        override fun echo(request: EchoRequest, responseObserver: StreamObserver<EchoResponse>) {
            responseObserver.onNext(EchoResponse(message = request.message))
            responseObserver.onCompleted()
        }

        /**
         * Echos the message content of the [EchoRequest] back to caller as stream of
         * [EchoResponse], repeated as specified by the count parameter in the request.
         */
        override fun echoResponseStream(
            request: EchoRequest,
            responseObserver: StreamObserver<EchoResponse>
        ) {
            repeat(request.count) {
                responseObserver.onNext(EchoResponse(message = request.message))
            }
            responseObserver.onCompleted()
        }

        /**
         * Echos the concatenated message content of a stream of [EchoRequest] back to caller as
         * [EchoResponse], with the response count field set to the number of requests received.
         */
        override fun echoRequestStream(
            responseObserver: StreamObserver<EchoResponse>
        ): StreamObserver<EchoRequest> {
            val pushed = mutableListOf<String>()
            return object : StreamObserver<EchoRequest> {
                override fun onNext(value: EchoRequest) {
                    pushed += value.message
                }

                override fun onError(t: Throwable) {
                }

                override fun onCompleted() {
                    responseObserver.onNext(
                            EchoResponse(message = pushed.joinToString(), count = pushed.size)
                    )
                    responseObserver.onCompleted()
                }
            }
        }

        /**
         * Echos the message content of a stream of [EchoRequest] back to caller as a stream of
         * [EchoResponse].
         */
        override fun echoStream(
            responseObserver: StreamObserver<EchoResponse>
        ): StreamObserver<EchoRequest> {
            return object : StreamObserver<EchoRequest> {
                override fun onNext(value: EchoRequest) {
                    responseObserver.onNext(EchoResponse(message = value.message))
                }

                override fun onError(t: Throwable) {
                }

                override fun onCompleted() {
                    responseObserver.onCompleted()
                }
            }
        }
    }

    /**
     * Create a new [StreamObserver] of element type [T] that collects all observations into a list.
     *
     * @param[observations]
     *   container to collect observations in.
     * @param[onError]
     *   function to invoke when error is signaled.
     * @param[onComplete]
     *   function to invoke when completion is signaled.
     */
    private fun <T> newCollectingObserver(
        observations: MutableList<T>,
        onError: (Throwable) -> Unit = {},
        onComplete: () -> Unit = {}
    ): StreamObserver<T> {
        return object : StreamObserver<T> {
            override fun onNext(value: T) {
                observations += value
            }

            override fun onError(t: Throwable) = onError(t)

            override fun onCompleted() = onComplete()
        }
    }

    /**
     * Test that unary call is functional.
     */
    @Test
    fun unary() {
        // Setup fixture.
        val name = InProcessServerBuilder.generateName()
        val server = InProcessServerBuilder.forName(name)
                .directExecutor()
                .addService(SimpleRpcService())
                .build()
                .start()
        val channel = InProcessChannelBuilder.forName(name).directExecutor().build()
        val client = RpcServiceStub(channel)

        // Setup observation capture.
        val observations = mutableListOf<EchoResponse>()
        val latch = CountDownLatch(1)
        val clientObserver = newCollectingObserver(observations, onComplete = { latch.countDown() })

        // Send a few messages.
        val inputs = (0..10).map { EchoRequest("$it") }.toList()
        inputs.forEach { client.echo(it, clientObserver) }

        // Wait a maximum amount of time to not block test execution.
        Assertions.assertThat(latch.await(100, TimeUnit.MILLISECONDS)).isTrue()

        // Check that observations match sent messages.
        Assertions.assertThat(observations.map { it.message }).isEqualTo(inputs.map { it.message })

        // Shutdown.
        channel.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
        server.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
    }

    /**
     * Test that server streaming call is functional.
     */
    @Test
    fun serverStreaming() {
        // Setup fixture.
        val name = InProcessServerBuilder.generateName()
        val server = InProcessServerBuilder.forName(name)
                .directExecutor()
                .addService(SimpleRpcService())
                .build()
                .start()
        val channel = InProcessChannelBuilder.forName(name).directExecutor().build()
        val client = RpcServiceStub(channel)

        // Setup observation capture.
        val observations = mutableListOf<EchoResponse>()
        val latch = CountDownLatch(1)
        val clientObserver = newCollectingObserver(observations, onComplete = { latch.countDown() })

        // Send a few messages (each message is asked to be echoed multiple times.
        val messageCount = 10
        val echoCount = 5
        val inputs = (1..messageCount).map { EchoRequest("$it", echoCount) }.toList()
        inputs.forEach { client.echoResponseStream(it, clientObserver) }

        // Wait a maximum amount of time to not block test execution.
        Assertions.assertThat(latch.await(100, TimeUnit.MILLISECONDS)).isTrue()

        // Check that observations match sent messages.
        val messages = inputs.flatMap { input -> MutableList(input.count) { input } }
        Assertions.assertThat(observations.map { it.message }).isEqualTo(messages.map { it.message })

        // Shutdown.
        channel.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
        server.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
    }

    /**
     * Test that client streaming call is functional.
     */
    @Test
    fun clientStreaming() {
        // Setup fixture.
        val name = InProcessServerBuilder.generateName()
        val server = InProcessServerBuilder.forName(name)
                .directExecutor()
                .addService(SimpleRpcService())
                .build()
                .start()
        val channel = InProcessChannelBuilder.forName(name).directExecutor().build()
        val client = RpcServiceStub(channel)

        // Setup observation capture.
        val observations = mutableListOf<EchoResponse>()
        val latch = CountDownLatch(1)
        val clientObserver = newCollectingObserver(observations, onComplete = { latch.countDown() })

        // Send a few messages (each message is asked to be echoed multiple times.
        val messageCount = 10
        val inputs = (1..messageCount).map { EchoRequest("$it") }.toList()
        client.echoRequestStream(clientObserver).apply {
            inputs.forEach { onNext(it) }
            onCompleted()
        }

        // Wait a maximum amount of time to not block test execution.
        Assertions.assertThat(latch.await(100, TimeUnit.MILLISECONDS)).isTrue()

        // Check that observations match sent messages.
        Assertions.assertThat(observations.size).isEqualTo(1)
        Assertions.assertThat(observations.first().count).isEqualTo(messageCount)
        Assertions.assertThat(observations.first().message)
                .isEqualTo(inputs.joinToString { it.message })

        // Shutdown.
        channel.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
        server.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
    }

    /**
     * Test that bidirectional streaming call is functional.
     */
    @Test
    fun bidirectionalStreaming() {
        // Setup fixture.
        val name = InProcessServerBuilder.generateName()
        val server = InProcessServerBuilder.forName(name)
                .directExecutor()
                .addService(SimpleRpcService())
                .build()
                .start()
        val channel = InProcessChannelBuilder.forName(name).directExecutor().build()
        val client = RpcServiceStub(channel)

        // Setup observation capture.
        val observations = mutableListOf<EchoResponse>()
        val latch = CountDownLatch(1)
        val clientObserver = newCollectingObserver(observations, onComplete = { latch.countDown() })

        // Send a few messages (each message is asked to be echoed multiple times.
        val messageCount = 10
        val inputs = (1..messageCount).map { EchoRequest("$it") }.toList()
        client.echoStream(clientObserver).apply {
            inputs.forEach { onNext(it) }
            onCompleted()
        }

        // Wait a maximum amount of time to not block test execution.
        Assertions.assertThat(latch.await(100, TimeUnit.MILLISECONDS)).isTrue()

        // Check that observations match sent messages.
        Assertions.assertThat(observations.map { it.message }).isEqualTo(inputs.map { it.message })

        // Shutdown.
        channel.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
        server.shutdown().awaitTermination(1000, TimeUnit.MILLISECONDS)
    }
}
