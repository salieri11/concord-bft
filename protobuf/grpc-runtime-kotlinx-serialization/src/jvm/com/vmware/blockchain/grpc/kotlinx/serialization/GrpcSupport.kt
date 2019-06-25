/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.grpc.kotlinx.serialization

import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf
import io.grpc.MethodDescriptor
import io.grpc.stub.ServerCalls
import io.grpc.stub.StreamObserver
import kotlinx.io.ByteArrayInputStream
import kotlinx.io.InputStream
import kotlinx.serialization.KSerializer

/**
 * Class enclosure of various static utility functions pertaining to GRPC's Java runtime support.
 *
 * Note:
 * The class enclosure is not strictly necessary in Kotlin. But it does help interop with Java as
 * well as compile-time code-generation.
 */
class GrpcSupport {
    companion object {
        /**
         * Utility function to create a new marshaller instance conforming to
         * [MethodDescriptor.Marshaller] as defined by GRPC's Java core module.
         *
         * @param[serializer]
         *   underlying serializer to use for marshalling / unmarshalling.
         * @param[prototype]
         *   a default prototype instance of [T].
         *
         * @return
         *   a new instance of [MethodDescriptor.Marshaller] for [T].
         */
        @JvmStatic
        inline fun <reified T> newMarshaller(
            serializer: KSerializer<T>,
            prototype: T
        ): MethodDescriptor.Marshaller<T> {
            return object : MethodDescriptor.PrototypeMarshaller<T> {
                override fun getMessageClass(): Class<T> = T::class.java

                override fun getMessagePrototype(): T = prototype

                override fun stream(value: T): InputStream {
                    return ByteArrayInputStream(ProtoBuf.plain.dump(serializer, value))
                }

                override fun parse(stream: InputStream): T {
                    return ProtoBuf.plain.load(serializer, stream.readAllBytes())
                }
            }
        }

        /**
         * Utility function to create a new handler instance conforming to [ServerCalls.UnaryMethod]
         * as defined by GRPC's Java stub module.
         *
         * @param[method]
         *   the underlying processing function to be adapted.
         *
         * @return
         *   a new instance of [ServerCalls.UnaryMethod] for [RequestT] and [ResponseT].
         */
        @JvmStatic
        fun <RequestT, ResponseT> newUnaryMethod(
            method: (RequestT, StreamObserver<ResponseT>) -> Unit
        ): ServerCalls.UnaryMethod<RequestT, ResponseT> {
            return ServerCalls.UnaryMethod(method)
        }

        /**
         * Utility function to create a new handler instance conforming to
         * [ServerCalls.ClientStreamingMethod] as defined by GRPC's Java stub module.
         *
         * @param[method]
         *   the underlying processing function to be adapted.
         *
         * @return
         *   a new instance of [ServerCalls.ClientStreamingMethod] for [RequestT] and [ResponseT].
         */
        @JvmStatic
        fun <RequestT, ResponseT> newClientStreamingMethod(
            method: (StreamObserver<ResponseT>) -> StreamObserver<RequestT>
        ): ServerCalls.ClientStreamingMethod<RequestT, ResponseT> {
            return ServerCalls.ClientStreamingMethod(method)
        }

        /**
         * Utility function to create a new handler instance conforming to
         * [ServerCalls.ServerStreamingMethod] as defined by GRPC's Java stub module.
         *
         * @param[method]
         *   the underlying processing function to be adapted.
         *
         * @return
         *   a new instance of [ServerCalls.ServerStreamingMethod] for [RequestT] and [ResponseT].
         */
        @JvmStatic
        fun <RequestT, ResponseT> newServerStreamingMethod(
            method: (RequestT, StreamObserver<ResponseT>) -> Unit
        ): ServerCalls.ServerStreamingMethod<RequestT, ResponseT> {
            return ServerCalls.ServerStreamingMethod(method)
        }

        /**
         * Utility function to create a new handler instance conforming to
         * [ServerCalls.BidiStreamingMethod] as defined by GRPC's Java stub module.
         *
         * @param[method]
         *   the underlying processing function to be adapted.
         *
         * @return
         *   a new instance of [ServerCalls.BidiStreamingMethod] for [RequestT] and [ResponseT].
         */
        @JvmStatic
        fun <RequestT, ResponseT> newBiDirectionalStreamingMethod(
            method: (StreamObserver<ResponseT>) -> StreamObserver<RequestT>
        ): ServerCalls.BidiStreamingMethod<RequestT, ResponseT> {
            return ServerCalls.BidiStreamingMethod(method)
        }
    }
}

