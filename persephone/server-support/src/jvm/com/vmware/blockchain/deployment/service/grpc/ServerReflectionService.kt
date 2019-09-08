/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.service.grpc

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.grpc.kotlinx.serialization.ChannelStreamObserver
import com.vmware.blockchain.grpc.reflection.v1alpha.ErrorResponse
import com.vmware.blockchain.grpc.reflection.v1alpha.ExtensionRequest
import com.vmware.blockchain.grpc.reflection.v1alpha.FileDescriptorResponse
import com.vmware.blockchain.grpc.reflection.v1alpha.ListServiceResponse
import com.vmware.blockchain.grpc.reflection.v1alpha.ServerReflectionImplBase
import com.vmware.blockchain.grpc.reflection.v1alpha.ServerReflectionRequest
import com.vmware.blockchain.grpc.reflection.v1alpha.ServerReflectionResponse
import com.vmware.blockchain.grpc.reflection.v1alpha.ServiceResponse
import com.vmware.blockchain.protobuf.DescriptorProto
import com.vmware.blockchain.protobuf.FileDescriptorProto
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoFileDescriptor
import com.vmware.blockchain.protobuf.kotlinx.serialization.decodeBase64
import io.grpc.InternalNotifyOnServerBuild
import io.grpc.Server
import io.grpc.Status
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlin.coroutines.CoroutineContext

/**
 * Concrete implementation of [com.vmware.blockchain.grpc.reflection.v1alpha.ServerReflection].
 */
class ServerReflectionService(
    private val context: CoroutineContext = Dispatchers.Default
) : ServerReflectionImplBase(), InternalNotifyOnServerBuild, CoroutineScope {

    /** Logging instance. */
    private val log by logger()

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = SupervisorJob()

    /** Mapping of service name to its hosting Protocol Buffer file descriptor schema. */
    private lateinit var protoFileDescriptors: Map<String, ProtoFileDescriptor>

    /** Mapping of symbol name (canonical) to its hosting Protocol Buffer file descriptor schema. */
    private val protoFileDescriptorBySymbols: Map<String, ProtoFileDescriptor> by lazy {
        initializeSymbols()
    }

    override fun notifyOnBuild(server: Server) {
        protoFileDescriptors = server.services
                .filter { it.serviceDescriptor.schemaDescriptor is ProtoFileDescriptor }
                .associate {
                    it.serviceDescriptor.name to
                            it.serviceDescriptor.schemaDescriptor as ProtoFileDescriptor
                }
    }

    override fun serverReflectionInfo(
        responseObserver: StreamObserver<ServerReflectionResponse>
    ): StreamObserver<ServerReflectionRequest> {
        // Create a receive-channel based stream observer.
        val receiver = ChannelStreamObserver<ServerReflectionRequest>()

        // Keep each managed session in its own scoped coroutine.
        launch(coroutineContext) {
            for (request in receiver.asReceiveChannel()) {
                val response = when {
                    request.listServices.isNotEmpty() -> listServices(request)
                    request.fileByFilename.isNotEmpty() ->
                        getFile(request, protoFileDescriptors[request.fileByFilename])
                    request.fileContainingSymbol.isNotEmpty() ->
                        getFile(request, protoFileDescriptorBySymbols[request.fileContainingSymbol])
                    request.fileContainingExtension != ExtensionRequest.defaultValue -> {
                        ServerReflectionResponse(
                                validHost = request.host,
                                originalRequest = request,
                                errorResponse = ErrorResponse(
                                        Status.Code.UNIMPLEMENTED.value(),
                                        "Requested type not implemented"
                                )
                        )
                    }
                    else -> listServices(request)
                }

                // Send the response.
                responseObserver.onNext(response)
            }
        }.invokeOnCompletion { error ->
            if (error != null) {
                log.error { "Unhandled error handling request stream, error(${error.message})" }

                responseObserver.onError(error)
            } else {
                // Coroutine exited, so the managed session is now done.
                responseObserver.onCompleted()
            }
        }

        return receiver
    }

    /**
     * Initialize the internal symbol index table based on registered services.
     */
    private fun initializeSymbols(): Map<String, ProtoFileDescriptor> {
        val symbolTable = mutableMapOf<String, ProtoFileDescriptor>()
        val serializer = ProtoBuf.plain

        // Construct the symbol table for each transitively-dependent proto file descriptor.
        protoFileDescriptors.values.forEach { protoFile ->
            val descriptor = serializer.load(
                    FileDescriptorProto.serializer(),
                    ByteString.of(*protoFile.encodedData.toByteArray()).decodeBase64().toByteArray()
            )

            // Figure out if there is a package namespace prefix.
            val packageNamePrefix = descriptor.`package`.takeIf { it.isNotEmpty() } ?: ""

            // Add every defined service in this descriptor by its canonical name.
            for (service in descriptor.service) {
                symbolTable["$packageNamePrefix.${service.name}"] = protoFile

                // Add every defined method in this service by its canonical name.
                for (method in service.method) {
                    symbolTable["$packageNamePrefix.${service.name}.${method.name}"] = protoFile
                }
            }

            // Add every defined message (include nested) in this descriptor by its canonical name.
            for (message in descriptor.messageType) {
                getAllMessageTypeNames(packageNamePrefix, message).forEach {
                    symbolTable[it] = protoFile
                }
            }
        }

        return symbolTable
    }

    /**
     * Recursively resolve canonical type names of all nested messages of a given [DescriptorProto]
     * message.
     *
     * @param[prefix]
     *   prefix name to base the canonical type name on.
     * @param[descriptor]
     *   message type to resolve for canonical type name as well as perform additional recursive
     *   nested type-lookup.
     *
     * @return
     *   a [List] of canonical type names of this message type and its nested message types.
     */
    private fun getAllMessageTypeNames(prefix: String, descriptor: DescriptorProto): List<String> {
        val nextPrefixName = "$prefix.${descriptor.name}"
        val result = mutableListOf(nextPrefixName)

        return descriptor.nestedType.flatMapTo(result) {
            getAllMessageTypeNames(nextPrefixName, it)
        }
    }

    /**
     * Handle request to obtain the Protocol Buffer file descriptor content and its transitive
     * dependencies, based on a given schema [ProtoFileDescriptor] instance.
     *
     * @param[request]
     *   file descriptor retrieval request.
     * @param[fileDescriptor]
     *   schema of the file descriptor to retrieve data content.
     *
     * @return
     *   response payload after processing the request.
     */
    private fun getFile(
        request: ServerReflectionRequest,
        fileDescriptor: ProtoFileDescriptor?
    ): ServerReflectionResponse {
        return fileDescriptor
                ?.let { descriptor ->
                    val payload = descriptor.resolveDependencies().map {
                        ByteString.of(*it.encodedData.toByteArray()).decodeBase64()
                    }

                    ServerReflectionResponse(
                            validHost = request.host,
                            originalRequest = request,
                            fileDescriptorResponse = FileDescriptorResponse(payload)
                    )
                }
                ?: ServerReflectionResponse(
                        validHost = request.host,
                        originalRequest = request,
                        errorResponse = ErrorResponse(
                                Status.Code.NOT_FOUND.value(),
                                "Requested file not found"
                        )
                )
    }

    /**
     * Handle request to list registered services.
     *
     * @param[request]
     *   service listing request.
     *
     * @return
     *   response payload after processing the request.
     */
    private fun listServices(request: ServerReflectionRequest): ServerReflectionResponse {
        return ServerReflectionResponse(
                validHost = request.host,
                originalRequest = request,
                listServicesResponse = ListServiceResponse(
                        service = protoFileDescriptors.keys.map { ServiceResponse(it) }
                )
        )
    }
}
