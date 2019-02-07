/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.plugin.kotlin

import com.google.protobuf.DescriptorProtos
import com.google.protobuf.compiler.PluginProtos
import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.KModifier
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import com.squareup.kotlinpoet.ParameterSpec
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.TypeSpec
import com.squareup.kotlinpoet.asTypeName
import com.vmware.blockchain.grpc.GrpcSupport
import io.grpc.BindableService
import io.grpc.CallOptions
import io.grpc.Channel
import io.grpc.MethodDescriptor
import io.grpc.ServerServiceDefinition
import io.grpc.ServiceDescriptor
import io.grpc.stub.ClientCalls
import io.grpc.stub.ServerCalls
import io.grpc.stub.StreamObserver

/**
 * Denotes the context for code-generation based on a given Protocol Buffer descriptor file.
 *
 * @param[packageName]
 *   name of the package that all generated types should be grouped in.
 */
data class Context(val packageName: String)

/**
 * Generator for generating a file containing the top-level service type denoted by a Protocol
 * Buffer `message` descriptor.
 *
 * @param[context]
 *   context for code-generation metadata.
 * @param[descriptor]
 *   `message` descriptor to generate code for.
 */
class ServiceFileGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.ServiceDescriptorProto
) {
    /**
     * Create a new [PluginProtos.CodeGeneratorResponse.File] instance corresponding to the
     * descriptor associated with this generator.
     */
    fun generate(): PluginProtos.CodeGeneratorResponse.File {
        val file = PluginProtos.CodeGeneratorResponse.File.newBuilder()
        file.name = "${context.packageName.replace('.', '/')}/${descriptor.name}Grpc.kt"

        // Generate the FileSpec.
        val fileSpec = FileSpec.builder(context.packageName, descriptor.name)
                .addType(ServiceGenerator(context, descriptor).generate())
                .addType(ServerStubGenerator(context, descriptor).generate())
                .addType(AsyncClientStubGenerator(context, descriptor).generate())
                .build()

        // Write it out to string, allow KotlinPoet to register this new type in the process.
        file.content = buildString { fileSpec.writeTo(this) }

        return file.build()
    }
}

/**
 * Generator for generating a `class` as denoted by a given Protocol Buffer `service` descriptor.
 *
 * @param[context]
 *   context for code-generation metadata.
 * @param[descriptor]
 *   `service` descriptor to generate code for.
 */
class ServiceGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.ServiceDescriptorProto
) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        val selfTypeName = ClassName.bestGuess("${context.packageName}.${descriptor.name}")
        val methodDescriptors = descriptor.methodList
                .map { it.toMethodDescriptorPropertySpec(selfTypeName) }
        val serviceDescriptorValue = CodeBlock.builder()
                .indent().indent()
                .addStatement("%T.newBuilder(%S)", ServiceDescriptor::class, selfTypeName.canonicalName)
                .indent().indent()
                .apply { methodDescriptors.forEach { addStatement(".addMethod(%N)", it.name) } }
                .addStatement(".build()")
                .unindent().unindent()
                .unindent().unindent()
                .build()
        val serviceDescriptor = PropertySpec.builder("serviceDescriptor", ServiceDescriptor::class)
                .initializer(serviceDescriptorValue)
                .build()
        val companion = TypeSpec.companionObjectBuilder()
                .addProperties(methodDescriptors)
                .addProperty(serviceDescriptor)
                .build()
        return TypeSpec.classBuilder(selfTypeName)
                .addType(companion)
                .build()
    }
}

/**
 * Generator for generating a `class` for asynchronous server stub for RPC opertaions as denoted by
 * a given Protocol Buffer `service` descriptor.
 *
 * @param[context]
 *   context for code-generation metadata.
 * @param[descriptor]
 *   `service` descriptor to generate code for.
 */
class ServerStubGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.ServiceDescriptorProto
) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        val serviceTypeName = ClassName.bestGuess("${context.packageName}.${descriptor.name}")
        val selfTypeName = ClassName.bestGuess("${context.packageName}.${descriptor.name}ImplBase")
        val methods = descriptor.methodList.map { it.toServerStubMethodSpec(serviceTypeName) }
        val bindService = FunSpec.builder("bindService")
                .addModifiers(KModifier.OVERRIDE)
                .returns(ServerServiceDefinition::class.asTypeName())
                .addCode(CodeBlock.builder()
                                 .add("«")
                                 .add("return %T.builder(%T.serviceDescriptor)",
                                      ServerServiceDefinition::class.asTypeName(),
                                      serviceTypeName)
                                 .apply {
                                     descriptor.methodList.forEach {
                                         add(it.toServerStubBindMethodCodeBlock(serviceTypeName))
                                     }
                                 }
                                 .add(".build()")
                                 .add("\n»")
                                 .build()
                )
                .build()
        return TypeSpec.classBuilder(selfTypeName)
                .addModifiers(KModifier.OPEN)
                .addSuperinterface(BindableService::class)
                .addFunctions(methods)
                .addFunction(bindService)
                .build()
    }
}

/**
 * Generator for generating a `class` for asynchronous client stub for RPC opertaions as denoted by
 * a given Protocol Buffer `service` descriptor.
 *
 * @param[context]
 *   context for code-generation metadata.
 * @param[descriptor]
 *   `service` descriptor to generate code for.
 */
class AsyncClientStubGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.ServiceDescriptorProto
) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        val serviceTypeName = ClassName.bestGuess("${context.packageName}.${descriptor.name}")
        val selfTypeName = ClassName.bestGuess("${context.packageName}.${descriptor.name}Stub")
        val channelParam = ParameterSpec.builder("channel", Channel::class).build()
        val callOptionsParam = ParameterSpec.builder("callOptions", CallOptions::class).build()
        val callOptionsWithDefaultParam = callOptionsParam.toBuilder()
                .defaultValue("%T.%L", CallOptions::class.asTypeName(), "DEFAULT")
                .build()
        val channel = PropertySpec.builder(channelParam.name, channelParam.type)
                .initializer(channelParam.name)
                .build()
        val callOptions = PropertySpec.builder(callOptionsParam.name, callOptionsParam.type)
                .initializer(callOptionsParam.name)
                .build()
        val constructor = FunSpec.constructorBuilder()
                .addParameter(channelParam)
                .addParameter(callOptionsWithDefaultParam)
                .build()
        val methods = descriptor.methodList
                .map { it.toAsyncClientStubMethodSpec(serviceTypeName, channel, callOptions) }

        return TypeSpec.classBuilder(selfTypeName)
                .addModifiers(KModifier.DATA)
                // If mimicking Java's reference GRPC stub creation:
                // .apply {
                //     val buildMethod = FunSpec.builder("build")
                //             // .addModifiers(KModifier.OVERRIDE)
                //             .addParameter(channelParam)
                //             .addParameter(callOptionsParam)
                //             .returns(selfTypeName)
                //             .addCode("return %T(${channelParam.name}, ${callOptionsParam.name})", selfTypeName)
                //             .build()
                //
                //     superclass(AbstractStub::class.asTypeName().parameterizedBy(selfTypeName))
                //     addSuperclassConstructorParameter("%L", channelParam.name)
                //     addSuperclassConstructorParameter("%L", callOptionsParam.name)
                //     addFunction(buildMethod)
                // }
                .primaryConstructor(constructor)
                .addProperty(channel)
                .addProperty(callOptions)
                .addFunctions(methods)
                .build()
    }
}

/**
 * Resolve a given protocol buffer service method descriptor's associated type.
 *
 * @param[descriptor]
 *   the protocol buffer service method descriptor.
 *
 * @return
 *   the method type described by the descriptor, as [MethodDescriptor.MethodType].
 */
private fun getMethodType(
    descriptor: DescriptorProtos.MethodDescriptorProto
): MethodDescriptor.MethodType {
    return when {
        !descriptor.clientStreaming && !descriptor.serverStreaming ->
            MethodDescriptor.MethodType.UNARY
        !descriptor.clientStreaming && descriptor.serverStreaming ->
            MethodDescriptor.MethodType.SERVER_STREAMING
        descriptor.clientStreaming && !descriptor.serverStreaming ->
            MethodDescriptor.MethodType.CLIENT_STREAMING
        else -> MethodDescriptor.MethodType.BIDI_STREAMING
    }
}

/**
 * Extension function to [DescriptorProtos.MethodDescriptorProto] to create a [PropertySpec]
 * instance that describes the associated [MethodDescriptor].
 *
 * @param[service]
 *   the enclosing protocol buffer service descriptor's type.
 *
 * @return
 *   the method descriptor as a [PropertySpec].
 */
private fun DescriptorProtos.MethodDescriptorProto.toMethodDescriptorPropertySpec(
    service: ClassName
): PropertySpec {
    val serviceName = ClassName.bestGuess(service.canonicalName)
    val requestType = ClassName.bestGuess(inputType.removePrefix("."))
    val responseType = ClassName.bestGuess(outputType.removePrefix("."))
    val type = MethodDescriptor::class.asTypeName().parameterizedBy(requestType, responseType)
    val value = CodeBlock.builder()
            .indent().indent()
            .addStatement("%T.newBuilder<%T,·%T>()", MethodDescriptor::class, requestType, responseType)
            .indent().indent()
            .addStatement(".setType(%T.%L)", MethodDescriptor.MethodType::class, getMethodType(this))
            .addStatement(".setFullMethodName(%S)", "$serviceName/$name")
            .addStatement(".setSampledToLocalTracing(%L)", true)
            .addStatement(
                    ".setRequestMarshaller(%T.newMarshaller(%T.serializer(),·%T.defaultValue))",
                    GrpcSupport::class.asTypeName(),
                    requestType,
                    requestType
            )
            .addStatement(
                    ".setResponseMarshaller(%T.newMarshaller(%T.serializer(),·%T.defaultValue))",
                    GrpcSupport::class.asTypeName(),
                    responseType,
                    responseType
            )
            .addStatement(".build()")
            .unindent().unindent()
            .unindent().unindent()
            .build()

    return PropertySpec.builder("${name}Descriptor", type).initializer(value).build()
}

/**
 * Extension function to [DescriptorProtos.MethodDescriptorProto] to create a [FunSpec]
 * instance that describes the server stub service method.
 *
 * @param[service]
 *   the enclosing protocol buffer service descriptor's type.
 *
 * @return
 *   the server stub method declaration as a [FunSpec].
 */
private fun DescriptorProtos.MethodDescriptorProto.toServerStubMethodSpec(
    service: ClassName
): FunSpec {
    val response = StreamObserver::class.asTypeName()
            .parameterizedBy(ClassName.bestGuess(outputType.removePrefix(".")))
            .let { ParameterSpec.builder("responseObserver", it).build() }
    val requestType = ClassName.bestGuess(inputType.removePrefix("."))
    val request = ParameterSpec.builder("request", requestType).build()

    val format = when {
        !clientStreaming && !serverStreaming ->
            "%T.asyncUnimplementedUnaryCall(%T.%LDescriptor,·%L)"
        !clientStreaming && serverStreaming ->
            "%T.asyncUnimplementedUnaryCall(%T.%LDescriptor,·%L)"
        clientStreaming && !serverStreaming ->
            "return %T.asyncUnimplementedStreamingCall(%T.%LDescriptor,·%L)"
        else ->
            "return %T.asyncUnimplementedStreamingCall(%T.%LDescriptor,·%L)"
    }

    return FunSpec.builder(name)
            .addModifiers(KModifier.OPEN)
            .apply {
                if (clientStreaming) {
                    returns(StreamObserver::class.asTypeName().parameterizedBy(requestType))
                } else {
                    addParameter(request)
                }
            }
            .addParameter(response)
            .addCode("$format\n", ServerCalls::class.asTypeName(), service, name, response.name)
            .build()
}

/**
 * Extension function to [DescriptorProtos.MethodDescriptorProto] to create a [CodeBlock]
 * corresponding to the initialization of [BindableService.bindService] that pertains to the
 * method descriptor.
 *
 * @param[service]
 *   the enclosing protocol buffer service descriptor's type.
 *
 * @return
 *   the initialization code snippet that pertains to the method descriptor.
 */
private fun DescriptorProtos.MethodDescriptorProto.toServerStubBindMethodCodeBlock(
    service: ClassName
): CodeBlock {
    val format = when {
        !clientStreaming && !serverStreaming ->
            "%T.asyncUnaryCall(%T.newUnaryMethod(::%N))"
        !clientStreaming && serverStreaming ->
            "%T.asyncServerStreamingCall(%T.newServerStreamingMethod(::%N))"
        clientStreaming && !serverStreaming ->
            "%T.asyncClientStreamingCall(%T.newClientStreamingMethod(::%N))"
        else ->
            "%T.asyncBidiStreamingCall(%T.newBiDirectionalStreamingMethod(::%N))"
    }

    return CodeBlock.builder()
            .add(" .addMethod(%T.%LDescriptor,·$format)",
                 service,
                 name,
                 ServerCalls::class.asTypeName(),
                 GrpcSupport::class.asTypeName(),
                 name
            )
            .build()
}

/**
 * Extension function to [DescriptorProtos.MethodDescriptorProto] to create a [FunSpec]
 * instance that describes the asynchronous client stub service method.
 *
 * @param[service]
 *   the enclosing protocol buffer service descriptor's type.
 * @param[channel]
 *   the [PropertySpec] associated with the `channel` property / parameter.
 * @param[callOptions]
 *   the [PropertySpec] associated with the `callOptions` property / parameter.
 */
private fun DescriptorProtos.MethodDescriptorProto.toAsyncClientStubMethodSpec(
    service: ClassName,
    channel: PropertySpec,
    callOptions: PropertySpec
): FunSpec {
    // Async calls always have response as a stream (e.g. unary call yields stream of 1 event).
    val response = StreamObserver::class.asTypeName()
            .parameterizedBy(ClassName.bestGuess(outputType.removePrefix(".")))
            .let { ParameterSpec.builder("responseObserver", it).build() }
    val requestType = ClassName.bestGuess(inputType.removePrefix("."))
    val request = ParameterSpec.builder("request", requestType).build()

    val codeBlock = when {
        !clientStreaming && !serverStreaming ->
            "%type:T.asyncUnaryCall(%channel:N.newCall(%service:T.%method:N,·%options:N),·%request:N,·%response:N)"
        !clientStreaming && serverStreaming ->
            "%type:T.asyncServerStreamingCall(%channel:N.newCall(%service:T.%method:N,·%options:N),·%request:N,·%response:N)"
        clientStreaming && !serverStreaming ->
            "return %type:T.asyncClientStreamingCall(%channel:N.newCall(%service:T.%method:N,·%options:N),·%response:N)"
        else -> "return %type:T.asyncBidiStreamingCall(%channel:N.newCall(%service:T.%method:N,·%options:N),·%response:N)"
    }.let {
        CodeBlock.builder()
                .add("«")
                .addNamed(it, mapOf(
                        "type" to ClientCalls::class,
                        "channel" to channel.name,
                        "service" to ClassName.bestGuess(service.canonicalName),
                        "method" to "${name}Descriptor",
                        "options" to callOptions.name,
                        "request" to request.name,
                        "response" to response.name
                ))
                .add("\n»")
                .build()
    }

    return FunSpec.builder(name)
            .apply {
                if (clientStreaming) {
                    returns(StreamObserver::class.asTypeName().parameterizedBy(requestType))
                } else {
                    addParameter(request)
                }
            }
            .addParameter(response)
            .addCode(codeBlock)
            .build()
}

/**
 * Protocol Buffer Compiler (protoc) plugin program main entry point.
 *
 * The contract mandates that the plugin receives input on STDIN, in the form of a protocol buffer
 * formatted request ([PluginProtos.CodeGeneratorRequest]), and outputs to STDOUT, in the form of
 * a protocol buffer formatted response ([PluginProtos.CodeGeneratorResponse]).
 */
fun main() {
    val request = PluginProtos.CodeGeneratorRequest.parseFrom(System.`in`)
    var response = PluginProtos.CodeGeneratorResponse.newBuilder()

    request.protoFileList.forEach { descriptor ->
        val context = Context(descriptor.`package`)

        descriptor.serviceList.forEach { message ->
            response = response.addFile(ServiceFileGenerator(context, message).generate())
        }
    }

    System.out.write(response.build().toByteArray())
}
