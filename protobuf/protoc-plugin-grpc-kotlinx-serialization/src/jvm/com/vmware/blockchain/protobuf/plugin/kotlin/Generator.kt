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
import com.vmware.blockchain.grpc.kotlinx.serialization.GrpcSupport
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
 * @param[sourcePackageName]
 *   name of the package in the source descriptor file.
 */
data class Context(val packageName: String, val sourcePackageName: String) {
    /**
     * Resolve the best-guess type name of a given input type's [String] literal representation
     * declared in Protocol Buffer descriptors.
     *
     * Note:
     * Unlike the analogous method in protoc-plugin-kotlinx-serialization, this version does
     * not resolve against known types accumulated while processing code generation. The implication
     * is that any message types declared in a .proto definition pertaining to service RPCs must
     * also appear within the same source .proto definition file (i.e. share the same package
     * namespace).
     *
     * @param[literal]
     *   string literal of a type name to resolve.
     *
     * @returns
     *   resolved type name.
     */
    fun resolveTypeLiteral(literal: String): String {
        return literal.removePrefix(".").replaceFirst(sourcePackageName, packageName)
    }
}

/**
 * Generator for generating a file containing the top-level service type denoted by a Protocol
 * Buffer `message` descriptor.
 *
 * @param[context]
 *   metadata context for code-generation.
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
 *   metadata context for code-generation.
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
        val serviceName = "${context.sourcePackageName}.${descriptor.name}"
        val selfTypeName = ClassName.bestGuess("${context.packageName}.${descriptor.name}")
        val methodDescriptors = descriptor.methodList
                .map { it.toMethodDescriptorPropertySpec(context, descriptor) }
        val serviceDescriptorValue = CodeBlock.builder()
                .indent().indent()
                .addStatement("%T.newBuilder(%S)", ServiceDescriptor::class, serviceName)
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
        context: Context,
        service: DescriptorProtos.ServiceDescriptorProto
    ): PropertySpec {
        val serviceName = "${context.sourcePackageName}.${service.name}"
        val requestType = ClassName.bestGuess(context.resolveTypeLiteral(inputType))
        val responseType = ClassName.bestGuess(context.resolveTypeLiteral(outputType))
        val type = MethodDescriptor::class.asTypeName().parameterizedBy(requestType, responseType)
        val value = CodeBlock.builder()
                .indent().indent()
                .addStatement(
                        "%T.newBuilder<%T,·%T>()",
                        MethodDescriptor::class,
                        requestType,
                        responseType
                )
                .indent().indent()
                .addStatement(
                        ".setType(%T.%L)",
                        MethodDescriptor.MethodType::class,
                        getMethodType(this)
                )
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

        return PropertySpec.builder("${name.decapitalize()}Descriptor", type)
                .initializer(value)
                .build()
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
}

/**
 * Generator for generating a `class` for asynchronous server stub for RPC operations as denoted by
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
        val methods = descriptor.methodList
                .map { it.toServerStubMethodSpec(context, serviceTypeName) }
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
        val normalized = name.decapitalize()
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
                     normalized,
                     ServerCalls::class.asTypeName(),
                     GrpcSupport::class.asTypeName(),
                     normalized
                )
                .build()
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
        context: Context,
        service: ClassName
    ): FunSpec {
        val normalized = name.decapitalize()
        val response = StreamObserver::class.asTypeName()
                .parameterizedBy(ClassName.bestGuess(context.resolveTypeLiteral(outputType)))
                .let { ParameterSpec.builder("responseObserver", it).build() }
        val requestType = ClassName.bestGuess(context.resolveTypeLiteral(inputType))
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

        return FunSpec.builder(normalized)
                .addModifiers(KModifier.OPEN)
                .apply {
                    if (clientStreaming) {
                        returns(StreamObserver::class.asTypeName().parameterizedBy(requestType))
                    } else {
                        addParameter(request)
                    }
                }
                .addParameter(response)
                .addCode(
                        "$format\n",
                        ServerCalls::class.asTypeName(),
                        service,
                        normalized,
                        response.name
                )
                .build()
    }
}

/**
 * Generator for generating a `class` for asynchronous client stub for RPC opertaions as denoted by
 * a given Protocol Buffer `service` descriptor.
 *
 * @param[context]
 *   metadata context for code-generation.
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
                .map {
                    it.toAsyncClientStubMethodSpec(context, serviceTypeName, channel, callOptions)
                }

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
        context: Context,
        service: ClassName,
        channel: PropertySpec,
        callOptions: PropertySpec
    ): FunSpec {
        // Async calls always have response as a stream (e.g. unary call yields stream of 1 event).
        val response = StreamObserver::class.asTypeName()
                .parameterizedBy(ClassName.bestGuess(context.resolveTypeLiteral(outputType)))
                .let { ParameterSpec.builder("responseObserver", it).build() }
        val requestType = ClassName.bestGuess(context.resolveTypeLiteral(inputType))
        val request = ParameterSpec.builder("request", requestType).build()

        val normalized = name.decapitalize()
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
                            "method" to "${normalized}Descriptor",
                            "options" to callOptions.name,
                            "request" to request.name,
                            "response" to response.name
                    ))
                    .add("\n»")
                    .build()
        }

        return FunSpec.builder(normalized)
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
}

/** Plugin parameter to disable using "java_package" file option. */
const val PARAMETER_DISABLE_JAVA_PACKAGE: String = "disable-java-package"

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

    // Parse the input parameter as a comma-delimited sequence of "key=value" or "key" options.
    val parameters = request.parameter.split(",").associate {
        val parts = it.split("=", limit = 2)
        if (parts.size == 1) {
            parts[0] to null
        } else {
            parts[0] to parts[1]
        }
    }

    request.protoFileList
            .filter { request.fileToGenerateList.contains(it.name) }
            .forEach { descriptor ->
                val outputPackage = descriptor.`package`
                        .takeIf { parameters.containsKey(PARAMETER_DISABLE_JAVA_PACKAGE) }
                        ?: descriptor.options.javaPackage
                val context = Context(outputPackage, descriptor.`package`)

                descriptor.serviceList.forEach { message ->
                    response = response.addFile(ServiceFileGenerator(context, message).generate())
                }
            }

    System.out.write(response.build().toByteArray())
}
