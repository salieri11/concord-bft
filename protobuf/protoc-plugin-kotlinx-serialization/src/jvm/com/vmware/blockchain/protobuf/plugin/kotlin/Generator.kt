/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.plugin.kotlin

import com.google.protobuf.DescriptorProtos
import com.google.protobuf.compiler.PluginProtos
import com.squareup.kotlinpoet.AnnotationSpec
import com.squareup.kotlinpoet.ClassName
import com.squareup.kotlinpoet.CodeBlock
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.FunSpec
import com.squareup.kotlinpoet.KModifier
import com.squareup.kotlinpoet.ParameterizedTypeName.Companion.parameterizedBy
import com.squareup.kotlinpoet.ParameterSpec
import com.squareup.kotlinpoet.ParameterizedTypeName
import com.squareup.kotlinpoet.PropertySpec
import com.squareup.kotlinpoet.TypeName
import com.squareup.kotlinpoet.TypeSpec
import com.squareup.kotlinpoet.asClassName
import com.squareup.kotlinpoet.asTypeName
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.GeneratedModel
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoFileDescriptor
import com.vmware.blockchain.protobuf.kotlinx.serialization.encodeBase64
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Required
import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable
import kotlinx.serialization.protobuf.ProtoNumberType
import kotlinx.serialization.protobuf.ProtoType

/**
 * Denotes the context for code-generation based on a given Protocol Buffer descriptor file.
 *
 * @property[knownFiles]
 *   mapping of known .proto definitions to the associated type's canonical class name.
 * @property[knownTypes]
 *   mapping of known type literals (as would be seen in Protocol Buffer descriptor) to the
 *   associated type's canonical class name.
 * @property[packageName]
 *   name of the package that all generated types should be grouped in.
 * @property[sourcePackageName]
 *   name of the package in the source descriptor file.
 */
data class Context(
    val knownFiles: MutableMap<String, String>,
    val knownTypes: MutableMap<String, String>,
    val packageName: String,
    val sourcePackageName: String
) {
    /**
     * Resolve the best-guess type name of a given input type's [String] literal representation
     * declared in Protocol Buffer descriptors.
     *
     * @param[literal]
     *   string literal of a type name to resolve.
     *
     * @returns
     *   resolved type name.
     */
    fun resolveTypeLiteral(literal: String): String {
        // Look up known types, or best-guess based on the current context's package.
        val typeName = literal.removePrefix(".").replaceFirst(sourcePackageName, packageName)
        return knownTypes.getOrDefault(literal, typeName)
    }

    /**
     * Register a type using a given Protocol Buffer message or enum descriptor's name.
     *
     * @param[name]
     *   descriptor's name (as type name).
     */
    fun registerType(name: String) {
        knownTypes[".$sourcePackageName.$name"] = "$packageName.$name"
    }

    /**
     * Resolve the best-guess type name of [ProtoFileDescriptor] corresponding to the input file
     * descriptor name [String] literal.
     *
     * @param[literal]
     *   string literal of a descriptor name to resolve.
     *
     * @returns
     *   resolved type name.
     */
    fun resolveFileDescriptorType(literal: String): String {
        // Look up known types, or best-guess based on the current context's package.
        val typeName = literal.replaceFirst(sourcePackageName, packageName)
        return knownFiles.getOrDefault(literal, typeName)
    }

    /**
     * Register a .proto file using a given canonical type name designated for this .proto file.
     *
     * @param[name]
     *   file descriptor's name (.proto).
     * @param[typeName]
     *   type name assigned to the file descriptor name.
     */
    fun registerFile(name: String, typeName: String) {
        knownFiles[name] = typeName
    }
}

/**
 * Generator for generating a file describing the Protocol Buffer source (.proto).
 *
 * @param[context]
 *   metadata context for code-generation.
 * @param[descriptor]
 *   `file` proto descriptor to generate code for.
 */
class ProtoFileGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.FileDescriptorProto
) {
    /**
     * Create a new [PluginProtos.CodeGeneratorResponse.File] instance corresponding to the
     * descriptor associated with this generator.
     */
    fun generate(): PluginProtos.CodeGeneratorResponse.File {
        val file = PluginProtos.CodeGeneratorResponse.File.newBuilder()
        val name = descriptor.name.split("/").last()
                .removeSuffix(".proto").plus("_proto")
                .split("_")
                .joinToString(separator = "") { it.capitalize() }
        file.name = "${context.packageName.replace('.', '/')}/$name.kt"

        // Register this proto file with the context.
        context.registerFile(descriptor.name, "${context.packageName}.$name")

        // Generate the FileSpec.
        val fileSpec = FileSpec.builder(context.packageName, descriptor.name)
                .addType(generateProtoType(name))
                .build()

        // Write it out to string, allow KotlinPoet to register this new type in the process.
        file.content = buildString { fileSpec.writeTo(this) }

        return file.build()
    }

    /**
     * Create a new [TypeSpec] instance corresponding to the [DescriptorProtos.FileDescriptorProto]
     * associated with this generator.
     */
    private fun generateProtoType(name: String): TypeSpec {
        val selfTypeName = ClassName.bestGuess("${context.packageName}.$name")
        val protoPropertyType = String::class

        // Clear source code info field (does not impact runtime regeneration of the descriptor).
        val protoData = descriptor.toBuilder().clearSourceCodeInfo().build().toByteArray()
        val encodedData = String(ByteString.of(*protoData).encodeBase64().toByteArray())
        val encodedDataProperty = PropertySpec.builder("encodedData", protoPropertyType)
                .addModifiers(KModifier.OVERRIDE)
                .initializer("%S", encodedData)
                .build()
        val dependenciesPropertyType = List::class.parameterizedBy(ProtoFileDescriptor::class)
        val dependenciesTypes = descriptor.dependencyList
                .map { ClassName.bestGuess(context.resolveFileDescriptorType(it)) }
                .toTypedArray()
        val dependenciesProperty = PropertySpec.builder("dependencies", dependenciesPropertyType)
                .addModifiers(KModifier.OVERRIDE)
                .initializer(
                        "listOf(${(dependenciesTypes.indices).joinToString(", ") { "%T" }})",
                        *dependenciesTypes
                )
                .build()
        val companion = TypeSpec.companionObjectBuilder()
                .addSuperinterface(ProtoFileDescriptor::class)
                .addProperty(encodedDataProperty)
                .addProperty(dependenciesProperty)
                .build()

        return TypeSpec.classBuilder(selfTypeName)
                .addType(companion)
                .build()
    }
}

/**
 * Generator for generating a file containing the top-level message type denoted by a Protocol
 * Buffer `message` descriptor.
 *
 * @param[context]
 *   metadata context for code-generation.
 * @param[descriptor]
 *   `message` descriptor to generate code for.
 */
class MessageFileGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.DescriptorProto
) {
    /**
     * Create a new [PluginProtos.CodeGeneratorResponse.File] instance corresponding to the
     * descriptor associated with this generator.
     */
    fun generate(): PluginProtos.CodeGeneratorResponse.File {
        val file = PluginProtos.CodeGeneratorResponse.File.newBuilder()
        file.name = "${context.packageName.replace('.', '/')}/${descriptor.name}.kt"

        // Generate the FileSpec.
        val fileSpec = FileSpec.builder(context.packageName, descriptor.name)
                .addType(MessageGenerator(context, descriptor).generate())
                .build()

        // Write it out to string, allow KotlinPoet to register this new type in the process.
        file.content = buildString { fileSpec.writeTo(this) }

        return file.build()
    }
}

/**
 * Generator for generating a file containing the top-level enum type denoted by a Protocol Buffer
 * `enum` descriptor.
 *
 * @param[context]
 *   metadata context for code-generation.
 * @param[descriptor]
 *   `enum` descriptor to generate code for.
 */
class EnumFileGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.EnumDescriptorProto
) {
    /**
     * Create a new [PluginProtos.CodeGeneratorResponse.File] instance corresponding to the
     * descriptor associated with this generator.
     */
    fun generate(): PluginProtos.CodeGeneratorResponse.File {
        val file = PluginProtos.CodeGeneratorResponse.File.newBuilder()
        file.name = "${context.packageName.replace('.', '/')}/${descriptor.name}.kt"

        // Generate the FileSpec.
        val fileSpec = FileSpec.builder(context.packageName, descriptor.name)
                .addType(EnumGenerator(context, descriptor).generate())
                .build()

        // Write it out to string, allow KotlinPoet to register this new type in the process.
        file.content = buildString { fileSpec.writeTo(this) }

        return file.build()
    }
}

/**
 * Generator for generating a `class` as denoted by a given Protocol Buffer `message` descriptor.
 *
 * @param[context]
 *   metadata context for code-generation.
 * @param[descriptor]
 *   `message` descriptor to generate code for.
 */
class MessageGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.DescriptorProto
) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        // Register this type with the context.
        context.registerType(descriptor.name)

        val (mapEntry, nonMapEntry) = descriptor.nestedTypeList.partition { it.options.mapEntry }
        val nestedTypes = nonMapEntry
                .map { nestedType -> MessageGenerator(context, nestedType).generate() }
        val enumTypes = descriptor.enumTypeList
                .map { enumType -> EnumGenerator(context, enumType).generate() }
        val properties = descriptor.fieldList
                .map { it.toPropertySpec(mapEntry) }
        val constructor = descriptor.fieldList
                .map { it.toParameterSpec(mapEntry) }
                .let { FunSpec.constructorBuilder().addParameters(it).build() }
        val selfTypeName = ClassName.bestGuess(descriptor.name)
        val serializer = FunSpec.builder("getSerializer")
                .addAnnotation(AnnotationSpec.builder(JvmStatic::class).build())
                .returns(KSerializer::class.asTypeName().parameterizedBy(selfTypeName))
                .addCode(CodeBlock.of("return serializer()\n"))
                .build()
        val companion = TypeSpec.companionObjectBuilder()
                .addProperty(PropertySpec.builder("defaultValue", selfTypeName)
                                     .initializer("%T()", selfTypeName)
                                     .build())
                .addFunction(serializer)
                .build()
        return TypeSpec.classBuilder(selfTypeName)
                .addAnnotation(AnnotationSpec.builder(Serializable::class).build())
                .addAnnotation(AnnotationSpec.builder(GeneratedModel::class).build())
                .apply {
                    /* Data class must have at least 1 field declared. */
                    if (descriptor.fieldCount > 0) {
                        addModifiers(KModifier.DATA)
                    }
                }
                .primaryConstructor(constructor)
                .addProperties(properties)
                .addType(companion)
                .addTypes(enumTypes)
                .addTypes(nestedTypes)
                .build()
    }

    /**
     * Extension function to [DescriptorProtos.FieldDescriptorProto] to create a [PropertySpec]
     * instance that describes the field descriptor.
     *
     * @param[mapEntryTypes]
     *   list of known map entry types (to help differentiate list from map for repeated fields).
     */
    private fun DescriptorProtos.FieldDescriptorProto.toPropertySpec(
        mapEntryTypes: List<DescriptorProtos.DescriptorProto> = emptyList()
    ): PropertySpec {
        return PropertySpec.builder(getFieldName(this), getFieldType(this, mapEntryTypes))
                .initializer(getFieldName(this)) // Set initializer to point to constructor param.
                .build()
    }

    /**
     * Extension function to [DescriptorProtos.FieldDescriptorProto] to create a [ParameterSpec]
     * instance that describes the field descriptor.
     *
     * @param[mapEntryTypes]
     *   list of known map entry types (to help differentiate list from map for repeated fields).
     */
    private fun DescriptorProtos.FieldDescriptorProto.toParameterSpec(
        mapEntryTypes: List<DescriptorProtos.DescriptorProto> = emptyList()
    ): ParameterSpec {
        val fieldType = getFieldType(this, mapEntryTypes)
        return ParameterSpec.builder(getFieldName(this), fieldType)
                .addAnnotation(AnnotationSpec.builder(SerialId::class)
                                       .addMember("%L", number).build())
                .apply {
                    if (label == DescriptorProtos.FieldDescriptorProto.Label.LABEL_REQUIRED) {
                        addAnnotation(AnnotationSpec.builder(Required::class).build())
                    }
                }
                .defaultValue(getFieldDefaultValue(this, fieldType))
                .apply {
                    val fixed = AnnotationSpec.builder(ProtoType::class)
                            .addMember("%T.%L", ProtoNumberType::class, ProtoNumberType.FIXED)
                    val signed = AnnotationSpec.builder(ProtoType::class)
                            .addMember("%T.%L", ProtoNumberType::class, ProtoNumberType.SIGNED)
                    when (type) {
                        DescriptorProtos.FieldDescriptorProto.Type.TYPE_FIXED32 ->
                            addAnnotation(fixed.build())
                        DescriptorProtos.FieldDescriptorProto.Type.TYPE_FIXED64 ->
                            addAnnotation(fixed.build())
                        DescriptorProtos.FieldDescriptorProto.Type.TYPE_SFIXED32 ->
                            addAnnotation(fixed.build())
                        DescriptorProtos.FieldDescriptorProto.Type.TYPE_SFIXED64 ->
                            addAnnotation(fixed.build())
                        DescriptorProtos.FieldDescriptorProto.Type.TYPE_SINT32 ->
                            addAnnotation(signed.build())
                        DescriptorProtos.FieldDescriptorProto.Type.TYPE_SINT64 ->
                            addAnnotation(signed.build())
                        else -> Unit
                    }
                }
                .build()
    }

    private fun getFieldName(field: DescriptorProtos.FieldDescriptorProto): String {
        return field.name
                .splitToSequence('_')
                .let { parts ->
                    parts.take(1).map { it.toLowerCase() } + parts.drop(1).map { it.capitalize() }
                }
                .joinToString("")
    }

    /**
     * Resolve a given protocol buffer message field descriptor's associated base type, before
     * considering other type labels and modifiers (e.g. repeated).
     *
     * @param[field]
     *   the protocol buffer field descriptor.
     *
     * @return
     *   the type described by the field descriptor, as a [ClassName].
     */
    private fun getFieldBaseType(field: DescriptorProtos.FieldDescriptorProto): ClassName {
        return when (field.type) {
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_INT32 -> Int::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_SINT32 -> Int::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_SFIXED32 -> Int::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_UINT32 -> Int::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_FIXED32 -> Int::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_INT64 -> Long::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_SINT64 -> Long::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_SFIXED64 -> Long::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_UINT64 -> Long::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_FIXED64 -> Long::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_FLOAT -> Float::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_DOUBLE -> Double::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_BOOL -> Boolean::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_STRING -> String::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_BYTES -> ByteString::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_GROUP -> Any::class.asTypeName()
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_ENUM ->
                ClassName.bestGuess(context.resolveTypeLiteral(field.typeName))
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_MESSAGE ->
                ClassName.bestGuess(context.resolveTypeLiteral(field.typeName))
            else -> Any::class.asTypeName()
        }
    }

    /**
     * Resolve a given protocol buffer message field descriptor's associated type.
     *
     * @param[field]
     *   the protocol buffer field descriptor.
     * @param[mapEntryTypes]
     *   list of known map entry types (to help differentiate list from map for repeated fields).
     *
     * @return
     *   the type described by the field descriptor, as a [TypeName].
     */
    private fun getFieldType(
        field: DescriptorProtos.FieldDescriptorProto,
        mapEntryTypes: List<DescriptorProtos.DescriptorProto> = emptyList()
    ): TypeName {
        val baseType = getFieldBaseType(field)

        // Convert repeated to List/Map, otherwise return base type.
        return if (field.label == DescriptorProtos.FieldDescriptorProto.Label.LABEL_REPEATED) {
            // Assumption: provided mapEntryTypes have unique names by type's simple name.
            mapEntryTypes.firstOrNull { it.name == baseType.simpleName }
                    ?.let { resolveMapEntryType(it) }
                    ?: List::class.asTypeName().parameterizedBy(baseType)
        } else {
            baseType
        }
    }

    /**
     * Resolve a given protocol buffer message descriptor as a map entry type, according to the
     * official specification.
     *
     * ```
     * For maps fields:
     *     map<KeyType, ValueType> map_field = 1;
     *
     * The parsed descriptor looks like:
     *     message MapFieldEntry {
     *         option map_entry = true;
     *         optional KeyType key = 1;
     *         optional ValueType value = 2;
     *     }
     *     repeated MapFieldEntry map_field = 1;
     * ```
     *
     * @param[entry]
     *   descriptor corresponding to the map entry type.
     */
    private fun resolveMapEntryType(entry: DescriptorProtos.DescriptorProto): TypeName {
        // Sanity-check for pre-condition.
        require(entry.fieldCount == 2)

        val (keyType, valueType) = if (entry.fieldList[0].number == 1) {
            // Key type was first.
            getFieldType(entry.fieldList[0]) to getFieldBaseType(entry.fieldList[1])
        } else {
            // Value type was first.
            getFieldType(entry.fieldList[1]) to getFieldBaseType(entry.fieldList[0])
        }

        return Map::class.asTypeName().parameterizedBy(keyType, valueType)
    }

    /**
     * Resolve a given protocol buffer message field descriptor's associated type's default value.
     *
     * @param[field]
     *   the protocol buffer field descriptor.
     * @param[resolvedType]
     *   resolved Kotlin type for the field descriptor.
     *
     * @return
     *   the default value for the type described by the field descriptor, as a [CodeBlock].
     */
    private fun getFieldDefaultValue(
        field: DescriptorProtos.FieldDescriptorProto,
        resolvedType: TypeName
    ): CodeBlock {
        // List collection's default value is just an empty list (non-null).
        return if (field.label == DescriptorProtos.FieldDescriptorProto.Label.LABEL_REPEATED) {
            CodeBlock.of(
                    "%L",
                    (resolvedType as? ParameterizedTypeName)
                            ?.rawType
                            ?.takeIf { it == Map::class.asClassName() }
                            ?.let { "emptyMap()" }
                            ?: "emptyList()"
            )
        } else {
            // Currently, the resolved Kotlin type does not factor in to resolving default value.
            when (field.type) {
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_INT32 -> CodeBlock.of("%L", "0")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_SINT32 -> CodeBlock.of("%L", "0")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_SFIXED32 -> CodeBlock.of("%L", "0")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_UINT32 -> CodeBlock.of("%L", "0")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_FIXED32 -> CodeBlock.of("%L", "0")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_INT64 -> CodeBlock.of("%L", "0L")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_SINT64 -> CodeBlock.of("%L", "0L")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_SFIXED64 -> CodeBlock.of("%L", "0L")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_UINT64 -> CodeBlock.of("%L", "0L")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_FIXED64 -> CodeBlock.of("%L", "0L")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_FLOAT -> CodeBlock.of("%L", "0.0F")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_DOUBLE -> CodeBlock.of("%L", "0.0")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_BOOL -> CodeBlock.of("%L", "false")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_STRING -> CodeBlock.of("%L","\"\"")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_ENUM ->
                    CodeBlock.of(
                            "%T.%L",
                            ClassName.bestGuess(context.resolveTypeLiteral(field.typeName)),
                            "defaultValue"
                    )
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_BYTES ->
                    CodeBlock.of("%T.%L", ByteString::class, "empty()")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_GROUP -> CodeBlock.of("%L", "null")
                DescriptorProtos.FieldDescriptorProto.Type.TYPE_MESSAGE ->
                    CodeBlock.of(
                            "%T.%L",
                            ClassName.bestGuess(context.resolveTypeLiteral(field.typeName)),
                            "defaultValue"
                    )
                else -> CodeBlock.of("%L", "$field.defaultValue")
            }
        }
    }
}

/**
 * Generator for generating a `enum class` as denoted by a given Protocol Buffer `enum` descriptor.
 *
 * @param[context]
 *   metadata context for code-generation.
 * @param[descriptor]
 *   `enum` descriptor to generate code for.
 */
class EnumGenerator(
    private val context: Context,
    private val descriptor: DescriptorProtos.EnumDescriptorProto
) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        // Register this type with the context.
        context.registerType(descriptor.name)

        val selfTypeName = ClassName.bestGuess(descriptor.name)
        val companion = TypeSpec.companionObjectBuilder()
                .addProperty(PropertySpec.builder("defaultValue", selfTypeName)
                                     .initializer("values()[0]")
                                     .build())
                .build()
        return TypeSpec.enumBuilder(selfTypeName)
                .addAnnotation(AnnotationSpec.builder(GeneratedModel::class).build())
                .primaryConstructor(FunSpec.constructorBuilder()
                                            .addParameter("value", Int::class)
                                            .build())
                .addProperty(PropertySpec.builder("value", Int::class, KModifier.PRIVATE)
                                     .initializer("value")
                                     .build())
                .addType(companion)
                .apply {
                    for (constant in descriptor.valueList) {
                        addEnumConstant(
                                constant.name,
                                TypeSpec.anonymousClassBuilder()
                                        .addSuperclassConstructorParameter("%L", constant.number)
                                .build())
                    }
                }
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

    // Map to accumulate types as they get generated.
    val knownFiles = mutableMapOf<String, String>()
    val knownTypes = mutableMapOf<String, String>()

    request.protoFileList
            .filter { request.fileToGenerateList.contains(it.name) }
            .forEach { descriptor ->
                val outputPackage = descriptor.`package`
                        .takeIf { parameters.containsKey(PARAMETER_DISABLE_JAVA_PACKAGE) }
                        ?: descriptor.options.javaPackage
                val context = Context(knownFiles, knownTypes, outputPackage, descriptor.`package`)

                response.addFile(ProtoFileGenerator(context, descriptor).generate())

                descriptor.messageTypeList.forEach { message ->
                    response = response.addFile(MessageFileGenerator(context, message).generate())
                }

                descriptor.enumTypeList.forEach { enum ->
                    response = response.addFile(EnumFileGenerator(context, enum).generate())
                }
            }

    System.out.write(response.build().toByteArray())
}
