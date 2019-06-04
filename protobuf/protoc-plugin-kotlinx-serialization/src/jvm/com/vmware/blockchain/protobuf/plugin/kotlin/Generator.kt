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
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable
import kotlinx.serialization.protobuf.ProtoNumberType
import kotlinx.serialization.protobuf.ProtoType

/**
 * Denotes the context for code-generation based on a given Protocol Buffer descriptor file.
 *
 * @param[packageName]
 *   name of the package that all generated types should be grouped in.
 */
data class Context(val packageName: String)

/**
 * Generator for generating a file containing the top-level message type denoted by a Protocol
 * Buffer `message` descriptor.
 *
 * @param[context]
 *   context for code-generation metadata.
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
                .addType(MessageGenerator(descriptor).generate())
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
 *   context for code-generation metadata.
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
                .addType(EnumGenerator(descriptor).generate())
                .build()

        // Write it out to string, allow KotlinPoet to register this new type in the process.
        file.content = buildString { fileSpec.writeTo(this) }

        return file.build()
    }
}

/**
 * Generator for generating a `class` as denoted by a given Protocol Buffer `message` descriptor.
 *
 * @param[descriptor]
 *   `message` descriptor to generate code for.
 */
class MessageGenerator(private val descriptor: DescriptorProtos.DescriptorProto) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        val (mapEntry, nonMapEntry) = descriptor.nestedTypeList.partition { it.options.mapEntry }
        val nestedTypes = nonMapEntry
                .map { nestedType -> MessageGenerator(nestedType).generate() }
        val enumTypes = descriptor.enumTypeList
                .map { enumType -> EnumGenerator(enumType).generate() }
        val properties = descriptor.fieldList
                .map { it.toPropertySpec(mapEntry) }
        val constructor = descriptor.fieldList
                .map { it.toParameterSpec(mapEntry) }
                .let { FunSpec.constructorBuilder().addParameters(it).build() }
        val selfTypeName = ClassName.bestGuess(descriptor.name)
        val serializer = FunSpec.builder("getSerializer")
                .addAnnotation(AnnotationSpec.builder(JvmStatic::class).build())
                .returns(KSerializer::class.asTypeName().parameterizedBy(selfTypeName))
                .addCode(CodeBlock.of("return %T.serializer()\n", selfTypeName))
                .build()
        val companion = TypeSpec.companionObjectBuilder()
                .addProperty(PropertySpec.builder("defaultValue", selfTypeName)
                                     .initializer("%T()", selfTypeName)
                                     .build())
                .addFunction(serializer)
                .build()
        return TypeSpec.classBuilder(selfTypeName)
                .addAnnotation(AnnotationSpec.builder(Serializable::class).build())
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
}

/**
 * Generator for generating a `enum class` as denoted by a given Protocol Buffer `enum` descriptor.
 *
 * @param[descriptor]
 *   `enum` descriptor to generate code for.
 */
class EnumGenerator(private val descriptor: DescriptorProtos.EnumDescriptorProto) {
    /**
     * Create a new [TypeSpec] instance corresponding to the descriptor associated with this
     * generator.
     */
    fun generate(): TypeSpec {
        val selfTypeName = ClassName.bestGuess(descriptor.name)
        val companion = TypeSpec.companionObjectBuilder()
                .addProperty(PropertySpec.builder("defaultValue", selfTypeName)
                                     .initializer("%T.values()[0]", selfTypeName)
                                     .build())
                .build()
        return TypeSpec.enumBuilder(selfTypeName)
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

/**
 * Extension function to [DescriptorProtos.FieldDescriptorProto] to create a [PropertySpec] instance
 * that describes the field descriptor.
 *
 * @param[mapEntryTypes]
 *   list of known map entry types (to help differentiate list from map for repeated fields).
 */
private fun DescriptorProtos.FieldDescriptorProto.toPropertySpec(
    mapEntryTypes: List<DescriptorProtos.DescriptorProto> = emptyList()
): PropertySpec {
    return PropertySpec.builder(getFieldName(this), getFieldType(this, mapEntryTypes))
            .initializer(getFieldName(this)) // Set initializer to point to constructor param name.
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
            .addAnnotation(AnnotationSpec.builder(SerialId::class).addMember("%L", number).build())
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
    val typeName = field.typeName.removePrefix(".")
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
        DescriptorProtos.FieldDescriptorProto.Type.TYPE_ENUM -> ClassName.bestGuess(typeName)
        DescriptorProtos.FieldDescriptorProto.Type.TYPE_BYTES -> ByteString::class.asTypeName()
        DescriptorProtos.FieldDescriptorProto.Type.TYPE_GROUP -> Any::class.asTypeName()
        DescriptorProtos.FieldDescriptorProto.Type.TYPE_MESSAGE ->
            ClassName.bestGuess(typeName)
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
 * Resolve a given protocol buffer message descriptor as a map entry type, according to the official
 * specification.
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
    val typeName = field.typeName.removePrefix(".")

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
                CodeBlock.of("%T.%L", ClassName.bestGuess(typeName), "defaultValue")
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_BYTES ->
                CodeBlock.of("%T.%L", ByteString::class, "empty()")
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_GROUP -> CodeBlock.of("%L", "null")
            DescriptorProtos.FieldDescriptorProto.Type.TYPE_MESSAGE ->
                CodeBlock.of("%T.%L", ClassName.bestGuess(typeName), "defaultValue")
            else -> CodeBlock.of("%L", "$field.defaultValue")
        }
    }
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

        descriptor.messageTypeList.forEach { message ->
            response = response.addFile(MessageFileGenerator(context, message).generate())
        }

        descriptor.enumTypeList.forEach { enum ->
            response = response.addFile(EnumFileGenerator(context, enum).generate())
        }
    }

    System.out.write(response.build().toByteArray())
}
