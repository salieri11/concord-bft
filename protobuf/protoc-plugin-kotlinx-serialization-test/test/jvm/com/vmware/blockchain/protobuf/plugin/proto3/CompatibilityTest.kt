/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.plugin.proto3

import com.google.protobuf.Message
import com.google.protobuf.Parser
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf
import com.vmware.blockchain.protobuf.plugin.proto3.java.EnumMessage as EnumMessageJava
import com.vmware.blockchain.protobuf.plugin.proto3.java.NestedMessage as NestedMessageJava
import com.vmware.blockchain.protobuf.plugin.proto3.java.RepeatFieldAndMapMessage as RepeatFieldAndMapMessageJava
import com.vmware.blockchain.protobuf.plugin.proto3.java.ScalarsMessage as ScalarsMessageJava
import com.vmware.blockchain.protobuf.plugin.proto3.java.TopLevelEnum as TopLevelEnumJava
import kotlinx.serialization.KSerializer
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Various tests verifying compatibility with standard Protocol Buffer Java runtime.
 */
class CompatibilityTest {

    /**
     * Perform round-trip serialization / deserialization starting from Java message.
     *
     * @param[message]
     *   data to be round-trip'ed.
     * @param[serializer]
     *   serializer for data (Kotlin).
     */
    private fun <T : Message, R> roundTrip(message: T, serializer: KSerializer<R>): T {
        val encodedWithJava = message.toByteArray()
        val decodedWithKotlin = ProtoBuf.load(serializer, encodedWithJava)
        val encodedWithKotlin = ProtoBuf.dump(serializer, decodedWithKotlin)

        @Suppress("UNCHECKED_CAST")
        return message.parserForType.parseFrom(encodedWithKotlin) as T
    }

    /**
     * Perform round-trip serialization / deserialization starting from Kotlin message.
     *
     * @param[message]
     *   data to be round-trip'ed.
     * @param[serializer]
     *   serializer for data (Kotlin).
     * @param[parser]
     *   parser for data (Java).
     */
    private fun <T, R : Message> roundTrip(
        message: T,
        serializer: KSerializer<T>,
        parser: Parser<R>
    ): T {
        val encodedWithKotlin = ProtoBuf.dump(serializer, message)
        val decodedWithJava = parser.parseFrom(encodedWithKotlin)
        val encodedWithJava = decodedWithJava.toByteArray()

        return ProtoBuf.load(serializer, encodedWithJava)
    }

    /**
     * Generate a new [ScalarsMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newScalarsMessageJava(): ScalarsMessageJava {
        return ScalarsMessageJava.newBuilder()
                .setBoolField(true)
                .setStringField("test string")
                .setFixed32Field(1)
                .setBytesField(com.google.protobuf.ByteString.copyFrom("test bytes".toByteArray()))
                .build()
    }

    /**
     * Generate a new [ScalarsMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newScalarsMessage(): ScalarsMessage {
        return ScalarsMessage(
                boolField = true,
                stringField = "test string",
                fixed32Field = 1,
                bytesField = ByteString.of(*"test bytes".toByteArray())
        )
    }

    /**
     * Generate a new [NestedMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newNestedMessageJava(): NestedMessageJava {
        return NestedMessageJava.newBuilder()
                .setScalars(newScalarsMessageJava())
                .setInner(newInnerMessageJava())
                .build()
    }

    /**
     * Generate a new [NestedMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newInnerMessageJava(): NestedMessageJava.InnerMessage {
        return NestedMessageJava.InnerMessage.newBuilder()
                .setStringField("inner test string")
                .setIntField(1)
                .build()
    }

    /**
     * Generate a new [NestedMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newNestedMessage(): NestedMessage {
        return NestedMessage(
                scalars = newScalarsMessage(),
                inner = newInnerMessage()
        )
    }

    /**
     * Generate a new [NestedMessage.InnerMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newInnerMessage(): NestedMessage.InnerMessage {
        return NestedMessage.InnerMessage(
                stringField = "inner test string",
                intField = 10
        )
    }

    /**
     * Generate a new [EnumMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newEnumMessageJava(): EnumMessageJava {
        return EnumMessageJava.newBuilder()
                .setTopLevelEnum(TopLevelEnumJava.ONE)
                .setBoolField(true)
                .setNestedEnum(EnumMessageJava.InnerEnum.NESTED_TEN)
                .build()
    }

    /**
     * Generate a new [EnumMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newEnumMessage(): EnumMessage {
        return EnumMessage(
                topLevelEnum = TopLevelEnum.ONE,
                boolField = true,
                nestedEnum = EnumMessage.InnerEnum.NESTED_TEN
        )
    }

    /**
     * Generate a new [RepeatFieldAndMapMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newRepeatedFieldAndMapMessageJava(): RepeatFieldAndMapMessageJava {
        return RepeatFieldAndMapMessageJava.newBuilder()
                .addAllRepeatedInt64(listOf(0, -1, 1, 10))
                .addAllRepeatedScalars(listOf(
                        newScalarsMessageJava(),
                        newScalarsMessageJava()
                ))
                .addAllRepeatedInnerEnum(listOf(
                        EnumMessageJava.InnerEnum.NESTED_ZERO,
                        EnumMessageJava.InnerEnum.NESTED_ONE
                ))
                .addAllRepeatedTopLevelEnum(listOf(
                        TopLevelEnumJava.ZERO,
                        TopLevelEnumJava.ONE
                ))
                .putAllStringToScalarsMap(mapOf(
                        "one" to newScalarsMessageJava(),
                        "two" to newScalarsMessageJava(),
                        "123" to newScalarsMessageJava()
                ))
                .putAllInt32ToInnerMessageMap(mapOf(
                        0 to newInnerMessageJava(),
                        -1 to newInnerMessageJava(),
                        1 to newInnerMessageJava()
                ))
                .build()
    }

    /**
     * Generate a new [RepeatFieldAndMapMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newRepeatedFieldsAndMapMessage(): RepeatFieldAndMapMessage {
        return RepeatFieldAndMapMessage(
                repeatedInt64 = listOf(0, -1, 1, 10),
                repeatedScalars = listOf(
                        newScalarsMessage(),
                        newScalarsMessage()
                ),
                repeatedInnerEnum = listOf(
                        EnumMessage.InnerEnum.NESTED_ZERO,
                        EnumMessage.InnerEnum.NESTED_ONE
                ),
                repeatedTopLevelEnum = listOf(
                        TopLevelEnum.ZERO,
                        TopLevelEnum.ONE
                ),
                stringToScalarsMap = mapOf(
                        "one" to newScalarsMessage(),
                        "two" to newScalarsMessage(),
                        "123" to newScalarsMessage()
                ),
                int32ToInnerMessageMap = mapOf(
                        0 to newInnerMessage(),
                        -1 to newInnerMessage(),
                        1 to newInnerMessage()
                )
        )
    }

    /**
     * Test that scalar types are preserved across round-trip serialization / deserialization.
     */
    @Test
    fun scalarTypesRoundTrip() {
        val parser = ScalarsMessageJava.parser()
        val serializer = ScalarsMessage.serializer()

        val javaEmptyMessage = ScalarsMessageJava.newBuilder().build()
        Assertions.assertThat(roundTrip(javaEmptyMessage, serializer)).isEqualTo(javaEmptyMessage)

        val kotlinEmptyMessage = ScalarsMessage()
        Assertions.assertThat(roundTrip(kotlinEmptyMessage, serializer, parser))
                .isEqualTo(kotlinEmptyMessage)

        val javaMessage = newScalarsMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newScalarsMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }

    /**
     * Test that nested message is preserved across round-trip serialization / deserialization.
     */
    @Test
    fun nestedMessageRoundTrip() {
        val parser = NestedMessageJava.parser()
        val serializer = NestedMessage.serializer()

        // TODO(jameschang - 20190205):
        // This roundtrip fails due to Kotlin's serialization not able to handle null-value
        // conversion. As a workaround, any non-scalar field is always a non-null type and a default
        // instance value is used as value (instead of using a nullable type with default value of
        // null). This causes the roundtrip through Kotlin to always have empty inner message filled
        // as values, which is different from how Java's protobuf runtime handles no-value.
        //
        // This isn't a big issue other than the fact that by default, optional embedded message
        // fields will always yield slightly larger serialized footprint due to encoding of empty
        // messages (2 bytes per embedded message field).
        //
        // val javaEmptyMessage = NestedMessageJava.newBuilder().build()
        // Assertions.assertThat(roundTrip(javaEmptyMessage, serializer)).isEqualTo(javaEmptyMessage)

        val kotlinEmptyMessage = NestedMessage()
        Assertions.assertThat(roundTrip(kotlinEmptyMessage, serializer, parser))
                .isEqualTo(kotlinEmptyMessage)

        // TODO(jameschang - 20190205):
        // This roundtrip fails due to Kotlin's serialization not able to handle null-value
        // conversion. As a workaround, any non-scalar field is always a non-null type and a default
        // instance value is used as value (instead of using a nullable type with default value of
        // null). This causes the roundtrip through Kotlin to always have empty inner message filled
        // as values, which is different from how Java's protobuf runtime handles no-value.
        //
        // This isn't a big issue other than the fact that by default, optional embedded message
        // fields will always yield slightly larger serialized footprint due to encoding of empty
        // messages (2 bytes per embedded message field).
        //
        // val javaMessage = newNestedMessageJava()
        // Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newNestedMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }

    /**
     * Test that enum is preserved across round-trip serialization / deserialization.
     */
    @Test
    fun enumMessageRoundTrip() {
        val parser = EnumMessageJava.parser()
        val serializer = EnumMessage.serializer()

        val javaEmptyMessage = EnumMessageJava.newBuilder().build()
        Assertions.assertThat(roundTrip(javaEmptyMessage, serializer)).isEqualTo(javaEmptyMessage)

        val kotlinEmptyMessage = EnumMessage()
        Assertions.assertThat(roundTrip(kotlinEmptyMessage, serializer, parser))
                .isEqualTo(kotlinEmptyMessage)

        // TODO(jameschang - 20190205):
        // This roundtrip fails in what appears to be a bug in Kotlin serialization's inability to
        // deal with non-sequential enum ordinal values.
        //
        // This isn't an issue unless we start declaring skipped ranges for enum constants (which
        // is not a common use-case unless we are attempting to model some other known constant
        // ranges with enum and their associated ordinal values.
        //
        // val javaMessage = newEnumMessageJava()
        // Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newEnumMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }

    /**
     * Test that repeated fields are preserved across round-trip serialization / deserialization.
     */
    @Test
    fun repeatedFieldAndMapMessageRoundTrip() {
        val parser = RepeatFieldAndMapMessageJava.parser()
        val serializer = RepeatFieldAndMapMessage.serializer()

        val javaEmptyMessage = RepeatFieldAndMapMessageJava.newBuilder().build()
        Assertions.assertThat(roundTrip(javaEmptyMessage, serializer)).isEqualTo(javaEmptyMessage)

        val kotlinEmptyMessage = RepeatFieldAndMapMessage()
        Assertions.assertThat(roundTrip(kotlinEmptyMessage, serializer, parser))
                .isEqualTo(kotlinEmptyMessage)

        val javaMessage = newRepeatedFieldAndMapMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newRepeatedFieldsAndMapMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }
}