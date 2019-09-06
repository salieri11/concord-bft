/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.plugin.proto2

import com.google.protobuf.Message
import com.google.protobuf.Parser
import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf
import com.vmware.blockchain.protobuf.plugin.proto2.java.EnumMessage as EnumMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.NestedMessage as NestedMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.RepeatFieldAndMapMessage as RepeatFieldAndMapMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.RequiredEnumMessage as RequiredEnumMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.RequiredNestedMessage as RequiredNestedMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.RequiredScalarsMessage as RequiredScalarsMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.ScalarsMessage as ScalarsMessageJava
import com.vmware.blockchain.protobuf.plugin.proto2.java.TopLevelEnum as TopLevelEnumJava
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

        return ProtoBuf().load(serializer, encodedWithJava)
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
                .setNestedEnum(EnumMessageJava.InnerEnum.NESTED_ONE)
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
                nestedEnum = EnumMessage.InnerEnum.NESTED_ONE
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
     * Generate a new [RequiredScalarsMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredScalarsMessageJava(): RequiredScalarsMessageJava {
        return RequiredScalarsMessageJava.newBuilder()
                .setBoolField(true)
                .setStringField("test string")
                .setFloatField(0.0F)
                .setDoubleField(0.0)
                .setInt32Field(1)
                .setFixed32Field(2)
                .setUint32Field(3)
                .setSfixed32Field(-1)
                .setSint32Field(-2)
                .setInt64Field(2)
                .setFixed64Field(4)
                .setUint64Field(6)
                .setSfixed64Field(-2)
                .setSint64Field(-4)
                .setBytesField(com.google.protobuf.ByteString.copyFrom("test bytes".toByteArray()))
                .build()
    }

    /**
     * Generate a new [RequiredScalarsMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredScalarsMessage(): RequiredScalarsMessage {
        return RequiredScalarsMessage(
                boolField = true,
                stringField = "test string",
                floatField = 0.0F,
                doubleField = 0.0,
                int32Field = 1,
                fixed32Field = 2,
                uint32Field = 3,
                sfixed32Field = -1,
                sint32Field = -2,
                int64Field = 2,
                fixed64Field = 4,
                uint64Field = 6,
                sfixed64Field = -2,
                sint64Field = -4,
                bytesField = ByteString.of(*"test bytes".toByteArray())
        )
    }

    /**
     * Generate a new [NestedMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredNestedMessageJava(): RequiredNestedMessageJava {
        return RequiredNestedMessageJava.newBuilder()
                .setScalars(newRequiredScalarsMessageJava())
                .setInner(newRequiredInnerMessageJava())
                .setAnotherInner(newRequiredInnerMessageJava())
                .build()
    }

    /**
     * Generate a new [RequiredNestedMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredInnerMessageJava(): RequiredNestedMessageJava.RequiredInnerMessage {
        return RequiredNestedMessageJava.RequiredInnerMessage.newBuilder()
                .setStringField("inner test string")
                .setIntField(1)
                .build()
    }

    /**
     * Generate a new [RequiredNestedMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredNestedMessage(): RequiredNestedMessage {
        return RequiredNestedMessage(
                scalars = newRequiredScalarsMessage(),
                inner = newRequiredInnerMessage()
        )
    }

    /**
     * Generate a new [RequiredNestedMessage.RequiredInnerMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredInnerMessage(): RequiredNestedMessage.RequiredInnerMessage {
        return RequiredNestedMessage.RequiredInnerMessage(
                stringField = "inner test string",
                intField = 10
        )
    }

    /**
     * Generate a new [RequiredEnumMessageJava] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredEnumMessageJava(): RequiredEnumMessageJava {
        return RequiredEnumMessageJava.newBuilder()
                .setTopLevelEnum(TopLevelEnumJava.ONE)
                .setBoolField(true)
                .setNestedEnum(EnumMessageJava.InnerEnum.NESTED_ONE)
                .build()
    }

    /**
     * Generate a new [RequiredEnumMessage] instance.
     *
     * @return a generated instance.
     */
    private fun newRequiredEnumMessage(): RequiredEnumMessage {
        return RequiredEnumMessage(
                topLevelEnum = TopLevelEnum.ONE,
                boolField = true,
                nestedEnum = EnumMessage.InnerEnum.NESTED_ONE
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

        val javaEmptyMessage = NestedMessageJava.newBuilder().build()
        Assertions.assertThat(roundTrip(javaEmptyMessage, serializer)).isEqualTo(javaEmptyMessage)

        val kotlinEmptyMessage = NestedMessage()
        Assertions.assertThat(roundTrip(kotlinEmptyMessage, serializer, parser))
                .isEqualTo(kotlinEmptyMessage)

        val javaMessage = newNestedMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

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

        val javaMessage = newEnumMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

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

    /**
     * Test that non-optional scalar types are preserved across round-trip serialization /
     * deserialization.
     */
    @Test
    fun requiredScalarTypesRoundTrip() {
        val parser = RequiredScalarsMessageJava.parser()
        val serializer = RequiredScalarsMessage.serializer()

        val javaMessage = newRequiredScalarsMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newRequiredScalarsMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }

    /**
     * Test that non-optional nested message is preserved across round-trip serialization /
     * deserialization.
     */
    @Test
    fun requiredNestedMessageRoundTrip() {
        val parser = RequiredNestedMessageJava.parser()
        val serializer = RequiredNestedMessage.serializer()

        val javaMessage = newRequiredNestedMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newRequiredNestedMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }

    /**
     * Test that non-optional enum is preserved across round-trip serialization / deserialization.
     */
    @Test
    fun requiredEnumMessageRoundTrip() {
        val parser = RequiredEnumMessageJava.parser()
        val serializer = RequiredEnumMessage.serializer()

        val javaMessage = newRequiredEnumMessageJava()
        Assertions.assertThat(roundTrip(javaMessage, serializer)).isEqualTo(javaMessage)

        val kotlinMessage = newRequiredEnumMessage()
        Assertions.assertThat(roundTrip(kotlinMessage, serializer, parser)).isEqualTo(kotlinMessage)
    }
}