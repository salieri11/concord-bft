/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.kotlinx.serialization

import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.EmptyModule
import net.jqwik.api.Arbitraries
import net.jqwik.api.Arbitrary
import net.jqwik.api.ForAll
import net.jqwik.api.Property
import net.jqwik.api.Provide
import org.assertj.core.api.Assertions

/**
 * Unit tests verifying the serialization correctness surrounding [ByteString].
 */
class ByteStringSerializationTest {

    @Serializable
    data class BoxedBytes(@SerialId(1) val value: ByteString = ByteString.EMPTY)

    companion object {
        private val json = Json(JsonConfiguration.Stable, EmptyModule)
        private val protobuf = ProtoBuf()
    }

    /**
     * Provider of arbitrary [ByteString] values with shrinking support.
     */
    @Provide
    fun byteStrings(): Arbitrary<ByteString> {
        return Arbitraries.bytes()
                .array(ByteArray::class.java)
                .ofMinSize(0).ofMaxSize(1000)
                .map { ByteString.of(*it) }
    }

    /**
     * Test that round-trip Protocol Buffer serialization of random [ByteString] does not lose
     * nor create additional information.
     */
    @Property
    fun protocolBufferRoundTrip(@ForAll("byteStrings") value: ByteString) {
        val content = BoxedBytes(value)
        val payload = protobuf.dump(BoxedBytes.serializer(), content)
        val roundTrip = protobuf.load(BoxedBytes.serializer(), payload)
        Assertions.assertThat(roundTrip).isEqualTo(content)
    }

    /**
     * Test that round-trip JSON serialization of random [ByteString] does not lose nor create
     * additional information.
     */
    @Property
    fun jsonRoundTrip(@ForAll("byteStrings") value: ByteString) {
        val content = BoxedBytes(value)
        val payload = json.stringify(BoxedBytes.serializer(), content)
        val roundTrip = json.parse(BoxedBytes.serializer(), payload)
        Assertions.assertThat(roundTrip).isEqualTo(content)
    }
}
