/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.kotlinx.serialization

import net.jqwik.api.Arbitraries
import net.jqwik.api.Arbitrary
import net.jqwik.api.ForAll
import net.jqwik.api.Property
import net.jqwik.api.Provide
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Unit tests verifying the functionality of [ByteString].
 */
class ByteStringTest {

    /**
     * Test that slicing a [ByteString] actually shares the memory buffer.
     */
    @Test
    fun sliceShareBuffer() {
        val value = "TestData".toByteArray(Charsets.UTF_8)
        val size = value.size
        val data = ByteString.of(*value)

        // Create a slice starting from 0.
        val slice = data.slice(0, size shr 1)!!

        // Create a full slice (clone of original).
        val fullSlice = data.slice(0, data.length)!!

        // Create a slice starting from an offset.
        val offsetSlice = data.slice(1, data.length - 1)!!

        // Force an unsafe modification (to detect whether buffer was really shared).
        data.asByteArray()[0] = 'm'.toByte()
        data.asByteArray()[1] = 'o'.toByte()
        data.asByteArray()[2] = 'd'.toByte()

        // Check that all ByteString's have the same content (shared buffer).
        val sliceString = String(slice.asByteArray(), Charsets.UTF_8)
        Assertions.assertThat(sliceString).hasSize(size shr 1).isEqualTo("modt")

        val fullSliceString = String(fullSlice.asByteArray(), Charsets.UTF_8)
        Assertions.assertThat(fullSliceString).hasSize(size).isEqualTo("modtData")

        val offsetSliceString = String(offsetSlice.asByteArray(), Charsets.UTF_8)
        Assertions.assertThat(offsetSliceString).hasSize(size - 1).isEqualTo("odtData")
    }

    /**
     * Test that slicing a [ByteString] fails if slicing parameters are out of range.
     */
    @Test
    fun sliceFailIfInvalidRange() {
        val value = "TestData"
        val size = value.length
        val data = ByteString.of(*value.toByteArray(Charsets.UTF_8))

        // Check various out-of-range conditions.
        Assertions.assertThat(data.slice(0, size + 1) == null).isTrue()
        Assertions.assertThat(data.slice(1, size) == null).isTrue()
        Assertions.assertThat(data.slice(-1, 1) == null).isTrue()
        Assertions.assertThat(data.slice(-1, 0) == null).isTrue()
        Assertions.assertThat(data.slice(0, -1) == null).isTrue()
        Assertions.assertThat(data.slice(-1, -1) == null).isTrue()
    }

    /**
     * Test overall empty [ByteString] and empty slice equality.
     */
    @Test
    fun sliceEmpty() {
        // Slicing zero-length off of a non-empty ByteString should result in an empty instance.
        val value = "TestData"
        val data = ByteString.of(*value.toByteArray(Charsets.UTF_8))
        Assertions.assertThat(data.slice(0, 0))
                .isNotNull
                .isEqualTo(ByteString.empty())

        // Creating a ByteString with zero-length should result in an empty instance.
        val empty = ByteString.of(value.toByteArray(Charsets.UTF_8), 0, 0)
        Assertions.assertThat(empty).isEqualTo(ByteString.empty())
    }

    /**
     * Test that [ByteString.toByteArray] decouples the memory sharing from origin instance.
     */
    @Test
    fun toByteArrayDifferentBuffer() {
        val value = "TestData".toByteArray(Charsets.UTF_8)
        val size = value.size
        val data = ByteString.of(*value)

        val clone = data.toByteArray()

        // Force an unsafe modification (to detect whether buffer was really shared).
        data.asByteArray()[0] = 'm'.toByte()
        data.asByteArray()[1] = 'o'.toByte()
        data.asByteArray()[2] = 'd'.toByte()

        // Check that ByteString's do not have the same content (different buffer).
        Assertions.assertThat(clone)
                .hasSize(size)
                .isEqualTo(value)
                .isNotEqualTo(data.asByteArray())
    }

    /**
     * Test overall content equality of [ByteString].
     */
    @Test
    fun sameContentEqualByteString() {
        val value = "TestData"
        val length = value.length
        val data = ByteString.of(*value.toByteArray(Charsets.UTF_8))
        Assertions.assertThat(data.length).isEqualTo(length)

        val sameData = ByteString.of(*value.toByteArray(Charsets.UTF_8))
        Assertions.assertThat(sameData.length).isEqualTo(length)
        Assertions.assertThat(data.hashCode())
                .isEqualTo(sameData.hashCode())
                .isNotEqualTo(0)

        val offsetSameData = ByteString.of(*"X$value".toByteArray(Charsets.UTF_8))
        Assertions.assertThat(offsetSameData.length).isEqualTo(length + 1)

        // Regardless of the origin of the data, equal content should be equal ByteString.
        Assertions.assertThat(data).isEqualTo(sameData)
        Assertions.assertThat(offsetSameData.slice(1, length))
                .isNotNull
                .isEqualTo(data)
                .isEqualTo(sameData)
        Assertions.assertThat(offsetSameData.slice(1, length).hashCode()).isEqualTo(data.hashCode())
    }

    /**
     * Provider of arbitrary [String] values with shrinking support.
     */
    @Provide
    fun strings(): Arbitrary<String> {
        return Arbitraries.strings()
                .all()
                .ofMinLength(0).ofMaxLength(1000)
    }

    /**
     * Test the property invariant that round-trip of base64 encode and decode through [ByteString]
     * is not lossy nor noisy.
     */
    @Property
    fun base64RoundTrip(@ForAll("strings") value: String) {
        val data = ByteString.of(*value.toByteArray(Charsets.UTF_8))

        Assertions.assertThat(data.encodeBase64().decodeBase64()).isEqualTo(data)
    }

    /**
     * Test the property invariant that round-trip of UTF-8 encode and decode through [ByteString]
     * is not lossy nor noisy.
     */
    @Property
    fun utf8RoundTrip(@ForAll("strings") value: String) {
        val bytes = value.toByteArray(Charsets.UTF_8)
        val str = String(bytes, Charsets.UTF_8)

        Assertions.assertThat(ByteString.of(*bytes).utf8()).isEqualTo(str)
    }
}
