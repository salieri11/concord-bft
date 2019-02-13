/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import net.jqwik.api.Arbitraries
import net.jqwik.api.Arbitrary
import net.jqwik.api.ForAll
import net.jqwik.api.Property
import net.jqwik.api.Provide
import org.assertj.core.api.Assertions

/**
 * Basic functionality test of [MonotonicInt] features.
 */
class MonotonicIntTest {

    /**
     * Provider of arbitrary [Int] values with shrinking support.
     */
    @Provide
    fun integers(): Arbitrary<Int> {
        return Arbitraries.integers().between(0, Int.MAX_VALUE - 1)
    }

    @Property
    fun toByteArrayRoundTrip(@ForAll("integers") value: Int) {
        val version = MonotonicInt(value)
        Assertions.assertThat(version).isEqualTo(version.asByteArray().toMonotonicInt())
    }

    @Property
    fun nextVersion(@ForAll("integers") value: Int) {
        val version = MonotonicInt(value)
        val next = version.next()

        Assertions.assertThat(version).isLessThan(next)
        Assertions.assertThat(next).isGreaterThan(version)
        Assertions.assertThat(version).isNotEqualTo(next)

        // Specific property of [MonotonicInt].
        Assertions.assertThat(next.value).isEqualTo(version.value + 1)
    }
}