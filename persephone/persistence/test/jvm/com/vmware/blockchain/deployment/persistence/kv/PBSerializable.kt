/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Optional
import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable

/**
 * Example type that can be serialized to wire-compatible Protocol Buffer.
 */
@Serializable
data class PBSerializable(
    @SerialId(1) @Optional val string: String = "",
    @SerialId(2) @Optional val int: Int = 0,
    @SerialId(3) @Optional val long: Long = 0
) {
    companion object {
        /**
         * Retrieve the [KSerializer] for [PBSerializable].
         *
         * Note: This class is necessary for Java-callers since the compile-time generated
         * [KSerializer] accessor relies on Kotlin metadata (embedded in JVM bytecode for JVM
         * platform), which is otherwise unavailable for Java-callers at Java programming language
         * level.
         */
        @JvmStatic
        fun getSerializer(): KSerializer<PBSerializable> {
            return PBSerializable.serializer()
        }
    }
}
