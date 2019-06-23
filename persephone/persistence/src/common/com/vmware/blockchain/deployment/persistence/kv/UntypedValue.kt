/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf
import com.vmware.blockchain.protobuf.kotlinx.serialization.decodeBase64
import com.vmware.blockchain.protobuf.kotlinx.serialization.encodeBase64
import com.vmware.blockchain.protobuf.kotlinx.serialization.utf8
import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.Serializer

/**
 * Implementation of [KeyValueStore.Value] that represents the content as simple [ByteArray].
 *
 * @param[data]
 *   binary content represented by this [UntypedValue] instance.
 */
@Serializable
class UntypedValue(override val data: ByteArray) : ByteString(data), KeyValueStore.Value {

    @Serializer(forClass = UntypedValue::class)
    companion object UnTypedValueSerializer : KSerializer<UntypedValue> {

        override fun serialize(encoder: Encoder, obj: UntypedValue) {
            when (encoder) {
                is ProtoBuf.ProtobufWriter -> encoder.encodeTaggedBytes(obj.asByteArray())
                else -> encoder.encodeString(obj.encodeBase64().utf8())
            }
        }

        override fun deserialize(decoder: Decoder): UntypedValue {
            return when (decoder) {
                is ProtoBuf.ProtobufReader -> UntypedValue(decoder.decodeTaggedBytes())
                else -> UntypedValue(
                        // Inefficient, but avoids dealing with base64 logic directly here.
                        // (i.e. defers to ByteStringSupport, which is multi-platform).
                        of(*decoder.decodeString().toByteArray()).decodeBase64().asByteArray()
                )
            }
        }
    }
}
