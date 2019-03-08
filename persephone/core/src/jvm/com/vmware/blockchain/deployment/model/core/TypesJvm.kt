/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.model.core

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializer

/**
 * Type alias redeclaration of [java.net.URI] to syntactically shield URI implementation details.
 */
actual typealias URI = java.net.URI

/**
 * Singleton Serializer Object for [URI].
 */
@Serializer(forClass = URI::class)
actual object URISerializer : KSerializer<URI> {

    override fun serialize(encoder: Encoder, obj: URI) {
        encoder.encodeString(obj.toString())
    }

    override fun deserialize(decoder: Decoder): URI {
        return URI(decoder.decodeString())
    }
}

/**
 * Type alias redeclaration of [java.util.UUID] to syntactically shield UUID implementation details.
 */
actual typealias UUID = java.util.UUID

/**
 * Singleton Serializer Object for [UUID].
 */
@Serializer(forClass = UUID::class)
actual object UUIDSerializer : KSerializer<UUID> {

    override fun serialize(encoder: Encoder, obj: UUID) {
        encoder.encodeString(obj.toString())
    }

    override fun deserialize(decoder: Decoder): UUID {
        return java.util.UUID.fromString(decoder.decodeString())
    }
}
