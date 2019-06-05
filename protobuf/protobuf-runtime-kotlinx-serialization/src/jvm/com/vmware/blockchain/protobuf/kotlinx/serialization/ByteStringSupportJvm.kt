/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.kotlinx.serialization

import java.util.Arrays
import java.util.Base64

actual fun ByteString.contentEquals(other: ByteString): Boolean {
    // Use JVM's unsigned (type-unsafe) array mismatch-check function.
    return Arrays.equals(
            data, offset, offset + length,
            other.data, other.offset, other.offset + other.length
    )
}

actual fun ByteString.encodeBase64(): ByteString {
    // Note:
    // The processing is not as efficient as it can be as a temporary buffer is allocated. More
    // efficient scheme would be to encode directly from the underlying raw storage, which is
    // accessible in this function. This would avoid create the temporary ByteArray buffer.
    return ByteString(Base64.getEncoder().encode(asByteArray()))
}

actual fun ByteString.decodeBase64(): ByteString {
    return ByteString(Base64.getDecoder().decode(asByteArray()))
}
