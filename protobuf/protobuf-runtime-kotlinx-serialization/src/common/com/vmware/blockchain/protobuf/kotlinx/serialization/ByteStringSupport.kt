/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.kotlinx.serialization

/**
 * Verify if a given [ByteString] represents the same data content as this instance.
 *
 * @param[other]
 *   the other [ByteString] instance to check against.
 *
 * @return
 *   `true` if both instances contain the same content, `false` otherwise.
 */
expect fun ByteString.contentEquals(other: ByteString): Boolean

/**
 * Convert the data content to a new [ByteString] encoded in base64 encoding.
 *
 * @return
 *   base64 encoding of the data as a [ByteString].
 */
expect fun ByteString.encodeBase64(): ByteString

/**
 * Converts a base64-encoded [ByteString] to a decoded [ByteString].
 *
 * @return
 *   decoded content of the base64-encoded string as a [ByteString].
 */
expect fun ByteString.decodeBase64(): ByteString

/**
 * Convert the data content into a [String].
 *
 * Note: The conversion does not assume that the binary payload encodes to a valid UTF-8
 * sequence.
 */
fun ByteString.utf8(): String {
    return String(data, offset, length, Charsets.UTF_8)
}
