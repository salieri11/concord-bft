@file:JvmName("NetworkAddress")
package com.vmware.blockchain.deployment.orchestration

import kotlin.random.Random

/**
 * Convert an [Int] to the IPv4 address it represents in canonical format.
 *
 * @param[value]
 *   integer value to convert.
 *
 * @return
 *   canonical IPv4 address as a [String].
 */
fun toIPv4Address(value: Int): String {
    val first = (value ushr 24)
    val second = (value and 0x00FF0000) ushr 16
    val third = (value and 0x0000FF00) ushr 8
    val fourth = (value and 0x000000FF)

    return "$first.$second.$third.$fourth"
}

/**
 * Convert an [String] to the IPv4 address represented as an [Int]
 *
 * @param[value]
 *   string value to convert.
 *
 * @return
 *   IPv4 address as a [Int].
 */
fun toIPv4Address(value: String): Int {
    val parts = value.split(".", limit = 4).map { it.toInt() and 0x000000FF }

    return (parts[0] shl 24) + (parts[1] shl 16) + (parts[2] shl 8) + parts[3]
}

/**
 * Randomly generate an IPv4 address sub-network constrained within a prefix network range.
 *
 * @param[prefix]
 *   prefix of the network range to create a sub-network within.
 * @param[prefixSubnet]
 *   number of bits used for mask of the prefix network to constrain by.
 * @param[subnet]
 *   number of bits used for mask of the sub-network to generate.
 *
 * @return
 *   generated subnet CIDR as an [Int].
 */
fun randomSubnet(prefix: Int, prefixSubnet: Int, subnet: Int): Int {
    return prefix + (Random.nextBits(subnet - prefixSubnet) shl (Int.SIZE_BITS - subnet))
}
