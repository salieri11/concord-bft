@file:JvmName("NetworkAddress")
package com.vmware.blockchain.deployment.orchestration

import kotlin.random.Random

/**
 * Convert an [Int] to the IPv4 address it represents in canonical format.
 *
 * @return
 *   canonical IPv4 address as a [String].
 */
fun Int.toIPv4Address(): String {
    val first = (this ushr 24)
    val second = (this and 0x00FF0000) ushr 16
    val third = (this and 0x0000FF00) ushr 8
    val fourth = (this and 0x000000FF)

    return "$first.$second.$third.$fourth"
}

/**
 * Convert a [String] to the IPv4 address represented as an [Int]
 *
 * @return
 *   IPv4 address as a [Int].
 */
fun String.toIPv4Address(): Int {
    val parts = split(".", limit = 4).map { it.toInt() and 0x000000FF }

    return (parts[0] shl 24) + (parts[1] shl 16) + (parts[2] shl 8) + parts[3]
}

/**
 * Calculate the IPv4 network subnet netmask for subnet of a given size.
 *
 * @param[size]
 *   size of the IPv4 subnet.
 *
 * @return
 *   netmask of the subnet as an [Int].
 */
fun toIPv4SubnetMask(size: Int): Int = ((1 shl (32 - size)) - 1).inv()

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
