/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

/**
 * Network util class.
 */
public class NetworkAddress {

    /**
     * Convert a [String] to the IPv4 address represented as an [Int]
     *
     * @return IPv4 address as a [Long].
     */
    public static int toIPv4Address(String input) {
        int result = 0;

        String[] ipAddressInArray = input.split("\\.");

        for (int i = 3; i >= 0; i--) {

            long ip = Long.parseLong(ipAddressInArray[3 - i]);

            // left shifting 24,16,8,0 and bitwise OR

            // 1. 192 << 24
            // 1. 168 << 16
            // 1. 1 << 8
            // 1. 2 << 0
            result |= ip << (i * 8);
        }

        return result;
    }

    public static int randomSubnet(int prefix, int prefixSubnet, int subnet) {
        return prefix; //+ (RandomU.nextBits(subnet - prefixSubnet) << (Int.SIZE_BITS - subnet))
    }

    /**
     * Hex to IP converter.
     */
    public static String convertHexToIp(String hex) {
        String ip = "";

        for (int j = 0; j < hex.length(); j += 2) {
            String sub = hex.substring(j, j + 2);
            int num = Integer.parseInt(sub, 16);
            ip += num + ".";
        }

        ip = ip.substring(0, ip.length() - 1);
        return ip;
    }
}