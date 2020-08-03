/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration;

import java.util.AbstractMap;

import com.google.common.net.InetAddresses;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.v1.Address;

/**
 * common utilities for orchestrators.
 */
public class OrchestratorUtils {

    /**
     * get ip address to allocate.
     * @param ipamClient ipamClient
     * @param networkSegmentName networkSegmentName
     * @param ip ip in string
     * @param name name
     * @return pair of name and ip address as string.
     */
    public static AbstractMap.SimpleEntry<String, String> getAddress(IpamClient ipamClient, String networkSegmentName,
                                                                     String ip, String name) {
        if (ip.isEmpty()) {
            Address add = ipamClient.allocatedPrivateIp(networkSegmentName);
            return new AbstractMap.SimpleEntry<>(add.getName(),
                    InetAddresses.fromInteger(add.getValue()).getHostAddress());
        }

        if (!InetAddresses.isInetAddress(ip)) {
            throw new IllegalArgumentException("Provided ip is not a valid Inet address. ip: " + ip);
        }

        return new AbstractMap.SimpleEntry<>(name, ip);
    }

}
