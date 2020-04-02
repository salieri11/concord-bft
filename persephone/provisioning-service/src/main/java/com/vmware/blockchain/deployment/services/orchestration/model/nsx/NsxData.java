/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.model.nsx;

import static java.util.Collections.emptyList;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Holds all the data classes for Nsx.
 */
public class NsxData {

    /**
     * Nat rule class.
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class NatRule {

        Action action;
        String id;
        String displayName;
        String path;
        @Builder.Default
        boolean enabled = true;
        boolean logging;
        String sourceNetwork;
        String destinationNetwork;
        String translatedNetwork;
        String translatedPorts;
        int sequenceNumber;
        String service;
        FirewallMatch firewallMatch;
        @Builder.Default
        List<String> scope = emptyList();

        /**
         * Enum action.
         */
        public enum Action {
            SNAT,
            DNAT,
            REFLEXIVE,
            NO_SNAT,
            NO_DNAT,
        }

        /**
         * Enum for Firewall.
         */
        enum FirewallMatch {
            MATCH_EXTERNAL_ADDRESS,
            MATCH_INTERNAL_ADDRESS,
            BYPASS
        }
    }

    /**
     * Data class for public ip.
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PublicIp {
        String id;
        String displayName;
        String ip;
    }

    /**
     * Data class for segment.
     */
    @Data
    @Builder
    public static class Segment {

        String id;
        String path;
        String type;
        List<SegmentSubnet> subnets;

        enum Type {
            ROUTED,
            EXTENDED,
            DISCONNECTED
        }
    }

    /**
     * Data class for segment subnet.
     */
    @Data
    @Builder
    public static class SegmentSubnet {
        List<String> dhcpRanges;
        String gatewayAddress;
        String network;
    }
}
