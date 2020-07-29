/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility Helpers.
 */
public class ConfigUtilHelpers {

    private static final Logger log = LoggerFactory.getLogger(ConfigUtilHelpers.class);

    static final int DEFAULT_PORT = 3501;
    static final int CLIENT_PROXY_PER_COMMITTER = 4;
    static final int CLIENT_PROXY_PER_COMMITTER_SPLIT = 1;
    static final int CLIENT_PROXY_PER_PARTICIPANT = 15;

    /**
     * Enum holding config properties.
     */
    enum ConfigProperty {
        F_VAL("f_val"),
        C_VAL("c_val"),
        DAML_ENABLED("daml_enable"),
        ETHEREUM_ENABLED("eth_enable"),
        HLF_ENABLED("hlf_enable"),
        NODE("node"),
        PARTICIPANT_NODES("participant_nodes"),
        PARTICIPANT_NODE("participant_node"),
        PARTICIPANT_NODE_HOST("participant_node_host"),
        EXTERNAL_CLIENTS("external_clients"),
        NUM_PARTICIPANTS("num_of_participant_nodes"),
        REPLICA("replica"),
        CLIENT("client"),
        CLIENT_PROXY("client_proxy"),
        REPLICA_HOST("replica_host"),
        CLIENT_HOST("client_host"),
        COMMITTER_PORT("replica_port"),
        CLIENT_PORT("client_port"),
        NUM_EXTERNAL_CLIENTS("num_of_external_clients"),
        CLIENT_PROXY_PER_REPLICA("client_proxies_per_replica");

        String name;

        ConfigProperty(String propertyName) {
            this.name = propertyName;
        }
    }

    /**
     * Helper method to calculate f_val for cluster.
     */
    static int getFVal(int clusterSize) {
        int realSize = clusterSize - 1;
        int fSize = realSize / 3;
        // if (realSize % 3 == 0 && fSize > 1) {
        //     fSize--;
        // }
        return fSize;
    }

    /**
     * Helper method to calculate c_val for cluster.
     */
    static int getCVal(int clusterSize, int fVal) {
        //return ((clusterSize-1) - 3*fVal )/ 2;
        return 0;
    }

    /**
     * validate fval and cval.
     */
    static boolean validateSbft(int clusterSize, int fVal, int cVal) {
        if (clusterSize == 0) {
            log.error("List of host IP provided is NULL!");
            return false;
        }
        if (clusterSize < 4) {
            log.error("Minimum cluster size is 4!");
            return false;
        }
        if ((3 * fVal + 2 * cVal + 1) > clusterSize) {
            log.error("fVal / cVal are invalid for the list of host IP provided");
            return false;
        }
        return true;
    }

    /**
     * validate host ips.
     */
    static boolean validateSbft(int clusterSize) {
        if (clusterSize == 0) {
            log.error("List of host IP provided is NULL!");
            return false;
        }
        if (clusterSize < 4) {
            log.error("Minimum cluster size is 4!");
            return false;
        }
        return true;
    }

    /**
     * Clone Map.
     */
    static <K, V> Map<K, V> clone(Map<K, V> original) {
        return original.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                        Map.Entry::getValue));
    }

}
