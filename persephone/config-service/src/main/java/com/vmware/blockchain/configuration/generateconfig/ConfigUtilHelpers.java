/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * Utility Helpers.
 */
public class ConfigUtilHelpers {

    static final int DEFAULT_PORT = 3501;
    static final int CLIENT_PROXY_PER_COMMITTER = 4;
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
        CLIENT_PORT("client_port");

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
     * Clone Map.
     */
    static <K, V> Map<K, V> clone(Map<K, V> original) {
        return original.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                        Map.Entry::getValue));
    }

}
