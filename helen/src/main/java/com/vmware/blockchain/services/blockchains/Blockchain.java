/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * Class representing Blockchains.
 */
@EntityColumnName("helen.blockchain")
@Data
@EqualsAndHashCode(callSuper = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Blockchain extends AbstractEntity {

    static String BLOCKCHAIN_VERSION = "version";
    static String DAML_SDK_VERSION = "daml-sdk-version";

    /**
     * A node entry.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Deprecated
    public static class NodeEntry {
        UUID nodeId;
        String ip;
        String hostName;
        String url;
        String cert;
        UUID zoneId;
    }

    /**
     * Enum to determine blockchain type.
     */
    public enum BlockchainType {
        ETHEREUM,
        DAML
    }

    /**
     * Enum to denote current blockchain state.
     */
    public enum BlockchainState {
        INACTIVE,
        ACTIVE,
        FAILED
    }

    @LinkedEntityId
    UUID consortium;

    BlockchainType type;

    BlockchainState state;

    @Deprecated
    List<NodeEntry> nodeList;

    // Store software versions for blockchain
    @Builder.Default
    Map<String, String> metadata = new HashMap<>();

    // Do not refer or use this in consumer API.
    Map<String, List<String>> rawResourceInfo;

    /**
     * Get the version string for the blockchain.
     * @return version.
     */
    public String getBlockchainVersionString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Blockchain Version: ");

        // For backward data compatibility.
        if (metadata != null) {
            sb.append(metadata.getOrDefault(BLOCKCHAIN_VERSION, "NA"));

            if (type == BlockchainType.DAML) {
                sb.append(", DAML SDK Version: ");
                sb.append(metadata.getOrDefault(DAML_SDK_VERSION, "NA"));
            }
        }
        return sb.toString();
    }
}
