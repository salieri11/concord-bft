/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Utility class for generating the generic configurations irrespective node type.
 */
public class GenericConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(GenericConfigUtil.class);

    /**
     * generic config path.
     */
    public static String configPath = "/generic/identifiers.env";

    /**
     * Generates the generic config file with node ids.
     *
     * @param nodeProperties Map of node properties.
     * @return map of host ips vs configs.
     */
    public Map<Integer, String> getGenericConfig(List<NodeProperty> nodeProperties, Properties properties) {

        Map<Integer, String> configMap = new HashMap<>();

        Map<Integer, String> clientGroupIds = new HashMap<>();
        Map<Integer, String> nodeIds = new HashMap<>();

        nodeProperties.stream().forEach(nodeProperty -> {
            switch (nodeProperty.getName()) {
                case CLIENT_GROUP_ID:
                    clientGroupIds.putAll(nodeProperty.getValueMap());
                    break;
                case NODE_ID:
                    nodeIds.putAll(nodeProperty.getValueMap());
                    break;
                default:
                    log.debug("property {} not relevant for generic", nodeProperty.getName());
            }
        });

        String blockchainId = properties.getValuesMap()
                .getOrDefault(NodeProperty.Name.BLOCKCHAIN_ID.toString(), "");
        String consortiumId = properties.getValuesMap().getOrDefault(
                NodeProperty.Name.CONSORTIUM_ID.toString(), "");

        nodeIds.forEach((node, value) -> {
            StringBuilder builder = new StringBuilder();
            builder.append("NODE_UUID=")
                    .append(value)
                    .append("\n")
                    .append("PARTICIPANT_ID=")
                    .append(clientGroupIds.getOrDefault(node, value))
                    .append("\n")
                    .append("BLOCKCHAIN_ID=")
                    .append(blockchainId)
                    .append("\n")
                    .append("CONSORTIUM_ID=")
                    .append(consortiumId);
            configMap.put(node, builder.toString());
        });

        return configMap;
    }

    /**
     * Generates the generic config file with node ids.
     *
     * @param consortiumId id
     * @param blockchainId id
     * @param nodeInfo     info
     * @return config
     */
    public String getGenericConfig(String consortiumId, String blockchainId, NodesInfo.Entry nodeInfo) {
        StringBuilder builder = new StringBuilder();
        builder.append("NODE_UUID=")
                .append(nodeInfo.getId())
                .append("\n")
                .append("BLOCKCHAIN_ID=")
                .append(blockchainId)
                .append("\n")
                .append("CONSORTIUM_ID=")
                .append(consortiumId);

        return builder.toString();
    }
}
