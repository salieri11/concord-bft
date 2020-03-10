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
     * @param nodeProperties Map of node properties.
     * @return map of host ips vs configs.
     */
    public Map<Integer, String> getGenericConfig(List<NodeProperty> nodeProperties) {

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

        nodeIds.forEach((node, value) -> {
            StringBuilder builder = new StringBuilder();
            builder.append("NODE_UUID=")
                    .append(value)
                    .append("CLIENT_GROUP_ID=")
                    .append(clientGroupIds.getOrDefault(node, value));
            configMap.put(node, builder.toString());
        });

        return configMap;
    }

}
