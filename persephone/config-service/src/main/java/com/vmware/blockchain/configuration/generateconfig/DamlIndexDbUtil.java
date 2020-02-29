/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.List;

import com.vmware.blockchain.deployment.v1.NodeProperty;

/**
 * Utility class for generating the config(s) for Daml Index DB file.
 */
public class DamlIndexDbUtil {

    /**
     * file path.
     */
    public static final String envVarPath = "/daml-index-db/environment-vars";

    /**
     * Utility to daml ledger api config.
     * @return json string
     */
    public String generateConfig(List<NodeProperty> nodeProperties) {

        var nodeId =
                nodeProperties.stream().filter(nodeProperty -> nodeProperty.getName().equals(NodeProperty.Name.NODE_ID))
                        .findFirst();

        // Assuming user sends one client per configuration.
        // TODO: might require a change when multiple clients deploys together.
        var nodeName = nodeId.get().getValueMap().get(0).replace("-", "_");

        StringBuilder builder = new StringBuilder();
        builder.append("export POSTGRES_USER=indexdb");
        builder.append(System.getProperty("line.separator"));
        builder.append("export POSTGRES_MULTIPLE_DATABASES=" + nodeName);
        builder.append(System.getProperty("line.separator"));
        builder.append("export MAX_CONNECTIONS=300");
        builder.append(System.getProperty("line.separator"));
        builder.append("export BUFFER_SIZE=80MB");
        return builder.toString();
    }

}
