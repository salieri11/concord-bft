/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.util.List;

import com.google.common.base.Strings;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.Properties;

import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;


/**
 * Utility class for generating the config(s) for Daml Ledger Api file.
 */
@Slf4j
@NoArgsConstructor
public class DamlLedgerApiUtil {

    /**
     * file path.
     */
    public static final String envVarPath = "/daml-ledger-api/environment-vars";

    /**
     * Utility to daml ledger api config.
     *
     * @return json string
     */
    public String generateConfig(Properties properties, List<NodeProperty> nodeProperties) {

        var nodeId = nodeProperties.stream().filter(nodeProperty ->
                                                            nodeProperty.getName().equals(NodeProperty.Name.NODE_ID))
                .findFirst();

        // Assuming user sends one client per configuration.
        // TODO: might require a change when multiple clients deploys together.
        var nodeName = nodeId.get().getValueMap().get(0).replace("-", "_");

        StringBuilder builder = new StringBuilder();
        builder.append("export INDEXDB_HOST=daml_index_db");
        builder.append(System.getProperty("line.separator"));
        builder.append("export INDEXDB_PORT=5432");
        builder.append(System.getProperty("line.separator"));
        builder.append("export INDEXDB_USER=indexdb");
        builder.append(System.getProperty("line.separator"));
        builder.append("export REPLICAS=" + getReplicas(properties));
        builder.append(System.getProperty("line.separator"));
        builder.append("export PARTICIPANT_ID=p" + nodeName);
        builder.append(System.getProperty("line.separator"));
        builder.append("export JAVA_OPTS=\"-Xmx10G -XX:ErrorFile=/config/daml-ledger-api/cores/err_pid%p.log\"");
        builder.append(System.getProperty("line.separator"));
        builder.append("export THIN_REPLICA_SETTINGS=\"--jaeger-agent-address jaeger-agent:6831\"");
        addAuthJwt(builder, properties);

        return builder.toString();
    }

    private String getReplicas(Properties properties) {
        String replicas = properties.getValuesMap().get(NodeProperty.Name.COMMITTERS.toString());
        if (Strings.isNullOrEmpty(replicas)) {
            return "concord:50051";
        } else {
            return replicas;
        }
    }

    /**
     * Appends the auth jwt property if present.
     *
     * @param builder        builder
     * @param properties properties
     */
    private void addAuthJwt(StringBuilder builder, Properties properties) {
        var authToken = properties.getValuesMap().get(NodeProperty.Name.CLIENT_AUTH_JWT.toString());
        if (!Strings.isNullOrEmpty(authToken)) {
            builder.append(System.getProperty("line.separator"));
            builder.append("export AUTH_SETTINGS=\"--auth-jwt-rs256-jwks " + authToken + "\"");
        }
    }
}
