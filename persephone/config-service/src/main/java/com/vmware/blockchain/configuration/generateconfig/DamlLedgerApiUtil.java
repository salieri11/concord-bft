/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import com.google.common.base.Strings;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
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
    public String generateConfig(NodesInfo.Entry nodeInfo) {
        StringBuilder builder = new StringBuilder();

        builder.append("export INDEXDB_HOST=daml_index_db");
        builder.append(System.getProperty("line.separator"));

        builder.append("export INDEXDB_PORT=5432");
        builder.append(System.getProperty("line.separator"));

        builder.append("export INDEXDB_USER=indexdb");
        builder.append(System.getProperty("line.separator"));

        builder.append("export REPLICAS=" + getReplicas(nodeInfo.getProperties()));
        builder.append(System.getProperty("line.separator"));

        builder.append("export JAVA_OPTS=\"-XX:+UseG1GC -Xmx10G "
                       + "-XX:ErrorFile=/config/daml-ledger-api/cores/err_pid%p.log\"");
        builder.append(System.getProperty("line.separator"));

        builder.append("export THIN_REPLICA_SETTINGS=\"--use-thin-replica --jaeger-agent-address jaeger-agent:6831\"");

        addProperties(builder, nodeInfo);

        addBftClient(builder, nodeInfo.getProperties());

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

    private void addBftClient(StringBuilder builder, Properties properties) {
        if (properties.getValuesMap()
                .getOrDefault(DeploymentAttributes.ENABLE_BFT_CLIENT.toString(), "False")
                .equalsIgnoreCase("True")) {
            builder.append(System.getProperty("line.separator"));
            builder.append("export BFT_CLIENT_SETTINGS=\"--use-bft-client --bft-client-config-path=/config"
                    + BftClientConfigUtil.configPath + "\"");
        }
    }

    /**
     * Appends the auth jwt and client group id properties if present.
     *
     * @param builder  builder
     * @param nodeInfo nodeinfo
     */
    private void addProperties(StringBuilder builder, NodesInfo.Entry nodeInfo) {
        Properties properties = nodeInfo.getProperties();
        // Add auth token
        var authToken = properties.getValuesMap().get(NodeProperty.Name.CLIENT_AUTH_JWT.toString());
        if (!Strings.isNullOrEmpty(authToken)) {
            builder.append(System.getProperty("line.separator"));
            builder.append("export AUTH_SETTINGS=\"--auth-jwt-rs256-jwks " + authToken + "\"");
        }
        // Add client group id
        var clientGroupId = properties.getValuesMap().get(NodeProperty.Name.CLIENT_GROUP_ID.toString());
        if (!Strings.isNullOrEmpty(clientGroupId)) {
            builder.append(System.getProperty("line.separator"));
            builder.append("export PARTICIPANT_ID=" + clientGroupId);
        } else {
            builder.append(System.getProperty("line.separator"));
            builder.append("export PARTICIPANT_ID=" + convertToParticipantId(nodeInfo.getId()));
        }
    }

    /**
     * Temp work-around.
     * @param nodeId id
     * @return pid
     */
    public static String convertToParticipantId(String nodeId) {
        return "p" + nodeId.replace("-", "_");
    }
}
