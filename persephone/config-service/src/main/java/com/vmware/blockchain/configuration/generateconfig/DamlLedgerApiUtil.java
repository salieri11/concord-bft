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
        builder.append(System.lineSeparator());

        builder.append("export INDEXDB_PORT=5432");
        builder.append(System.lineSeparator());

        builder.append("export INDEXDB_USER=indexdb");
        builder.append(System.lineSeparator());

        builder.append("export REPLICAS=" + getReplicas(nodeInfo.getProperties()));
        builder.append(System.lineSeparator());

        builder.append("export JAVA_OPTS=\"-XX:+UseG1GC -Xmx10G "
                       + "-XX:ErrorFile=/config/daml-ledger-api/cores/err_pid%p.log\"");
        builder.append(System.lineSeparator());

        builder.append("export THIN_REPLICA_SETTINGS=\"--use-thin-replica --jaeger-agent-address jaeger-agent:6831\"");
        builder.append(System.lineSeparator());

        addProperties(builder, nodeInfo);
        builder.append("export BFT_CLIENT_SETTINGS=\"--use-bft-client --bft-client-config-path=/config"
                       + Constants.DAML_BFT_CLIENT_CONFIG_PATH + "\"");
        builder.append(System.lineSeparator());

        return builder.toString().trim();
    }

    private String getReplicas(Properties properties) {
        String replicas = properties.getValuesMap().get(NodeProperty.Name.COMMITTERS.name());
        if (Strings.isNullOrEmpty(replicas)) {
            return "concord:50051";
        } else {
            return replicas;
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
        var authToken = properties.getValuesMap().get(NodeProperty.Name.CLIENT_AUTH_JWT.name());
        if (!Strings.isNullOrEmpty(authToken)) {
            builder.append("export AUTH_SETTINGS=\"--auth-jwt-rs256-jwks " + authToken + "\"");
            builder.append(System.lineSeparator());
        }
        addClientGroupId(builder, nodeInfo, properties);
        addPreexecutionThreshold(builder, properties);
        addTlsCredentials(builder, properties);
    }

    private void addClientGroupId(StringBuilder builder, NodesInfo.Entry nodeInfo, Properties properties) {
        // Add client group id
        var clientGroupId = properties.getValuesMap().get(NodeProperty.Name.CLIENT_GROUP_ID.name());
        if (!Strings.isNullOrEmpty(clientGroupId)) {
            // TODO Remove the convertor after new release.
            builder.append("export PARTICIPANT_ID=" + convertToParticipantId(clientGroupId));
            builder.append(System.lineSeparator());
        } else {
            builder.append("export PARTICIPANT_ID=" + convertToParticipantId(nodeInfo.getId()));
            builder.append(System.lineSeparator());
        }
    }

    private void addPreexecutionThreshold(StringBuilder builder, Properties properties) {
        if (properties.getValuesMap()
                .getOrDefault(DeploymentAttributes.PREEXECUTION_ENABLED.name(), "True")
                .equalsIgnoreCase("True")) {
            builder.append("export PRE_EXECUTION_TIME_THRESHOLD=");
            builder.append(properties.getValuesMap()
                    .getOrDefault(DeploymentAttributes.PREEXECUTION_THRESHOLD.name(), "0s"));
            builder.append(System.lineSeparator());
        }
    }

    private void addTlsCredentials(StringBuilder builder, Properties properties) {
        String pem = properties.getValuesMap().getOrDefault(NodeProperty.Name.TLS_PEM.name(), "");
        String crt = properties.getValuesMap().getOrDefault(NodeProperty.Name.TLS_CRT.name(), "");
        String cacrt = properties.getValuesMap().getOrDefault(NodeProperty.Name.TLS_CACRT.name(), "");

        // Certificate values can either br provided or not.
        // If they are provided, make sure they are all present.
        // If they are not provided, take no action.
        // This is to maintain backward compatibility.

        boolean tlsEnabled = !Strings.isNullOrEmpty(pem)
                && !Strings.isNullOrEmpty(crt)
                && !Strings.isNullOrEmpty(cacrt);

        if (tlsEnabled) {
            builder.append("export PEM=" + pem);
            builder.append(System.lineSeparator());
            builder.append("export CRT=" + crt);
            builder.append(System.lineSeparator());
            builder.append("export CACRT=" + cacrt);
            builder.append(System.lineSeparator());
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
