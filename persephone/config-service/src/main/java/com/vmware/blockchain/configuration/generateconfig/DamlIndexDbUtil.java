/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;

/**
 * Utility class for generating the config(s) for Daml Index DB file.
 */
public class DamlIndexDbUtil {

    private static final Logger log = LoggerFactory.getLogger(DamlIndexDbUtil.class);

    /**
     * Utility to daml ledger api config.
     * @return json string
     */
    public String generateConfig(NodesInfo.Entry nodeInfo) {

        StringBuilder builder = new StringBuilder();
        builder.append("export POSTGRES_USER=indexdb");
        builder.append(System.lineSeparator());
        String pwd = nodeInfo.getProperties().getValuesMap().get(NodeProperty.Name.DAML_DB_PASSWORD.name());
        if (pwd != null) {
            builder.append("export POSTGRES_PASSWORD=" + pwd);
            builder.append(System.lineSeparator());
        }
        var clientGroupId =
                nodeInfo.getProperties().getValuesMap().getOrDefault(NodeProperty.Name.CLIENT_GROUP_ID.name(),
                                                                     nodeInfo.getId());
        var nodeName = DamlLedgerApiUtil.convertToParticipantId(clientGroupId);
        // Remove this once 0.8 version is deprecated
        builder.append("export POSTGRES_MULTIPLE_DATABASES=" + nodeName);
        builder.append(System.lineSeparator());
        builder.append("export MAX_CONNECTIONS=300");
        builder.append(System.lineSeparator());
        builder.append("export BUFFER_SIZE=4096MB");
        builder.append(System.lineSeparator());
        builder.append("export POSTGRES_CONFIG_FILE_OPT=\"-c config_file=/config"
                       + Constants.DAML_DB_POSTGRES_CONFIG_PATH + "\"");
        return builder.toString();
    }

    /**
     * New conf file for index-db.
     * @return json string
     */
    public String getPostGresConfig() {
        try {
            InputStream inputStream = new FileInputStream("postgresql.conf");
            return new String(inputStream.readAllBytes());
        } catch (IOException | NullPointerException ex) {
            log.error("Postgres config could not be read due to: {}", ex.getLocalizedMessage());
            throw new RuntimeException(ex);
        }
    }
}
