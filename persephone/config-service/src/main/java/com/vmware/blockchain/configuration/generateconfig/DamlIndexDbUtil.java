/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import com.vmware.blockchain.deployment.v1.NodesInfo;

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
    public String generateConfig(NodesInfo.Entry nodeInfo) {
        var nodeName = DamlLedgerApiUtil.convertToParticipantId(nodeInfo.getId());

        StringBuilder builder = new StringBuilder();
        builder.append("export POSTGRES_USER=indexdb");
        builder.append(System.getProperty("line.separator"));
        builder.append("export POSTGRES_MULTIPLE_DATABASES=" + nodeName);
        builder.append(System.getProperty("line.separator"));
        builder.append("export MAX_CONNECTIONS=300");
        builder.append(System.getProperty("line.separator"));
        builder.append("export BUFFER_SIZE=4096MB");
        return builder.toString();
    }

}
