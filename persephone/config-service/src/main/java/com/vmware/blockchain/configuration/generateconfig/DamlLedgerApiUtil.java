/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import com.google.common.base.Strings;

/**
 * Utility class for generating the config(s) for Daml Ledger Api file.
 */
public class DamlLedgerApiUtil {

    /**
     * This value cannot be changed without a change in persephone.
     */
    public static String REPLICAS_KEY = "replicas";

    private String replicas;

    public DamlLedgerApiUtil(String replicas) {
        this.replicas = replicas;
    }

    /**
     * file path.
     */
    public static final String envVarPath = "/daml-ledger-api/environment-vars";

    /**
     * Utility to daml ledger api config.
     * @return json string
     */
    public String generateConfig() {
        StringBuilder builder = new StringBuilder();
        builder.append("export INDEXDB_HOST=daml_index_db");
        builder.append(System.getProperty("line.separator"));
        builder.append("export INDEXDB_PORT=5432");
        builder.append(System.getProperty("line.separator"));
        builder.append("export INDEXDB_USER=indexdb");
        builder.append(System.getProperty("line.separator"));
        builder.append("export REPLICAS=" + getReplicas());
        builder.append(System.getProperty("line.separator"));
        builder.append("export PARTICIPANT_ID=daml_ledger_api");
        builder.append(System.getProperty("line.separator"));
        builder.append("export JAVA_OPTS=-Xmx4G");
        return builder.toString();
    }

    private String getReplicas() {
        if (Strings.isNullOrEmpty(replicas)) {
            return "concord:50051";
        } else {
            return replicas;
        }
    }
}
