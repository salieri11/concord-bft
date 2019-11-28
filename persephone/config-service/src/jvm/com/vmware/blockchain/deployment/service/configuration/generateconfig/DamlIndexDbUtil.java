/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.generateconfig;

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
    public String generateConfig() {
        StringBuilder builder = new StringBuilder();
        builder.append("export POSTGRES_USER=indexdb");
        builder.append(System.getProperty("line.separator"));
        builder.append("export POSTGRES_MULTIPLE_DATABASES=daml_ledger_api");
        builder.append(System.getProperty("line.separator"));
        builder.append("export MAX_CONNECTIONS=300");
        builder.append(System.getProperty("line.separator"));
        builder.append("export BUFFER_SIZE=80MB");
        return builder.toString();
    }

}
