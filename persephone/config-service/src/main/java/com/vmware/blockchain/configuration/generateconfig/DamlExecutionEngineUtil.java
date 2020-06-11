/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

/**
 * Utility class for generating the config(s) for Daml Execution Engine file.
 */
public class DamlExecutionEngineUtil {

    /**
     * file path.
     */
    public static final String envVarPath = "/daml-execution-engine/environment-vars";

    /**
     * Utility to daml execution engine config.
     *
     * @return json string
     */
    public String generateConfig() {

        StringBuilder builder = new StringBuilder();
        builder.append("export JAVA_OPTS=\"-XX:+UseG1GC -Xmx10G");
        return builder.toString();
    }
}

