/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

/**
 * Utility class for generating the config(s) for Fluentd file.
 */
public class LoggingUtil {

    private String config;

    public LoggingUtil(String config) {
        this.config = config;
    }

    /**
     * file path.
     */
    public static final String envVarPath = "/fluentd/environment-vars";

    /**
     * Utility to fluentd.
     * @return json string
     */
    public String generateConfig() {
        StringBuilder builder = new StringBuilder();
        builder.append(config);
        return builder.toString();
    }

}
