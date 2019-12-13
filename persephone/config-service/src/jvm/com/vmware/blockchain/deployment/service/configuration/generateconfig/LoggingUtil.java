/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.generateconfig;

/**
 * Utility class for generating the config(s) for Fluentd file.
 */
public class LoggingUtil {

    /**
     * This value cannot be changed without a change in persephone.
     */
    public static String LOGGING_CONFIG = "loggingConfig";

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
