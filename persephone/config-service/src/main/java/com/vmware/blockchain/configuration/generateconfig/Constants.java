/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

/**
 * Configuration utility constants.
 * Contains config file paths and file access restriction patterns, if the file contains sensitive data.
 * */
public class Constants {

    /**
     * DAML ledger API BFT client config path.
     */
    public static final String DAML_BFT_CLIENT_CONFIG_PATH = "/daml-ledger-api/config-public/bftclient.config";

    /**
     * Concord config file path.
     */
    public static final String CONCORD_CONFIG_PATH = "/concord/config-local/concord.config";
    /**
     * File permissions Posix style.
     */
    public static final String CONCORD_CONFIG_FILE_PERMISSIONS = "rw-r--r--";
    /**
     * Concord deploy file path.
     */
    public static final String CONCORD_DEPLOY_CONFIG_PATH = "/concord/config-local/deployment.config";
    /**
     * Concord secrets file path.
     */
    public static final String CONCORD_SECRETS_CONFIG_PATH = "/concord/config-local/secrets.config";
    /**
     * DAML env vars file path.
     */
    public static final String DAML_ENV_VARS_PATH = "/daml-execution-engine/environment-vars";
    /**
     * DAML index DB config path.
     */
    public static final String DAML_DB_ENV_VARS_PATH = "/daml-index-db/environment-vars";
    /**
     * DAML postgres DB config path.
     */
    public static final String DAML_DB_POSTGRES_CONFIG_PATH = "/daml-index-db/postgresql.conf";
    /**
     * Genesis file path.
     */
    public static final String GENESIS_CONFIG_PATH = "/concord/config-public/genesis.json";
    /**
     * Fluentd Logging file path.
     */
    public static final String LOGGING_CONFIG_PATH = "/fluentd/logging.env";
    /**
     * Fluentd Logging file permissions Posix style.
     */
    public static final String LOGGING_CONFIG_FILE_PERMISSIONS = "rw-------";
    /**
     * Telegraf config path.
     */
    public static final String TELEGRAF_CONFIG_PATH = "/telegraf/telegraf.conf";
    /**
     * Wavefront config path.
     */
    public static final String WAVEFRONT_CONFIG_PATH = "/wavefront-proxy/wavefront.conf";
    /**
     * Wavefront file permissions Posix style.
     */
    public static final String WAVEFRONT_CONFIG_FILE_PERMISSIONS = "rw-------";
    /**
     * Generic config path.
     */
    public static String GENERIC_IDENTIFIERS_PATH = "/generic/identifiers.env";
}
