/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.configuration;

/**
 * Enumeration of gRPC-based service's endpoints and its associated constants.
 */
enum ServerEndpoints {

    PROVISIONING_SERVER(
            "fleet.provisioning.service.url",
            "grpc://persephone-provisioning:9002"
    ),
    FLEET_MANAGEMENT_SERVER(
            "fleet.management.service.url",
            "grpc://persephone-fleet:9004"
    );

    static String SCHEME_GRPC_TLS = "grpcs";
    static String SCHEME_GRPC = "grpc";

    /** Environment property key to lookup to resolve for value at runtime. */
    private final String key;

    /** Default URL to use if no value is specified in the environment. */
    private final String defaultUrl;

    ServerEndpoints(String key, String defaultUrl) {
        this.key = key;
        this.defaultUrl = defaultUrl;
    }

    String getKey() {
        return key;
    }

    String getDefaultUrl() {
        return defaultUrl;
    }
}
