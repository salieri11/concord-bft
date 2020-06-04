/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.security.SecureRandom;
import java.util.UUID;

import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;

import lombok.extern.slf4j.Slf4j;

/**
 * Implementation of ConfigurationService server.
 */
@Slf4j
public class ConfigurationServiceUtil {

    /**
     * Generate a new {@link ConfigurationSessionIdentifier}.
     *
     * @return
     *   a new {@link ConfigurationSessionIdentifier} instance.
     */
    @Deprecated
    static ConfigurationSessionIdentifier newSessionId() {
        return ConfigurationSessionIdentifier.newBuilder().setIdentifier(new SecureRandom().nextLong()).build();
    }

    static ConfigurationSessionIdentifier newSessionUId() {
        return ConfigurationSessionIdentifier.newBuilder().setId(UUID.randomUUID().toString()).build();
    }
}
