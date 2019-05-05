/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableMap;

import lombok.Data;

/**
 * An interface for retrieving brief information about a particular version of a particular contract.
 */
@Data
public class BriefVersionInfo {
    String address;

    Object metadata;

    String version;

    /**
     * Create an instance from a give Contract.
     */
    public BriefVersionInfo(Contract c) {
        address = c.getAddress();
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            metadata = objectMapper.readValue(c.getMetadata(), Object.class);
        } catch (IOException e) {
            metadata = ImmutableMap.of("error", "Could not read metadata");
        }
        version = c.getVersionName();
    }
}
