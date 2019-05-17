/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableMap;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * An interface for retrieving brief information about a particular version of a particular contract.
 */
@Data
@NoArgsConstructor
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
        } catch (Exception e) {
            metadata = ImmutableMap.of("error", "Could not read metadata");
        }
        version = c.getVersionName();
    }
}
