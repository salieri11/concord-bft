/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import java.util.List;
import java.util.UUID;

import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * VMC on AWS Zone entity.
 */
@EntityColumnName("helen.zone")
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class VmcAwsZone extends Zone {

    /**
     * Logging information for VMC AWS.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class LogManagementVmcAws {
        LogDestination destination;
        String address;
        String token;
    }

    @LinkedEntityId
    UUID orgId;

    // Place holder for right now.
    String cspUrl;
    String vmcUrl;
    String refreshToken;

    String organization;
    String datacenter;
    String resourcePool;
    String storage;
    String folder;
    Network network;
    List<LogManagementVmcAws> logManagements;

}
