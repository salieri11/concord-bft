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
 * On Premises Zone entity.
 */
@EntityColumnName("helen.zone")
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
public class OnPremZone extends Zone {

    /**
     * Logging information for On Prem.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class LogManagementOnPrem {
        LogDestination destination;
        String address;
        String username;
        String password;
        int logInsightAgentId;
    }

    @LinkedEntityId
    UUID orgId;

    EndPoint vCenter;
    String resourcePool;
    String storage;
    String folder;
    Network network;
    Zone.OutboundProxy outboundProxy;
    EndPoint containerRepo;
    List<LogManagementOnPrem> logManagements;
}
