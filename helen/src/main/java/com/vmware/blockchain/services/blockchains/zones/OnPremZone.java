/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

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
        @NotNull(message = "Destination cannot be empty")
        LogDestination destination;
        @NotBlank(message = "Address cannot be empty")
        String address;
        @NotNull(message = "Port cannot be empty")
        Integer port;
        @NotBlank(message = "Username cannot be empty")
        String username;
        @NotBlank(message = "Password cannot be empty")
        String password;
        int logInsightAgentId;
    }

    /**
     * Repo to pick up images from.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class EndPoint {
        @NotBlank(message = "url cannot be null")
        String url;
        @NotBlank(message = "username cannot be null")
        String username;
        @NotBlank(message = "password cannot be null")
        String password;
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
