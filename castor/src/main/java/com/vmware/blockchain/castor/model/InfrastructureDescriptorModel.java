/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.net.URL;
import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * Infrastructure Descriptor. Contains properties required to support the deployment of blockchains.
 * Blockchain-specific properties are defined in the deployment descriptor.
 */
@Getter
@Setter
@Builder
@EqualsAndHashCode
public class InfrastructureDescriptorModel {

    /**
     * Required organization.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode(onlyExplicitlyIncluded = true)
    public static class Organization {
        private URL sddcUrl;

        private String dockerImage;
        private String damlSdk;
        private UUID templateId;

        @Min(value = 0, message = "invalid.mincpu")
        private int cpuCount;
        @Min(value = 0, message = "invalid.minmemory")
        private int memoryGb;

        // Blockchain VM storage specs - these are per kind
        @Min(value = 0, message = "invalid.mindisk")
        @Max(value = 62 * 1024, message = "invalid.maxdisk")
        private int clientDiskGb;

        @Min(value = 0, message = "invalid.mindisk")
        @Max(value = 62 * 1024, message = "invalid.maxdisk")
        private int committerDiskGb;

        private boolean generatePassword;
    }

    /**
     * Required network.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Network {
        @NotBlank(message = "network.name.not.specified")
        private String name;
        private String gateway;
        private int subnet;
        private List<String> nameServers;
    }

    /**
     * Optional outbound proxy.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class OutboundProxy {
        private String httpHost;
        private int httpPort;
        private String httpsHost;
        private int httpsPort;
    }

    /**
     * Required vCenter.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class VCenter {
        private URL url;
        @NotBlank(message = "vcenter.username.not.specified")
        private String userName;
        @NotBlank(message = "vcenter.password.not.specified")
        private String password;
        @NotBlank(message = "vcenter.resourcepool.not.specified")
        private String resourcePool;
        @NotBlank(message = "vcenter.storage.not.specified")
        private String storage;
        @NotBlank(message = "vcenter.folder.not.specified")
        private String folder;
    }

    /**
     * Optional container registry.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class ContainerRegistry {
        private URL url;
        @NotBlank(message = "container.username.not.specified")
        private String userName;
        @NotBlank(message = "container.password.not.specified")
        private String password;
    }

    /**
     * Optional Wavefront.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Wavefront {
        private URL url;
        private String token;
    }

    /**
     * Optional Elasticsearch.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class ElasticSearch {
        private URL url;
        private String userName;
        private String password;
    }

    /**
     * Optional log management.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class LogManagement {
        // 0: LOG_INTELLIGENCE
        // 1: LOG_INSIGHT
        @Pattern(regexp = "^LOG_INTELLIGENCE|LOG_INSIGHT$", message = "logmanagement.type.invalid")
        String type;

        String address;
        int port;
        String userName;
        String password;

        // Optional, set if type == LOG_INSIGHT
        int logInsightAgentId;
    }

    /**
     * Required zone.
     */
    @Getter
    @Setter
    @Builder
    @EqualsAndHashCode
    public static class Zone {
        @NotBlank(message = "zone.name.invalid")
        private String name;

        private String latitude;
        private String longitude;

        @Valid
        @NotNull(message = "vcenter.not.specified")
        private VCenter vCenter;

        @Valid
        @NotNull(message = "network.not.specified")
        private Network network;

        @Valid
        private OutboundProxy outboundProxy;

        @Valid
        private ContainerRegistry containerRegistry;

        @Valid
        private Wavefront wavefront;

        @Valid
        private ElasticSearch elasticSearch;

        @Valid
        private List<LogManagement> logManagement;
    }

    @Valid
    @NotNull(message = "organization.not.specified")
    private Organization organization;

    // List of all zones identified by their ids, to be used for Committers and Clients in the deployment descriptor.
    // This *should* match the zoneId fields specified in Committers and Clients in the deployment descriptor.
    @Valid
    @NotEmpty(message = "zones.not.specified")
    private List<Zone> zones;
}
