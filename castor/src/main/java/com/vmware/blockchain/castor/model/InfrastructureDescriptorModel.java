/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.model;

import java.net.URL;
import java.util.List;
import java.util.UUID;

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
        @EqualsAndHashCode.Exclude
        private UUID orgId;

        private URL sddcUrl;

        private String dockerImage;
        private String damlSdk;
        private UUID templateId;
        private int cpuCount;
        private int memoryGb;

        private boolean enableBftClient;
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
        private String userName;
        private String password;
        private String resourcePool;
        private String storage;
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
        private String userName;
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
        String type;

        // DINKARTODO: Should the address and port be consolidated into address?
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
        private String name;
        private String latitude;
        private String longitude;
        private VCenter vCenter;
        private Network network;
        private OutboundProxy outboundProxy;
        private ContainerRegistry containerRegistry;
        private Wavefront wavefront;
        private ElasticSearch elasticSearch;
        private List<LogManagement> logManagement;
    }

    private Organization organization;

    // List of all zones identified by their ids, to be used for Committers and Clients in the deployment descriptor.
    // This *should* match the zoneId fields specified in Committers and Clients in the deployment descriptor.
    private List<Zone> zones;
}
