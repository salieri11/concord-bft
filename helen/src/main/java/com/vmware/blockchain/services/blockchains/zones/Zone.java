/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import static com.fasterxml.jackson.annotation.JsonTypeInfo.As.EXISTING_PROPERTY;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.vmware.blockchain.dao.AbstractEntity;
import com.vmware.blockchain.dao.EntityColumnName;
import com.vmware.blockchain.dao.LinkedEntityId;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * The Orchestration Site view from Persephone.
 * Note that this extends Abstract Entity, but
 */
@EntityColumnName("helen.zone")
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = EXISTING_PROPERTY, property = "type",
        visible = true, defaultImpl = Zone.class)
@JsonSubTypes({
        @Type(value = OnPremZone.class, name = "ON_PREM"),
        @Type(value = VmcAwsZone.class, name = "VMC_AWS"),
    })
public class Zone extends AbstractEntity {

    public static final String NAME_KEY = "name";
    public static final String LAT_KEY = "geo-latitude";
    public static final String LONG_KEY = "geo-longitude";


    /**
     * Network information.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Network {
        @NotBlank(message = "Network name cannot be null")
        String name;
        @Valid
        List<@NotBlank(message = "IP cannot be empty") String> ipPool;
        @NotBlank(message = "Gateway cannot be empty")
        String gateway;
        @NotBlank(message = "Subnet cannot be empty")
        String subnet;
        List<String> nameServers;
    }

    /**
     * Outbound Proxy information.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class OutboundProxy {
        String httpHost;
        int httpPort;
        String httpsHost;
        int httpsPort;
    }

    enum Action {
        RELOAD,
        TEST
    }

    /**
     * Zone types.
     */
    public enum Type {
        NONE,
        ON_PREM,
        VMC_AWS
    }

    /**
     *  Log Destination Type.
     */
    public enum LogDestination {
        LOG_INTELLIGENCE(0),
        LOG_INSIGHT(1);

        private final int value;

        LogDestination(final int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }

    }

    /**
     * Wavefront Endpoint Config.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Wavefront {
        String url;
        String token;
    }

    /**
     * Elasticsearch Endpoint Config.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Elasticsearch {
        String url;
        String username;
        String password;
    }


    @LinkedEntityId
    UUID orgId;

    String name;
    String latitude;
    String longitude;
    Type type;
    Wavefront wavefront;
    Elasticsearch elasticsearch;

    /**
     * Default constructor.
     */
    public Zone(UUID id, Type type, Map<String, String> labels) {
        this.setId(id);
        this.name = labels.get(NAME_KEY);
        this.latitude = labels.get(LAT_KEY);
        this.longitude = labels.get(LONG_KEY);
        this.type = type;
    }

    /**
     * Contstructor for testing.
     */
    public Zone(UUID id, Type type) {
        this.setId(id);
        this.type = type;
    }
}
