/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Builder;
import lombok.Data;

/**
 * response data class for node status.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
public class HealthStatusResponse {

    @JsonProperty("service_status")
    HealthStatus status;
    String exception;

    /**
     * enum for health status.
     */
    public enum HealthStatus {
        HEALTHY("HEALTHY"),
        UNHEALTHY("UNHEALTHY"),
        SERVICE_UNAVAILABLE("SERVICE UNAVAILABLE");

        String healthStatus;

        HealthStatus(String healthStatus) {
            this.healthStatus = healthStatus;
        }
    }
}
