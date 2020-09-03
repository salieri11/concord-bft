/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.concord;

import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Concord health queries.
 */
@Slf4j
public class ConcordHealth implements ComponentHealth {

    @Override
    public HealthStatusResponse getHealth() {
        /** TODO: UNIMPLEMENTED. */
        return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE).build();
    }
}
