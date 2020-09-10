/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.daml;

import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Daml health queries.
 */
@Slf4j
public class DamlHealth implements ComponentHealth {

    DamlHealthServiceInvoker damlHealthServiceInvoker;

    public DamlHealth(DamlHealthServiceInvoker damlHealthServiceInvoker) {
        this.damlHealthServiceInvoker = damlHealthServiceInvoker;
    }

    @Override
    public HealthStatusResponse getHealth() {
        log.info("Invoking daml health query..");
        try {
            return damlHealthServiceInvoker.getHealthResponse();
        } catch (Exception ex) {
            log.error("Exception while querying concord health:\n{}\n", ex.getLocalizedMessage());
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(ex.getLocalizedMessage())
                    .build();
        }
    }
}
