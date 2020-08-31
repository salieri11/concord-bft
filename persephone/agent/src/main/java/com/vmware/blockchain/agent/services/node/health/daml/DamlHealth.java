/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
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
        return damlHealthServiceInvoker.getHealthResponse();
    }
}
