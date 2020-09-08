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

    ConcordHealthServiceInvoker concordHealthServiceInvoker;

    public ConcordHealth(ConcordHealthServiceInvoker concordHealthServiceInvoker) {
        this.concordHealthServiceInvoker = concordHealthServiceInvoker;
    }

    @Override
    public HealthStatusResponse getHealth() {
        log.info("Invoking concord health query..");
        try {
            return concordHealthServiceInvoker.getConcordHealth();
        } catch (Exception ex) {
            log.error("Exception while querying concord health:\n{}\n", ex.getLocalizedMessage());
            return HealthStatusResponse.builder()
                    .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                    .exception(ex.getLocalizedMessage())
                    .build();
        }
    }
}
