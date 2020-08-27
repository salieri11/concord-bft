/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.concord;

import java.util.Collections;
import java.util.List;

import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

import io.micrometer.core.instrument.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Concord health queries.
 */
@Slf4j
public class ConcordHealth implements ComponentHealth {

    @Override
    public HealthStatusResponse getHealth() {
        List<Tag> tags = Collections.singletonList(
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), "concord"));
        this.metricsAgent.gauge(1, "concord health status",
                MetricsConstants.MetricsNames.CONCORD_HEALTH_STATUS, tags);
        return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.HEALTHY).build();
    }
}
