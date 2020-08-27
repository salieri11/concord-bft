/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.daml;

import java.util.Arrays;
import java.util.List;

import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

import io.micrometer.core.instrument.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Daml execution engine health queries.
 */
@Slf4j
public class DamlExecutionEngineHealth implements ComponentHealth {

    private final String damlExecutionEngineService = "validator";

    @Override
    public HealthStatusResponse getHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(),
                        ConcordModelSpecification.NodeType.DAML_COMMITTER.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), damlExecutionEngineService));
        this.metricsAgent.gauge(1, "Daml health status",
                MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);

        // TODO: call grpc health check service for execution engine.
        return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.HEALTHY).build();
    }
}
