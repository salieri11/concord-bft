/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
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
 * Daml Ledger API health queries.
 */
@Slf4j
public class DamlLedgerApiHealth implements ComponentHealth {

    private final String damlLedgerApiServiceIndex = "index";
    private final String damlLedgerApiServiceRead = "read";

    @Override
    public HealthStatusResponse getHealth() {
        if (getDamlIndexHealth() && getDamlReadHealth()) {
            return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.HEALTHY).build();
        }
        return HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.UNHEALTHY).build();
    }

    /**
     * Placeholder for daml index health.
     * @return daml index health status
     */
    private boolean getDamlIndexHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(),
                        ConcordModelSpecification.NodeType.DAML_PARTICIPANT.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), damlLedgerApiServiceIndex));
        this.metricsAgent.gauge(1, "Daml health status",
                MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
        return true;
    }

    /**
     * Placeholder for daml read health.
     * @return read health status
     */
    private boolean getDamlReadHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(),
                        ConcordModelSpecification.NodeType.DAML_PARTICIPANT.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), damlLedgerApiServiceRead));
        this.metricsAgent.gauge(1, "Daml health status",
                MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
        return true;
    }
}
