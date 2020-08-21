/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.status;

import java.util.Arrays;
import java.util.List;

import org.springframework.stereotype.Component;

import com.vmware.blockchain.agent.services.metrics.MetricsAgent;
import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification.NodeType;

import io.micrometer.core.instrument.Tag;
import io.micrometer.prometheus.PrometheusConfig;
import io.micrometer.prometheus.PrometheusMeterRegistry;

/**
 * Util class to monitor health of components.
 */
@Component
public class NodeComponentHealthUtil {

    private final String damlExecutionEngineService = "validator";
    private final String damlLedgerApiServiceIndex = "index";
    private final String damlLedgerApiServiceRead = "read";
    private final String damlLedgerApiServiceWrite = "write";

    private final MetricsAgent metricsAgent;

    /**
     * Constructor.
     */
    NodeComponentHealthUtil() {
        List<Tag> tags = Arrays.asList(Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.name(),
                NodeComponentHealthUtil.class.getName()));
        this.metricsAgent = new MetricsAgent(new PrometheusMeterRegistry(PrometheusConfig.DEFAULT), tags);
    }

    /**
     * place holder for daml health.
     * @param nodeType committer or participant
     * @return health status
     */
    boolean getDamlHealth(NodeType nodeType) {
        if (nodeType.equals(NodeType.DAML_COMMITTER)) {
            return getDamlCommitterHealth();
        }
        return getDamlParticipantHealth();
    }

    /**
     * placeholder for concord health.
     * @return health status
     */
    boolean getConcordHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(), NodeType.DAML_COMMITTER.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), "concord"));
        this.metricsAgent.gauge(1, "concord health status",
                MetricsConstants.MetricsNames.CONCORD_HEALTH_STATUS, tags);
        return true;
    }

    /**
     * Placeholder for daml committer node health.
     * @return execution engine health status
     */
    private boolean getDamlCommitterHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(), NodeType.DAML_COMMITTER.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), damlExecutionEngineService));
        this.metricsAgent.gauge(1, "Daml health status",
                MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);

        // TODO: call grpc health check service for execution engine.
        return true;
    }

    /**
     * Placeholder for daml participant node health.
     * @return daml participant node health.
     */
    private boolean getDamlParticipantHealth() {
        // TODO: create single grpc channel and call all health check services on that. Pass arguments accordingly.
        return getDamlIndexHealth() && getDamlReadHealth() && getDamlWriteHealth();
    }

    /**
     * Placeholder for daml index health.
     * @return daml index health status
     */
    private boolean getDamlIndexHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(), NodeType.DAML_PARTICIPANT.name()),
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
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(), NodeType.DAML_PARTICIPANT.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), damlLedgerApiServiceRead));
        this.metricsAgent.gauge(1, "Daml health status",
                MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
        return true;
    }

    /**
     * Placeholder for daml write health.
     * @return write health status
     */
    private boolean getDamlWriteHealth() {
        List<Tag> tags = Arrays.asList(
                Tag.of(MetricsConstants.MetricsTags.TAG_NODE_TYPE.name(), NodeType.DAML_PARTICIPANT.name()),
                Tag.of(MetricsConstants.MetricsTags.TAG_COMPONENT.name(), damlLedgerApiServiceWrite));
        this.metricsAgent.gauge(1, "Daml health status",
                MetricsConstants.MetricsNames.DAML_HEALTH_STATUS, tags);
        return true;
    }
}
