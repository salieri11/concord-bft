/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import java.util.Collections;
import java.util.List;

import com.vmware.blockchain.agent.services.metrics.MetricsAgent;
import com.vmware.blockchain.agent.services.metrics.MetricsConstants;

import io.micrometer.core.instrument.Tag;

/**
 * health check interface.
 */
public interface ComponentHealth {

    List<Tag> tags = Collections.singletonList(Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.metricsTagName,
            ComponentHealth.class.getName()));
    MetricsAgent metricsAgent = new MetricsAgent(tags);

    /**
     * gets health status of services.
     * @return {@link HealthStatusResponse}
     */
    HealthStatusResponse getHealth();
}
