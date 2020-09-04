/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import com.vmware.blockchain.agent.services.metrics.MetricsConstants;

import io.micrometer.core.instrument.Tag;

/**
 * health check interface.
 */
public interface ComponentHealth {

    Tag tag = Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.metricsTagName,
            ComponentHealth.class.getName());

    /**
     * gets health status of services.
     * @return {@link HealthStatusResponse}
     */
    HealthStatusResponse getHealth();
}
