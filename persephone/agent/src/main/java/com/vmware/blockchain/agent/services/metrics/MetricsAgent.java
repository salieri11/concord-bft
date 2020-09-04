/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.metrics;

import java.util.Iterator;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;

/**
 * Class holding metrics functions.
 */
@Component
public class MetricsAgent {
    private final MeterRegistry meterRegistry;

    /**
     * Constructor.
     */
    @Autowired
    public MetricsAgent(MeterRegistry meterRegistry) {
        this.meterRegistry = meterRegistry;
    }

    /**
     * metrics counter.
     * @param metricsName metrics name
     * @param tags tags if any
     * @return {@link Counter}
     */
    public Counter getCounter(MetricsConstants.MetricsNames metricsName, List<Tag> tags) {
        return meterRegistry.counter(metricsName.metricsName, getIterable(tags));
    }

    /**
     * metrics gauge.
     * @param value gaige value
     * @param metricsName metrics name
     * @param tags tags if any
     * @return {@link Integer}
     */
    public Integer gaugeValue(int value, MetricsConstants.MetricsNames metricsName, List<Tag> tags) {
        return meterRegistry.gauge(metricsName.metricsName, getIterable(tags), value);
    }

    /**
     * metrics timer.
     * @param metricsName metrics name
     * @param tags added tags if any
     * @return {@link Timer}
     */
    public Timer getTimer(MetricsConstants.MetricsNames metricsName, List<Tag> tags) {
        return meterRegistry.timer(metricsName.metricsName, getIterable(tags));
    }

    private Iterable<Tag> getIterable(List<Tag> list) {
        Tag[] array = list.toArray(new Tag[0]);
        return () -> new Iterator<>() {
            private int index;

            @Override
            public boolean hasNext() {
                return array.length > index;
            }

            @Override
            public Tag next() {
                return array[index++];
            }
        };
    }
}
