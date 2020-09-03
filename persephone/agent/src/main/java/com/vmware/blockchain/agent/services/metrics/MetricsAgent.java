/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.metrics;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.ToDoubleFunction;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;

/**
 * Class holding metrics functions.
 */
public class MetricsAgent {
    private final MeterRegistry meterRegistry;
    List<Tag> metricTags;


    /**
     * Constructor.
     * @param metricsTags metrics tag
     */
    public MetricsAgent(List<Tag> metricsTags) {
        this.meterRegistry = Metrics.globalRegistry;
        this.metricTags = metricsTags;
    }

    /**
     * metrics counter.
     * @param desc description
     * @param metricsName metrics name
     * @param addedTags tags if any
     * @return {@link Counter}
     */
    public Counter getCounter(String desc, MetricsConstants.MetricsNames metricsName, List<Tag> addedTags) {
        return Counter.builder(metricsName.metricsName)
                .tags(getAllTags(addedTags))
                .description(desc)
                .register(meterRegistry);
    }

    /**
     * metrics gauge.
     * @param value gaige value
     * @param desc description
     * @param metricsName metrics name
     * @param addedTags tags if any
     * @return {@link Gauge}
     */
    public Gauge gauge(int value, String desc,
                       MetricsConstants.MetricsNames metricsName, List<Tag> addedTags) {
        ToDoubleFunction<Integer> val = Double::valueOf;
        return Gauge.builder(metricsName.metricsName, value, val)
                .tags(getAllTags(addedTags))
                .description(desc)
                .register(meterRegistry);
    }

    /**
     * metrics timer.
     * @param desc description
     * @param metricsName metrics name
     * @param addedTags added tags if any
     * @return {@link Timer}
     */
    public Timer getTimer(String desc, MetricsConstants.MetricsNames metricsName, List<Tag> addedTags) {
        return Timer.builder(metricsName.metricsName)
                .tags(getAllTags(addedTags))
                .description(desc)
                .register(meterRegistry);
    }

    private Iterable<Tag> getAllTags(List<Tag> additionalTags) {
        if (additionalTags.isEmpty()) {
            return getIterable(this.metricTags);
        }
        List<Tag> list = new ArrayList<>();
        list.addAll(this.metricTags);
        list.addAll(additionalTags);
        return getIterable(list);
    }

    private Iterable<Tag> getIterable(List<Tag> list) {
        Tag[] array = list.toArray(new Tag[list.size()]);
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
