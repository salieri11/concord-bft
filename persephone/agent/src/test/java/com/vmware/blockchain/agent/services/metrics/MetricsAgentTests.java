/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.metrics;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.Meter;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;

/**
 * Test class for {@link MetricsAgent}.
 */
public class MetricsAgentTests {

    private static MetricsAgent metricsAgent;
    private static List<Tag> additionalTags;

    @BeforeAll
    static void setUp() {
        List<Tag> tags = Arrays.asList(Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.name(), "unit_test"));
        additionalTags = Arrays.asList(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "testCounter"),
                Tag.of(MetricsConstants.MetricsTags.TAG_IMAGE.name(), "unitTesting"));
        metricsAgent = new MetricsAgent(new SimpleMeterRegistry(), tags);
    }

    @Test
    void testCounter() {
        Counter counter = metricsAgent.getCounter("unit testing",
                MetricsConstants.MetricsNames.CONTAINERS_PULL_IMAGES,
                additionalTags);

        counter.increment();
        Assertions.assertTrue(counter.count() == 1);
        verifyNamesAndTags(counter.getId(), MetricsConstants.MetricsNames.CONTAINERS_PULL_IMAGES);
    }

    @Test
    void testTimer() {
        Timer timer = metricsAgent.getTimer("unit testing",
                MetricsConstants.MetricsNames.CONTAINERS_LAUNCH,
                additionalTags);

        timer.record(() -> {
            try {
                TimeUnit.MILLISECONDS.sleep(1500);
            } catch (InterruptedException ignored) { }
        });

        timer.record(3000, TimeUnit.MILLISECONDS);

        Assertions.assertEquals(2, timer.count());
        Assertions.assertTrue(4510 > timer.totalTime(TimeUnit.MILLISECONDS)
                && 4500 <= timer.totalTime(TimeUnit.MILLISECONDS));
        verifyNamesAndTags(timer.getId(), MetricsConstants.MetricsNames.CONTAINERS_LAUNCH);
    }

    @Test
    void testGauge() {
        int value = 1234;
        Gauge gauge = metricsAgent.gauge(value, "unit testing",
                MetricsConstants.MetricsNames.WRITE,
                additionalTags);

        Assertions.assertEquals(gauge.value(), Double.valueOf(value));
        verifyNamesAndTags(gauge.getId(), MetricsConstants.MetricsNames.WRITE);
    }

    private void verifyNamesAndTags(Meter.Id meterId, MetricsConstants.MetricsNames metricName) {
        Assertions.assertEquals(meterId.getName(), metricName.name());
        Assertions.assertEquals(meterId.getTags().size(), 3);
        Assertions.assertEquals(meterId.getTag(MetricsConstants.MetricsTags.TAG_IMAGE.name()), "unitTesting");
        Assertions.assertEquals(meterId.getTag(MetricsConstants.MetricsTags.TAG_METHOD.name()), "testCounter");
        Assertions.assertEquals(meterId.getTag(MetricsConstants.MetricsTags.TAG_SERVICE.name()), "unit_test");
    }
}

