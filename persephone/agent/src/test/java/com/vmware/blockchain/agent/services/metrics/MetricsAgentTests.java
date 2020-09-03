/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.metrics;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.Meter;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;

/**
 * Test class for {@link MetricsAgent}.
 */
@ExtendWith(SpringExtension.class)
@ComponentScan(basePackageClasses = { MetricsAgentTests.class})
public class MetricsAgentTests {

    private static final MetricsAgent metricsAgent = new MetricsAgent(new SimpleMeterRegistry());

    private static final List<Tag> tags = Arrays.asList(
            Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.metricsTagName, "unit_test"),
            Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.metricsTagName, "testCounter"),
            Tag.of(MetricsConstants.MetricsTags.TAG_IMAGE.metricsTagName, "unitTesting"));

    @Test
    void testCounter() {
        Counter counter = metricsAgent.getCounter(MetricsConstants.MetricsNames.CONTAINERS_PULL_IMAGES, tags);

        counter.increment();
        Assertions.assertEquals(1, counter.count());
        verifyNamesAndTags(counter.getId(), MetricsConstants.MetricsNames.CONTAINERS_PULL_IMAGES);

        counter.increment();
        counter.increment();
        Assertions.assertEquals(3, counter.count());
    }

    @Test
    void testTimer() {
        Timer timer = metricsAgent.getTimer(MetricsConstants.MetricsNames.CONTAINERS_LAUNCH, tags);

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
        int value1 = 1234;
        int value2 = 5678;
        int v1 = metricsAgent.gaugeValue(value1, MetricsConstants.MetricsNames.WRITE, tags);

        Assertions.assertEquals(value1, v1);

        int v2 = metricsAgent.gaugeValue(value2, MetricsConstants.MetricsNames.WRITE, tags);

        Assertions.assertEquals(value2, v2);
    }

    private void verifyNamesAndTags(Meter.Id meterId, MetricsConstants.MetricsNames metricName) {
        Assertions.assertEquals(meterId.getName(), metricName.metricsName);
        Assertions.assertEquals(meterId.getTags().size(), 3);
        Assertions.assertEquals(meterId.getTag(MetricsConstants.MetricsTags.TAG_IMAGE.metricsTagName), "unitTesting");
        Assertions.assertEquals(meterId.getTag(MetricsConstants.MetricsTags.TAG_METHOD.metricsTagName), "testCounter");
        Assertions.assertEquals(meterId.getTag(MetricsConstants.MetricsTags.TAG_SERVICE.metricsTagName), "unit_test");
    }
}

