/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Test for TelegrafConfigUtil.
 */
public class TelegrafConfigUtilTest {

    private TelegrafConfigUtil telegrafConfigUtil;

    @BeforeEach
    public void createObject() {
        this.telegrafConfigUtil = new TelegrafConfigUtil("TelegrafConfigTemplate.config",
                "MetricsConfig.yaml");
    }

    @Test
    public void testMetricsConfig() throws IOException {
        var actual = telegrafConfigUtil.getMetricsConfigYaml();

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("MetricsConfig.yaml").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected));
    }

    @Test
    public void testTelegrafConfigHappyPath() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest"))
                .build();
        List<String> urlList = List.of("\"my.url.1\"", "\"my.url.2\"");

        Map<Integer, String> actual = telegrafConfigUtil.getTelegrafConfig(hostIps, properties, urlList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfig.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$REPLICA", hostIps.get(key));
            Assertions.assertThat(value.equals(expect));
        });
    }

    @Test
    public void testNoHostIp() {
        List<String> hostIps = new ArrayList<>();
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest"))
                .build();
        List<String> urlList = List.of("\"my.url.1\"", "\"my.url.2\"");

        Map<Integer, String> actual = telegrafConfigUtil.getTelegrafConfig(hostIps, properties, urlList);

        Assertions.assertThat(actual.isEmpty());
    }

    @Test
    public void testPaths() {
        Assertions.assertThat(TelegrafConfigUtil.metricsConfigPath.equals(
                "/concord/config-public/metrics_config.yaml"));
        Assertions.assertThat(TelegrafConfigUtil.configPath.equals("/telegraf/telegraf.conf"));
    }
}
