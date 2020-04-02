/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.yaml.snakeyaml.Yaml;

import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Test for TelegrafConfigUtil.
 */
public class TelegrafConfigUtilTest {

    private TelegrafConfigUtil telegrafConfigUtil;

    Map<Integer, String> nodeIpMap = new HashMap<>();
    Map<Integer, String> nodeIdMap = new HashMap<>();
    List<NodeProperty> propertyList;

    /**
     * Initialize.
     */
    @BeforeEach
    public void createObject() {
        this.telegrafConfigUtil = new TelegrafConfigUtil("TelegrafConfigTemplate.conf",
                "MetricsConfig.yaml");

        nodeIpMap.put(0, "10.0.0.1");
        nodeIpMap.put(1, "10.0.0.2");
        nodeIpMap.put(2, "10.0.0.3");
        nodeIpMap.put(3, "10.0.0.4");

        nodeIdMap.put(0, "node-0");
        nodeIdMap.put(1, "node-1");
        nodeIdMap.put(2, "node-2");
        nodeIdMap.put(3, "node-3");

        propertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_IP)
                        .putAllValue(nodeIpMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_ID)
                        .putAllValue(nodeIdMap).build());
    }

    @Test
    public void testMetricsConfig() throws IOException {
        var actual = telegrafConfigUtil.getMetricsConfigYaml();

        ClassLoader classLoader = getClass().getClassLoader();
        Yaml yaml = new Yaml();
        Map<String, Object> metricsConfig;
        StringWriter writer = new StringWriter();
        metricsConfig = yaml.load(classLoader.getResourceAsStream("MetricsConfig.yaml"));
        yaml.dump(metricsConfig, writer);
        var expected = writer.toString();

        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigHappyPathCommitter() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest"))
                .build();

        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_EXECUTION_ENGINE,
                ServiceType.CONCORD,
                ServiceType.GENERIC);

        Map<Integer, String> actual = telegrafConfigUtil.getTelegrafConfig(propertyList, properties, servicesList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigCommiter.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$REPLICA", nodeIpMap.get(key));
            Assertions.assertThat(value.equalsIgnoreCase(expect)).isTrue();
        });
    }

    @Test
    public void testTelegrafConfigHappyPathParticipant() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest"))
                .build();

        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);

        Map<Integer, String> actual = telegrafConfigUtil.getTelegrafConfig(propertyList, properties, servicesList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigParticipant.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        String indexDbInput = "address = \"postgres://indexdb@daml_index_db/$DBNAME\"";
        actual.forEach((key, value) -> {
            String nodeDbInput = indexDbInput.replace("$DBNAME", "pnode_" + key);
            var expect = expected
                    .replace("$REPLICA", nodeIpMap.get(key))
                    .replace("#$DBINPUT", nodeDbInput);
            Assertions.assertThat(value.equalsIgnoreCase(expect)).isTrue();
        });
    }

    @Test
    public void testPaths() {
        Assertions.assertThat(TelegrafConfigUtil.metricsConfigPath.equals(
                "/concord/config-public/metrics_config.yaml")).isTrue();
        Assertions.assertThat(TelegrafConfigUtil.configPath.equals("/telegraf/telegraf.conf")).isTrue();
    }
}
