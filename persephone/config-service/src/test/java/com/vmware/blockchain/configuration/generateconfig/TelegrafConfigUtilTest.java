/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
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
        this.telegrafConfigUtil = new TelegrafConfigUtil("TelegrafConfigTemplate.conf");

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
    public void testTelegrafConfigHappyPathCommitter() throws IOException {
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_EXECUTION_ENGINE,
                ServiceType.CONCORD,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1").addAllServices(servicesList).build();
        String nodeType = "REPLICA";
        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium", "unitTest",
                                                             nodeInfo, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigCommiter.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigHappyPathParticipant() throws IOException {
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                "unitTest", nodeInfo, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigParticipant.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigElasticSearch() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.ELASTICSEARCH_URL.toString(), "myurl.com",
                        NodeProperty.Name.ELASTICSEARCH_USER.toString(), "myuser",
                        NodeProperty.Name.ELASTICSEARCH_PWD.toString(), "mypwd"))
                .build();

        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_EXECUTION_ENGINE,
                ServiceType.CONCORD,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "REPLICA";

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                "unitTest", nodeInfo, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigES.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigElasticSearchNoUser() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.ELASTICSEARCH_URL.toString(), "myurl.com"))
                .build();

        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_EXECUTION_ENGINE,
                ServiceType.CONCORD,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "REPLICA";

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                "unitTest", nodeInfo, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigESNoUser.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testPaths() {
        Assertions.assertThat(Constants.TELEGRAF_CONFIG_PATH.equals("/telegraf/telegraf.conf")).isTrue();
    }

    @Test
    public void testEnablePrometheusOutputPlugin() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.TELEGRAF_USERNAME.toString(), "telegraf",
                        NodeProperty.Name.TELEGRAF_PASSWORD.toString(), "ahoy hoy!!!",
                        NodeProperty.Name.TELEGRAF_TLS_KEY.toString(), "BEGIN CERTIFICATE blah blah END CERTFICATE",
                        NodeProperty.Name.TELEGRAF_TLS_CERT.toString(), "BEGIN CERTIFICATE blah blah END CERTFICATE")
                )
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_EXECUTION_ENGINE,
                ServiceType.CONCORD,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1").setProperties(properties).addAllServices(servicesList).build();
        String nodeType = "REPLICA";

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafEnablePullPrometheusClient.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
    }
}
