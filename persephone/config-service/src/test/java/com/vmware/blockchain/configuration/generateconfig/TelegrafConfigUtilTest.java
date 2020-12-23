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
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
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
    public void testTelegrafConfigHappyPathReplica() throws IOException {
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
        File file = new File(classLoader.getResource("SampleTelegrafConfigReplica.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigHappyPathParticipant() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.2466"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
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

    @Test
    public void testTelegrafConfigPrometeusInputWithUsernameAndCustomPassword() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.2466",
                                     NodeProperty.Name.DAML_DB_PASSWORD.name(), "customPass"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        Properties properties1100 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.1.0.0",
                                     NodeProperty.Name.DAML_DB_PASSWORD.name(), "customPass"))
                .build();
        NodesInfo.Entry nodeInfo1100 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties1100)
                .addAllServices(servicesList)
                .build();

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        String actual1100 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo1100, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigCustomPass.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual1100.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigPrometeusInputCustomPassword5ComponentTag() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.0.2466",
                                     NodeProperty.Name.DAML_DB_PASSWORD.name(), "customPass"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        Properties properties11000 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.1.0.0.0",
                                     NodeProperty.Name.DAML_DB_PASSWORD.name(), "customPass"))
                .build();
        NodesInfo.Entry nodeInfo11000 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties11000)
                .addAllServices(servicesList)
                .build();

        Properties properties11111 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.1.1.1.1",
                                     NodeProperty.Name.DAML_DB_PASSWORD.name(), "customPass"))
                .build();
        NodesInfo.Entry nodeInfo11111 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties11111)
                .addAllServices(servicesList)
                .build();

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        String actual11000 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo11000, nodeType);

        String actual11111 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo11111, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigCustomPass.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual11000.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual11111.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigPrometeusInputWithUsernameAndNoPassword() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.2465"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        Properties properties0900 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.9.0.0"))
                .build();
        NodesInfo.Entry nodeInfo0900 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties0900)
                .addAllServices(servicesList)
                .build();

        Properties properties10067 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.0.0.67"))
                .build();
        NodesInfo.Entry nodeInfo10067 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties10067)
                .addAllServices(servicesList)
                .build();

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        String actual0900 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo0900, nodeType);

        String actual10067 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                  "unitTest", nodeInfo10067, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigNoPass.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual0900.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual10067.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigPrometeusInputNoPassword5ComponentTag() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.0.2465"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        Properties properties09000 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.9.0.0.0"))
                .build();
        NodesInfo.Entry nodeInfo09000 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties09000)
                .addAllServices(servicesList)
                .build();

        Properties properties09111 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.9.1.1.1"))
                .build();
        NodesInfo.Entry nodeInfo09111 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties09111)
                .addAllServices(servicesList)
                .build();

        Properties properties100067 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.0.0.0.67"))
                .build();
        NodesInfo.Entry nodeInfo100067 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties100067)
                .addAllServices(servicesList)
                .build();

        Properties properties10000 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.0.0.0.0"))
                .build();
        NodesInfo.Entry nodeInfo10000 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties10000)
                .addAllServices(servicesList)
                .build();

        Properties properties10111 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.0.1.1.1"))
                .build();
        NodesInfo.Entry nodeInfo10111 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties10111)
                .addAllServices(servicesList)
                .build();

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        String actual0900 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo09000, nodeType);

        String actual09111 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo09111, nodeType);

        String actual100067 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                  "unitTest", nodeInfo100067, nodeType);

        String actual10000 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                  "unitTest", nodeInfo10000, nodeType);

        String actual10111 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo10111, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigNoPass.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual0900.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual09111.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual100067.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual10000.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual10111.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigPrometeusInputWithUsernameAndDefaultPassword() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.2466"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        Properties properties1100 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.1.0.0"))
                .build();
        NodesInfo.Entry nodeInfo1100 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties1100)
                .addAllServices(servicesList)
                .build();

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        String actual1100 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo1100, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigParticipant.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual1100.equalsIgnoreCase(expected)).isTrue();
    }

    @Test
    public void testTelegrafConfigPrometeusInputWithDefaultPassword5ComponentTag() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "0.0.0.0.2466"))
                .build();
        List<ServiceType> servicesList = List.of(
                ServiceType.DAML_INDEX_DB,
                ServiceType.DAML_LEDGER_API,
                ServiceType.GENERIC);
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties)
                .addAllServices(servicesList)
                .build();
        String nodeType = "CLIENT";

        Properties properties11000 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.1.0.0.0"))
                .build();
        NodesInfo.Entry nodeInfo11000 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties11000)
                .addAllServices(servicesList)
                .build();

        Properties properties11111 = Properties.newBuilder()
                .putAllValues(Map.of(DeploymentAttributes.IMAGE_TAG.name(), "1.1.1.1.1"))
                .build();
        NodesInfo.Entry nodeInfo11111 = NodesInfo.Entry.newBuilder()
                .setNodeIp("10.0.0.1")
                .setId("node-0")
                .setProperties(properties11111)
                .addAllServices(servicesList)
                .build();

        String actual = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                             "unitTest", nodeInfo, nodeType);

        String actual11000 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo11000, nodeType);

        String actual11111 = telegrafConfigUtil.getTelegrafConfig("testConsortium",
                                                                 "unitTest", nodeInfo11111, nodeType);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleTelegrafConfigParticipant.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));
        Assertions.assertThat(actual.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual11000.equalsIgnoreCase(expected)).isTrue();
        Assertions.assertThat(actual11111.equalsIgnoreCase(expected)).isTrue();
    }
}
