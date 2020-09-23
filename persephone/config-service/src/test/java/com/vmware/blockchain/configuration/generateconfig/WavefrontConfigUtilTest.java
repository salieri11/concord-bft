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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Test for WavefrontConfigUtil.
 */
public class WavefrontConfigUtilTest {

    private WavefrontConfigUtil wavefrontConfigUtil;

    Map<Integer, String> nodeIpMap = new HashMap<>();
    Map<Integer, String> wfUrlMap = new HashMap<>();
    Map<Integer, String> wfTokenMap = new HashMap<>();

    Properties properties;

    /** initialize. **/
    @BeforeEach
    public void createObject() {
        this.wavefrontConfigUtil = new WavefrontConfigUtil("wavefrontConfigTemplate.conf");

        nodeIpMap.put(0, "10.0.0.1");
        nodeIpMap.put(1, "10.0.0.2");
        nodeIpMap.put(2, "10.0.0.3");
        nodeIpMap.put(3, "10.0.0.4");

        wfUrlMap.put(0, "my.test.url");
        wfUrlMap.put(1, "my.test.url");
        wfUrlMap.put(2, "my.test.url");
        wfUrlMap.put(3, "my.test.url");

        wfTokenMap.put(0, "mytesttoken");
        wfTokenMap.put(1, "mytesttoken");
        wfTokenMap.put(2, "mytesttoken");
        wfTokenMap.put(3, "mytesttoken");

        properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest"))
                .build();
    }

    @AfterEach
    public void destroyObject() {
        nodeIpMap = new HashMap<>();
    }

    @Test
    public void testWavefrontConfigHappyPath() throws IOException {
        Map<Integer, String> wavefrontProxyUrl = Map.of(
                0, "my.proxy.host",
                1, "my.proxy.host",
                2, "my.proxy.host",
                3, "my.proxy.host");

        Map<Integer, String> wavefrontProxyPort = Map.of(
                0, "0000",
                1, "0000",
                2, "0000",
                3, "0000");

        Map<Integer, String> wavefrontProxyUser = Map.of(
                0, "testuser",
                1, "testuser",
                2, "testuser",
                3, "testuser");

        Map<Integer, String> wavefrontProxyPwd = Map.of(
                0, "testpassword",
                1, "testpassword",
                2, "testpassword",
                3, "testpassword");

        List<NodeProperty> propertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_IP)
                        .putAllValue(nodeIpMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_URL)
                        .putAllValue(wfUrlMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_TOKEN)
                        .putAllValue(wfTokenMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_HOST)
                        .putAllValue(wavefrontProxyUrl).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_PORT)
                        .putAllValue(wavefrontProxyPort).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_USER)
                        .putAllValue(wavefrontProxyUser).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD)
                        .putAllValue(wavefrontProxyPwd).build());

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, propertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfig.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", nodeIpMap.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigHappyPathDifferentProxy() throws IOException {
        Map<Integer, String> wavefrontProxyUrl = Map.of(
                0, "my.proxy0.host",
                1, "my.proxy1.host",
                2, "my.proxy2.host",
                3, "my.proxy3.host");

        Map<Integer, String> wavefrontProxyPort = Map.of(
                0, "0000",
                1, "0001",
                2, "0002",
                3, "0003");

        List<NodeProperty> propertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_IP)
                        .putAllValue(nodeIpMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_URL)
                        .putAllValue(wfUrlMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_TOKEN)
                        .putAllValue(wfTokenMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_HOST)
                        .putAllValue(wavefrontProxyUrl).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_PORT)
                        .putAllValue(wavefrontProxyPort).build());

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, propertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxyUser.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected
                    .replace("$HOSTNAME", nodeIpMap.get(key))
                    .replace("proxyHost=my.proxy.host", "proxyHost=my.proxy"
                            + key + ".host")
                    .replace("proxyPort=0000", "proxyPort=000" + key);
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigNoProxyUser() throws IOException {

        Map<Integer, String> wavefrontProxyUrl = Map.of(
                0, "my.proxy.host",
                1, "my.proxy.host",
                2, "my.proxy.host",
                3, "my.proxy.host");

        Map<Integer, String> wavefrontProxyPort = Map.of(
                0, "0000",
                1, "0000",
                2, "0000",
                3, "0000");

        List<NodeProperty> propertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_IP)
                        .putAllValue(nodeIpMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_URL)
                        .putAllValue(wfUrlMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_TOKEN)
                        .putAllValue(wfTokenMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_HOST)
                        .putAllValue(wavefrontProxyUrl).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_PORT)
                        .putAllValue(wavefrontProxyPort).build());

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, propertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxyUser.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", nodeIpMap.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigNoProxy() throws IOException {

        List<NodeProperty> propertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_IP)
                        .putAllValue(nodeIpMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_URL)
                        .putAllValue(wfUrlMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_TOKEN)
                        .putAllValue(wfTokenMap).build());

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, propertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxy.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", nodeIpMap.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigUserHasNoProxyThrowsException() throws IOException {

        Map<Integer, String> wavefrontProxyUser = Map.of(
                0, "testuser",
                1, "testuser",
                2, "testuser",
                3, "testuser");

        Map<Integer, String> wavefrontProxyPwd = Map.of(
                0, "testpassword",
                1, "testpassword",
                2, "testpassword",
                3, "testpassword");

        List<NodeProperty> propertyList = List.of(
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.NODE_IP)
                        .putAllValue(nodeIpMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_URL)
                        .putAllValue(wfUrlMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_TOKEN)
                        .putAllValue(wfTokenMap).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_USER)
                        .putAllValue(wavefrontProxyUser).build(),
                NodeProperty.newBuilder()
                        .setName(NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD)
                        .putAllValue(wavefrontProxyPwd).build());

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, propertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxy.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", nodeIpMap.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testPath() {
        Assertions.assertThat(Constants.WAVEFRONT_CONFIG_PATH.equals("/wavefront-proxy/wavefront.conf")).isTrue();
    }
}
