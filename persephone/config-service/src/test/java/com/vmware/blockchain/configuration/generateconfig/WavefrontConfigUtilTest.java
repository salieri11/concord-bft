/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Test for WavefrontConfigUtil.
 */
public class WavefrontConfigUtilTest {

    private WavefrontConfigUtil wavefrontConfigUtil;

    @BeforeEach
    public void createObject() {
        this.wavefrontConfigUtil = new WavefrontConfigUtil("wavefrontConfigTemplate.conf");
    }

    @Test
    public void testWavefrontConfigHappyPath() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest",
                        NodeProperty.Name.WAVEFRONT_URL.toString(), "my.test.url",
                        NodeProperty.Name.WAVEFRONT_TOKEN.toString(), "mytesttoken",
                        NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString(), "my.proxy.host",
                        NodeProperty.Name.WAVEFRONT_PROXY_PORT.toString(), "0000",
                        NodeProperty.Name.WAVEFRONT_PROXY_USER.toString(), "testuser",
                        NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD.toString(), "testpassword"))
                .build();

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, hostIps);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfig.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", hostIps.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigNoProxyUser() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest",
                        NodeProperty.Name.WAVEFRONT_URL.toString(), "my.test.url",
                        NodeProperty.Name.WAVEFRONT_TOKEN.toString(), "mytesttoken",
                        NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString(), "my.proxy.host",
                        NodeProperty.Name.WAVEFRONT_PROXY_PORT.toString(), "0000"))
                .build();

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, hostIps);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxyUser.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", hostIps.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigNoProxy() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest",
                        NodeProperty.Name.WAVEFRONT_URL.toString(), "my.test.url",
                        NodeProperty.Name.WAVEFRONT_TOKEN.toString(), "mytesttoken"))
                .build();

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, hostIps);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxy.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", hostIps.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontConfigNoProxyWithUserHasNoProxy() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest",
                        NodeProperty.Name.WAVEFRONT_URL.toString(), "my.test.url",
                        NodeProperty.Name.WAVEFRONT_TOKEN.toString(), "mytesttoken",
                        NodeProperty.Name.WAVEFRONT_PROXY_USER.toString(), "testuser",
                        NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD.toString(), "testpassword"))
                .build();

        Map<Integer, String> actual = wavefrontConfigUtil.getWavefrontConfig(properties, hostIps);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleWavefrontConfigNoProxy.conf").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        actual.forEach((key, value) -> {
            var expect = expected.replace("$HOSTNAME", hostIps.get(key));
            Assertions.assertThat(value.equals(expect)).isTrue();
        });
    }

    @Test
    public void testWavefrontNoPortThrowsException() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest",
                        NodeProperty.Name.WAVEFRONT_URL.toString(), "my.test.url",
                        NodeProperty.Name.WAVEFRONT_TOKEN.toString(), "mytesttoken",
                        NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString(), "my.proxy.host",
                        NodeProperty.Name.WAVEFRONT_PROXY_USER.toString(), "testuser",
                        NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD.toString(), "testpassword"))
                .build();

        assertThrows(IllegalArgumentException.class, () -> {
            wavefrontConfigUtil.getWavefrontConfig(properties, hostIps);
        });
    }

    @Test
    public void testWavefrontNoPasswordThrowsException() throws IOException {
        List<String> hostIps = List.of("10.0.0.1", "10.0.0.2", "10.0.0.3", "10.0.0.4");
        Properties properties = Properties.newBuilder()
                .putAllValues(Map.of(
                        NodeProperty.Name.BLOCKCHAIN_ID.toString(), "unitTest",
                        NodeProperty.Name.WAVEFRONT_URL.toString(), "my.test.url",
                        NodeProperty.Name.WAVEFRONT_TOKEN.toString(), "mytesttoken",
                        NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString(), "my.proxy.host",
                        NodeProperty.Name.WAVEFRONT_PROXY_PORT.toString(), "0000",
                        NodeProperty.Name.WAVEFRONT_PROXY_USER.toString(), "testuser"))
                .build();

        assertThrows(IllegalArgumentException.class, () -> {
            wavefrontConfigUtil.getWavefrontConfig(properties, hostIps);
        });
    }

    @Test
    public void testPath() {
        Assertions.assertThat(WavefrontConfigUtil.configPath.equals("/wavefront-proxy/wavefront.conf")).isTrue();
    }
}
