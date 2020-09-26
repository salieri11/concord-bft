/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Test for DamlLedgerApiUtil.
 */
public class DamlLedgerApiUtilTest {

    @Test
    public void testHappyPath() throws IOException {

        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE").build();
        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfig.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected));

        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(NodeProperty.Name.COMMITTERS.name(), "10.0.0.1:50051")).build();
        nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE").setProperties(properties).build();
        actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        file = new File(classLoader.getResource("SampleDamlLedgerApiConfig.txt").getFile());
        expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual).isEqualTo(expected);
    }

    @Test
    public void testHappyPathWithJwt() throws IOException {
        String authJwtToken =
                "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJPbmxpbmUgSldUIEJ1aWxkZXIiLCJpYXQiOjE1ODU4NTI0M"
                + "TUsImV4cCI6MTYxNzM4ODQxNSwiYXVkIjoid3d3LmV4YW1wbGUuY29tIiwic3ViIjoianJvY2tldEBleGFtcGxlLmNvb"
                + "SIsIkdpdmVuTmFtZSI6IkpvaG5ueSIsIlN1cm5hbWUiOiJSb2NrZXQiLCJFbWFpbCI6Impyb2NrZXRAZXhhbXBsZS5jb"
                + "20iLCJSb2xlIjpbIk1hbmFnZXIiLCJQcm9qZWN0IEFkbWluaXN0cmF0b3IiXX0.lOZceSXpHdKYzZsGS8koNdb_I_1o"
                + "lt6bdHcgOeRWuhY";

        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(NodeProperty.Name.COMMITTERS.name(), "10.0.0.1:50051",
                                              NodeProperty.Name.CLIENT_AUTH_JWT.name(), authJwtToken)).build();
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE")
                .setProperties(properties).build();

        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithAuthJwt.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();

    }

    @Test
    public void testHappyPathWithJwtandClientGroup() throws IOException {
        String authJwtToken =
                "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJPbmxpbmUgSldUIEJ1aWxkZXIiLCJpYXQiOjE1ODU4NTI0M"
                        + "TUsImV4cCI6MTYxNzM4ODQxNSwiYXVkIjoid3d3LmV4YW1wbGUuY29tIiwic3ViIjoianJvY2tldEBleGFtcGxlLmNvb"
                        + "SIsIkdpdmVuTmFtZSI6IkpvaG5ueSIsIlN1cm5hbWUiOiJSb2NrZXQiLCJFbWFpbCI6Impyb2NrZXRAZXhhbXBsZS5jb"
                        + "20iLCJSb2xlIjpbIk1hbmFnZXIiLCJQcm9qZWN0IEFkbWluaXN0cmF0b3IiXX0.lOZceSXpHdKYzZsGS8koNdb_I_1o"
                        + "lt6bdHcgOeRWuhY";

        String clientGroupId = "2a7a9eac-cf5c-40a2-8c58-646b6850b72d";

        Properties properties = Properties.newBuilder()
                .putAllValues(
                        ImmutableMap.of(
                                NodeProperty.Name.COMMITTERS.name(), "10.0.0.1:50051",
                                NodeProperty.Name.CLIENT_AUTH_JWT.name(), authJwtToken,
                                NodeProperty.Name.CLIENT_GROUP_ID.name(), clientGroupId))
                .build();

        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE")
                .setProperties(properties).build();

        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithAuthJwtAndClientGroup.txt")
                .getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();

    }

    @Test
    public void testHappyPathWithTlsCredentials() throws IOException {
        String pem = "PEM_VALUE";
        String crt = "CRT_VALUE";
        String cacrt = "CACRT_VALUE";

        Properties properties = Properties.newBuilder()
                .putAllValues(
                        ImmutableMap.of(
                                NodeProperty.Name.COMMITTERS.name(), "10.0.0.1:50051",
                                NodeProperty.Name.TLS_PEM.name(), pem,
                                NodeProperty.Name.TLS_CRT.name(), crt,
                                NodeProperty.Name.TLS_CACRT.name(), cacrt))
                .build();

        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE")
                .setProperties(properties).build();

        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithTlsCredentials.txt")
                .getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();

    }

    @Test
    public void testHappyPathWithBftClient() throws IOException {
        String authJwtToken =
                "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJPbmxpbmUgSldUIEJ1aWxkZXIiLCJpYXQiOjE1ODU4NTI0M"
                        + "TUsImV4cCI6MTYxNzM4ODQxNSwiYXVkIjoid3d3LmV4YW1wbGUuY29tIiwic3ViIjoianJvY2tldEBleGFtcGxlLmNvb"
                        + "SIsIkdpdmVuTmFtZSI6IkpvaG5ueSIsIlN1cm5hbWUiOiJSb2NrZXQiLCJFbWFpbCI6Impyb2NrZXRAZXhhbXBsZS5jb"
                        + "20iLCJSb2xlIjpbIk1hbmFnZXIiLCJQcm9qZWN0IEFkbWluaXN0cmF0b3IiXX0.lOZceSXpHdKYzZsGS8koNdb_I_1o"
                        + "lt6bdHcgOeRWuhY";
        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(NodeProperty.Name.COMMITTERS.name(), "10.0.0.1:50051",
                        NodeProperty.Name.CLIENT_AUTH_JWT.name(), authJwtToken,
                        DeploymentAttributes.ENABLE_BFT_CLIENT.name(), "True"))
                .build();
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE")
                .setProperties(properties).build();

        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();

        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithBftClient.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();

    }

    @Test
    public void testHappyPathWithPreexecutionThreshold() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(DeploymentAttributes.PREEXECUTION_ENABLED.name(), "True",
                        DeploymentAttributes.PREEXECUTION_THRESHOLD.name(), "100ms")).build();
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE")
                .setProperties(properties).build();

        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithPreexecutionEnabled.txt")
                .getFile());
        String expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testHappyPathWithDefaultPreexecutionThreshold() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(DeploymentAttributes.PREEXECUTION_ENABLED.name(), "True")).build();
        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE")
                .setProperties(properties).build();

        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithDefaultPreexecutionThreshold.txt")
                .getFile());
        String expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testHappyPathWithPreexecutionDeploymentDisabled() throws IOException {
        Properties properties = Properties.newBuilder()
                .putAllValues(
                        ImmutableMap.of(NodeProperty.Name.COMMITTERS.name(), "10.0.0.1:50051",
                                DeploymentAttributes.PREEXECUTION_ENABLED.name(), "false"))
                .build();

        NodesInfo.Entry nodeInfo = NodesInfo.Entry.newBuilder().setId("TEST-NODE").setProperties(properties).build();
        String actual = new DamlLedgerApiUtil().generateConfig(nodeInfo);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfig.txt")
                .getFile());
        String expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual).isEqualTo(expected);
    }

    @Test
    public void testPath() {
        Assertions.assertThat(DamlLedgerApiUtil.envVarPath.equals("/daml-ledger-api/environment-vars")).isTrue();
    }
}
