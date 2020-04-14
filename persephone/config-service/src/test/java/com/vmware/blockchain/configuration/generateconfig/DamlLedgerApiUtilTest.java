/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Test for DamlLedgerApiUtil.
 */
public class DamlLedgerApiUtilTest {

    @Test
    public void testHappyPath() throws IOException {

        Map<Integer, String> nodeMap = new HashMap<>() {
            {
                put(0, "TEST-NODE");
            }
        };
        List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder()
                                                              .setName(NodeProperty.Name.NODE_ID)
                                                              .putAllValue(nodeMap).build());

        String actual = new DamlLedgerApiUtil().generateConfig(Properties.newBuilder().build(), nodePropertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfig.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected));

        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(NodeProperty.Name.COMMITTERS.toString(), "10.0.0.1:50051")).build();
        actual = new DamlLedgerApiUtil().generateConfig(properties, nodePropertyList);

        file = new File(classLoader.getResource("SampleDamlLedgerApiConfig.txt").getFile());
        expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();
    }

    @Test
    public void testHappyPathWithJwt() throws IOException {

        Map<Integer, String> nodeMap = new HashMap<>() {
            {
                put(0, "TEST-NODE");
            }
        };

        String authJwtToken =
                "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJPbmxpbmUgSldUIEJ1aWxkZXIiLCJpYXQiOjE1ODU4NTI0M"
                + "TUsImV4cCI6MTYxNzM4ODQxNSwiYXVkIjoid3d3LmV4YW1wbGUuY29tIiwic3ViIjoianJvY2tldEBleGFtcGxlLmNvb"
                + "SIsIkdpdmVuTmFtZSI6IkpvaG5ueSIsIlN1cm5hbWUiOiJSb2NrZXQiLCJFbWFpbCI6Impyb2NrZXRAZXhhbXBsZS5jb"
                + "20iLCJSb2xlIjpbIk1hbmFnZXIiLCJQcm9qZWN0IEFkbWluaXN0cmF0b3IiXX0.lOZceSXpHdKYzZsGS8koNdb_I_1o"
                + "lt6bdHcgOeRWuhY";
        List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder()
                                                              .setName(NodeProperty.Name.NODE_ID)
                                                              .putAllValue(nodeMap).build());

        Properties properties = Properties.newBuilder()
                .putAllValues(ImmutableMap.of(NodeProperty.Name.COMMITTERS.toString(), "10.0.0.1:50051",
                                              NodeProperty.Name.CLIENT_AUTH_JWT.toString(), authJwtToken)).build();
        String actual = new DamlLedgerApiUtil().generateConfig(properties, nodePropertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlLedgerApiConfigWithAuthJwt.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();

    }

    @Test
    public void testExceptionWhenNoPropertyPresent() throws IOException {
        assertThrows(NoSuchElementException.class, () -> {
            List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder().build());
            new DamlLedgerApiUtil().generateConfig(Properties.newBuilder().build(), nodePropertyList);
        });

        assertThrows(NoSuchElementException.class, () -> {
            List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder()
                                                                  .setName(NodeProperty.Name.BLOCKCHAIN_ID)
                                                                  .putAllValue(new HashMap<>())
                                                                  .build());
            new DamlLedgerApiUtil().generateConfig(Properties.newBuilder().build(), nodePropertyList);
        });
    }

    @Test
    public void testPath() {
        Assertions.assertThat(DamlLedgerApiUtil.envVarPath.equals("/daml-ledger-api/environment-vars")).isTrue();
    }
}