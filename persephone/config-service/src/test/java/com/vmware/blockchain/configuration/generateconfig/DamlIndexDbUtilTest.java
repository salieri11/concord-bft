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

import com.vmware.blockchain.deployment.v1.NodeProperty;

/**
 * Test for DamlIndexDbUtil.
 */
public class DamlIndexDbUtilTest {

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

        String actual = new DamlIndexDbUtil().generateConfig(nodePropertyList);

        ClassLoader classLoader = getClass().getClassLoader();
        File file = new File(classLoader.getResource("SampleDamlIndexDbConfig.txt").getFile());
        var expected = new String(Files.readAllBytes(file.toPath()));

        Assertions.assertThat(actual.equals(expected)).isTrue();

    }

    @Test
    public void testExceptionWhenNoPropertyPresent() throws IOException {
        assertThrows(NoSuchElementException.class, () -> {
            List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder().build());
            new DamlIndexDbUtil().generateConfig(nodePropertyList);
        });

        assertThrows(NoSuchElementException.class, () -> {
            List<NodeProperty> nodePropertyList = List.of(NodeProperty.newBuilder()
                    .setName(NodeProperty.Name.BLOCKCHAIN_ID)
                    .putAllValue(new HashMap<>())
                    .build());
            new DamlIndexDbUtil().generateConfig(nodePropertyList);
        });
    }

    @Test
    public void testPath() {
        Assertions.assertThat(DamlIndexDbUtil.envVarPath.equals("/daml-index-db/environment-vars")).isTrue();
    }
}
