/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

/**
 * ConfigYaml Unit test configuration.
 */
public class ConcordConfigUtilTest {

    private static String filePath = "/tmp/concordConfigUtilTest";

    @Test
    void testConfigUtilPositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml");
        Assertions.assertThat(util.generateInputConfigYaml(hostIps, filePath, null, 0)).isTrue();
    }

    @Test
    void testConfigUtilNegative() {
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml");
        Assertions.assertThat(util.generateInputConfigYaml(new ArrayList<>(), filePath, null, 0)).isFalse();
    }

    @Test
    void testConfigUtilInvalidNode() {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml");
        Assertions.assertThat(util.generateInputConfigYaml(hostIps, filePath, null, 0)).isFalse();
    }

    @Test
    void testConfigUtilInvalidConfig() {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml");
        Assertions.assertThat(util.generateInputConfigYaml(hostIps, 1, 2, filePath, null, 0)).isFalse();
    }

    @Test
    void testConfigUtilDefaultSamplePositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("concord1");
        hostIps.add("concord2");
        hostIps.add("concord3");
        hostIps.add("concord4");
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml");
        Assertions.assertThat(util.generateInputConfigYaml(hostIps, filePath, null, 0)).isTrue();
    }

    @Test
    void testConfigUtilSevenPositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");
        hostIps.add("10.0.0.5");
        hostIps.add("10.0.0.6");
        hostIps.add("10.0.0.7");
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml");
        Assertions.assertThat(util.generateInputConfigYaml(hostIps, filePath, null, 0)).isTrue();
    }

    @AfterEach
    void cleanup() throws IOException {
        Files.deleteIfExists(Paths.get(filePath));
    }
}

