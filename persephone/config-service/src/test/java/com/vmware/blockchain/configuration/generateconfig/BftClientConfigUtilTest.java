/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.yaml.snakeyaml.Yaml;

/**
 * ConfigYaml Unit test configuration.
 */
public class BftClientConfigUtilTest {

    private static String filePath = "/tmp/bftConfigUtilTest";

    @Test
    void testConfigUtilPositive() throws IOException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");

        List<String> participantIps = new ArrayList<String>();
        participantIps.add("10.0.0.5");
        participantIps.add("10.0.0.6");

        BftClientConfigUtil util = new BftClientConfigUtil("BFTClientConfigTemplate.yaml");

        var actualDump = util.generateConfigYaml(hostIps, participantIps, filePath);

        Assertions.assertThat(actualDump).isTrue();

        Yaml yaml = new Yaml();
        Map<String, Object> expected;
        Map<String, Object> actual;

        ClassLoader classLoader = getClass().getClassLoader();
        expected = yaml.load(classLoader.getResourceAsStream("SampleBftConfig.yaml"));

        actual = yaml.load(new FileInputStream(filePath));

        Assertions.assertThat(actual.size() == expected.size()).isTrue();
        Assertions.assertThat(expected.entrySet().stream()
                .allMatch(e -> e.getValue().equals(actual.get(e.getKey())))).isTrue();
    }

    @Test
    void testPath() {
        Assertions.assertThat(BftClientConfigUtil.configPath.equals("/daml-ledger-api/config-public/bftclient.config"))
                .isTrue();
    }
}
