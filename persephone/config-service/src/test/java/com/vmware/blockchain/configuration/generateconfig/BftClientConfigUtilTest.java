/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.yaml.snakeyaml.Yaml;

import com.vmware.blockchain.configuration.util.BlockchainClient;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;

/**
 * ConfigYaml Unit test configuration.
 */
public class BftClientConfigUtilTest {

    private static String filePath = "/tmp/bftConfigUtilTest";
    private static UUID sessionId = UUID.randomUUID();

    @Test
    void testConfigUtilPositive() throws IOException {
        List<BlockchainReplica> replicas =
                List.of(new BlockchainReplica("ID1", "10.0.0.1"),
                        new BlockchainReplica("ID2", "10.0.0.2"),
                        new BlockchainReplica("ID3", "10.0.0.3"),
                        new BlockchainReplica("ID4", "10.0.0.4"));
        List<BlockchainClient> clients =
                List.of(new BlockchainClient("ID5", "10.0.0.5"),
                        new BlockchainClient("ID6", "10.0.0.6"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().replicas(replicas).clients(clients).build();

        BftClientConfigUtil util = new BftClientConfigUtil("BFTClientConfigTemplate.yaml",
                                                           sessionId);
        var actualDump = util.generateConfigYaml(nodeList, 15, filePath);

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
    void testConfigUtilNegative() throws IOException {
        BlockchainNodeList nodeList = BlockchainNodeList.builder().build();
        BftClientConfigUtil util = new BftClientConfigUtil("BFTClientConfigTemplate.yaml",
                                                           sessionId);

        Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> util.generateConfigYaml(nodeList, 15, filePath))
                .withMessage("No Replicas available to process.");
    }


    @Test
    void testPath() {
        Assertions.assertThat(Constants.DAML_BFT_CLIENT_CONFIG_PATH
                                      .equals("/daml-ledger-api/config-public/bftclient.config"))
                .isTrue();
    }
}
