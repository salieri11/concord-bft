/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.util.Assert;

import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReadReplica;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;

/**
 * ConfigYaml Unit test configuration.
 */
public class ConcordConfigUtilTest {

    private static String filePath = "/tmp/concordConfigUtilTest";
    private static ConfigurationSessionIdentifier sessionId = ConfigurationSessionIdentifier.newBuilder().build();

    @Test
    void testConfigUtilPositive() throws IOException {
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("10.0.0.1", "10.0.0.1"),
                                         new BlockchainReplica("10.0.0.2", "10.0.0.2"),
                                         new BlockchainReplica("10.0.0.3", "10.0.0.3"),
                                         new BlockchainReplica("10.0.0.4", "10.0.0.4"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().replicas(replicas).build();
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        Assertions.assertThat(
                util.generateInputConfigYaml(nodeList, filePath, ConcordModelSpecification.BlockchainType.DAML,
                                             0, bcFeatures)
        ).isTrue();
    }

    @Test
    void testConfigUtilNegative() {
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        BlockchainNodeList nodeList = BlockchainNodeList.builder().build();
        Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> util.generateInputConfigYaml(nodeList, filePath, null, 0,
                                                               bcFeatures))
                .withMessage("Replicas are missing.");
    }

    @Test
    void testConfigUtilInvalidNode() {
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("10.0.0.1", "10.0.0.1"),
                                         new BlockchainReplica("10.0.0.2", "10.0.0.2"),
                                         new BlockchainReplica("10.0.0.3", "10.0.0.3"),
                                         new BlockchainReplica("10.0.0.4", "10.0.0.4"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().replicas(replicas).build();

        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> util.generateInputConfigYaml(nodeList, filePath, null, 0,
                                                               bcFeatures))
                .withMessage("Replicas or other required parameters missing.");
    }

    @Test
    void testConfigUtilInvalidConfig() {

        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("10.0.0.1", "10.0.0.1"),
                                         new BlockchainReplica("10.0.0.2", "10.0.0.2"),
                                         new BlockchainReplica("10.0.0.3", "10.0.0.3"),
                                         new BlockchainReplica("10.0.0.4", "10.0.0.4"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().replicas(replicas).build();
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);

        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        Assertions.assertThatExceptionOfType(ConfigServiceException.class)
                .isThrownBy(() -> util.generateInputConfigYaml(nodeList, filePath, null, 0,
                                                               bcFeatures))
                .withMessage("Replicas or other required parameters missing.");
    }

    @Test
    void testConfigUtilDefaultSamplePositive() throws IOException {
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("concord1", "concord1"),
                                                   new BlockchainReplica("concord2", "concord2"),
                                                   new BlockchainReplica("concord3", "concord3"),
                                                   new BlockchainReplica("concord4", "concord4"));

        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("concord5", "concord5"),
                                                           new BlockchainReadReplica("concord6", "concord6"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().replicas(replicas).readReplicas(readReplicas).build();
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        Assertions.assertThat(
                util.generateInputConfigYaml(nodeList, filePath, ConcordModelSpecification.BlockchainType.DAML,
                                             0, bcFeatures)
        ).isTrue();
    }

    @Test
    void testConfigUtilSevenPositive() throws IOException {
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("10.0.0.1", "10.0.0.1"),
                                         new BlockchainReplica("10.0.0.2", "10.0.0.2"),
                                         new BlockchainReplica("10.0.0.3", "10.0.0.3"),
                                         new BlockchainReplica("10.0.0.4", "10.0.0.4"),
                                         new BlockchainReplica("10.0.0.5", "10.0.0.5"),
                                         new BlockchainReplica("10.0.0.6", "10.0.0.6"),
                                         new BlockchainReplica("10.0.0.7", "10.0.0.7"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().replicas(replicas).build();
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        Assertions.assertThat(
                util.generateInputConfigYaml(nodeList, filePath, ConcordModelSpecification.BlockchainType.DAML,
                                             0, bcFeatures)
        ).isTrue();
    }

    @Test
    void testConfigUtilPreexecutionEnabled() throws IOException {
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("10.0.0.1", "10.0.0.1"),
                                         new BlockchainReplica("10.0.0.2", "10.0.0.2"),
                                         new BlockchainReplica("10.0.0.3", "10.0.0.3"),
                                         new BlockchainReplica("10.0.0.4", "10.0.0.4"),
                                         new BlockchainReplica("10.0.0.5", "10.0.0.5"),
                                         new BlockchainReplica("10.0.0.6", "10.0.0.6"),
                                         new BlockchainReplica("10.0.0.7", "10.0.0.7"));
        BlockchainNodeList nodeList = BlockchainNodeList.builder().replicas(replicas).build();
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(true).build();
        Assertions.assertThat(
                util.generateInputConfigYaml(nodeList, filePath, ConcordModelSpecification.BlockchainType.DAML,
                                             0, bcFeatures)
        ).isTrue();
    }

    @Test
    void testConfigUtilSamplePositiveWithReadReplica() throws IOException {
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("concord1", "concord1"),
                                         new BlockchainReplica("concord2", "concord2"),
                                         new BlockchainReplica("concord3", "concord3"),
                                         new BlockchainReplica("concord4", "concord4"));

        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("concord5", "concord5"),
                                                 new BlockchainReadReplica("concord6", "concord6"));

        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().readReplicas(readReplicas).replicas(replicas).build();

        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        Assertions.assertThat(
                util.generateInputConfigYaml(nodeList, filePath,
                                             ConcordModelSpecification.BlockchainType.DAML, 0, bcFeatures)
        ).isTrue();
    }

    @Test
    void testGetPrincipalsForReplicasNewFormat() throws IOException, ConfigServiceException {
        String principalsContentNew = "{"
                                      + "\"replicas\": {"
                                      + "\"1\": [6, 10, 14, 18, 0],"
                                      + "\"2\": [7, 11, 15, 19, 1],"
                                      + "\"3\": [8, 12, 16, 20, 2],"
                                      + "\"4\": [9, 13, 17, 21, 3]"
                                      + "},"
                                      + "\"ro-replicas\": {"
                                      + "\"5\": [],"
                                      + "\"6\": []"
                                      + "}"
                                      + "}";
        var principalsMap = getPrincipals(principalsContentNew);
        Assertions.assertThat(principalsMap).isNotNull().hasSize(6);
    }

    @Test
    void testGetPrincipalsForReplicasOldFormat() throws IOException, ConfigServiceException {
        String principalsContentOld = "{\"1\":[6,10,14,18,0],\"2\":[7,11,15,19,1],\"3\":[8,12,16,20,2],"
                                      + "\"4\":[9,13,17,21,3]}";
        var principalsMap = getPrincipals(principalsContentOld);
        Assertions.assertThat(principalsMap).isNotNull().hasSize(4);
    }

    @Test
    void testGetPrincipalsForClientsNewFormat() throws IOException, ConfigServiceException {
        String principalsContentNew = "{\"clients\": {\"5\":[21,22,23,24,25,26,27,28,29,30,31,32,33,34,35],"
                                      + "\"6\":[36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]}}";
        var principalsMap = getPrincipals(principalsContentNew);
        Assertions.assertThat(principalsMap).isNotNull().hasSize(2);
    }

    @Test
    void testGetPrincipalsForClientsOldFormat() throws IOException, ConfigServiceException {
        String principalsContentOld = "{\"5\":[21,22,23,24,25,26,27,28,29,30,31,32,33,34,35],"
                                      + "\"6\":[36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]}";
        var principalsMap = getPrincipals(principalsContentOld);
        Assertions.assertThat(principalsMap).isNotNull().hasSize(2);
    }

    private Map<Integer, List<Integer>> getPrincipals(String principalsContent) throws IOException,
                                                                                       ConfigServiceException {
        FileWriter fileWriter = null;
        Map<Integer, List<Integer>> principalsMap;
        try {
            File principalsFile = new File("principals.json");
            fileWriter = new FileWriter(principalsFile);
            fileWriter.write(principalsContent);
            fileWriter.flush();
            return ConfigUtilHelpers.getPrincipals(principalsFile.getAbsolutePath(), ConfigurationSessionIdentifier
                    .newBuilder().setId("sessionId").build());
        } catch (IOException | ConfigServiceException e) {
            throw e;
        } finally {
            if (fileWriter != null) {
                try {
                    fileWriter.close();
                } catch (IOException ioe) {
                    throw ioe;
                }
            }
        }
    }

    @Test
    void testConfigUtilObjStoreDisabled() {
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder().isPreExecutionDeployment(false).build();
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("concord1", "concord1"),
                                                   new BlockchainReplica("concord2", "concord2"),
                                                   new BlockchainReplica("concord3", "concord3"),
                                                   new BlockchainReplica("concord4", "concord4"));

        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("concord5", "concord5"),
                                                           new BlockchainReadReplica("concord6", "concord6"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().replicas(replicas).readReplicas(readReplicas).build();
        util.generateInputConfigYaml(nodeList, filePath, ConcordModelSpecification.BlockchainType.DAML, 0,
                                     bcFeatures);
        Assert.hasText(filePath, "Configuration template is missing.");
        try {
            String templateStr = Files.readString(Paths.get(filePath));
            Assert.doesNotContain("ro_", templateStr, "Template still has ro node reference.");
        } catch (IOException ioe) {
            // Exception
        }
    }

    @Test
    void testConfigUtilObjStoreEnabled() {
        ConcordConfigUtil util = new ConcordConfigUtil("ConcordConfigTemplate.yaml", sessionId);
        BlockchainFeatures bcFeatures =
                BlockchainFeatures.builder().isPreExecutionDeployment(false).isObjectStoreEnabled(true).build();
        List<BlockchainReplica> replicas = List.of(new BlockchainReplica("concord1", "concord1"),
                                                   new BlockchainReplica("concord2", "concord2"),
                                                   new BlockchainReplica("concord3", "concord3"),
                                                   new BlockchainReplica("concord4", "concord4"));

        List<BlockchainReadReplica> readReplicas = List.of(new BlockchainReadReplica("concord5", "concord5"),
                                                           new BlockchainReadReplica("concord6", "concord6"));
        BlockchainNodeList nodeList =
                BlockchainNodeList.builder().replicas(replicas).readReplicas(readReplicas).build();
        util.generateInputConfigYaml(nodeList, filePath, ConcordModelSpecification.BlockchainType.DAML, 0,
                                     bcFeatures);
        Assert.hasText(filePath, "Configuration template is missing.");
        try {
            String templateStr = Files.readString(Paths.get(filePath));
            Assertions.assertThat(templateStr.contains("ro_"));
        } catch (IOException ioe) {
            // Exception
        }
    }


    @AfterEach
    void cleanup() throws IOException {
        //Files.deleteIfExists(Paths.get(filePath));
    }
}

