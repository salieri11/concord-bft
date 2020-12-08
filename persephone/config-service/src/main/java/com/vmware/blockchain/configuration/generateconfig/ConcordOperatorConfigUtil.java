/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static com.vmware.blockchain.configuration.generateconfig.Constants.CONCORD_OPERATOR_KEY_FOLDER_PATH;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

/**
 * Utility class for generating concord operator configurations.
 */
public class ConcordOperatorConfigUtil {
    private static final Logger log = LoggerFactory.getLogger(ConcordOperatorConfigUtil.class);

    public static final String operatorConfigFileName = "operator.config";
    public static final String operatorConfigPath = "/daml-ledger-api/concord-operator/" + operatorConfigFileName;

    private String operatorConfigTemplatePath;
    private UUID sessionId;

    public ConcordOperatorConfigUtil(String operatorConfigTemplatePath, UUID sessionId) {
        this.operatorConfigTemplatePath = operatorConfigTemplatePath;
        this.sessionId = sessionId;
    }

    /**
     * Utility to generate concord config.
     */
    public String getConcordOperatorConfig(BlockchainNodeList nodeList, int clientProxyPerParticipant,
                                           BlockchainFeatures blockchainFeatures, int maxPrincipalValue) {
        if (!blockchainFeatures.isOperatorEnabled()) {
            return null;
        }

        try {
            // Prefix the directory name with 'replicas' so as to differentiate between clients and replicas.
            var outputPath = Files.createTempDirectory("operator-");

            var principalsMapFile = Paths.get(outputPath.toString(), "principals.json").toString();
            var inputYamlPath = Paths.get(outputPath.toString(), "dockerOperatorConfigInput.yaml").toString();

            generateConfigYaml(nodeList, clientProxyPerParticipant, inputYamlPath, maxPrincipalValue);

            List<CompletableFuture<Process>> cmd = new ArrayList<>();
            var configGenOutputPath = Paths.get(outputPath.toString(), "configGenOutput.txt").toString();
            var configGenErrorPath = Paths.get(outputPath.toString(), "configGenError.txt").toString();
            List<String> configCmd = ConfigUtilHelpers.concordGenConfigCmd.stream().collect(Collectors.toList());
            configCmd.addAll(List.of("--configuration-input", inputYamlPath, "--report-principal-locations",
                                     principalsMapFile, "--operator-conf", "true"));

            var configFuture = new ProcessBuilder(configCmd)
                    .directory(outputPath.toFile())
                    .redirectError(new File(configGenErrorPath))
                    .redirectOutput(new File(configGenOutputPath))
                    .start()
                    .onExit();
            List<String> operatorConfigs = new ArrayList<>();
            var work = configFuture.whenCompleteAsync((process, error) -> {
                if (error == null) {
                    try {
                        operatorConfigs.add(Files.readString(Paths.get(outputPath.toString(),
                                                               operatorConfigFileName)));
                    } catch (Throwable collectError) {
                        log.error("Cannot collect generated bft configuration. Session Id {}", sessionId, collectError);
                    } finally {
                        // Now that we have collected the files, we can delete the temporary directory.
                        ConfigUtilHelpers.deleteTemporaryFiles(outputPath, true);
                    }
                } else {
                    log.error("Cannot run bft config generation process. Session Id {}", sessionId, error);
                }
            });

            work.join();
            return operatorConfigs.get(0);
        } catch (IOException e) {
            log.error("Exception while generating operator config {}. Session Id {}", e, sessionId);
            var msg = "Failed to generate Concord Operator configurations for session Id : " + sessionId;
            throw new ConfigServiceException(ErrorCode.CONCORD_OPERATOR_CONFIGURATION_FAILURE, msg, e);
        }
    }

    /**
     * Utility method for generating input config yaml file.
     *
     * @param nodeList                  nodelist
     * @param configYamlPath            Yaml file path
     * @return True if config generation was a success, false otherwise.
     */
    boolean generateConfigYaml(BlockchainNodeList nodeList, int clientProxyPerParticipant, String configYamlPath,
                               int maxPrincipalValue)
            throws ConfigServiceException {
        if (!ValidationUtil.isValid(nodeList) || !ValidationUtil.isValid(nodeList.getReplicas())) {
            log.error("Replicas are missing.");
            throw new ConfigServiceException(ErrorCode.CLIENT_CONFIG_INVALID_INPUT_FAILURE,
                                             "No Replicas available to process.");
        }
        if (!ConfigUtilHelpers.validateSbft(nodeList.getReplicaSize())) {
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_REPLICA_SIZE_FAILURE,
                                             "Invalid number of replicas.");
        }
        int clusterSize = nodeList.getReplicaSize();
        int fVal = ConfigUtilHelpers.getFVal(clusterSize);
        int cVal = ConfigUtilHelpers.getCVal(clusterSize, fVal);
        return generateConfigYaml(nodeList, fVal, cVal, clientProxyPerParticipant, configYamlPath, maxPrincipalValue);
    }

    /**
     * Utility method for generating input config yaml file.
     *
     * @param nodeList                  nodelist
     * @param fVal                      fVal
     * @param cVal                      cVal
     * @param configYamlPath            yaml file path
     * @return True if configuration was generated successfully, false otherwise.
     */
    boolean generateConfigYaml(BlockchainNodeList nodeList, int fVal, int cVal,
                               int clientProxyPerParticipant,
                               String configYamlPath, int maxPrincipalValue) throws ConfigServiceException {
        if (!ValidationUtil.isValid(nodeList) || !ValidationUtil.isValid(nodeList.getReplicas()) || !ValidationUtil
                .isValid(configYamlPath)) {
            log.error("Essential parameters are missing.");
            throw new ConfigServiceException(ErrorCode.CLIENT_CONFIG_INVALID_INPUT_FAILURE, "Invalid node list.");
        }

        if (!ConfigUtilHelpers.validateSbft(nodeList.getReplicaSize(), fVal, cVal)) {
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_REPLICA_SIZE_FAILURE,
                                             "Invalid number of replicas.");
        }

        Path path = Paths.get(configYamlPath);

        DumperOptions options = new DumperOptions();
        options.setIndent(2);
        options.setPrettyFlow(true);
        options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        Yaml yaml = new Yaml(options);

        Map<String, Object> configInput = null;
        try {
            configInput = yaml.load(new FileInputStream(operatorConfigTemplatePath));
        } catch (FileNotFoundException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized config yaml input template",
                     operatorConfigTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            configInput = yaml.load(classLoader.getResourceAsStream("BFTClientConfigTemplate.yaml"));
        }

        if (configInput == null) {
            throw new ConfigServiceException(ErrorCode.CLIENT_CONFIG_INVALID_INPUT_FAILURE,
                                             "Configuration input file is missing.");
        }

        // Add generic configs
        configInput.put(ConfigUtilHelpers.ConfigProperty.F_VAL.name, fVal);
        configInput.put(ConfigUtilHelpers.ConfigProperty.C_VAL.name, cVal);
        configInput.put(ConfigUtilHelpers.ConfigProperty.NUM_PARTICIPANTS.name, nodeList.getClientSize());
        configInput.put(ConfigUtilHelpers.ConfigProperty.TLS_CERTIFICATES_FOLDER_PATH.name,
                        CertificatesGenerator.IDENTITY_PATH_PREFIX
                        + CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
        configInput
                .put(ConfigUtilHelpers.ConfigProperty.CLIENTS_PER_PARTICIPANT_NODE.name, clientProxyPerParticipant);
        configInput.put(ConfigUtilHelpers.ConfigProperty.OPERATOR_SIGNING_KEYS.name,
                        CONCORD_OPERATOR_KEY_FOLDER_PATH);

        // Prepare per replica config
        List node = (List) configInput.get(ConfigUtilHelpers.ConfigProperty.NODE.name);
        Map<String, Object> nodeConfig = (Map<String, Object>) node.get(0);

        List replicaConfig = (List) nodeConfig.get(ConfigUtilHelpers.ConfigProperty.REPLICA.name);
        Map<String, Object> replicaValues = (Map<String, Object>) replicaConfig.get(0);

        List resultNodes = new ArrayList();

        for (BlockchainReplica replica : nodeList.getReplicas()) {
            replicaValues.put(ConfigUtilHelpers.ConfigProperty.REPLICA_HOST.name, replica.getIp());
            replicaValues.put(ConfigUtilHelpers.ConfigProperty.COMMITTER_PORT.name,
                              ConfigUtilHelpers.DEFAULT_PORT);
            nodeConfig.put(ConfigUtilHelpers.ConfigProperty.REPLICA.name,
                           new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(replicaValues))));
            resultNodes.add(ConfigUtilHelpers.clone(nodeConfig));
        }

        configInput.put(ConfigUtilHelpers.ConfigProperty.NODE.name, resultNodes);

        // Operator entry hardcoded
        Map<String, Object> operatorValues = new HashMap<>();
        operatorValues.put("operator_node_host", "0.0.0.0");
        operatorValues.put("operator_port", "3600");
        operatorValues.put("operator_id", maxPrincipalValue);
        configInput.put("operator_node",
                           new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(operatorValues))));

        BufferedWriter writer = null;
        try {
            writer = Files.newBufferedWriter(path);
            yaml.dump(configInput, writer);
            writer.flush();
            return true;
        } catch (IOException x) {
            log.error("IOException while writing to yaml out file.", x);
            return false;
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException ioe) {
                    log.error("Trouble closing yaml out file.", ioe);
                }
            }
        }
    }
}
