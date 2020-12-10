/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

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
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

/**
 * Utility class for generating the input for Configuration Yaml file.
 */
public class BftClientConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(BftClientConfigUtil.class);

    private String bftClientConfigTemplatePath;
    private UUID sessionId;

    public int maxPrincipalId;

    // Do we want to keep temporary files?
    private boolean keepTempFiles;

    public BftClientConfigUtil(String bftClientConfigTemplatePath, UUID sessionId) {
        this.bftClientConfigTemplatePath = bftClientConfigTemplatePath;
        this.sessionId = sessionId;
    }

    /**
     * Constructor with keepTempFiles directive.
     *
     * @param bftClientConfigTemplatePath clients template path
     * @param sessionId                   session id
     * @param keepTempFiles               Directive to keep temporary files
     */
    public BftClientConfigUtil(String bftClientConfigTemplatePath, UUID sessionId,
                               boolean keepTempFiles) {
        this.bftClientConfigTemplatePath = bftClientConfigTemplatePath;
        this.sessionId = sessionId;
        this.keepTempFiles = keepTempFiles;
    }

    /**
     * persistence.
     */
    public final Map<Integer, List<Integer>> nodePrincipal = new HashMap<>();


    /**
     * Generate and get concord configuration for the blockchain.
     *
     * @param nodeList                  node list
     * @param clientProxyPerParticipant number of client proxies per participant
     * @return a map of node and configration
     * @throws IOException during any error
     */
    public Map<String, String> getBftClientConfig(BlockchainNodeList nodeList, int clientProxyPerParticipant)
            throws IOException {
        try {
            if (!ValidationUtil.isValid(nodeList.getReplicas())) {
                log.error("Replicas are missing.");
                throw new ConfigServiceException(ErrorCode.CLIENT_CONFIG_INVALID_INPUT_FAILURE,
                                                 "Replicas are missing.");
            }
            // Prefix the directory name with 'clients' so as to differentiate between clients and replicas.
            var outputPath = Files.createTempDirectory("clients-");
            log.info("outputPath {}", outputPath.toString());

            var principalsMapFile = Paths.get(outputPath.toString(), "principals.json").toString();
            var inputYamlPath = Paths.get(outputPath.toString(), "dockerConfigurationInput.yaml").toString();
            var configGenOutputPath = Paths.get(outputPath.toString(), "configGenOutput.txt").toString();
            var configGenErrorPath = Paths.get(outputPath.toString(), "configGenError.txt").toString();
            // A call to generate configuration template yaml file.
            generateConfigYaml(nodeList, clientProxyPerParticipant, inputYamlPath);

            // TODO: Dependency on config-gen tool can be avoided here.
            // Config gen tool is only giving the principal ids
            List<String> configCmd = ConfigUtilHelpers.getConcordGenConfigCmd().stream().collect(Collectors.toList());
            configCmd.addAll(List.of("--configuration-input", inputYamlPath, "--report-principal-locations",
                                     principalsMapFile, "--client-conf", "true"));
            var configFuture = new ProcessBuilder(configCmd)
                    .directory(outputPath.toFile())
                    .redirectError(new File(configGenErrorPath))
                    .redirectOutput(new File(configGenOutputPath))
                    .start()
                    .onExit();

            var result = new HashMap<String, String>();
            var work = configFuture.whenCompleteAsync((process, error) -> {
                if (error == null) {
                    try {
                        // Process principals file and get the maps of ids.
                        var principalsMap = ConfigUtilHelpers.getPrincipals(principalsMapFile, sessionId);
                        principalsMap.forEach((key, value) -> nodePrincipal.putIfAbsent(key - 1, value));

                        for (int num = 0; num < nodeList.getClientSize(); num++) {
                            var path = outputPath.resolve("Participant" + num + ".config");

                            var value = Files.readString(path);
                            if (value.isBlank() || value.isEmpty()) {
                                throw new IOException("No configuration generated in path " + path.toString()
                                                      + " for session id " + sessionId);
                            }
                            result.put(nodeList.getClients().get(num).getId(), value);
                        }
                    } catch (Throwable collectError) {
                        log.error("Cannot collect generated bft configuration. Session Id {}", sessionId, collectError);
                    } finally {
                        // Now that we have collected the files, we can delete the temporary directory.
                        ConfigUtilHelpers.deleteTemporaryFiles(outputPath, keepTempFiles);
                    }
                } else {
                    log.error("Cannot run bft config generation process. Session Id {}", sessionId, error);
                }
            });

            work.join();
            return result;

        } catch (IOException e) {
            log.error("Exception while generating bft config {}. Session Id {}", e, sessionId);
            throw e;
        }
    }

    /**
     * Utility method for generating input config yaml file.
     *
     * @param nodeList                  nodelist
     * @param clientProxyPerParticipant Number of client proxies per Client
     * @param configYamlPath            Yaml file path
     * @return True if config generation was a success, false otherwise.
     */
    boolean generateConfigYaml(BlockchainNodeList nodeList, int clientProxyPerParticipant, String configYamlPath)
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
        return generateConfigYaml(nodeList, fVal, cVal, clientProxyPerParticipant, configYamlPath);
    }

    /**
     * Utility method for generating input config yaml file.
     *
     * @param nodeList                  nodelist
     * @param fVal                      fVal
     * @param cVal                      cVal
     * @param clientProxyPerParticipant number of client proxies per client
     * @param configYamlPath            yaml file path
     * @return True if configuration was generated successfully, false otherwise.
     */
    boolean generateConfigYaml(BlockchainNodeList nodeList, int fVal, int cVal, int clientProxyPerParticipant,
                               String configYamlPath) throws ConfigServiceException {
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
            configInput = yaml.load(new FileInputStream(bftClientConfigTemplatePath));
        } catch (FileNotFoundException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized config yaml input template",
                     bftClientConfigTemplatePath, e.getLocalizedMessage());
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

        // Prepare participant configs
        List participantNodes = (List) configInput.get(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODES.name);
        Map<String, Object> participantNodesConfig = (Map<String, Object>) participantNodes.get(0);
        List participantNode = (List) participantNodesConfig.get(
                ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODE.name);
        Map<String, Object> participantConfig = (Map<String, Object>) participantNode.get(0);

        List externalClients = (List) participantConfig.get(ConfigUtilHelpers.ConfigProperty.EXTERNAL_CLIENTS.name);
        Map<String, Object> clientValues = (Map<String, Object>) externalClients.get(0);
        List client = (List) clientValues.get(ConfigUtilHelpers.ConfigProperty.CLIENT.name);
        Map<String, Object> clientPort = (Map<String, Object>) client.get(0);

        List participantNodeRes = new ArrayList();
        Map<String, Object> participantNodeResMap = new HashMap<>();
        int counter = 1;
        for (int i = 0; i < nodeList.getClientSize(); i++) {
            List resultClients = new ArrayList();
            var participantConfigRes = ConfigUtilHelpers.clone(participantConfig);
            participantConfigRes.put(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODE_HOST.name, "0.0.0.0");

            for (int j = 0; j < clientProxyPerParticipant; j++) {
                var port = ConfigUtilHelpers.DEFAULT_PORT + counter + j;
                clientPort.put(ConfigUtilHelpers.ConfigProperty.CLIENT_PORT.name, port);
                clientValues.put(ConfigUtilHelpers.ConfigProperty.CLIENT.name,
                                 new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(clientPort))));
                resultClients.add(ConfigUtilHelpers.clone(clientValues));
            }
            counter = counter + clientProxyPerParticipant;

            participantConfigRes.put(ConfigUtilHelpers.ConfigProperty.EXTERNAL_CLIENTS.name, resultClients);
            participantNodeResMap.put(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODE.name,
                                      new ArrayList(Collections.singletonList(
                                              ConfigUtilHelpers.clone(participantConfigRes))));
            participantNodeRes.add(ConfigUtilHelpers.clone(participantNodeResMap));
        }

        configInput.put(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODES.name, participantNodeRes);
        configInput
                .put(ConfigUtilHelpers.ConfigProperty.CLIENTS_PER_PARTICIPANT_NODE.name, clientProxyPerParticipant);

        // Also add num_ro_replicas. I was asked to put this back, otherwise principalId computation formula fails.
        configInput.put(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_NUM_RO_REPLICAS.name,
                        nodeList.getReadReplicaSize());

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
