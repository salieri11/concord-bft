/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.BufferedWriter;
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
import java.util.concurrent.CompletableFuture;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.configuration.util.BlockchainReadReplica;
import com.vmware.blockchain.configuration.util.BlockchainReplica;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

/**
 * Utility class for generating concord configurations.
 */
public class ConcordConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(ConcordConfigUtil.class);

    private String concordConfigTemplatePath;

    private ConfigurationSessionIdentifier sessionId;

    // Do we want to keep temporary files?
    private boolean keepTempFiles;

    public ConcordConfigUtil(String concordConfigTemplatePath, ConfigurationSessionIdentifier sessionId) {
        this.concordConfigTemplatePath = concordConfigTemplatePath;
        this.sessionId = sessionId;
    }

    /**
     * Constructor with keepTempFiles directive.
     *
     * @param concordConfigTemplatePath template path
     * @param sessionId                 session id
     * @param keepTempFiles             Directive to keep temporary files
     */
    public ConcordConfigUtil(String concordConfigTemplatePath, ConfigurationSessionIdentifier sessionId,
                             boolean keepTempFiles) {
        this.concordConfigTemplatePath = concordConfigTemplatePath;
        this.sessionId = sessionId;
        this.keepTempFiles = keepTempFiles;
    }

    /**
     * persistence.
     */
    public final Map<Integer, List<Integer>> nodePrincipal = new HashMap<>();

    /**
     * Generate and return configuration for all Concord nodes.
     *
     * @param nodeList       nodelist
     * @param blockchainType blockchain type
     * @param bftClients     number of BFT clients
     * @param bcFeatures     Blockchain features
     * @return A map of configurations
     * @throws IOException when config generation fails.
     */
    public Map<String, Map<String, String>> getConcordConfig(BlockchainNodeList nodeList, BlockchainType blockchainType,
                                                             int bftClients, BlockchainFeatures bcFeatures)
            throws IOException, ConfigServiceException {
        if (!ValidationUtil.isValid(nodeList) || !ValidationUtil.isValid(nodeList.getReplicas())
            || !ValidationUtil.isValid(blockchainType) || !ValidationUtil.isValid(bcFeatures)) {
            log.error("Essential parameters are missing. replicas: {}, blockchainType: {}, bcFeatures: {}.",
                      nodeList.getReplicas(), blockchainType, bcFeatures.toString());
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_INPUT_FAILURE,
                                             "Essential Parameters are missing.");
        }

        try {
            // Prefix the directory name with 'replicas' so as to differentiate between clients and replicas.
            var outputPath = Files.createTempDirectory("replicas-");

            var principalsMapFile = Paths.get(outputPath.toString(), "principals.json").toString();
            var inputYamlPath = Paths.get(outputPath.toString(), "dockerConfigurationInput.yaml").toString();

            generateInputConfigYaml(nodeList, inputYamlPath, blockchainType, bftClients, bcFeatures);

            var configFutures = ConfigUtilHelpers
                    .getConcGenConfigCmd(inputYamlPath, principalsMapFile, outputPath, bcFeatures.isSplitConfig());

            var result = new HashMap<String, Map<String, String>>();
            var work = CompletableFuture.allOf(configFutures.toArray(new CompletableFuture[configFutures.size()]))
                    .whenCompleteAsync((process, error) -> {
                        if (error == null) {
                            try {

                                // Process principals file and get the maps of ids.
                                var principalsMap = ConfigUtilHelpers.getPrincipals(principalsMapFile, sessionId);
                                principalsMap.forEach((key, value) -> nodePrincipal.putIfAbsent(key - 1, value));

                                // Add read replias only when object store feature is enabled.
                                if (bcFeatures.isObjectStoreEnabled()) {
                                    putReadReplicasInNodePrincipals(nodePrincipal, nodeList);
                                }

                                log.info("Node principal after adding read replicas {}", nodePrincipal);

                                result.putAll(ConfigUtilHelpers.getConcordNodeConfigs(outputPath,
                                                                                      nodeList.getReplicaNodeIds(),
                                                                                      bcFeatures.isSplitConfig()));
                            } catch (Throwable collectError) {
                                log.error("Cannot collect generated cluster configuration for session id : {}",
                                          sessionId, collectError);
                            } finally {
                                // Now that we have collected the files, we can delete the temporary directory.
                                ConfigUtilHelpers.deleteTemporaryFiles(outputPath, keepTempFiles);
                            }
                        } else {
                            log.error("Cannot run config generation process for session id : {}",
                                      sessionId, error);
                        }
                    });
            work.join();
            return result;
        } catch (IOException e) {
            log.error("Exception while generating concord config for session id : {} \n{}",
                      sessionId, e);
            throw e;
        }
    }

    /**
     * Generate configuration for all nodes.
     *
     * @param nodeList       List of nodes
     * @param configYamlPath Yaml file path
     * @param blockchainType Blockchain type
     * @param bftClients     BFT client count
     * @param bcFeatures     Features for this blockchain
     * @return True if config generation is a success, false otherwise.
     */
    boolean generateInputConfigYaml(BlockchainNodeList nodeList, String configYamlPath, BlockchainType blockchainType,
                                    int bftClients, BlockchainFeatures bcFeatures) throws ConfigServiceException {
        if (!ValidationUtil.isValid(nodeList) || !ValidationUtil.isValid(nodeList.getReplicas())) {
            log.error("Replicas are missing.");
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_INPUT_FAILURE,
                                             "Replicas are missing.");
        }
        if (!ConfigUtilHelpers.validateSbft(nodeList.getReplicaSize())) {
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_REPLICA_SIZE_FAILURE,
                                             "Invalid number of replicas available.");
        }
        int clusterSize = nodeList.getReplicaSize();
        int fVal = ConfigUtilHelpers.getFVal(clusterSize);
        int cVal = ConfigUtilHelpers.getCVal(clusterSize, fVal);
        return generateInputConfigYaml(nodeList, fVal, cVal, configYamlPath, blockchainType, bftClients, bcFeatures);
    }

    /**
     * Generate configuration for all nodes.
     *
     * @param nodeList       List of nodes
     * @param fVal           fVal
     * @param cVal           cVal
     * @param configYamlPath Yaml file path
     * @param blockchainType Blockchain type
     * @param bftClients     BFT client count
     * @param bcFeatures     Features for this blockchain
     * @return True if config generation is a success, false otherwise.
     */
    boolean generateInputConfigYaml(BlockchainNodeList nodeList, int fVal, int cVal, String configYamlPath,
                                    BlockchainType blockchainType, int bftClients, BlockchainFeatures bcFeatures)
            throws ConfigServiceException {
        // Are all required parameters available?
        if (!ValidationUtil.isValid(nodeList) || !ValidationUtil.isValid(nodeList.getReplicas())
            || !ValidationUtil.isValid(blockchainType) || !ValidationUtil.isValid(bcFeatures)) {
            log.error("Essential parameters are missing. replicas: {}, bcType: {}, bcFeatures: ",
                      nodeList.getReplicas(), blockchainType, bcFeatures);
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_INPUT_FAILURE,
                                             "Replicas or other required parameters missing.");
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
            configInput = yaml.load(new FileInputStream(concordConfigTemplatePath));
        } catch (FileNotFoundException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized config yaml input template",
                     concordConfigTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            configInput = yaml.load(classLoader.getResourceAsStream("ConcordConfigTemplate.yaml"));
        }

        if (configInput == null) {
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_INPUT_FAILURE,
                                             "Configuration input file is missing.");
        }

        //FIXME: Add provision to have more than one BlockchainType
        if (blockchainType == null || blockchainType.equals(BlockchainType.ETHEREUM)) {
            configInput.put(ConfigUtilHelpers.ConfigProperty.ETHEREUM_ENABLED.name, true);

            // FIXME: for https://jira.eng.vmware.com/browse/VB-1925
            //  Remove once bug is fixed OR when remodeled to have per-blockchain config template
            configInput.put("FEATURE_time_service", false);
        } else if (blockchainType.equals(BlockchainType.DAML)) {
            configInput.put(ConfigUtilHelpers.ConfigProperty.DAML_ENABLED.name, true);

        } else if (blockchainType.equals(BlockchainType.HLF)) {
            configInput.put(ConfigUtilHelpers.ConfigProperty.HLF_ENABLED.name, true);
        }

        configInput.put(ConfigUtilHelpers.ConfigProperty.F_VAL.name, fVal);
        configInput.put(ConfigUtilHelpers.ConfigProperty.C_VAL.name, cVal);
        configInput.put(ConfigUtilHelpers.ConfigProperty.NUM_EXTERNAL_CLIENTS.name, bftClients);

        if (bcFeatures.isPreExecutionDeployment()) {
            configInput.put(ConfigUtilHelpers.ConfigProperty.PREEXECUTION_ENABLED.name,
                            bcFeatures.isPreExecutionDeployment());
        }

        // Process and populate replica nodes.
        populateReplicaNodes(configInput, nodeList.getReplicas());

        // Process read replicas when object store feature is enabled.
        if (bcFeatures.isObjectStoreEnabled()) {
            populateReadReplicaNodes(configInput, nodeList.getReadReplicas());

            // Also add num_ro_replicas
            configInput.put(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_NUM_RO_REPLICAS.name,
                            nodeList.getReadReplicaSize());
        } else {
            // Remove read replica template and configuration from configInput.
            removeReadReplicaElements(configInput);
        }

        BufferedWriter writer = null;
        try {
            writer = Files.newBufferedWriter(path);
            yaml.dump(configInput, writer);
            writer.flush();
            log.info("Generated out Concord Config yaml file at {}", path);
            return true;
        } catch (IOException x) {
            log.error("IOException: %s%n", x);
            return false;
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException ioe) {
                    log.error("Exception while closing BufferedWriter.", ioe);
                }
            }
        }
    }

    /**
     * Clone read replica object, populate it with values and add it to configuration map.
     * ro_node:
     *   -service_host:0.0 .0.0
     *   service_port: 5458
     *   jaeger_agent: jaeger- agent:5831
     *   logger_config: /concord / config-public/log4cplus.properties
     *   s3 - bucket - name:blockchain
     *   s3 - access - key:concordbft
     *   s3 - protocol:HTTP
     *   s3 -url:minio: 9000
     *   s3 - secret - key:concordbft
     *
     * @param configInput  Configuration map
     * @param readReplicas List of read replicas.
     */
    private void populateReadReplicaNodes(Map<String, Object> configInput, List<BlockchainReadReplica> readReplicas) {
        if (readReplicas == null) {
            log.error("There are no read replicas to add.");
            return;
        }
        List roNode = (List) configInput.getOrDefault(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_RO_NODE.name,
                                                      new ArrayList<>());
        Map<String, Object> roNodeConfig = (roNode.get(0) != null) ? (Map<String, Object>) roNode.get(0) :
                                           new HashMap<>();

        List resultRoNodes = new ArrayList();
        for (BlockchainReadReplica readReplica : readReplicas) {
            roNodeConfig.put(BlockchainReadReplica.OBJ_STORE_ACCESS_KEY,
                             (readReplica.getObjStoreAccessKey() != null) ? readReplica.getObjStoreAccessKey() : "");
            roNodeConfig.put(BlockchainReadReplica.OBJ_STORE_BUCKET_NAME_KEY,
                             (readReplica.getObjStoreBucketName() != null) ? readReplica.getObjStoreBucketName() : "");
            roNodeConfig.put(BlockchainReadReplica.OBJ_STORE_PROTOCOL_KEY,
                             (readReplica.getObjStoreProtocol() != null) ? readReplica.getObjStoreProtocol() : "");
            roNodeConfig.put(BlockchainReadReplica.OBJ_STORE_SECRET_KEY,
                             (readReplica.getObjStoreSecret() != null) ? readReplica.getObjStoreSecret() : "");
            roNodeConfig.put(BlockchainReadReplica.OBJ_STORE_URL_KEY,
                             (readReplica.getObjStoreUrl() != null) ? readReplica.getObjStoreUrl() : "");

            // Clone read replica and add it to result nodes list.
            resultRoNodes.add(ConfigUtilHelpers.clone(roNodeConfig));
        }
        configInput.put(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_RO_NODE.name, resultRoNodes);
    }


    /**
     * Clone Replica populate values and add it to Configuration map.
     *
     * @param configInput Configuration map
     * @param replicas    List of replicas
     */
    private void populateReplicaNodes(Map<String, Object> configInput, List<BlockchainReplica> replicas) {
        if (replicas == null) {
            log.error("There are no replicas to add.");
            return;
        }
        List node = (List) configInput.getOrDefault(ConfigUtilHelpers.ConfigProperty.NODE.name, new ArrayList<>());
        Map<String, Object> nodeConfig = node.get(0) != null ? (Map<String, Object>) node.get(0) : new HashMap<>();

        // FIXME: Concord to fix: Why is replica a list?
        List replicaConfig = (List) nodeConfig.getOrDefault(ConfigUtilHelpers.ConfigProperty.REPLICA.name,
                                                            new ArrayList<>());
        Map<String, Object> replicaValues = replicaConfig.get(0) != null ? (Map<String, Object>) replicaConfig.get(0) :
                                            new HashMap<>();

        List clientConfig = (List) nodeConfig.getOrDefault(ConfigUtilHelpers.ConfigProperty.CLIENT_PROXY.name,
                                                           new ArrayList<>());
        Map<String, Object> clientValues = clientConfig.get(0) != null ? (Map<String, Object>) clientConfig.get(0) :
                                           new HashMap<>();

        List resultNodes = new ArrayList();

        for (BlockchainReplica replica : replicas) {
            List resultClients = new ArrayList();

            replicaValues.put(ConfigUtilHelpers.ConfigProperty.REPLICA_HOST.name, replica.getIp());

            for (int j = 0; j < ConfigUtilHelpers.CLIENT_PROXY_PER_COMMITTER; j++) {

                clientValues.put(ConfigUtilHelpers.ConfigProperty.CLIENT_HOST.name, replica.getIp());
                clientValues.put(ConfigUtilHelpers.ConfigProperty.CLIENT_PORT.name,

                                 ConfigUtilHelpers.DEFAULT_PORT + j + 1);
                resultClients.add(ConfigUtilHelpers.clone(clientValues));
            }

            nodeConfig.put(ConfigUtilHelpers.ConfigProperty.CLIENT_PROXY.name, resultClients);
            nodeConfig.put(ConfigUtilHelpers.ConfigProperty.REPLICA.name,

                           new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(replicaValues))));
            resultNodes.add(ConfigUtilHelpers.clone(nodeConfig));
        }
        configInput.put(ConfigUtilHelpers.ConfigProperty.NODE.name, resultNodes);
    }


    /**
     * Add Read replicas to nodePrincipal object. Temporary method until conc_genconfig script is enhanced for read
     * replicas.
     *
     * @param nodePrincipal Replica node principals map.
     */
    private void putReadReplicasInNodePrincipals(Map<Integer, List<Integer>> nodePrincipal,
                                                 BlockchainNodeList nodeList) {
        if (nodePrincipal != null && nodePrincipal.keySet().size() == nodeList.getAllReplicasSize()) {
            log.info("All replicas including read replicas are already captured. No action required.");
            return;
        }
        log.info("Capture read replicas in nodePrincipal.");
        // Figure out the index from principals map.
        int index = nodePrincipal.size();
        for (int ind = 0; ind < nodeList.getReadReplicaSize(); ind++) {
            nodePrincipal.putIfAbsent((index + ind), List.of());
        }
    }

    private void removeReadReplicaElements(Map<String, Object> configInput) {
        log.info("Removing read replica references from configInput because object store is disabled.");
        if (configInput.containsKey(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_NUM_RO_REPLICAS.name)) {
            configInput.remove(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_NUM_RO_REPLICAS.name);
        }
        if (configInput.containsKey(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_RO_NODE.name)) {
            configInput.remove(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_RO_NODE.name);
        }
        if (configInput.containsKey(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_RO_NODE_TEMPLATE.name)) {
            configInput.remove(ConfigUtilHelpers.ConfigProperty.OBJ_STORE_RO_NODE_TEMPLATE.name);
        }
    }
}
