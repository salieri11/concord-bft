/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

/**
 * Utility Helpers.
 */
public class ConfigUtilHelpers {

    private static final Logger log = LoggerFactory.getLogger(ConfigUtilHelpers.class);

    public static final int DEFAULT_PORT = 3501;
    public static final int CLIENT_PROXY_PER_COMMITTER = 4;
    public static final String DEPLOY = "deploy";
    public static final String SECRET = "secret";
    public static final String CONCORD = "concord";

    // Default script command, can be overwritten for tests.
    private static final String CONCORD_GEN_CONFIG_CMD_DEFAULT = "/app/conc_genconfig";
    private static List<String> concordGenConfigCmd = List.of(CONCORD_GEN_CONFIG_CMD_DEFAULT);
    private static final String CLIENTS_KEY = "clients";
    private static final String READ_REPLICAS_KEY = "ro-replicas";
    private static final String REPLICAS_KEY = "replicas";

    /**
     * Enum holding config properties.
     */
    enum ConfigProperty {
        F_VAL("f_val"),
        C_VAL("c_val"),
        DAML_ENABLED("daml_enable"),
        ETHEREUM_ENABLED("eth_enable"),
        HLF_ENABLED("hlf_enable"),
        NODE("node"),
        PARTICIPANT_NODES("participant_nodes"),
        PARTICIPANT_NODE("participant_node"),
        PARTICIPANT_NODE_HOST("participant_node_host"),
        EXTERNAL_CLIENTS("external_clients"),
        NUM_PARTICIPANTS("num_of_participant_nodes"),
        REPLICA("replica"),
        CLIENT("client"),
        CLIENT_PROXY("client_proxy"),
        REPLICA_HOST("replica_host"),
        CLIENT_HOST("client_host"),
        COMMITTER_PORT("replica_port"),
        CLIENT_PORT("client_port"),
        NUM_EXTERNAL_CLIENTS("num_of_external_clients"),
        CLIENTS_PER_PARTICIPANT_NODE("clients_per_participant_node"),
        PREEXECUTION_ENABLED("preexecution_enabled"),
        OBJ_STORE_RO_NODE("ro_node"),
        OBJ_STORE_NUM_RO_REPLICAS("num_ro_replicas"),
        OBJ_STORE_RO_NODE_TEMPLATE("ro_node__TEMPLATE");

        String name;

        ConfigProperty(String propertyName) {
            this.name = propertyName;
        }
    }

    /**
     * Helper method to calculate f_val for cluster.
     */
    static int getFVal(int clusterSize) {
        int realSize = clusterSize - 1;
        int fSize = realSize / 3;
        // if (realSize % 3 == 0 && fSize > 1) {
        //     fSize--;
        // }
        return fSize;
    }

    /**
     * Helper method to calculate c_val for cluster.
     */
    static int getCVal(int clusterSize, int fVal) {
        //return ((clusterSize-1) - 3*fVal )/ 2;
        return 0;
    }

    /**
     * validate fval and cval.
     */
    static boolean validateSbft(int clusterSize, int fVal, int cVal) {
        if (clusterSize == 0) {
            log.error("List of host IP provided is NULL!");
            return false;
        }
        if (clusterSize < 4) {
            log.error("Minimum cluster size is 4!");
            return false;
        }
        if ((3 * fVal + 2 * cVal + 1) > clusterSize) {
            log.error("fVal / cVal are invalid for the list of host IP provided");
            return false;
        }
        return true;
    }

    /**
     * validate host ips.
     */
    static boolean validateSbft(int clusterSize) {
        if (clusterSize == 0) {
            log.error("List of host IP provided is NULL!");
            return false;
        }
        if (clusterSize < 4) {
            log.error("Minimum cluster size is 4!");
            return false;
        }
        return true;
    }

    /**
     * Clone Map.
     */
    static <K, V> Map<K, V> clone(Map<K, V> original) {
        if (!ValidationUtil.isValid(original)) {
            return new HashMap<>();
        }
        return original.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                        Map.Entry::getValue));
    }

    /**
     * Creates processes to run conc_genconfig command.
     *
     * @return CompletableFuture futures for the command runs.
     */
    static List<CompletableFuture<Process>> getConcGenConfigCmd(String inputYamlPath, String principalsMapFile,
                                                                Path outputPath, boolean isSplitConfig)
            throws IOException, ConfigServiceException {
        if (!ValidationUtil.isValid(inputYamlPath) || !ValidationUtil.isValid(principalsMapFile)
            || !ValidationUtil.isValid(outputPath)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_FAILURE, "Input parameters are missing.");
        }

        List<CompletableFuture<Process>> cmd = new ArrayList<>();
        var configGenOutputPath = Paths.get(outputPath.toString(), "configGenOutput.txt").toString();
        var configGenErrorPath = Paths.get(outputPath.toString(), "configGenError.txt").toString();
        List<String> configCmd = concordGenConfigCmd.stream().collect(Collectors.toList());
        configCmd.addAll(List.of("--configuration-input", inputYamlPath, "--report-principal-locations",
                                 principalsMapFile));
        if (isSplitConfig) {
            List<String> deployConfGenCmd = new ArrayList<>();
            deployConfGenCmd.addAll(configCmd);
            deployConfGenCmd.addAll(List.of("--configuration-type", "deployment", "--output-name", DEPLOY));
            cmd.add(new ProcessBuilder(deployConfGenCmd)
                            .directory(outputPath.toFile())
                            .redirectOutput(new File(configGenOutputPath))
                            .redirectError(new File(configGenErrorPath))
                            .start()
                            .onExit());

            List<String> secretConfGenCmd = new ArrayList<>();
            secretConfGenCmd.addAll(configCmd);
            secretConfGenCmd.addAll(List.of("--configuration-type", "secrets", "--output-name", SECRET));
            cmd.add(new ProcessBuilder(secretConfGenCmd)
                            .directory(outputPath.toFile())
                            .redirectOutput(new File(configGenOutputPath))
                            .redirectError(new File(configGenErrorPath))
                            .directory(outputPath.toFile())
                            .redirectError(new File(configGenOutputPath))
                            .start()
                            .onExit());
        } else {
            cmd.add(new ProcessBuilder(configCmd)
                            .directory(outputPath.toFile())
                            .redirectError(new File(configGenErrorPath))
                            .redirectOutput(new File(configGenOutputPath))
                            .start()
                            .onExit());
        }
        return cmd;
    }

    /**
     * Creates configs for concord.
     *
     * @param nodeIds     nodeids
     * @param outputPath  path where conc_genconfig outputs --@param nodeIds     nodeids
     * @param splitconfig flag to denote split config
     * @return map of node ids against configs
     * @throws IOException during any error.
     */
    static Map<String, Map<String, String>> getConcordNodeConfigs(Path outputPath, List<String> nodeIds,
                                                                  boolean splitconfig)
            throws IOException, ConfigServiceException {
        if (nodeIds == null) {
            log.error("Nodes are not available to generate node configuration.");
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_NO_NODES_FAILURE,
                                             "Nodes are not available to generate configuration.");
        }
        var result = new HashMap<String, Map<String, String>>();
        if (splitconfig) {
            for (int num = 0; num < nodeIds.size(); num++) {
                var deploy = Files.readString(outputPath.resolve(DEPLOY + (num + 1) + ".config"));
                var secret = Files.readString(outputPath.resolve(SECRET + (num + 1) + ".config"));
                if (deploy.isBlank() || deploy.isEmpty() || secret.isBlank() || secret.isEmpty()) {
                    throw new IOException("deployment.config or secret.config not generated.");
                }
                Map<String, String> configs = Map.of(
                        DEPLOY, deploy,
                        SECRET, secret);
                result.put(nodeIds.get(num), configs);
            }
        } else {
            for (int num = 0; num < nodeIds.size(); num++) {
                var path = outputPath.resolve(CONCORD + (num + 1) + ".config");
                var value = Files.readString(path);
                if (value.isEmpty() || value.isBlank()) {
                    throw new IOException("concord.config not generated.");
                }
                var config = Map.of(CONCORD, Files.readString(path));
                result.put(nodeIds.get(num), config);
            }
        }
        return result;
    }


    /**
     * Set the concord config script name.
     *
     * @param configCmd command
     */
    public static void setConcordGenConfigCmd(String configCmd) {
        concordGenConfigCmd = List.of(configCmd.split(" "));
    }

    /**
     * Get the concord config script name.
     */
    public static List<String> getConcordGenConfigCmd() {
        return concordGenConfigCmd;
    }

    /**
     * Process and return principals as a map of ids and clients. This method caters to both clients and replicas cases.
     * Clients: Old style: {{ "4": [21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35] }, { "5": [36, 37, 38,
     * 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50] }} Newer style: { "clients": [{ "4": [21, 22, 23, 24, 25, 26, 27,
     * 28, 29, 30, 31, 32, 33, 34, 35] }, { "5": [36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50] }}
     * Replicas: Old style: {”1”:[6,10,14,18,0]}, {”2”:[7,11,15,19,1]}, {”3”:[8,12,16,20,2]}, {”4”:[9,13,17,21,3] Newer
     * style: {“replicas”: [{”1”:[6,10,14,18,0]}, {”2”:[7,11,15,19,1]}, {”3”:[8,12,16,20,2]}, {”4”:[9,13,17,21,3]},
     * “ro-replicas”: [{“5”: []}, {“6”: []}]}
     *
     * @param principalsMapFile principals map file
     * @return map of principals
     * @throws IOException            during IO error
     * @throws ConfigServiceException during any configuration error
     */
    protected static Map<Integer, List<Integer>> getPrincipals(String principalsMapFile,
                                                               ConfigurationSessionIdentifier sessionId)
            throws IOException, ConfigServiceException {
        var principalStr = Files.readString(Path.of(principalsMapFile));
        if (principalStr.isBlank() || principalStr.isEmpty()) {
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_FAILURE,
                                             "PrincipalIds are not generated by concord config for session id : "
                                             + sessionId);
        }
        Map<Integer, List<Integer>> principalsMap = new HashMap<>();
        try {
            // Is the principals file in the newer format?
            if (principalStr.contains(REPLICAS_KEY) || principalStr.contains(READ_REPLICAS_KEY)
                || principalStr.contains(CLIENTS_KEY)) {
                // Newer style
                TypeReference<Map<String, Map<Integer, List<Integer>>>> typeRef = new TypeReference<>() {
                };
                ObjectMapper objectMapper = new ObjectMapper();
                objectMapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);
                var principalsMapWithNames = objectMapper.readValue(principalStr, typeRef);
                final var finalPrincipalsMap = principalsMap;
                principalsMapWithNames.forEach((key, value) -> {
                    if (value != null) {
                        finalPrincipalsMap.putAll(value);
                    }
                });
                log.debug("Final principals map (new style) {}", principalsMap);
                return principalsMap;
            }

            TypeReference<Map<Integer, List<Integer>>> typeRef = new TypeReference<>() {
            };
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);
            principalsMap = objectMapper.readValue(principalStr, typeRef);
            log.debug("Final principals map (old style) {}", principalsMap);
        } catch (JsonProcessingException e) {
            log.error("Error processing principals file.", e);
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_PRINCIPALS,
                                             "Invalid content in principals file.", e);
        }
        return principalsMap;
    }


    /**
     * Delete temporary files unless keep.temporary.files is true.
     *
     * @param outputPath    path to temporary directory
     * @param keepTempFiles delete/keep temporary files directive
     */
    protected static void deleteTemporaryFiles(Path outputPath, boolean keepTempFiles) {
        // If 'keepTempFiles' directive is off, then delete the directory.
        // Even though we create a temp directory, it will be around unless we delete the file.
        if (outputPath != null && !keepTempFiles) {
            try {
                FileUtils.forceDelete(outputPath.toFile());
            } catch (IOException ioe) {
                log.warn("Error while deleting temporary files. Can be ignored.");
            }
        }
    }

}
