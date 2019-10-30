/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.generateconfig;

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
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.Yaml;

import com.vmware.blockchain.deployment.v1.ConcordModelSpecification.BlockchainType;

import kotlinx.serialization.UpdateMode;
import kotlinx.serialization.internal.ArrayListSerializer;
import kotlinx.serialization.internal.HashMapSerializer;
import kotlinx.serialization.internal.IntSerializer;
import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;

/**
 * Utility class for generating the input for Configuration Yaml file.
 */
public class ConcordConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(ConcordConfigUtil.class);

    /**
     * Enum holding config properties.
     */
    private enum ConfigProperty {
        F_VAL("f_val"),
        C_VAL("c_val"),
        DAML_ENABLED("daml_enable"),
        ETHEREUM_ENABLED("eth_enable"),
        HLF_ENABLED("hlf_enable"),
        NODE("node"),
        REPLICA("replica"),
        CLIENT_PROXY("client_proxy"),
        REPLICA_HOST("replica_host"),
        CLIENT_HOST("client_host"),
        CLIENT_PORT("client_port");

        String name;

        ConfigProperty(String propertyName) {
            this.name = propertyName;
        }
    }

    private static final String CONFIG_TEMPLATE_PATH = "/config/persephone/configuration/ConcordConfigTemplate.yaml";
    private static final int DEFAULT_PORT = 3501;
    public static final int CLIENT_PROXY_PER_NODE = 4;

    /**
     * file path.
     */
    public final String configPath = "/concord/config-local/concord.config";

    /**
     * persistence.
     */
    public final Map<Integer, List<Integer>> nodePrincipal = new HashMap<>();

    public int maxPrincipalId;

    /**
     * Helper method to calculate f_val for cluster.
     */
    private int getFVal(int clusterSize) {
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
    private int getCVal(int clusterSize, int fVal) {
        //return ((clusterSize-1) - 3*fVal )/ 2;
        return 0;
    }

    /**
     * Utility to generate concord config.
     */
    public Map<Integer, String> getConcordConfig(List<String> hostIps,
                                                 BlockchainType blockchainType) {
        try {
            var result = new HashMap<Integer, String>();

            var outputPath = Files.createTempDirectory(null);
            var principalsMapFile = Paths.get(outputPath.toString(), "principals.json").toString();
            var inputYamlPath = Paths.get(outputPath.toString(), "dockerConfigurationInput.yaml").toString();

            generateInputConfigYaml(hostIps, inputYamlPath, blockchainType);

            var configFuture = new ProcessBuilder("/app/conc_genconfig",
                                                  "--configuration-input",
                                                  inputYamlPath,
                                                  "--report-principal-locations",
                                                  principalsMapFile)
                    .directory(outputPath.toFile())
                    .start()
                    .onExit();

            var work = configFuture.whenCompleteAsync((process, error) -> {
                if (error == null) {
                    try {
                        var principalStr = Files.readString(Path.of(principalsMapFile));
                        if (principalStr.isBlank() || principalStr.isEmpty()) {
                            throw new Exception("PrincipalIds are not generated by concord config");
                        }

                        var principalListSerializer = new ArrayListSerializer<>(IntSerializer.INSTANCE);
                        var principalMapSerializer =
                                new HashMapSerializer<>(IntSerializer.INSTANCE, principalListSerializer);

                        var json = new Json(
                                new JsonConfiguration(
                                        false, /* encodeDefaults */
                                        true, /* strictMode */
                                        false, /* unquoted */
                                        false, /* allowStructuredMapKeys */
                                        false, /* prettyPrint */
                                        "    ", /* indent */
                                        false, /* useArrayPolymorphism */
                                        "type", /* classDiscriminator */
                                        UpdateMode.OVERWRITE /* updateMode */
                                ),
                                EmptyModule.INSTANCE
                        );

                        @SuppressWarnings("unchecked")
                        var principalsMap = (Map<Integer, List<Integer>>)
                                json.parse(principalMapSerializer, principalStr);

                        principalsMap.forEach((key, value) -> nodePrincipal.putIfAbsent(key - 1, value));

                        for (int num = 0; num < hostIps.size(); num++) {
                            var path = outputPath.resolve("concord" + (num + 1) + ".config");
                            result.put(num, Files.readString(path));
                        }
                    } catch (Throwable collectError) {
                        log.error("Cannot collect generated cluster configuration",
                                  collectError);
                    }
                } else {
                    log.error("Cannot run config generation process", error);
                }
            });

            work.join();
            return result;

        } catch (IOException e) {
            log.error("Exception while generating concord config {}", e.getLocalizedMessage());
            return null;
        }
    }

    /**
     * Utility method for generating input config yaml file.
     */
    boolean generateInputConfigYaml(List<String> hostIps, String configYamlPath,
                                    BlockchainType blockchainType) {
        if (hostIps == null) {
            log.error("generateInputConfigYaml: List of host IP provided is NULL!");
            return false;
        }
        if (hostIps.size() < 4) {
            log.error("generateInputConfigYaml: Minimum cluster size is 4!");
            return false;
        }
        int clusterSize = hostIps.size();
        int fVal = getFVal(clusterSize);
        int cVal = getCVal(clusterSize, fVal);
        return generateInputConfigYaml(hostIps, fVal, cVal, configYamlPath, blockchainType);
    }

    /**
     * Utility method for generating input config yaml file.
     */
    @SuppressWarnings({"unchecked"})
    boolean generateInputConfigYaml(List<String> hostIp, int fVal, int cVal, String configYamlPath,
                                    BlockchainType blockchainType) {
        if (hostIp == null) {
            log.error("generateInputConfigYaml: List of host IP provided is NULL!");
            return false;
        }
        if (hostIp.size() < 4) {
            log.error("generateInputConfigYaml: Minimum cluster size is 4!");
            return false;
        }
        if ((3 * fVal + 2 * cVal + 1) > hostIp.size()) {
            log.error("generateInputConfigYaml: fVal / cVal are invalid for the list of host IP provided");
            return false;
        }

        maxPrincipalId = (hostIp.size() + CLIENT_PROXY_PER_NODE * hostIp.size()) - 1;

        Path path = Paths.get(configYamlPath);

        Yaml yaml = new Yaml();
        Map<String, Object> configInput;
        try {
            configInput = yaml.load(new FileInputStream(CONFIG_TEMPLATE_PATH));
        } catch (FileNotFoundException e) {
            //FIXME: To make backwards compatible. This could be removed later.
            log.error(String.format("File %s does not exist: %s\n Using localized config yaml input template",
                    CONFIG_TEMPLATE_PATH, e.getLocalizedMessage()));
            ClassLoader classLoader = getClass().getClassLoader();
            configInput = yaml.load(classLoader.getResourceAsStream("ConcordConfigTemplate.yaml"));
        }

        //FIXME: Add provision to have more than one BlockchainType
        if (blockchainType == null || blockchainType.equals(BlockchainType.ETHEREUM)) {
            configInput.put(ConfigProperty.ETHEREUM_ENABLED.name, true);
        } else if (blockchainType.equals(BlockchainType.DAML)) {
            configInput.put(ConfigProperty.DAML_ENABLED.name, true);
        } else if (blockchainType.equals(BlockchainType.HLF)) {
            configInput.put(ConfigProperty.HLF_ENABLED.name, true);
        }

        configInput.put(ConfigProperty.F_VAL.name, fVal);
        configInput.put(ConfigProperty.C_VAL.name, cVal);

        // Prepare per replica config
        List node = (List) configInput.get(ConfigProperty.NODE.name);
        Map<String, Object> nodeConfig = (Map<String, Object>) node.get(0);

        //FIXME: Concord to fix: Why is replica a list?
        List replicaConfig = (List) nodeConfig.get(ConfigProperty.REPLICA.name);
        Map<String, Object> replicaValues = (Map<String, Object>) replicaConfig.get(0);

        List clientConfig = (List) nodeConfig.get(ConfigProperty.CLIENT_PROXY.name);
        Map<String, Object> clientValues = (Map<String, Object>) clientConfig.get(0);

        List resultNodes = new ArrayList();

        for (String s : hostIp) {
            List resultClients = new ArrayList();
            replicaValues.put(ConfigProperty.REPLICA_HOST.name, s);

            for (int j = 0; j < CLIENT_PROXY_PER_NODE; j++) {
                clientValues.put(ConfigProperty.CLIENT_HOST.name, s);
                clientValues.put(ConfigProperty.CLIENT_PORT.name, DEFAULT_PORT + j + 1);
                resultClients.add(clone(clientValues));
            }

            nodeConfig.put(ConfigProperty.CLIENT_PROXY.name, resultClients);
            nodeConfig.put(ConfigProperty.REPLICA.name, new ArrayList(Collections.singletonList(clone(replicaValues))));
            resultNodes.add(clone(nodeConfig));
        }

        configInput.put(ConfigProperty.NODE.name, resultNodes);

        try {
            BufferedWriter writer = Files.newBufferedWriter(path);
            yaml.dump(configInput, writer);
            writer.flush();
            writer.close();
            return true;
        } catch (IOException x) {
            log.error("IOException: %s%n", x);
            return false;
        }
    }

    private static <K, V> Map<K, V> clone(Map<K, V> original) {
        return original.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                        Map.Entry::getValue));
    }
}
