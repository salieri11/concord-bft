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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
        PREEXECUTION_ENABLED("preexecution_enabled");

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
        return original.entrySet()
                .stream()
                .collect(Collectors.toMap(Map.Entry::getKey,
                        Map.Entry::getValue));
    }

    /**
     * Creates conc_genconfig command.
     * @return CompletableFuture
     */
    static List<CompletableFuture<Process>> getConcGenConfigCmd(String inputYamlPath,
                                                          String principalsMapFile,
                                                          Path outputPath,
                                                          boolean isSplitConfig) throws IOException {
        List<CompletableFuture<Process>> cmd = new ArrayList<>();
        var configGenOutputPath = Paths.get(outputPath.toString(), "configGenOutput.txt").toString();
        if (isSplitConfig) {
            cmd.add(new ProcessBuilder("/app/conc_genconfig",
                    "--configuration-input",
                    inputYamlPath,
                    "--report-principal-locations",
                    principalsMapFile,
                    "--configuration-type",
                    "deployment",
                    "--output-name",
                    DEPLOY)
                    .directory(outputPath.toFile())
                            .redirectError(new File(configGenOutputPath))
                    .start()
                    .onExit());
            cmd.add(new ProcessBuilder("/app/conc_genconfig",
                    "--configuration-input",
                    inputYamlPath,
                    "--report-principal-locations",
                    principalsMapFile,
                    "--configuration-type",
                    "secrets",
                    "--output-name",
                    SECRET)
                    .directory(outputPath.toFile())
                            .redirectError(new File(configGenOutputPath))
                    .start()
                    .onExit());
        } else {
            cmd.add(new ProcessBuilder("/app/conc_genconfig",
                    "--configuration-input",
                    inputYamlPath,
                    "--report-principal-locations",
                    principalsMapFile)
                    .directory(outputPath.toFile())
                            .redirectError(new File(configGenOutputPath))
                    .start()
                    .onExit());
        }
        return cmd;
    }

    /**
     * Creates configs for concord.
     * @param outputPath path where conc_genconfig outputs
     * @param nodeIds nodeids
     * @param splitconfig flag to denote split config
     * @return map of node ids against configs
     * @throws IOException .
     */
    static Map<String, Map<String, String>> getConcordNodeConfigs(Path outputPath,
                                                     List<String> nodeIds,
                                                     boolean splitconfig) throws IOException {
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
}
