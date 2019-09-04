/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.generateconfig;

import java.io.BufferedWriter;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.service.configuration.generatecerts.CertificatesGenerator;

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
        SERVICE_HOST("- service_host: ", 2),
        SERVICE_PORT("service_port: ", 4),
        REPLICA("replica:", 4),
        DAML_EXECUTION_ENGINE_ADDRESS("daml_execution_engine_addr: ", 4),
        CLIENT_PROXY("client_proxy:", 4),
        REPLICA_HOST("- replica_host: ", 6),
        CLIENT_HOST("- client_host: ", 6),
        REPLICA_PORT("replica_port: ", 8),
        CLIENT_PORT("client_port: ", 8);

        String propertyName;
        int space;

        ConfigProperty(String propertyName, int space) {
            this.propertyName = propertyName;
            this.space = space;
        }
    }

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
                                                 String blockchainType) {
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
                                    String blockchainType) {
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
    boolean generateInputConfigYaml(List<String> hostIp, int fVal, int cVal, String configYamlPath,
                                    String blockchainType) {
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
        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            writer.write("client_proxies_per_replica: " + CLIENT_PROXY_PER_NODE);
            writer.newLine();
            writer.write("c_val: " + cVal);
            writer.newLine();
            writer.write("f_val: " + fVal);
            writer.newLine();

            // Temporary setup to distinguish concord type. This check will not occur in ConfigService
            // after it starts accepting params to override.
            if (blockchainType != null && blockchainType.equalsIgnoreCase("daml")) {
                writer.write("daml_enable: true");
                writer.newLine();
                writer.write("eth_enable: false");
                writer.newLine();
                writer.write("concord-bft_communication_buffer_length: 8388608");
                writer.newLine();
                writer.write("concord-bft_max_external_message_size: 8388608");
                writer.newLine();
                writer.write("concord-bft_max_reply_message_size: 8388608");
                writer.newLine();
                writer.write("concord-bft_max_num_of_reserved_pages: 65536");
                writer.newLine();
            } else {
                writer.write("daml_enable: false");
                writer.newLine();
            }
            writer.write("comm_to_use: tls");
            writer.newLine();
            writer.write("use_loopback_for_local_hosts: true");
            writer.newLine();
            writer.write("tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384");
            writer.newLine();
            writer.write("tls_certificates_folder_path: "
                         + URI.create(CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH).getPath());
            writer.newLine();
            writer.write("node__TEMPLATE:\n  genesis_block: /concord/config-public/genesis.json\n  "
                         + "blockchain_db_path: /concord/rocksdbdata/");
            writer.newLine();
            writer.write("node:");
            writer.newLine();
            for (String s : hostIp) {
                writePairProperty(writer, ConfigProperty.SERVICE_HOST, "0.0.0.0");
                writePairProperty(writer, ConfigProperty.SERVICE_PORT, "5458");
                writePairProperty(writer, ConfigProperty.DAML_EXECUTION_ENGINE_ADDRESS,
                                  "daml_execution_engine:55000");
                writePairProperty(writer, ConfigProperty.REPLICA, "");
                writePairProperty(writer, ConfigProperty.REPLICA_HOST, s);
                writePairProperty(writer, ConfigProperty.REPLICA_PORT, Integer.toString(DEFAULT_PORT));
                writePairProperty(writer, ConfigProperty.CLIENT_PROXY, "");

                for (int j = 0; j < CLIENT_PROXY_PER_NODE; j++) {
                    writePairProperty(writer, ConfigProperty.CLIENT_HOST, s);
                    writePairProperty(writer, ConfigProperty.CLIENT_PORT, Integer.toString((DEFAULT_PORT + j + 1)));
                }
            }
            writer.flush();
            writer.close();
            return true;
        } catch (IOException x) {
            log.error("IOException: %s%n", x);
            return false;
        }
    }

    private void writePairProperty(BufferedWriter writer, ConfigProperty property, String value)
            throws IOException {
        for (int i = 0; i < property.space; i++) {
            writer.write(" ");
        }
        writer.write(property.propertyName + value);
        writer.newLine();
    }
}