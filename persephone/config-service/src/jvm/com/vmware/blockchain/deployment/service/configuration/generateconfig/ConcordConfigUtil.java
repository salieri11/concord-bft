/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.generateconfig;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.service.configuration.generatecerts.CertificatesGenerator;


/**
 * Utility class for generating the input for Configuration Yaml file.
 * TODO: Looked at Snake YAML package which is a much cleaner way but
 *       requires lot of unnecessary data classes to be developed.
 *       This approach is simpler and generates much less code.
 */
public class ConcordConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(ConcordConfigUtil.class);

    private static final String DEFAULT_PATH_YAML = "/config/dockerConfigurationInput.yaml";
    private static final String CLIENT_PROXY_PER_REPLICA = "client_proxies_per_replica: ";
    private static final String C_VAL = "c_val: ";
    private static final String F_VAL = "f_val: ";
    private static final String NODE = "node:";
    private static final String SERVICE_HOST = "  - service_host: ";
    private static final String SERVICE_PORT = "    service_port: ";
    private static final String REPLICA      = "    replica:";
    private static final String REPLICA_HOST = "      - replica_host: ";
    private static final String REPLICA_PORT = "        replica_port: ";
    private static final String CLIENT_PROXY = "    client_proxy:";
    private static final String CLIENT_HOST  = "      - client_host: ";
    private static final String CLIENT_PORT  = "        client_port: ";
    private static final String PRINCIPAL_ID = "        principal_id: ";
    private static final int DEFAULT_PORT = 3501;

    public static final int CLIENT_PROXY_PER_NODE = 4;

    /** file path. */
    public final String configPath = "/concord/config-local/concord.config";

    /** persistence. */
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
    public Map<Integer, String> getConcordConfig(List<String> hostIps) {
        try {
            var result = new HashMap<Integer, String>();
            generateInputConfigYaml(hostIps, DEFAULT_PATH_YAML);

            var outputPath = Files.createTempDirectory(null);
            var configFuture = new ProcessBuilder("/app/conc_genconfig",
                    "--configuration-input",
                    DEFAULT_PATH_YAML)
                    .directory(outputPath.toFile())
                    .start()
                    .onExit();

            var work = configFuture.whenCompleteAsync((process, error) -> {
                if (error == null) {
                    try {
                        for (int num = 0; num < hostIps.size(); num++) {
                            var path = outputPath.resolve("concord" + (num + 1) + ".config");
                            result.put(num, Files.readString(path));

                            // TODO: Fill nodePrincipal from config output once available
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
    boolean generateInputConfigYaml(List<String> hostIps, String configYamlPath) {
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
        return generateInputConfigYaml(hostIps, fVal, cVal, configYamlPath);
    }

    /**
     * Utility method for generating input config yaml file.
     */
    boolean generateInputConfigYaml(List<String> hostIp, int fVal, int cVal, String configYamlPath) {
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
            writer.write(CLIENT_PROXY_PER_REPLICA + CLIENT_PROXY_PER_NODE);
            writer.newLine();
            writer.write(C_VAL + cVal);
            writer.newLine();
            writer.write(F_VAL + fVal);
            writer.newLine();
            writer.write("comm_to_use: udp");
            writer.newLine();
            writer.write("tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384");
            writer.newLine();
            writer.write("tls_certificates_folder_path: "
                    + CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH);
            writer.newLine();
            writer.write("node__TEMPLATE:\n  logger_config: /concord/config-local/log4cplus.properties\n"
                    + "  genesis_block: /concord/config-public/genesis.json\n  "
                    + "blockchain_db_path: /concord/rocksdbdata/");
            writer.newLine();
            writer.write(NODE);
            writer.newLine();
            for (int i = 0; i < hostIp.size(); i++) {
                writer.write(SERVICE_HOST + "0.0.0.0");
                writer.newLine();
                writer.write(SERVICE_PORT + "5458");
                writer.newLine();
                writer.write(REPLICA);
                writer.newLine();
                writer.write(REPLICA_HOST + hostIp.get(i));
                writer.newLine();
                writer.write(REPLICA_PORT + DEFAULT_PORT);
                writer.newLine();
                writer.write(CLIENT_PROXY);
                writer.newLine();

                for (int j = 0; j < CLIENT_PROXY_PER_NODE; j++) {
                    writer.write(CLIENT_HOST + hostIp.get(i));
                    writer.newLine();
                    writer.write(CLIENT_PORT + (DEFAULT_PORT + j + 1));
                    writer.newLine();
                }
                nodePrincipal.put(i, new ArrayList<>(Arrays.asList(5, 10, 15, 17)));
            }
            writer.flush();
            writer.close();
            return true;
        } catch (IOException x) {
            log.error("IOException: %s%n", x);
            return false;
        }
    }
}
