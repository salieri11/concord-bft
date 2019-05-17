/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
 
/**
 * Utility class for generating the input for Configuration Yaml file.
 * TODO: Looked at Snake YAML package which is a much cleaner way but
 *       requires lot of unnecessary data classes to be developed.
 *       This approach is simpler and generates much less code.
 */
public final class ConfigYaml  {

    private static final Logger log = LoggerFactory.getLogger(ConfigYaml.class);

    private static final String DEFAULT_PATH_YAML = "/config/dockerConfigurationInput.yaml";
    private static final String CLUSTER_SIZE = "client_proxies_per_replica: ";
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
    private static final int DEFAULT_PORT = 3501;
    private final String configYamlFilePath;
    

    /**
     * Empty constructor.
     */
    public ConfigYaml() {
        this(DEFAULT_PATH_YAML);
    }

    /**
     * Constructor with configpath specified.
     */
    public ConfigYaml(String path) {
        configYamlFilePath = path;
    }

    public String getConfigYamlFilePath() {
        return configYamlFilePath;
    }

    public static int getFVal(int clusterSize) {
        int realSize = clusterSize - 1;
        int fSize = realSize / 3;
        // if (realSize % 3 == 0 && fSize > 1) {
        //     fSize--;
        // }
        return fSize;
    }

    public static int getCVal(int clusterSize, int fVal) {
        //return ((clusterSize-1) - 3*fVal )/ 2;
        return 0;
    }

    /**
     * Utility method for generating the file.
     */
    public boolean generateConfigUtil(List<String> hostIp) {
        if (hostIp == null) {
            log.error("generateConfigUtil: List of host IP provided is NULL!");
            return false;
        }
        if (hostIp.size() < 4) {
            log.error("generateConfigUtil: Minimum cluster size is 4!");
            return false;
        }
        int clusterSize = hostIp.size();
        int f_val = getFVal(clusterSize);
        int c_val = getCVal(clusterSize, f_val);
        return generateConfigUtil(hostIp, f_val, c_val);
    }
    
    /**
     * Utility method for generating the file.
     */
    public boolean generateConfigUtil(List<String> hostIp, int f_val, int c_val) {
        if (hostIp == null) {
            log.error("generateConfigUtil: List of host IP provided is NULL!");
            return false;
        }
        if (hostIp.size() < 4) {
            log.error("generateConfigUtil: Minimum cluster size is 4!");
            return false;
        }
        if ((3*f_val + 2*c_val + 1) > hostIp.size()) {
            log.error("generateConfigUtil: f_val / c_val are invalid for the list of host IP provided");
            return false;
        }

        Path path = Paths.get(configYamlFilePath);
        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            writer.write(CLUSTER_SIZE + hostIp.size());
            writer.newLine();
            writer.write(C_VAL + c_val);
            writer.newLine();
            writer.write(F_VAL + f_val);
            writer.newLine();
            writer.write("comm_to_use: udp");
            writer.newLine();
            writer.write("tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384");
            writer.newLine();
            writer.write("tls_certificates_folder_path: /concord/config-local/cert");
            writer.newLine();
            writer.write("node__TEMPLATE:\n  logger_config: /concord/config-local/log4cplus.properties\n"
                + "  genesis_block: /concord/config-public/genesis.json\n  blockchain_db_path: /concord/rocksdbdata/");
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
                for (int j = 0; j < hostIp.size(); j++) {
                    writer.write(CLIENT_HOST + hostIp.get(i));
                    writer.newLine();
                    writer.write(CLIENT_PORT + (DEFAULT_PORT + j + 1));
                    writer.newLine();
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
}
