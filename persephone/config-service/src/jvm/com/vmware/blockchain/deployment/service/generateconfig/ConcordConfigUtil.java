/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.generateconfig;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.service.util.Constants;

/**
 * Utility class for generating the input for Configuration Yaml file.
 * TODO: Looked at Snake YAML package which is a much cleaner way but
 *       requires lot of unnecessary data classes to be developed.
 *       This approach is simpler and generates much less code.
 */
public class ConcordConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(ConcordConfigUtil.class);

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
     * Utility method for generating the file.
     */
    public String generateConfigUtil(List<String> hostIp) {
        if (hostIp == null) {
            log.error("generateConfigUtil: List of host IP provided is NULL!");
            return "";
        }
        if (hostIp.size() < 4) {
            log.error("generateConfigUtil: Minimum cluster size is 4!");
            return "";
        }
        int clusterSize = hostIp.size();
        int fVal = getFVal(clusterSize);
        int cVal = getCVal(clusterSize, fVal);
        return generateConfigUtil(hostIp, fVal, cVal);
    }

    /**
     * Utility method for generating the file.
     */
    String generateConfigUtil(List<String> hostIp, int fVal, int cVal) {
        if (hostIp == null) {
            log.error("generateConfigUtil: List of host IP provided is NULL!");
            return "";
        }
        if (hostIp.size() < 4) {
            log.error("generateConfigUtil: Minimum cluster size is 4!");
            return "";
        }
        if ((3 * fVal + 2 * cVal + 1) > hostIp.size()) {
            log.error("generateConfigUtil: fVal / cVal are invalid for the list of host IP provided");
            return "";
        }

        maxPrincipalId = (hostIp.size() + Constants.CLIENT_PROXY_PER_NODE * hostIp.size()) - 1;
        StringBuilder configString = new StringBuilder();
        configString.append(CLIENT_PROXY_PER_REPLICA + Constants.CLIENT_PROXY_PER_NODE);
        configString.append("\n");
        configString.append(C_VAL + cVal);
        configString.append("\n");
        configString.append(F_VAL + fVal);
        configString.append("\n");
        configString.append("comm_to_use: udp");
        configString.append("\n");
        configString.append("tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384");
        configString.append("\n");
        configString.append("tls_certificates_folder_path: " + Constants.TLS_IDENTITY_PATH);
        configString.append("\n");
        configString.append("node__TEMPLATE:\n  logger_config: /concord/config-local/log4cplus.properties\n"
                + "  genesis_block: /concord/config-public/genesis.json\n  blockchain_db_path: /concord/rocksdbdata/");
        configString.append("\n");
        configString.append(NODE);
        configString.append("\n");
        for (int i = 0; i < hostIp.size(); i++) {
            configString.append(SERVICE_HOST + "0.0.0.0");
            configString.append("\n");
            configString.append(SERVICE_PORT + "5458");
            configString.append("\n");
            configString.append(REPLICA);
            configString.append("\n");
            configString.append(REPLICA_HOST + hostIp.get(i));
            configString.append("\n");
            configString.append(REPLICA_PORT + DEFAULT_PORT);
            configString.append("\n");
            configString.append(CLIENT_PROXY);
            configString.append("\n");

            List<Integer> principalList = new ArrayList<>();
            for (int j = 0; j < Constants.CLIENT_PROXY_PER_NODE; j++) {
                configString.append(CLIENT_HOST + hostIp.get(i));
                configString.append("\n");
                configString.append(CLIENT_PORT + (DEFAULT_PORT + j + 1));
                configString.append("\n");

                int principalId = maxPrincipalId - i - j; // could be randomized later
                principalList.add(principalId);

                configString.append(PRINCIPAL_ID + principalId);
                configString.append("\n");
            }
            nodePrincipal.put(i, principalList);
        }

        return configString.toString();
    }
}
