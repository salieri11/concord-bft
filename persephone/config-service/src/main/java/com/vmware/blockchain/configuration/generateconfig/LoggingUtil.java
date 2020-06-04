/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.LOGGING_CONFIG;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.util.JsonFormat;
import com.vmware.blockchain.deployment.v1.LogManagement;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Utility class for generating the config(s) for Fluentd file.
 */
public class LoggingUtil {

    private static final Logger log = LoggerFactory.getLogger(LoggingUtil.class);

    private String loggingEnvTemplatePath;

    /**
     * file path.
     */
    public static final String envVarPath = "/fluentd/logging.env";

    public LoggingUtil(String loggingEnvTemplatePath) {
        this.loggingEnvTemplatePath = loggingEnvTemplatePath;
    }

    /**
     * Generates the logging config environment variable file. This would hold as spaceholder for any logging, metrics
     * and tracing g related configuration files. The method signature is subject to change.
     *
     * @param nodeProperties Map of node properties.
     * @return environment variables as json file.
     */
    @Deprecated
    public Map<Integer, String> generateConfig(List<NodeProperty> nodeProperties,
                                               Properties properties) {

        Map<Integer, String> configMap = new HashMap<>();

        String content = "";
        try {
            InputStream inputStream = new FileInputStream(loggingEnvTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized logging config input template",
                     loggingEnvTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("LoggingTemplate.env").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.warn("Logging config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        var consortiumId = properties.getValuesMap().getOrDefault(
                NodeProperty.Name.CONSORTIUM_ID.toString(), "");
        Map<Integer, String> nodeMap = new HashMap<>();
        Map<Integer, String> loggingMap = new HashMap<>();
        for (NodeProperty nodeProperty : nodeProperties) {
            switch (nodeProperty.getName()) {
                case NODE_ID:
                    nodeMap.putAll(nodeProperty.getValueMap());
                    break;
                case LOGGING_CONFIG:
                    loggingMap.putAll(nodeProperty.getValueMap());
                    break;
                default:
                    break;
            }
        }

        for (Map.Entry<Integer, String> entry : loggingMap.entrySet()) {
            LogManagement.Builder builder = LogManagement.newBuilder();
            try {
                JsonFormat.parser()
                        .ignoringUnknownFields().merge(entry.getValue(), builder);
                var logManagement = builder.build();

                String[] addresses = logManagement.getEndpoint().getAddress().split(":");
                String hostName;
                String port = "";

                try {
                    Integer portInt = Integer.parseInt(addresses[addresses.length - 1]);
                    port = portInt.toString();
                    hostName = String.join(":", Arrays.copyOfRange(addresses, 0, addresses.length - 1));
                } catch (NumberFormatException ex) {
                    hostName = logManagement.getEndpoint().getAddress();
                }

                var localContent = content
                        .replace("$consortiumId", consortiumId)
                        .replace("$replicaId", nodeMap.get(entry.getKey()))
                        .replace("$logDestination", logManagement.getDestination().name())
                        .replace("$lintAuthorizationBearer", logManagement.getEndpoint()
                                .getCredential().getTokenCredential().getToken())
                        .replace("$lintEndpointUrl", hostName)
                        .replace("$logInsightHost", hostName)
                        .replace("$logInsightPort", port)
                        .replace("$logInsightUsername", logManagement.getEndpoint()
                                .getCredential().getPasswordCredential().getUsername())
                        .replace("$logInsightPassword", logManagement.getEndpoint()
                                .getCredential().getPasswordCredential().getPassword())
                        .replace("$logInsightAgentId", String.valueOf(logManagement.getLogInsightAgentId()));

                configMap.put(entry.getKey(), localContent);
            } catch (InvalidProtocolBufferException | NullPointerException e) {
                log.error("Invalid protobuf or node uuid not present: {}", e.getLocalizedMessage());
            }
        }
        return configMap;
    }

    /**
     * Generates the logging config environment variable file. This would hold as spaceholder for any logging, metrics
     * and tracing g related configuration files. The method signature is subject to change.
     */
    public String generateConfig(String consortiumId, String blockchainId, NodesInfo.Entry nodeInfo) {

        String content = "";
        try {
            InputStream inputStream = new FileInputStream(loggingEnvTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized logging config input template",
                     loggingEnvTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("LoggingTemplate.env").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.warn("Logging config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        LogManagement.Builder builder = LogManagement.newBuilder();
        try {
            JsonFormat.parser()
                    .ignoringUnknownFields()
                    .merge(nodeInfo.getProperties().getValuesOrDefault(LOGGING_CONFIG.name(), ""), builder);
            var logManagement = builder.build();

            String[] addresses = logManagement.getEndpoint().getAddress().split(":");
            String hostName;
            String port = "";

            try {
                Integer portInt = Integer.parseInt(addresses[addresses.length - 1]);
                port = portInt.toString();
                hostName = String.join(":", Arrays.copyOfRange(addresses, 0, addresses.length - 1));
            } catch (NumberFormatException ex) {
                hostName = logManagement.getEndpoint().getAddress();
            }

            var localContent = content
                    .replace("$consortiumId", consortiumId)
                    .replace("$replicaId", nodeInfo.getId())
                    .replace("$logDestination", logManagement.getDestination().name())
                    .replace("$lintAuthorizationBearer", logManagement.getEndpoint()
                            .getCredential().getTokenCredential().getToken())
                    .replace("$lintEndpointUrl", hostName)
                    .replace("$logInsightHost", hostName)
                    .replace("$logInsightPort", port)
                    .replace("$logInsightUsername", logManagement.getEndpoint()
                            .getCredential().getPasswordCredential().getUsername())
                    .replace("$logInsightPassword", logManagement.getEndpoint()
                            .getCredential().getPasswordCredential().getPassword())
                    .replace("$logInsightAgentId", String.valueOf(logManagement.getLogInsightAgentId()));
            return localContent;
        } catch (InvalidProtocolBufferException | NullPointerException e) {
            log.error("Invalid protobuf or node uuid not present: {}", e.getLocalizedMessage());
        }
        return "";
    }

}
