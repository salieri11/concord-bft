/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.NodeProperty;

/**
 * Utility class for generating the generic configurations irrespective node type.
 */
public class GenericConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(GenericConfigUtil.class);

    private String identifiersTemplatePath;

    public GenericConfigUtil(String identifiersTemplatePath) {
        this.identifiersTemplatePath = identifiersTemplatePath;
    }

    /**
     * generic config path.
     */
    public static String configPath = "/generic/identifiers.env";

    /**
     * Generates the generic config file with node ids.
     * @param nodeProperties Map of node properties.
     * @return map of host ips vs configs.
     */
    public Map<Integer, String> getGenericConfig(List<NodeProperty> nodeProperties) {

        Map<Integer, String> configMap = new HashMap<>();

        String content = "";
        try {
            InputStream inputStream = new FileInputStream(identifiersTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.error("File {} does not exist: {}\n Using localized identifier config input template",
                    identifiersTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("IdentifiersTemplate.env").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.error("Identifier config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        Map<Integer, String> clientGroupIds = new HashMap<>();
        Map<Integer, String> nodeIds = new HashMap<>();

        nodeProperties.stream().forEach(nodeProperty -> {
            switch (nodeProperty.getName()) {
                case CLIENT_GROUP_ID:
                    clientGroupIds.putAll(nodeProperty.getValueMap());
                    break;
                case NODE_ID:
                    nodeIds.putAll(nodeProperty.getValueMap());
                    break;
                default:
                    log.debug("property {} not relevant for generic", nodeProperty.getName());
            }
        });

        String finalContent = content;
        nodeIds.forEach((node, value) -> {
            var localContent = finalContent
                    .replace("$NODEID", value.toString())
                    .replace("$CLIENTGROUPID", clientGroupIds.getOrDefault(node, value));
            configMap.put(node, localContent);
        });

        return configMap;
    }

}
