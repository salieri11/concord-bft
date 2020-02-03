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
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.Property;

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
     * @param propertyMap Map of node properties.
     * @return map of host ips vs configs.
     */
    public Map<Integer, String> getGenericConfig(Map<Property.Name, String> propertyMap) {

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
                File file = new File(classLoader.getResource("IdentifiersTemplate.config").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.error("Identifier config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        // Naive string representation support, might need rethinking at some point.
        String[] nodes = propertyMap.getOrDefault(Property.Name.NODE_ID, ";").split(";");

        for (String node : nodes) {
            var localContent = content.replace("$NODEID", node.split(":")[1]);
            localContent = localContent.replace("$CLIENTGROUPID",
                    propertyMap.getOrDefault(Property.Name.CLIENT_GROUP_ID, node.split(":")[1]));

            configMap.put(Integer.valueOf(node.split(":")[0]), localContent);
        }
        return configMap;
    }

}
