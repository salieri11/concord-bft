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
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Utility class to generate wavefront configurations.
 */
public class WavefrontConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(WavefrontConfigUtil.class);

    private String wavefrontTemplatePath;

    /**
     * wavefront config path.
     */
    public static final String configPath = "/wavefront-proxy/wavefront.conf";

    public WavefrontConfigUtil(String wavefrontTemplatePath) {
        this.wavefrontTemplatePath = wavefrontTemplatePath;
    }

    /**
     * Generate wavefront configurations.
     * @param properties raw properties.
     * @return wavefront configuration file contents.
     */
    public Map<Integer, String> getWavefrontConfig(Properties properties, List<String> hostIps) {
        Map<Integer, String> configMap = new HashMap<>();
        String content = "";
        try {
            InputStream inputStream = new FileInputStream(wavefrontTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.error("File {} does not exist: {}\n Using localized wavefront config input template",
                    wavefrontTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("wavefrontConfigTemplate.conf").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.error("Wavefront config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        content = content
                .replace("$BLOCKCHAIN_ID",
                        properties.getValuesOrDefault(NodeProperty.Name.BLOCKCHAIN_ID.toString(), ""))
                .replace("$SERVER",
                        properties.getValuesOrDefault(NodeProperty.Name.WAVEFRONT_URL.toString(), ""))
                .replace("$TOKEN",
                        properties.getValuesOrDefault(NodeProperty.Name.WAVEFRONT_TOKEN.toString(), ""));

        if (properties.getValuesMap().containsKey(NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString())) {
            content = content.replace("#proxyHost=localhost",
                    "proxyHost=" + properties.getValuesOrDefault(
                            NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString(), ""))
                    .replace("#proxyPort=8080",
                    "proxyPort=" + properties.getValuesOrThrow(
                            NodeProperty.Name.WAVEFRONT_PROXY_PORT.toString()));
            if (properties.getValuesMap().containsKey(NodeProperty.Name.WAVEFRONT_PROXY_USER.toString())) {
                content = content.replace("#proxyUser=$USER",
                        "proxyUser=" + properties.getValuesOrDefault(
                                NodeProperty.Name.WAVEFRONT_PROXY_HOST.toString(), ""))
                        .replace("#proxyPassword=$PASSWORD",
                                "proxyPassword=" + properties.getValuesOrThrow(
                                        NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD.toString()));
            }
        }

        for (int num = 0; num < hostIps.size(); num++) {
            String hostConfigCopy = content.replace("$HOSTNAME", hostIps.get(num));
            configMap.put(num, hostConfigCopy);
        }

        return configMap;
    }

}
