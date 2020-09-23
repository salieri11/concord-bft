/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.WAVEFRONT_PROXY_HOST;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.WAVEFRONT_PROXY_PASSWORD;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.WAVEFRONT_PROXY_PORT;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.WAVEFRONT_PROXY_USER;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.WAVEFRONT_TOKEN;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.WAVEFRONT_URL;

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
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Utility class to generate wavefront configurations.
 */
public class WavefrontConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(WavefrontConfigUtil.class);

    private String wavefrontTemplatePath;

    public WavefrontConfigUtil(String wavefrontTemplatePath) {
        this.wavefrontTemplatePath = wavefrontTemplatePath;
    }

    /**
     * Generate wavefront configurations.
     *
     * @param properties raw properties.
     * @return wavefront configuration file contents.
     */
    @Deprecated
    public Map<Integer, String> getWavefrontConfig(Properties properties,
                                                   List<NodeProperty> nodeProperties) {
        Map<Integer, String> configMap = new HashMap<>();
        String content = "";
        try {
            InputStream inputStream = new FileInputStream(wavefrontTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized wavefront config input template",
                     wavefrontTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("wavefrontConfigTemplate.conf").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.warn("Wavefront config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        Map<Integer, String> wfUrls = new HashMap<>();
        Map<Integer, String> wfTokens = new HashMap<>();
        Map<Integer, String> wfProxyUrls = new HashMap<>();
        Map<Integer, String> wfProxyPorts = new HashMap<>();
        Map<Integer, String> wfProxyUsername = new HashMap<>();
        Map<Integer, String> wfProxyPwd = new HashMap<>();
        Map<Integer, String> nodeIps = new HashMap<>();

        nodeProperties.stream().forEach(nodeProperty -> {
            switch (nodeProperty.getName()) {
                case WAVEFRONT_PROXY_HOST:
                    wfProxyUrls.putAll(nodeProperty.getValueMap());
                    break;
                case WAVEFRONT_PROXY_PORT:
                    wfProxyPorts.putAll(nodeProperty.getValueMap());
                    break;
                case WAVEFRONT_PROXY_USER:
                    wfProxyUsername.putAll(nodeProperty.getValueMap());
                    break;
                case WAVEFRONT_PROXY_PASSWORD:
                    wfProxyPwd.putAll(nodeProperty.getValueMap());
                    break;
                case NODE_IP:
                    nodeIps.putAll(nodeProperty.getValueMap());
                    break;
                case WAVEFRONT_URL:
                    wfUrls.putAll(nodeProperty.getValueMap());
                    break;
                case WAVEFRONT_TOKEN:
                    wfTokens.putAll(nodeProperty.getValueMap());
                    break;
                default:
                    log.debug("property {} not relevant for wavefront", nodeProperty.getName());
            }
        });

        content = content
                .replace("$BLOCKCHAIN_ID",
                         properties.getValuesOrDefault(NodeProperty.Name.BLOCKCHAIN_ID.toString(), ""));

        for (Map.Entry<Integer, String> nodeIp : nodeIps.entrySet()) {
            String hostConfigCopy = content
                    .replace("$HOSTNAME", nodeIp.getValue())
                    .replace("$SERVER", wfUrls.getOrDefault(nodeIp.getKey(), ""))
                    .replace("$TOKEN", wfTokens.getOrDefault(nodeIp.getKey(), ""));
            if (!wfProxyUrls.isEmpty()) {
                hostConfigCopy = hostConfigCopy.replace("#proxyHost=localhost",
                                                        "proxyHost=" + wfProxyUrls.get(nodeIp.getKey()))
                        .replace("#proxyPort=8080",
                                 "proxyPort=" + wfProxyPorts.get(nodeIp.getKey()));
                if (!wfProxyUsername.isEmpty()) {
                    hostConfigCopy = hostConfigCopy.replace("#proxyUser=$USER",
                                                            "proxyUser=" + wfProxyUsername.get(nodeIp.getKey()))
                            .replace("#proxyPassword=$PASSWORD",
                                     "proxyPassword=" + wfProxyPwd.get(nodeIp.getKey()));
                }
            }

            // TODO : negative scenarios not covered to throw exception by design. Needs impl.

            configMap.put(nodeIp.getKey(), hostConfigCopy);
        }

        return configMap;
    }

    /**
     * Generate wavefront configurations.
     */
    public String getWavefrontConfig(String blockchainId, NodesInfo.Entry nodeInfo) {
        String content = "";
        try {
            InputStream inputStream = new FileInputStream(wavefrontTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized wavefront config input template",
                     wavefrontTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("wavefrontConfigTemplate.conf").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.warn("Wavefront config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        content = content.replace("$BLOCKCHAIN_ID", blockchainId);

        String hostConfigCopy = content
                .replace("$HOSTNAME", nodeInfo.getNodeIp())
                .replace("$SERVER", nodeInfo.getProperties().getValuesOrDefault(WAVEFRONT_URL.name(), ""))
                .replace("$TOKEN", nodeInfo.getProperties().getValuesOrDefault(WAVEFRONT_TOKEN.name(), ""));
        var proxyUrl = nodeInfo.getProperties().getValuesOrDefault(WAVEFRONT_PROXY_HOST.name(), "");
        if (!proxyUrl.isEmpty()) {
            hostConfigCopy = hostConfigCopy.replace("#proxyHost=localhost",
                                                    "proxyHost=" + proxyUrl)
                    .replace("#proxyPort=8080",
                             "proxyPort=" + nodeInfo.getProperties()
                                     .getValuesOrDefault(WAVEFRONT_PROXY_PORT.name(), ""));
            var wfProxyUsername = nodeInfo.getProperties().getValuesOrDefault(WAVEFRONT_PROXY_USER.name(), "");
            if (!wfProxyUsername.isEmpty()) {
                hostConfigCopy = hostConfigCopy.replace("#proxyUser=$USER",
                                                        "proxyUser=" + wfProxyUsername)
                        .replace("#proxyPassword=$PASSWORD",
                                 "proxyPassword=" + nodeInfo.getProperties()
                                         .getValuesOrDefault(WAVEFRONT_PROXY_PASSWORD.name(), ""));
            }
        }
        return hostConfigCopy;
    }
}