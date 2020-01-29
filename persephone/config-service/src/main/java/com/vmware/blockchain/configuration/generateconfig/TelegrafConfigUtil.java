/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.Yaml;

/**
 * Utility class to generate telegraf configurations.
 */
public class TelegrafConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(TelegrafConfigUtil.class);

    private String telegrafTemplatePath;

    private String metricsConfigYamlPath;

    public TelegrafConfigUtil(String telegrafTemplatePath, String metricsConfigYamlPath) {
        this.telegrafTemplatePath = telegrafTemplatePath;
        this.metricsConfigYamlPath = metricsConfigYamlPath;
    }

    /**
     * telegraf config path.
     */
    public final String configPath = "/telegraf/telegraf.conf";

    /**
     * metrics config yaml path.
     */
    public static final String metricsConfigPath = "/concord/config-public/metrics_config.yaml";

    /**
     * Generate telegraf configurations.
     * @param hostIps hopst names/ips.
     * @return map of host ips vs configs.
     */
    public Map<Integer, String> getTelegrafConfig(List<String> hostIps) {

        Map<Integer, String> configMap = new HashMap<>();

        String content = "";
        try {
            InputStream inputStream = new FileInputStream(telegrafTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.error("File {} does not exist: {}\n Using localized telegraf config input template",
                    telegrafTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("TelegrafConfigTemplate.config").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.error("Telegraf config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        // FIXME: This could ideally be hostIps -> config once we remove dependency on list ordering.
        for (int num = 0; num < hostIps.size(); num++) {
            String hostConfigCopy = content.replace("$REPLICA", hostIps.get(num));
            configMap.put(num, hostConfigCopy);
        }

        return configMap;
    }

    /**
     * Get metrics config yaml.
     * @return String representation of metricsConfigYaml
     */
    public String getMetricsConfigYaml() {

        Yaml yaml = new Yaml();
        Map<String, Object> metricsConfig;
        try {
            metricsConfig = yaml.load(new FileInputStream(metricsConfigYamlPath));
        } catch (FileNotFoundException e) {
            // For unit tests only.
            log.error("File {} does not exist: {}\n Using localized metrics config yaml",
                    metricsConfigYamlPath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            metricsConfig = yaml.load(classLoader.getResourceAsStream("MetricsConfig.yaml"));
        }

        StringWriter writer = new StringWriter();
        yaml.dump(metricsConfig, writer);

        return writer.toString();
    }
}
