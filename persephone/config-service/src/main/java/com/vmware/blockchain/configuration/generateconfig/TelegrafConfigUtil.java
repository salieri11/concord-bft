/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.ELASTICSEARCH_PWD;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.ELASTICSEARCH_URL;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.ELASTICSEARCH_USER;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;

/**
 * Utility class to generate telegraf configurations.
 */
public class TelegrafConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(TelegrafConfigUtil.class);

    private String telegrafTemplatePath;

    public TelegrafConfigUtil(String telegrafTemplatePath) {
        this.telegrafTemplatePath = telegrafTemplatePath;
    }

    /**
     * telegraf config path.
     */
    public static final String configPath = "/telegraf/telegraf.conf";

    /**
     * Generate telegraf configurations.
     * @param nodeProperties node properties
     * @param properties raw properties.
     * @param servicesList services list.
     * @return map of host ips vs configs.
     */
    @Deprecated
    public Map<Integer, String> getTelegrafConfig(List<NodeProperty> nodeProperties,
                                                  Properties properties,
                                                  List<ConcordComponent.ServiceType> servicesList) {

        Map<Integer, String> configMap = new HashMap<>();

        String content = "";
        try {
            InputStream inputStream = new FileInputStream(telegrafTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized telegraf config input template",
                    telegrafTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("TelegrafConfigTemplate.conf").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.warn("Telegraf config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        var prometheusUrlList = getPrometheusUrls(servicesList);
        String prometheusUrls = "";
        if (!prometheusUrlList.isEmpty()) {
            prometheusUrls = String.join(",", prometheusUrlList);
        }

        content = content
                .replace("$BLOCKCHAIN_ID", properties.getValuesMap()
                        .getOrDefault(NodeProperty.Name.BLOCKCHAIN_ID.toString(), ""))
                .replace("$CONSORTIUM_ID", properties.getValuesMap()
                        .getOrDefault(NodeProperty.Name.CONSORTIUM_ID.toString(), ""))
                .replace("$URL", "[" + prometheusUrls + "]");

        Map<Integer, String> nodeIps = new HashMap<>();
        Map<Integer, String> nodeIds = new HashMap<>();

        // elasticsearch data
        Map<Integer, String> esUrls = new HashMap<>();
        Map<Integer, String> esUsername = new HashMap<>();
        Map<Integer, String> espassword = new HashMap<>();

        String postgressPluginStr = "#[[inputs.postgresql]]";
        String indexDbInput = "address = \"postgres://indexdb@daml_index_db/$DBNAME\"";

        nodeProperties.stream().forEach(nodeProperty -> {
            switch (nodeProperty.getName()) {
                case NODE_IP:
                    nodeIps.putAll(nodeProperty.getValueMap());
                    break;
                case NODE_ID:
                    nodeIds.putAll(nodeProperty.getValueMap());
                    break;
                case ELASTICSEARCH_URL:
                    esUrls.putAll(nodeProperty.getValueMap());
                    break;
                case ELASTICSEARCH_USER:
                    esUsername.putAll(nodeProperty.getValueMap());
                    break;
                case ELASTICSEARCH_PWD:
                    espassword.putAll(nodeProperty.getValueMap());
                    break;
                default:
                    log.debug("property {} not relevant for telegraf", nodeProperty.getName());
            }
        });

        // TODO : remove after concord name unification
        List<ConcordComponent.ServiceType> committerList = List.of(
                ConcordComponent.ServiceType.CONCORD,
                ConcordComponent.ServiceType.DAML_CONCORD,
                ConcordComponent.ServiceType.HLF_CONCORD);

        for (Map.Entry<Integer, String> nodeIp : nodeIps.entrySet()) {
            String hostConfigCopy = content.replace("$REPLICA", nodeIp.getValue());

            if (servicesList.contains(ConcordComponent.ServiceType.DAML_INDEX_DB)) {
                var nodeName = DamlLedgerApiUtil.convertToParticipantId(nodeIds.get(nodeIp.getKey()));
                String indexDbAddr = indexDbInput.replace("$DBNAME", nodeName);
                String postgressPlugin = postgressPluginStr.replace("#", "");
                hostConfigCopy = hostConfigCopy
                        .replace("#$DBINPUT", indexDbAddr)
                        .replace(postgressPluginStr, postgressPlugin);
            }

            if (servicesList.stream().anyMatch(element -> committerList.contains(element))) {
                hostConfigCopy = hostConfigCopy.replace("$VMTYPE", "committer");
            } else {
                hostConfigCopy = hostConfigCopy.replace("$VMTYPE", "client");
            }

            if (!esUrls.isEmpty()) {
                String url = esUrls.get(nodeIp.getKey());
                String username = esUsername.getOrDefault(nodeIp.getKey(), "");
                String pwd = espassword.getOrDefault(nodeIp.getKey(), "");

                hostConfigCopy = hostConfigCopy.concat("\n\n" + getElasticsearchConfig(url, username, pwd,
                        properties.getValuesMap().getOrDefault(
                                NodeProperty.Name.BLOCKCHAIN_ID.toString(), "vmware-blockchain")));
            }
            configMap.put(nodeIp.getKey(), hostConfigCopy);
        }

        return configMap;
    }

    /**
     * Generate telegraf configurations.
     * @param consortiumId id
     * @param blockchainId id
     * @param nodeInfo info
     * @param servicesList components
     * @return config
     */
    public String getTelegrafConfig(String consortiumId, String blockchainId, NodesInfo.Entry nodeInfo,
                                                  List<ConcordComponent.ServiceType> servicesList) {

        String content = "";
        try {
            InputStream inputStream = new FileInputStream(telegrafTemplatePath);
            content = new String(inputStream.readAllBytes());
        } catch (IOException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized telegraf config input template",
                     telegrafTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            try {
                File file = new File(classLoader.getResource("TelegrafConfigTemplate.conf").getFile());
                content = new String(Files.readAllBytes(file.toPath()));
            } catch (IOException | NullPointerException ex) {
                log.warn("Telegraf config could not be read due to: {}", ex.getLocalizedMessage());
                return null;
            }
        }

        var prometheusUrlList = getPrometheusUrls(servicesList);
        String prometheusUrls = "";
        if (!prometheusUrlList.isEmpty()) {
            prometheusUrls = String.join(",", prometheusUrlList);
        }

        content = content
                .replace("$BLOCKCHAIN_ID", blockchainId)
                .replace("$CONSORTIUM_ID", consortiumId)
                .replace("$URL", "[" + prometheusUrls + "]");

        String postgressPluginStr = "#[[inputs.postgresql]]";
        String indexDbInput = "address = \"postgres://indexdb@daml_index_db/$DBNAME\"";

        // TODO : remove after concord name unification
        List<ConcordComponent.ServiceType> committerList = List.of(
                ConcordComponent.ServiceType.CONCORD,
                ConcordComponent.ServiceType.DAML_CONCORD,
                ConcordComponent.ServiceType.HLF_CONCORD);

        String hostConfigCopy = content.replace("$REPLICA", nodeInfo.getNodeIp());

        if (servicesList.contains(ConcordComponent.ServiceType.DAML_INDEX_DB)) {
            var nodeName = DamlLedgerApiUtil.convertToParticipantId(nodeInfo.getId());
            String indexDbAddr = indexDbInput.replace("$DBNAME", nodeName);
            String postgressPlugin = postgressPluginStr.replace("#", "");
            hostConfigCopy = hostConfigCopy
                    .replace("#$DBINPUT", indexDbAddr)
                    .replace(postgressPluginStr, postgressPlugin);
        }

        if (servicesList.stream().anyMatch(element -> committerList.contains(element))) {
            hostConfigCopy = hostConfigCopy.replace("$VMTYPE", "committer");
        } else {
            hostConfigCopy = hostConfigCopy.replace("$VMTYPE", "client");
        }

        var esUrl = nodeInfo.getProperties().getValuesOrDefault(ELASTICSEARCH_URL.name(), "");
        if (!esUrl.isEmpty()) {
            String username = nodeInfo.getProperties().getValuesOrDefault(ELASTICSEARCH_USER.name(), "");
            String pwd = nodeInfo.getProperties().getValuesOrDefault(ELASTICSEARCH_PWD.name(), "");

            hostConfigCopy = hostConfigCopy.concat("\n\n" + getElasticsearchConfig(esUrl, username, pwd, blockchainId));
        }

        return hostConfigCopy;
    }

    private String getElasticsearchConfig(String urls, String username, String password, String blockchain) {
        StringBuilder config = new StringBuilder()
                .append("[[outputs.elasticsearch]]\n")
                .append("  urls = [ \"")
                .append(urls)
                .append("\" ]\n  timeout = \"5s\"")
                .append("\n  enable_sniffer = false")
                .append("\n  health_check_interval = \"10s\"");

        if (!username.isBlank()) {
            config.append("\n  username = \"")
                    .append(username)
                    .append("\"");
        }

        if (!password.isBlank()) {
            config.append("\n  password = \"")
                    .append(password)
                    .append("\"");
        }

        config.append("\n  index_name = \"" + blockchain + "\"")
                .append("\n  manage_template = true")
                .append("\n  template_name = \"vmware-blockchain\"")
                .append("\n  overwrite_template = false");

        return config.toString();
    }

    private List<String> getPrometheusUrls(List<ConcordComponent.ServiceType> servicesList) {
        List<String> prometheusUrls = new ArrayList<>();

        // TODO change to switch case once concord name is unified.
        if (servicesList.contains(ConcordComponent.ServiceType.DAML_CONCORD)
                || servicesList.contains(ConcordComponent.ServiceType.CONCORD)
                || servicesList.contains(ConcordComponent.ServiceType.HLF_CONCORD)) {
            prometheusUrls.add("\"http://concord:9891/metrics\"");
        }

        if (servicesList.contains(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE)) {
            prometheusUrls.add("\"http://daml_execution_engine:55001/metrics\"");
        }

        if (servicesList.contains(ConcordComponent.ServiceType.DAML_LEDGER_API)) {
            prometheusUrls.add("\"http://daml_ledger_api:55001/metrics\"");
        }
        return prometheusUrls;
    }
}
