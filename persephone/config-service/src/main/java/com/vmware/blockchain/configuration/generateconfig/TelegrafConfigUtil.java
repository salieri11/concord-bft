/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.ELASTICSEARCH_PWD;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.ELASTICSEARCH_URL;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.ELASTICSEARCH_USER;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.TELEGRAF_PASSWORD;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.TELEGRAF_TLS_CERT;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.TELEGRAF_TLS_KEY;
import static com.vmware.blockchain.deployment.v1.NodeProperty.Name.TELEGRAF_USERNAME;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

/**
 * Utility class to generate telegraf configurations.
 */
public class TelegrafConfigUtil {

    // These get created on the VM under /config/telegraf/certs/prometheus_client/*
    public static final String TELEGRAF_PROMETHEUS_CLIENT_KEY_PATH = "/telegraf/certs/prometheus_client/telegraf.key";
    public static final String TELEGRAF_PROMETHEUS_CLIENT_CERT_PATH = "/telegraf/certs/prometheus_client/telegraf.crt";
    public static final String DAML_DB_DEFAULT_USERNAME = "indexdb";
    public static final String DAML_DB_DEFAULT_PASSWORD = "indexdb";

    private static final Logger log = LoggerFactory.getLogger(TelegrafConfigUtil.class);

    private String telegrafTemplatePath;

    public TelegrafConfigUtil(String telegrafTemplatePath) {
        this.telegrafTemplatePath = telegrafTemplatePath;
    }

    /**
     * Generate telegraf configurations.
     * @param consortiumId id
     * @param blockchainId id
     * @param nodeInfo info
     * @param nodeType Node type
     * @return config
     */
    public String getTelegrafConfig(String consortiumId, String blockchainId, NodesInfo.Entry nodeInfo,
                                    String nodeType) {
        if (!ValidationUtil.isValid(consortiumId) || !ValidationUtil.isValid(blockchainId) || !ValidationUtil
                .isValid(nodeInfo) || !ValidationUtil.isValid(nodeType)) {
            log.error(
                    "Essential input parameters are invalid. consortiumId {} blockchainId {} nodeInfo {} nodeType {}.",
                    consortiumId, blockchainId, nodeInfo, nodeType);
            throw new ConfigServiceException(ErrorCode.CONCORD_CONFIGURATION_INVALID_INPUT_FAILURE,
                                             "Invalid input parameters.");
        }

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
        List<ConcordComponent.ServiceType> servicesList = nodeInfo.getServicesList();
        // If no service is available, then log a message..
        if (servicesList == null || servicesList.isEmpty()) {
            log.error("No services available for this node with Id {} and IP {}." + nodeInfo.getId(),
                      nodeInfo.getNodeIp());
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

        // Enable/disable telegraf prometheus client output plugin
        String telegrafUsername = nodeInfo.getProperties().getValuesOrDefault(TELEGRAF_USERNAME.name(), null);
        String telegrafPassword = nodeInfo.getProperties().getValuesOrDefault(TELEGRAF_PASSWORD.name(), null);

        if (telegrafUsername == null) {
            log.info("telegraf username/password not available, not enabling telegraf prometheus client");
            content = content.replace("$ENABLE_TELEGRAF_PULL", "#");
        } else {
            log.info("Enabling telegraf prometheus client, setting telegraf user name to: {}", telegrafUsername);
            content = content
                    .replace("$ENABLE_TELEGRAF_PULL", "")
                    .replace("$TELEGRAF_USERNAME", telegrafUsername)
                    .replace("$TELEGRAF_PASSWORD", telegrafPassword);

            // Enable HTTPS for the pull endpoint if key+cert are provided by the user
            String telegrafTlsKey = nodeInfo.getProperties().getValuesOrDefault(TELEGRAF_TLS_KEY.name(), null);
            String telegrafTlsCert = nodeInfo.getProperties().getValuesOrDefault(TELEGRAF_TLS_CERT.name(), null);
            if (telegrafTlsKey == null || telegrafTlsCert == null) {
                content = content.replace("$ENABLE_TELEGRAF_TLS ", "#");
                log.info("telegraf tls info is not available. Not enabling HTTPS");
            }
            else {
                content = content.replace("$ENABLE_TELEGRAF_TLS ", "");
                log.info("telegraf tls info is available. Enabling HTTPS");
            }
            // *NOTE* : The actual contents (key and cert data) are set up for processing by the Agent into
            // /config/telegraf/certs by ConfigurationServiceHelper::nodeIndependentConfigs() for case TELEGRAF.
        }

        String prometeusUser = "";
        String prometeusPass = "";

        String postgressPluginStr = "#[[inputs.postgresql]]";
        String passwordFromNodeInfo =
                nodeInfo.getProperties().getValuesMap().get(NodeProperty.Name.DAML_DB_PASSWORD.name());
        String tag = nodeInfo.getProperties().getValuesMap().get(DeploymentAttributes.IMAGE_TAG.name());
        String password = buildDamlDbPasswordForTelegraf(passwordFromNodeInfo, tag);
        if (!password.equals("")) {
            prometeusUser = DAML_DB_DEFAULT_USERNAME;
            prometeusPass = password;
            password = ":" + password;
        }
        String indexDbInput = "address = \"postgres://indexdb" + password + "@daml_index_db/daml_ledger_api\"";

        String hostConfigCopy = content.replace("$REPLICA", nodeInfo.getNodeIp());

        if (servicesList != null && servicesList.contains(ConcordComponent.ServiceType.DAML_INDEX_DB)) {
            String postgressPlugin = postgressPluginStr.replace("#", "");
            hostConfigCopy = hostConfigCopy
                    .replace("#$DBINPUT", indexDbInput)
                    .replace(postgressPluginStr, postgressPlugin);
        }

        String prometeusUsernameLine = "# username = \"$PROMETEUS_USER\"";
        String prometeusPassLine =     "# password = \"$PROMETEUS_PASS\"";
        if (!prometeusUser.equals("")) {
            hostConfigCopy = hostConfigCopy.replace(prometeusUsernameLine, "username = \"$PROMETEUS_USER\"")
                             .replace("$PROMETEUS_USER", prometeusUser)
                             .replace(prometeusPassLine, "password = \"$PROMETEUS_PASS\"")
                             .replace("$PROMETEUS_PASS", prometeusPass);
        }

        // Just use the node type name.
        hostConfigCopy = hostConfigCopy.replace("$VMTYPE", nodeType.toLowerCase()
                .replaceAll("_", ""));

        var esUrl = nodeInfo.getProperties().getValuesOrDefault(ELASTICSEARCH_URL.name(), "");
        if (!esUrl.isEmpty()) {
            String username = nodeInfo.getProperties().getValuesOrDefault(ELASTICSEARCH_USER.name(), "");
            String pwd = nodeInfo.getProperties().getValuesOrDefault(ELASTICSEARCH_PWD.name(), "");

            hostConfigCopy = hostConfigCopy.concat("\n\n" + getElasticsearchConfig(esUrl, username, pwd, blockchainId));
        }

        return hostConfigCopy;
    }

    private String buildDamlDbPasswordForTelegraf(String passwordFromNodeInfo, String tag) {
        if (StringUtils.hasText(passwordFromNodeInfo)) {
            return passwordFromNodeInfo;
        }
        if (!StringUtils.hasText(tag)) {
            return "";
        }
        String second = tag.split("\\.")[1];
        String fourth = tag.split("\\.")[3];
        if ((tag.startsWith("0.0.0") && Integer.parseInt(fourth) < 2466)
            || (tag.startsWith("1.0.0") && Integer.parseInt(fourth) <= 67)
            || (tag.startsWith("0.") && Integer.parseInt(second) > 0)) {
            return "";
        }
        return DAML_DB_DEFAULT_PASSWORD;
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
        prometheusUrls.add("\"http://agent:9081/actuator/prometheus\"");

        // If no service is available, then return.
        if (servicesList == null || servicesList.isEmpty()) {
            return prometheusUrls;
        }

        // TODO change to switch case once concord name is unified.
        if (servicesList.contains(ConcordComponent.ServiceType.DAML_CONCORD)
                || servicesList.contains(ConcordComponent.ServiceType.CONCORD)) {
            prometheusUrls.add("\"http://concord:9891/metrics\"");
        }

        if (servicesList.contains(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE)) {
            prometheusUrls.add("\"http://daml_execution_engine:55001/metrics\"");
        }

        if (servicesList.contains(ConcordComponent.ServiceType.DAML_LEDGER_API)) {
            prometheusUrls.add("\"http://daml_ledger_api:55001/metrics\"");
            prometheusUrls.add("\"http://daml_ledger_api:9873/metrics\"");
            prometheusUrls.add("\"http://daml_ledger_api:9891/metrics\"");
        }
        return prometheusUrls;
    }
}
