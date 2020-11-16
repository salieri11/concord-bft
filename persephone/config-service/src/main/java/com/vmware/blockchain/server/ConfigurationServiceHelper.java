/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;


import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.base.Strings;
import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.ConfigUtilHelpers;
import com.vmware.blockchain.configuration.generateconfig.Constants;
import com.vmware.blockchain.configuration.generateconfig.DamlExecutionEngineUtil;
import com.vmware.blockchain.configuration.generateconfig.DamlIndexDbUtil;
import com.vmware.blockchain.configuration.generateconfig.DamlLedgerApiUtil;
import com.vmware.blockchain.configuration.generateconfig.GenericConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.LoggingUtil;
import com.vmware.blockchain.configuration.generateconfig.TelegrafConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.ValidationUtil;
import com.vmware.blockchain.configuration.generateconfig.WavefrontConfigUtil;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.deployment.v1.IdentityFactors;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;

import lombok.extern.slf4j.Slf4j;

/**
 * Implementation of ConfigurationService server.
 */
@Slf4j
@Component
public class ConfigurationServiceHelper {

    /**
     * Telegraf config template path.
     **/
    private String telegrafConfigPath;

    /**
     * Wavefront config template path.
     **/
    private String wavefrontConfigPath;

    /**
     * Metrics config template path.
     **/
    private String metricsConfigPath;

    /**
     * Logging config template path.
     **/
    private String loggingEnvTemplatePath;

    /**
     * Directive to keep temporary files for debugging purposes.
     */
    private boolean keepTempFiles;

    @Autowired
    ConfigurationServiceHelper(@Value("${config.template.path:TelegrafConfigTemplate.conf}")
                                       String telegrafConfigTemplatePath,
                               @Value("${config.template.path:MetricsConfig.yaml}")
                                       String metricsConfigPath,
                               @Value("${config.template.path:wavefrontConfigTemplate.conf}")
                                       String wavefrontConfigPath,
                               @Value("${config.template.path:LoggingTemplate.env}")
                                       String loggingEnvTemplatePath,
                               @Value("${keep.temporary.files:false}")
                                       boolean keepTempFiles) {
        this.telegrafConfigPath = telegrafConfigTemplatePath;
        this.metricsConfigPath = metricsConfigPath;
        this.wavefrontConfigPath = wavefrontConfigPath;
        this.loggingEnvTemplatePath = loggingEnvTemplatePath;
        this.keepTempFiles = keepTempFiles;
    }

    List<ConfigurationComponent> getEthereumComponent() {
        List<ConfigurationComponent> output = new ArrayList<>();

        var certGen = new ConcordEcCertificatesGenerator();
        List<Identity> ethrpcIdentityList = certGen.generateSelfSignedCertificates(1,
                                                                                   ServiceType.ETHEREUM_API);

        output.add(ConfigurationComponent.newBuilder()
                           .setType(ServiceType.ETHEREUM_API)
                           .setComponentUrl(ethrpcIdentityList.get(0).getCertificate().getUrl())
                           .setComponent(ethrpcIdentityList.get(0).getCertificate().getBase64Value())
                           .setIdentityFactors(certGen.getIdentityFactor())
                           .build());

        output.add(ConfigurationComponent.newBuilder()
                           .setType(ServiceType.ETHEREUM_API)
                           .setComponentUrl(ethrpcIdentityList.get(0).getKey().getUrl())
                           .setComponent(ethrpcIdentityList.get(0).getKey().getBase64Value())
                           .setIdentityFactors(certGen.getIdentityFactor())
                           .build());

        return output;
    }

    ConfigurationComponent createGenesisComponent(@NotNull String genesis) {
        return ConfigurationComponent.newBuilder()
                .setType(ServiceType.CONCORD)
                .setComponentUrl(Constants.GENESIS_CONFIG_PATH)
                .setComponent(genesis)
                .setIdentityFactors(IdentityFactors.newBuilder().build())
                .build();
    }

    List<ConfigurationComponent> nodeIndependentConfigs(String consortiumId, String blockchainId,
                                                        NodesInfo.Entry nodeInfo, String nodeType)
            throws ConfigServiceException {
        if (consortiumId == null || blockchainId == null || nodeInfo == null || nodeType == null) {
            log.error("consortiumId {} blockchainId {} nodeInfo {} nodeType {}", consortiumId, blockchainId, nodeInfo,
                      nodeType);
            throw new ConfigServiceException(ErrorCode.CLIENT_CONFIG_INVALID_INPUT_FAILURE,
                                             "Essential input parameters are missing.");
        }
        List<ConfigurationComponent> nodeIsolatedConfiguration = new ArrayList<>();
        // Get service types for this node.
        List<ServiceType> serviceTypes = nodeInfo.getServicesList();
        // Are there any service types for this node? If not available, return an empty list.
        if (serviceTypes == null || serviceTypes.isEmpty()) {
            log.info("Service types are not available for this node type, is this a new node type? ");
            return nodeIsolatedConfiguration;
        }
        for (ServiceType serviceType : serviceTypes) {
            switch (serviceType) {
                case DAML_EXECUTION_ENGINE:
                    DamlExecutionEngineUtil executionEngineUtil = new DamlExecutionEngineUtil();
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                            .setType(serviceType)
                            .setComponentUrl(Constants.DAML_ENV_VARS_PATH)
                            .setComponent(executionEngineUtil.generateConfig())
                            .setIdentityFactors(IdentityFactors.newBuilder().build())
                            .build());
                    break;
                case DAML_LEDGER_API:
                    DamlLedgerApiUtil ledgerApiUtil = new DamlLedgerApiUtil();
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(serviceType)
                                                          .setComponentUrl(DamlLedgerApiUtil.envVarPath)
                                                          .setComponent(ledgerApiUtil.generateConfig(nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());

                    // placeholder to create bft config

                    Properties properties =  nodeInfo.getProperties();

                    String pem = properties.getValuesMap().getOrDefault(NodeProperty.Name.TLS_PEM.name(), "");
                    String crt = properties.getValuesMap().getOrDefault(NodeProperty.Name.TLS_CRT.name(), "");
                    String cacrt = properties.getValuesMap().getOrDefault(NodeProperty.Name.TLS_CACRT.name(), "");

                    // Certificate values can either be provided or not.
                    // If they are provided, make sure they are all present.
                    // If they are not provided, take no action.
                    // This is to maintain backward compatibility.

                    boolean tlsEnabled = !Strings.isNullOrEmpty(pem)
                            && !Strings.isNullOrEmpty(crt)
                            && !Strings.isNullOrEmpty(cacrt);

                    if (tlsEnabled) {
                        nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                .setType(serviceType)
                                .setComponentUrl(DamlLedgerApiUtil.tlsPemPath)
                                .setComponent(pem)
                                .setIdentityFactors(IdentityFactors.newBuilder().build())
                                .build());

                        nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                .setType(serviceType)
                                .setComponentUrl(DamlLedgerApiUtil.tlsCrtPath)
                                .setComponent(crt)
                                .setIdentityFactors(IdentityFactors.newBuilder().build())
                                .build());

                        nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                .setType(serviceType)
                                .setComponentUrl(DamlLedgerApiUtil.tlsCacrtPath)
                                .setComponent(cacrt)
                                .setIdentityFactors(IdentityFactors.newBuilder().build())
                                .build());
                    }

                    break;
                case DAML_INDEX_DB:
                    DamlIndexDbUtil damlIndexDbUtil = new DamlIndexDbUtil();
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(serviceType)
                                                          .setComponentUrl(Constants.DAML_DB_ENV_VARS_PATH)
                                                          .setComponent(
                                                                  damlIndexDbUtil.generateConfig(nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(serviceType)
                                                          .setComponentUrl(Constants.DAML_DB_POSTGRES_CONFIG_PATH)
                                                          .setComponent(
                                                                  damlIndexDbUtil.getPostGresConfig())
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    break;
                case CONCORD:
                    ConfigurationComponent genesisJson = createGenesisComponent(nodeInfo.getProperties()
                                                                                        .getValuesOrDefault(
                            NodeProperty.Name.GENESIS.name(), ""));
                    nodeIsolatedConfiguration.add(genesisJson);
                    break;
                case ETHEREUM_API:
                    nodeIsolatedConfiguration.addAll(getEthereumComponent());
                    break;
                case TELEGRAF:
                    var telegrafConfigUtil = new TelegrafConfigUtil(telegrafConfigPath);
                    var telegrafConfig =
                            telegrafConfigUtil.getTelegrafConfig(consortiumId, blockchainId, nodeInfo, nodeType);
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.TELEGRAF)
                                                          .setComponentUrl(Constants.TELEGRAF_CONFIG_PATH)
                                                          .setComponent(telegrafConfig)
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    break;

                case LOGGING:
                    LoggingUtil loggingUtil = new LoggingUtil(loggingEnvTemplatePath);
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.LOGGING)
                                                          .setComponentUrl(Constants.LOGGING_CONFIG_PATH)
                                                          .setComponent(loggingUtil.generateConfig(
                                                                  consortiumId, blockchainId, nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .setFilePermissions(
                                                                  Constants.LOGGING_CONFIG_FILE_PERMISSIONS)
                                                          .build());
                    break;
                case WAVEFRONT_PROXY:
                    var wavefrontConfigUtil = new WavefrontConfigUtil(wavefrontConfigPath);
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.WAVEFRONT_PROXY)
                                                          .setComponentUrl(Constants.WAVEFRONT_CONFIG_PATH)
                                                          .setComponent(
                                                                  wavefrontConfigUtil.getWavefrontConfig(blockchainId,
                                                                                                         nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .setFilePermissions(
                                                                  Constants.WAVEFRONT_CONFIG_FILE_PERMISSIONS)
                                                          .build());
                    break;
                default:
                    log.info("No config required for serviceType {}", serviceType);
            }
        }
        nodeIsolatedConfiguration.add(
                ConfigurationComponent.newBuilder()
                        .setType(ServiceType.GENERIC)
                        .setComponentUrl(Constants.GENERIC_IDENTIFIERS_PATH)
                        .setComponent(new GenericConfigUtil().getGenericConfig(consortiumId, blockchainId, nodeInfo))
                        .setIdentityFactors(IdentityFactors.newBuilder().build())
                        .build());
        return nodeIsolatedConfiguration;
    }

    /**
     * Build configurations for a supplied node.
     * @param nodeId node Id
     * @param componentList list of components
     * @param certGen Certificate generator
     * @param concordConfig Configuration for Replias
     * @param bftClientConfig Configuration for Clients
     * @param allNodeIdentityComponents TLS identities for all clients
     * @return Configuration for the supplied node.
     */
    List<ConfigurationComponent> buildNodeConfigs(String nodeId, List<ConfigurationComponent> componentList,
                                                    ConcordEcCertificatesGenerator certGen,
                                                    Map<String, Map<String, String>> concordConfig,
                                                    Map<String, String> bftClientConfig,
                                                    Map<String, List<IdentityComponent>> allNodeIdentityComponents) {
        if (!ValidationUtil.isValidNodeId(nodeId) || !ValidationUtil.isValidList(componentList)
            || !ValidationUtil.isValid(certGen) || !ValidationUtil.isValidMap(concordConfig)
            || !ValidationUtil.isValidMap(bftClientConfig) || !ValidationUtil.isValidMap(allNodeIdentityComponents)) {
            log.error("Invalid input parameters.");
            throw new ConfigServiceException(ErrorCode.GENERATE_NODE_CONFIG_INVALID_INPUT_FAILURE,
                                             "Invalid input parameters.");
        }
        log.info("nodeId {} componentList size {} concordConfig size {} bftClientConfig size {} "
                  + " allNodeIdentityComponents size {}.", nodeId, componentList.size(), concordConfig.size(),
                  bftClientConfig.size(), allNodeIdentityComponents.size());
        List<ConfigurationComponent> output = new ArrayList<>(componentList);

        if (concordConfig.containsKey(nodeId)) {
            if (concordConfig.get(nodeId) != null) {
                concordConfig.get(nodeId).forEach((key, value) -> {
                    if (key.equalsIgnoreCase(ConfigUtilHelpers.DEPLOY)) {
                        output.add(ConfigurationComponent.newBuilder()
                                           .setType(ServiceType.CONCORD)
                                           .setComponentUrl(Constants.CONCORD_DEPLOY_CONFIG_PATH)
                                           .setComponent(value)
                                           .setIdentityFactors(IdentityFactors.newBuilder().build())
                                           .build());
                    }
                    if (key.equalsIgnoreCase(ConfigUtilHelpers.SECRET)) {
                        output.add(ConfigurationComponent.newBuilder()
                                           .setType(ServiceType.CONCORD)
                                           .setComponentUrl(Constants.CONCORD_SECRETS_CONFIG_PATH)
                                           .setComponent(value)
                                           .setIdentityFactors(IdentityFactors.newBuilder().build())
                                           .build());
                    }
                    if (key.equalsIgnoreCase(ConfigUtilHelpers.CONCORD)) {
                        output.add(ConfigurationComponent.newBuilder()
                                           .setType(ServiceType.CONCORD)
                                           .setComponentUrl(Constants.CONCORD_CONFIG_PATH)
                                           .setComponent(value)
                                           .setIdentityFactors(IdentityFactors.newBuilder().build())
                                           .setFilePermissions(Constants.CONCORD_CONFIG_FILE_PERMISSIONS)
                                           .build());
                    }
                });

                if (allNodeIdentityComponents.get(nodeId) != null) {
                    allNodeIdentityComponents.get(nodeId).forEach(entry -> output
                            .add(ConfigurationComponent.newBuilder().setType(ServiceType.CONCORD)
                                         .setComponentUrl(entry.getUrl()).setComponent(entry.getBase64Value())
                                         .setIdentityFactors(certGen.getIdentityFactor()).build()));
                } else {
                    log.error("allNodeIdentityComponents is missing configuration for nodeId {}.", nodeId);
                }
            } else {
                log.error("concordConfig is missing configuration for node {}.", nodeId);
            }
        }

        if (bftClientConfig.containsKey(nodeId)) {
            if (bftClientConfig.get(nodeId) != null) {
                output.add(ConfigurationComponent.newBuilder()
                                   .setType(ServiceType.DAML_LEDGER_API)
                                   .setComponentUrl(Constants.DAML_BFT_CLIENT_CONFIG_PATH)
                                   .setComponent(bftClientConfig.get(nodeId))
                                   .setIdentityFactors(IdentityFactors.newBuilder().build())
                                   .build());
                if (allNodeIdentityComponents.get(nodeId) != null) {
                    allNodeIdentityComponents.get(nodeId).forEach(entry -> output
                            .add(ConfigurationComponent.newBuilder().setType(ServiceType.DAML_LEDGER_API)
                                         .setComponentUrl(entry.getUrl()).setComponent(entry.getBase64Value())
                                         .setIdentityFactors(certGen.getIdentityFactor()).build()));
                } else {
                    log.error("allNodeIdentityComponents is missing configuration for nodeId {}.", nodeId);
                }
            } else {
                log.error("bftClientConfig is missing configuration for node {}.", nodeId);
            }
        }
        log.debug("built configuration {}", output);
        return output;
    }

    /**
     * Return the value of keep.temporary.files property.
     *
     * @return keep temporary files directive
     */
    public boolean isKeepTempFiles() {
        return keepTempFiles;
    }

    /**
     * Set keep.temporary.files property.
     *
     * @param keepTempFiles keep temporary files
     */
    public void setKeepTempFiles(boolean keepTempFiles) {
        this.keepTempFiles = keepTempFiles;
    }

}