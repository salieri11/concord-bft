/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.DamlIndexDbUtil;
import com.vmware.blockchain.configuration.generateconfig.DamlLedgerApiUtil;
import com.vmware.blockchain.configuration.generateconfig.GenericConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.GenesisUtil;
import com.vmware.blockchain.configuration.generateconfig.LoggingUtil;
import com.vmware.blockchain.configuration.generateconfig.TelegrafConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.WavefrontConfigUtil;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityFactors;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodesInfo;

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

    private String bftClientConfigTemplatePath;

    @Autowired
    ConfigurationServiceHelper(@Value("${config.template.path:TelegrafConfigTemplate.conf}")
                                       String telegrafConfigTemplatePath,
                               @Value("${config.template.path:MetricsConfig.yaml}")
                                       String metricsConfigPath,
                               @Value("${config.template.path:wavefrontConfigTemplate.conf}")
                                       String wavefrontConfigPath,
                               @Value("${config.template.path:LoggingTemplate.env}")
                                       String loggingEnvTemplatePath,
                               @Value("${config.template.path:BFTClientConfigTemplate.yaml}")
                                       String bftClientConfigTemplatePath) {
        this.telegrafConfigPath = telegrafConfigTemplatePath;
        this.metricsConfigPath = metricsConfigPath;
        this.wavefrontConfigPath = wavefrontConfigPath;
        this.loggingEnvTemplatePath = loggingEnvTemplatePath;
        this.bftClientConfigTemplatePath = bftClientConfigTemplatePath;
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
                .setComponentUrl(GenesisUtil.genesisPath)
                .setComponent(genesis)
                .setIdentityFactors(IdentityFactors.newBuilder().build())
                .build();
    }

    List<ConfigurationComponent> nodeIndependentConfigs(String consortiumId, String blockchainId,
                                                        List<ServiceType> serviceTypes,
                                                        NodesInfo.Entry nodeInfo) {
        List<ConfigurationComponent> nodeIsolatedConfiguration = new ArrayList<>();
        for (ServiceType serviceType : serviceTypes) {

            switch (serviceType) {
                case DAML_LEDGER_API:
                    DamlLedgerApiUtil ledgerApiUtil = new DamlLedgerApiUtil();
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(serviceType)
                                                          .setComponentUrl(DamlLedgerApiUtil.envVarPath)
                                                          .setComponent(ledgerApiUtil.generateConfig(nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    break;
                case DAML_INDEX_DB:
                    DamlIndexDbUtil damlIndexDbUtil = new DamlIndexDbUtil();
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(serviceType)
                                                          .setComponentUrl(DamlIndexDbUtil.envVarPath)
                                                          .setComponent(
                                                                  damlIndexDbUtil.generateConfig(nodeInfo))
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
                    var telegrafConfigUtil = new TelegrafConfigUtil(telegrafConfigPath, metricsConfigPath);
                    var metricsConfigYaml = telegrafConfigUtil.getMetricsConfigYaml();
                    var telegrafConfig = telegrafConfigUtil.getTelegrafConfig(consortiumId, blockchainId,
                                                                              nodeInfo,
                                                                              serviceTypes);
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.TELEGRAF)
                                                          .setComponentUrl(TelegrafConfigUtil.metricsConfigPath)
                                                          .setComponent(metricsConfigYaml)
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.TELEGRAF)
                                                          .setComponentUrl(TelegrafConfigUtil.configPath)
                                                          .setComponent(telegrafConfig)
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    break;

                case LOGGING:
                    LoggingUtil loggingUtil = new LoggingUtil(loggingEnvTemplatePath);
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.LOGGING)
                                                          .setComponentUrl(LoggingUtil.envVarPath)
                                                          .setComponent(loggingUtil.generateConfig(
                                                                  consortiumId, blockchainId, nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    break;
                case WAVEFRONT_PROXY:
                    var wavefrontConfigUtil = new WavefrontConfigUtil(wavefrontConfigPath);
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(ServiceType.WAVEFRONT_PROXY)
                                                          .setComponentUrl(WavefrontConfigUtil.configPath)
                                                          .setComponent(
                                                                  wavefrontConfigUtil.getWavefrontConfig(blockchainId,
                                                                                                         nodeInfo))
                                                          .setIdentityFactors(IdentityFactors.newBuilder().build())
                                                          .build());
                    break;
                default:
                    log.info("No config required for serviceType {}", serviceType);
            }
        }
        nodeIsolatedConfiguration.add(
                ConfigurationComponent.newBuilder()
                        .setType(ServiceType.GENERIC)
                        .setComponentUrl(GenericConfigUtil.configPath)
                        .setComponent(new GenericConfigUtil().getGenericConfig(consortiumId, blockchainId, nodeInfo))
                        .setIdentityFactors(IdentityFactors.newBuilder().build())
                        .build());
        return nodeIsolatedConfiguration;
    }
}