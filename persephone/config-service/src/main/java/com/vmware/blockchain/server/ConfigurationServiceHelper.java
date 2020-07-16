/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generatecerts.CertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.BftClientConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.ConcordConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.DamlExecutionEngineUtil;
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
import com.vmware.blockchain.deployment.v1.IdentityComponent;
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

    @Autowired
    ConfigurationServiceHelper(@Value("${config.template.path:TelegrafConfigTemplate.conf}")
                                       String telegrafConfigTemplatePath,
                               @Value("${config.template.path:MetricsConfig.yaml}")
                                       String metricsConfigPath,
                               @Value("${config.template.path:wavefrontConfigTemplate.conf}")
                                       String wavefrontConfigPath,
                               @Value("${config.template.path:LoggingTemplate.env}")
                                       String loggingEnvTemplatePath) {
        this.telegrafConfigPath = telegrafConfigTemplatePath;
        this.metricsConfigPath = metricsConfigPath;
        this.wavefrontConfigPath = wavefrontConfigPath;
        this.loggingEnvTemplatePath = loggingEnvTemplatePath;
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
                case DAML_EXECUTION_ENGINE:
                    DamlExecutionEngineUtil executionEngineUtil = new DamlExecutionEngineUtil();
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                            .setType(serviceType)
                            .setComponentUrl(DamlExecutionEngineUtil.envVarPath)
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
                    nodeIsolatedConfiguration.add(ConfigurationComponent.newBuilder()
                                                          .setType(serviceType)
                                                          .setComponentUrl(DamlIndexDbUtil.postGresConfig)
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
                    var telegrafConfig = telegrafConfigUtil.getTelegrafConfig(consortiumId, blockchainId,
                                                                              nodeInfo,
                                                                              serviceTypes);
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

    /**
     * build node config.
     * TODO : refactor!!
     */
    List<ConfigurationComponent> buildNodeConifigs(String nodeId, List<ConfigurationComponent> componentList,
                                                    ConcordEcCertificatesGenerator certGen,
                                                    Map<String, String> concordConfig,
                                                    Map<String, String> bftClientConfig,
                                                    Map<String, List<IdentityComponent>> concordIdentityComponents,
                                                    Map<String, List<IdentityComponent>> bftIdentityComponents) {
        List<ConfigurationComponent> output = new ArrayList<>();
        output.addAll(componentList);

        if (concordConfig.containsKey(nodeId)) {
            output.add(ConfigurationComponent.newBuilder()
                    .setType(ServiceType.CONCORD)
                    .setComponentUrl(ConcordConfigUtil.configPath)
                    .setComponent(concordConfig.get(nodeId))
                    .setIdentityFactors(IdentityFactors.newBuilder().build())
                    .build());
        }

        if (concordIdentityComponents.containsKey(nodeId)) {
            concordIdentityComponents.get(nodeId).forEach(entry ->
                    output.add(ConfigurationComponent.newBuilder()
                            .setType(ServiceType.CONCORD)
                            .setComponentUrl(entry.getUrl())
                            .setComponent(entry.getBase64Value())
                            .setIdentityFactors(
                                    certGen.getIdentityFactor())
                            .build()));
        }

        if (bftIdentityComponents.containsKey(nodeId)) {
            bftIdentityComponents.get(nodeId).forEach(entry ->
                    output.add(ConfigurationComponent.newBuilder()
                            .setType(ServiceType.DAML_LEDGER_API)
                            .setComponentUrl(entry.getUrl())
                            .setComponent(entry.getBase64Value())
                            .setIdentityFactors(
                                    certGen.getIdentityFactor())
                            .build()));
        }

        if (bftClientConfig.containsKey(nodeId)) {
            output.add(ConfigurationComponent.newBuilder()
                    .setType(ServiceType.DAML_LEDGER_API)
                    .setComponentUrl(BftClientConfigUtil.configPath)
                    .setComponent(bftClientConfig.get(nodeId))
                    .setIdentityFactors(IdentityFactors.newBuilder().build())
                    .build());
        }

        return output;
    }

    /**
     * Get tls node identities.
     */
    Map<String, List<IdentityComponent>> getTlsNodeIdentities(ConcordConfigUtil concordConfigUtil,
                                                               BftClientConfigUtil bftClientConfigUtil,
                                                               ConcordEcCertificatesGenerator certGen,
                                                               ArrayList<String> nodeIdList,
                                                               boolean isBftEnabled) {

        Map<Integer, List<Integer>> nodePrincipal = new HashMap<>();
        int numPrincipals = concordConfigUtil.maxPrincipalId + 1;

        var concordNodePrincipals = concordConfigUtil.nodePrincipal;
        nodePrincipal.putAll(concordNodePrincipals);

        if (isBftEnabled) {
            numPrincipals = bftClientConfigUtil.maxPrincipalId + 1;
            var bftNodePrincipals = bftClientConfigUtil.nodePrincipal;
            bftNodePrincipals.forEach((key, value) -> nodePrincipal.put(concordNodePrincipals.size() + key, value));
        }

        List<Identity> tlsIdentityList =
                certGen.generateSelfSignedCertificates(numPrincipals,
                        ServiceType.CONCORD);

        Map<String, List<IdentityComponent>> tlsNodeIdentities = buildTlsIdentity(nodeIdList,
                tlsIdentityList,
                nodePrincipal,
                numPrincipals);
        return tlsNodeIdentities;
    }

    /**
     * Filter tls identities based on nodes and principal ids.
     */
    Map<String, List<IdentityComponent>> buildTlsIdentity(List<String> nodeIds,
                                                                  List<Identity> identities,
                                                                  Map<Integer, List<Integer>> principals,
                                                                  int numCerts) {

        Map<String, List<IdentityComponent>> result = new HashMap<>();

        // TODO: May remove logic once principals are available
        if (principals.size() == 0) {
            IntStream.range(0, nodeIds.size()).forEach(node -> {
                List<IdentityComponent> identityComponents = new ArrayList<>();
                identities.forEach(identity -> {
                    identityComponents.add(identity.getCertificate());
                    identityComponents.add(identity.getKey());
                });
                result.put(nodeIds.get(node), identityComponents);
            });
            return result;
        }

        for (int node : principals.keySet()) {
            List<IdentityComponent> nodeIdentities = new ArrayList<>();

            List<Integer> notPrincipal = IntStream.range(0, numCerts)
                    .boxed().collect(Collectors.toList());
            notPrincipal.removeAll(principals.get(node));

            List<Identity> serverList = new ArrayList<>(identities.subList(0, identities.size() / 2));
            List<Identity> clientList = new ArrayList<>(identities.subList(identities.size() / 2, identities.size()));

            notPrincipal.forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getCertificate());
            });

            // add self keys
            nodeIdentities.add(serverList.get(node).getKey());
            nodeIdentities.add(clientList.get(node).getKey());

            principals.get(node).forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(serverList.get(entry).getKey());
                nodeIdentities.add(clientList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getKey());
            });
            result.putIfAbsent(nodeIds.get(node), nodeIdentities);
        }

        log.info("Filtered tls identities based on nodes and principal ids.");
        return result;
    }

    /**
     * transform concord node identities to bft node identities.
     */
    Map<String, List<IdentityComponent>> convertToBftTlsNodeIdentities(Map<String, List<IdentityComponent>>
                                                                            tlsNodeIdentities) {
        Map<String, List<IdentityComponent>> bftIdentityComponents = new HashMap<>();

        tlsNodeIdentities.forEach((key, value) -> {
            List<IdentityComponent> bftIdentities = new ArrayList<>();
            value.forEach(val -> {
                String newUrl = val.getUrl().replace(
                        CertificatesGenerator.CONCORD_TLS_SECURITY_IDENTITY_PATH,
                        CertificatesGenerator.BFT_CLIENT_TLS_SECURITY_IDENTITY_PATH);
                IdentityComponent ident = IdentityComponent.newBuilder()
                        .setType(val.getType())
                        .setBase64Value(val.getBase64Value())
                        .setUrl(newUrl)
                        .build();
                bftIdentities.add(ident);
            });
            bftIdentityComponents.put(key, bftIdentities);
        });

        return bftIdentityComponents;
    }
}