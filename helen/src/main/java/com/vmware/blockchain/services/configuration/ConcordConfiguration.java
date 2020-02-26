/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.configuration;

import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.CONCORD;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_CONCORD;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_INDEX_DB;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_LEDGER_API;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.ETHEREUM_API;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.GENERIC;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.JAEGER_AGENT;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.LOGGING;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.TELEGRAF;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.WAVEFRONT_PROXY;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.ethereum.type.Genesis;
import com.vmware.blockchain.services.profiles.OrganizationService;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * This class provides the configuration required to deploy.
 */
@Getter
@Component
@Slf4j
public class ConcordConfiguration {

    private AuthHelper authHelper;
    private OrganizationService organizationService;
    private String version;
    private String template;
    private String dockerImageBuild;

    @Autowired
    public ConcordConfiguration(AuthHelper authHelper, OrganizationService organizationService,
                                @Value("${concord.model.specification.version:20190401.1}") String version,
                                @Value("${concord.model.specification.template:8abc7fda-9576-4b13-9beb-06f867cf2c7c}")
                                            String template,
                                @Value("${concord.model.specification.dockerImageBuild:latest}") String
                                            dockerImageBuild) {
        this.authHelper = authHelper;
        this.organizationService = organizationService;
        this.version = version;
        this.template = template;
        this.dockerImageBuild = dockerImageBuild;

        log.info("Concord Configuration loaded: version: {}, template: {}, dockerImageBuild: {}", version, template,
                 dockerImageBuild);
    }

    // ConcordComponents for blockchains
    private static final List<ConcordComponent.ServiceType> GENERIC_COMPONENTS = List.of(GENERIC, LOGGING,
            WAVEFRONT_PROXY, TELEGRAF);
    private static final List<ConcordComponent.ServiceType> ETHEREUM_COMPONENTS = List.of(CONCORD, JAEGER_AGENT,
            ETHEREUM_API);
    private static final List<ConcordComponent.ServiceType> DAML_COMPONENTS = List.of(DAML_CONCORD, JAEGER_AGENT,
            DAML_EXECUTION_ENGINE, DAML_INDEX_DB, DAML_LEDGER_API);

    //ConcordComponents for nodes
    private static final List<ConcordComponent.ServiceType> DAML_PARTICIPANT_COMPONENTS = List.of(DAML_INDEX_DB,
            DAML_LEDGER_API);
    private static final List<ConcordComponent.ServiceType> DAML_COMMITTER_COMPONENTS = List.of(DAML_CONCORD,
            JAEGER_AGENT, DAML_EXECUTION_ENGINE);

    public static final Map<ConcordComponent.ServiceType, String> STATIC_TAG_LIST = ImmutableMap.of(
            WAVEFRONT_PROXY, "5.7", TELEGRAF, "1.11", JAEGER_AGENT, "1.11"
    );

    private static final Map<ConcordModelSpecification.BlockchainType, List<ConcordComponent.ServiceType>>
            componentListForBlockchainType =
            ImmutableMap
                    .of(ConcordModelSpecification.BlockchainType.ETHEREUM,
                            Stream.concat(GENERIC_COMPONENTS.stream(), ETHEREUM_COMPONENTS.stream())
                                    .collect(Collectors.toList()),
                            ConcordModelSpecification.BlockchainType.DAML,
                            Stream.concat(GENERIC_COMPONENTS.stream(), DAML_COMPONENTS.stream())
                                    .collect(Collectors.toList()));

    private static final Map<ConcordModelSpecification.NodeType, List<ConcordComponent.ServiceType>>
            componentListForNodeType =
            ImmutableMap
                    .of(ConcordModelSpecification.NodeType.DAML_PARTICIPANT,
                            Stream.concat(GENERIC_COMPONENTS.stream(), DAML_PARTICIPANT_COMPONENTS.stream())
                                    .collect(Collectors.toList()),
                            ConcordModelSpecification.NodeType.DAML_COMMITTER,
                            Stream.concat(GENERIC_COMPONENTS.stream(), DAML_COMMITTER_COMPONENTS.stream())
                                    .collect(Collectors.toList()));

    private static final Map<ConcordComponent.ServiceType, String> componentToImageName =
            ImmutableMap.<ConcordComponent.ServiceType, String>builder()
                    .put(GENERIC, "vmwblockchain/agent")
                    .put(LOGGING, "vmwblockchain/fluentd")
                    .put(WAVEFRONT_PROXY, "vmwblockchain/wavefront-proxy")
                    .put(JAEGER_AGENT, "vmwblockchain/jaeger-agent")
                    .put(DAML_CONCORD, "vmwblockchain/concord-core")
                    .put(DAML_EXECUTION_ENGINE, "vmwblockchain/daml-execution-engine")
                    .put(DAML_INDEX_DB, "vmwblockchain/daml-index-db")
                    .put(DAML_LEDGER_API, "vmwblockchain/daml-ledger-api")
                    .put(CONCORD, "vmwblockchain/concord-core")
                    .put(ETHEREUM_API, "vmwblockchain/ethrpc")
                    .put(TELEGRAF, "vmwblockchain/telegraf").build();

    private String getImageTag(ConcordComponent.ServiceType type) {
        log.info("ServiceType: {}", type);

        StringBuilder sb = new StringBuilder(componentToImageName.get(type));
        sb.append(":");
        Map<String, String> orgProperties =
                organizationService.get(authHelper.getOrganizationId()).getOrganizationProperties();
        if (orgProperties == null) {
            orgProperties = new HashMap<>();
        }
        if (STATIC_TAG_LIST.containsKey(type)) {
            sb.append(STATIC_TAG_LIST.get(type));
        } else {
            sb.append(orgProperties.getOrDefault(Constants.ORG_DOCKER_IMAGE_OVERRIDE, dockerImageBuild));
        }
        return sb.toString();
    }

    /**
     * Get component list.
     * @param type blockchain
     * @return components.
     */
    public List<ConcordComponent> getComponentsByBlockchainType(ConcordModelSpecification.BlockchainType type) {
        List<ConcordComponent> response = new ArrayList<>();

        componentListForBlockchainType.get(type).stream().forEach(k -> response.add(
                ConcordComponent.newBuilder()
                        .setType(ConcordComponent.Type.CONTAINER_IMAGE)
                        .setServiceType(k)
                        .setName(getImageTag(k))
                        .build()
        ));
        return response;
    }

    /**
     * Get component list.
     * @param nodeType node
     * @return components.
     */
    public List<ConcordComponent> getComponentsByNodeType(ConcordModelSpecification.NodeType nodeType) {
        List<ConcordComponent> response = new ArrayList<>();

        componentListForNodeType.get(nodeType).stream().forEach(k -> response.add(
                ConcordComponent.newBuilder()
                        .setType(ConcordComponent.Type.CONTAINER_IMAGE)
                        .setServiceType(k)
                        .setName(getImageTag(k))
                        .build()
        ));
        return response;
    }

    /**
     * Static method that generates the Genesis block object.
     * @return genesis
     */
    public static Genesis getGenesisObject() {
        return Genesis.newBuilder().setConfig(
                Genesis.Config.newBuilder()
                        .setChainId(1)
                        .setHomesteadBlock(0)
                        .setEip155Block(0)
                        .setEip158Block(0)
                        .build()
        )
                .setNonce("0x0000000000000000")
                .setDifficulty("0x400")
                .setMixhash("0x0000000000000000000000000000000000000000000000000000000000000000")
                .setParentHash("0x0000000000000000000000000000000000000000000000000000000000000000")
                .setGasLimit("0xf4240")
                .putAllAlloc(Map.of(
                        "262c0d7ab5ffd4ede2199f6ea793f819e1abb019", Genesis.Wallet.newBuilder()
                                .setBalance("12345").build(),
                        "5bb088f57365907b1840e45984cae028a82af934", Genesis.Wallet.newBuilder()
                                .setBalance("0xabcdef").build(),
                        "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39", Genesis.Wallet.newBuilder()
                                .setBalance("0x7fffffffffffffff").build()
                ))
                .build();
    }
}