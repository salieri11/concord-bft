/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;

/**
 * Descriptor test utils.
 */
public class DescriptorTestUtills {

    private static final String CONSORTIUM_NAME = "consortium-1";
    private static final String ZONE_1_NAME = "test-zone-1 - A";

    private static final String VC_URL = "https://vcenter.sddc.vmware.com/";
    private static final String VC_USER = "admin@vsphere.local";
    private static final String VC_PWD = "The rain in Spain falls mainly on the plain";
    private static final String VC_RESOURCE_POOL = "testresourcepool";
    private static final String VC_STORAGE = "teststorage";
    private static final String VC_FOLDER = "testfolder";
    private static final String VC_NETWORK_NAME = "testnetwork";
    private static final String GATEWAY = "10.20.30.1";
    private static final List<String> NAMESERVERS = List.of("1.1.1.1", "1.0.0.1");

    private static Map<String, UUID> consortiumNameToUUIDMap = new HashMap<>();

    static {
        consortiumNameToUUIDMap.put(CONSORTIUM_NAME, UUID.randomUUID());
    }

    /**
     * Build the Organization model.
     * @return the model
     */
    public static InfrastructureDescriptorModel.Organization buildOrganization() {
        InfrastructureDescriptorModel.Organization organization = InfrastructureDescriptorModel.Organization.builder()
                // .dockerImage("dockerImage v1")
                .damlSdk("1.0.1")
                // .templateId(UUID.randomUUID())
                .cpuCount(4)
                .memoryGb(4)
                .clientDiskGb(12)
                .committerDiskGb(12)
                .generatePassword(true)
                .build();

        return organization;
    }

    private static InfrastructureDescriptorModel.VCenter buildVCenter() {
        URL vcUrl;
        try {
            vcUrl = new URL(VC_URL);
        } catch (MalformedURLException e) {
            throw new Error(e);
        }

        return InfrastructureDescriptorModel.VCenter.builder()
                .url(vcUrl)
                .userName(VC_USER)
                .password(VC_PWD)
                .resourcePool(VC_RESOURCE_POOL)
                .storage(VC_STORAGE)
                .folder(VC_FOLDER)
                .build();
    }

    private static InfrastructureDescriptorModel.Network buildNetwork() {
        return InfrastructureDescriptorModel.Network.builder()
                .name(VC_NETWORK_NAME)
                .gateway(GATEWAY)
                .subnet(24)
                .nameServers(NAMESERVERS)
                .build();
    }

    private static InfrastructureDescriptorModel.Zone buildZone(String zoneName) {

        InfrastructureDescriptorModel.VCenter vcenter = buildVCenter();
        InfrastructureDescriptorModel.Network network = buildNetwork();

        InfrastructureDescriptorModel.Zone zone = InfrastructureDescriptorModel.Zone.builder()
                .name(zoneName)
                .vCenter(vcenter)
                .network(network)
                // skip outbound proxy
                // skip ContainerRegistry
                // skip Wavefront
                // skip ElasticSearch
                // skip LogManagement
                .build();
        return zone;
    }

    /**
     * Build the infra descriptor model.
     * @return the model
     */
    public static InfrastructureDescriptorModel buildInfraDescriptorModel() {
        InfrastructureDescriptorModel.Zone zone1 = buildZone(ZONE_1_NAME);
        InfrastructureDescriptorModel.Organization organization = buildOrganization();
        return InfrastructureDescriptorModel.builder()
                .organization(organization)
                .zones(List.of(zone1))
                .build();
    }

    /**
     * Build the deployment descriptor model.
     * @return the model
     */
    public static DeploymentDescriptorModel buildDeploymentDescriptorModel() {
        DeploymentDescriptorModel.Blockchain blockchain = DeploymentDescriptorModel.Blockchain.builder()
                .blockchainType(DeploymentDescriptorModel.BlockchainType.DAML)
                .consortiumName(CONSORTIUM_NAME)
                .build();

        DeploymentDescriptorModel.Client client1 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .build();

        DeploymentDescriptorModel.Client client2 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .build();

        List<String> zones = List.of(ZONE_1_NAME, ZONE_1_NAME, ZONE_1_NAME, ZONE_1_NAME);

        List<DeploymentDescriptorModel.Committer> committers =
                zones.stream()
                        .map(n -> DeploymentDescriptorModel.Committer.builder().zoneName(n).build())
                        .collect(Collectors.toList());

        return DeploymentDescriptorModel.builder()
                .blockchain(blockchain)
                .clients(List.of(client1, client2))
                .committers(committers)
                .build();
    }
}
