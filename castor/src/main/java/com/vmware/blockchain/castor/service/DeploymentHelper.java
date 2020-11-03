/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.util.StringUtils;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
import com.vmware.blockchain.deployment.v1.ElasticSearch;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.IPv4Network;
import com.vmware.blockchain.deployment.v1.LogManagement;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSite;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.PasswordCredential;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.Sites;
import com.vmware.blockchain.deployment.v1.TransportSecurity;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Wavefront;

import lombok.extern.log4j.Log4j2;

/**
 * Utilities for constructing deployment gRPC payloads from infra and deployment descriptors.
 */
@Log4j2
public class DeploymentHelper {

    public static final String PROVISIONING_TIMEOUT_MINUTES_KEY = "castor.deployment.timeout.minutes";

    private static final String NAME_KEY = "name";
    private static final String LAT_KEY = "geo-latitude";
    private static final String LONG_KEY = "geo-longitude";

    private static final Map<DeploymentDescriptorModel.BlockchainType, BlockchainType>
            blockchainTypeMap = new HashMap<>();

    static {
        blockchainTypeMap.put(DeploymentDescriptorModel.BlockchainType.DAML, BlockchainType.DAML);
        blockchainTypeMap.put(DeploymentDescriptorModel.BlockchainType.ETHEREUM, BlockchainType.ETHEREUM);
    }

    /**
     * Entrypoint to construct a deployment request.
      * @param infrastructureDescriptorModel the infrastructure descriptor
     * @param deploymentDescriptorModel the deployment descriptor - of type provisioning, or reconfiguration
     * @return the deployment request payload
     */
    public static DeploymentRequest constructDeploymentRequest(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel) {

        // Build replica and client nodes
        NodeAssignment.Builder nodeAssignmentBuilder = NodeAssignment.newBuilder();
        // Build Replicas
        List<DeploymentDescriptorModel.Replica>
                replicas = deploymentDescriptorModel.getReplicas();
        buildReplicas(deploymentDescriptorModel, replicas, nodeAssignmentBuilder);
        // Build Clients
        List<DeploymentDescriptorModel.Client> clients = deploymentDescriptorModel.getClients();
        buildClients(deploymentDescriptorModel, clients, nodeAssignmentBuilder);

        // Build sites
        List<OrchestrationSite> orchestrationSites =
                buildSites(infrastructureDescriptorModel,
                           deploymentDescriptorModel.getReplicas(), deploymentDescriptorModel.getClients());

        // Build deployment spec
        DeploymentDescriptorModel.Blockchain blockchainDescriptor = deploymentDescriptorModel.getBlockchain();
        UUID consortiumId = UUID.randomUUID();
        String consortiumIdString = consortiumId.toString();
        String consortiumName = blockchainDescriptor.getConsortiumName();
        log.info("Generated consortium id: {} for consortium: {}", consortiumId, consortiumName);
        BlockchainType blockchainType = blockchainTypeMap.get(blockchainDescriptor.getBlockchainType());
        Sites sites = Sites.newBuilder().addAllInfoList(orchestrationSites).build();
        // Build properties
        Properties properties = buildProperties(infrastructureDescriptorModel);
        DeploymentSpec deploymentSpec = DeploymentSpec.newBuilder()
                .setConsortiumId(consortiumIdString)
                .setBlockchainType(blockchainType)
                .setSites(sites)
                .setNodeAssignment(nodeAssignmentBuilder.build())
                .setProperties(properties)
                .build();

        // Finally! deployment request
        DeploymentRequest deploymentRequest = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().setId("").build())
                .setSpec(deploymentSpec)
                .build();

        return deploymentRequest;
    }


    private static void buildReplicas(
            DeploymentDescriptorModel deploymentDescriptorModel,
            List<DeploymentDescriptorModel.Replica> replicas,
            NodeAssignment.Builder nodeAssignmentBuilder) {
        replicas.forEach(replica -> {
            Properties.Builder propBuilder = Properties.newBuilder();
            if (StringUtils.hasText(replica.getProvidedIp())) {
                propBuilder.putValues(NodeProperty.Name.VM_IP.name(), replica.getProvidedIp());
                propBuilder.putValues(
                        DeployedResource.DeployedResourcePropertyKey.PRIVATE_IP.name(), replica.getProvidedIp());
            }

            if (deploymentDescriptorModel.getReplicaNodeSpec() != null) {
                int diskSize = deploymentDescriptorModel.getReplicaNodeSpec().getDiskSizeGb();
                if (diskSize > 0) {
                    String clientDiskSizeString = String.valueOf(diskSize);
                    propBuilder.putValues(DeploymentAttributes.VM_STORAGE.name(), clientDiskSizeString);
                }

                int cpuCount = deploymentDescriptorModel.getReplicaNodeSpec().getCpuCount();
                if (cpuCount > 0) {
                    propBuilder.putValues(DeploymentAttributes.VM_CPU_COUNT.name(), String.valueOf(cpuCount));
                }

                int memory = deploymentDescriptorModel.getReplicaNodeSpec().getMemoryGb();
                if (memory > 0) {
                    propBuilder.putValues(DeploymentAttributes.VM_MEMORY.name(), String.valueOf(memory));
                }
            }

            nodeAssignmentBuilder.addEntries(
                    NodeAssignment.Entry.newBuilder()
                            .setType(NodeType.REPLICA)
                            .setNodeId(UUID.randomUUID().toString())
                            .setSite(OrchestrationSiteIdentifier.newBuilder().setId(replica.getZoneName()).build())
                            .setProperties(propBuilder)
            );
        });
    }

    private static void buildClients(
            DeploymentDescriptorModel deploymentDescriptorModel,
            List<DeploymentDescriptorModel.Client> clients,
            NodeAssignment.Builder nodeAssignmentBuilder) {

        // A map to hold groupIndex to groupId (UUID) mapping.
        final Map<String, String> groupMap = new HashMap<>();

        clients.forEach(client -> {
            Properties.Builder propBuilder = Properties.newBuilder();
            if (StringUtils.hasText(client.getAuthUrlJwt())) {
                propBuilder.putValues(NodeProperty.Name.CLIENT_AUTH_JWT.name(), client.getAuthUrlJwt());
            }
            if (StringUtils.hasText(client.getProvidedIp())) {
                propBuilder.putValues(NodeProperty.Name.VM_IP.name(), client.getProvidedIp());
                propBuilder.putValues(
                        DeployedResource.DeployedResourcePropertyKey.PRIVATE_IP.name(), client.getProvidedIp());
            }

            if (deploymentDescriptorModel.getClientNodeSpec() != null) {
                int diskSize = deploymentDescriptorModel.getClientNodeSpec().getDiskSizeGb();
                if (diskSize > 0) {
                    String clientDiskSizeString = String.valueOf(diskSize);
                    propBuilder.putValues(DeploymentAttributes.VM_STORAGE.name(), clientDiskSizeString);
                }

                int cpuCount = deploymentDescriptorModel.getClientNodeSpec().getCpuCount();
                if (cpuCount > 0) {
                    propBuilder.putValues(DeploymentAttributes.VM_CPU_COUNT.name(), String.valueOf(cpuCount));
                }

                int memory = deploymentDescriptorModel.getClientNodeSpec().getMemoryGb();
                if (memory > 0) {
                    propBuilder.putValues(DeploymentAttributes.VM_MEMORY.name(), String.valueOf(memory));
                }
            }

            // Process client group and add group properties.
            String groupId;
            String groupName = client.getGroupName();
            if (groupMap.containsKey(groupName)) {
                groupId = groupMap.get(groupName);
            } else {
                groupId = UUID.randomUUID().toString();
                groupMap.put(groupName, groupId);
            }

            if (StringUtils.hasText(groupName)) {
                propBuilder.putValues(NodeProperty.Name.CLIENT_GROUP_ID.name(), groupId);
                propBuilder.putValues(NodeProperty.Name.CLIENT_GROUP_NAME.name(), groupName);
            }


            nodeAssignmentBuilder.addEntries(
                    NodeAssignment.Entry.newBuilder().setType(NodeType.CLIENT)
                            .setNodeId(UUID.randomUUID().toString())
                            .setSite(OrchestrationSiteIdentifier.newBuilder().setId(client.getZoneName()).build())
                            .setProperties(propBuilder)
            );
        });
    }

    /**
     * Build sites info.
     * @param infrastructureDescriptorModel the infra descriptor
     * @param replicas the list of replicas specified in the deployment descriptor
     * @param clients the list of clients specified in the deployment descriptor
     * @return a list of orchestration sites (one per zone)
     */
    public static List<OrchestrationSite> buildSites(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            List<DeploymentDescriptorModel.Replica> replicas, List<DeploymentDescriptorModel.Client> clients) {

        Set<String> allUniqueZoneNames = new HashSet<>();
        Set<String> replicaZoneNames = replicas.stream().map(
                DeploymentDescriptorModel.Replica::getZoneName).collect(Collectors.toSet());
        allUniqueZoneNames.addAll(replicaZoneNames);
        Set<String> clientZoneNames = clients.stream()
                .map(DeploymentDescriptorModel.Client::getZoneName).collect(Collectors.toSet());
        allUniqueZoneNames.addAll(clientZoneNames);

        List<OrchestrationSite> sites = allUniqueZoneNames.stream()
                .map(zoneName -> toOrchestrationSite(zoneName, infrastructureDescriptorModel))
                .collect(Collectors.toList());
        return sites;
    }


    private static Properties buildProperties(
            InfrastructureDescriptorModel infrastructureDescriptorModel) {
        Properties.Builder propertiesBuilder = Properties.newBuilder();
        // Build properties

        if (infrastructureDescriptorModel.getOrganization().getAdvancedFeatures() != null
            && !infrastructureDescriptorModel.getOrganization().getAdvancedFeatures().isEmpty()) {
            propertiesBuilder.putAllValues(infrastructureDescriptorModel.getOrganization().getAdvancedFeatures());
        }

        String dockerImage = infrastructureDescriptorModel.getOrganization().getDockerImage();
        if (StringUtils.hasText(dockerImage)) {
            propertiesBuilder.putValues(DeploymentAttributes.IMAGE_TAG.name(), dockerImage);
        }

        String damlSdk = infrastructureDescriptorModel.getOrganization().getDamlSdk();
        if (StringUtils.hasText(damlSdk)) {
            propertiesBuilder.putValues(DeploymentAttributes.DAML_SDK_VERSION.name(), damlSdk);
        }

        UUID templateId = infrastructureDescriptorModel.getOrganization().getTemplateId();
        if (templateId != null) {
            propertiesBuilder.putValues(DeploymentAttributes.TEMPLATE_ID.name(), templateId.toString());
        }

        boolean generatePassword = infrastructureDescriptorModel.getOrganization().isGeneratePassword();
        if (generatePassword) {
            propertiesBuilder.putValues(DeploymentAttributes.GENERATE_PASSWORD.name(), "True");
        }
        return propertiesBuilder.build();
    }

    private static OrchestrationSite toOrchestrationSite(
            String zoneName, InfrastructureDescriptorModel infrastructureDescriptorModel) {

        OrchestrationSiteInfo orchestrationSiteInfo =
                toOrchestrationSiteInfo(zoneName, infrastructureDescriptorModel);

        OrchestrationSiteIdentifier siteIdentifier =
                OrchestrationSiteIdentifier.newBuilder().setId(zoneName).build();

        OrchestrationSite orchestrationSite = OrchestrationSite.newBuilder()
                .setInfo(orchestrationSiteInfo)
                .setId(siteIdentifier)
                .build();

        return orchestrationSite;
    }

    private static OrchestrationSiteInfo toOrchestrationSiteInfo(
            String zoneName, InfrastructureDescriptorModel infrastructureDescriptorModel) {

        Optional<InfrastructureDescriptorModel.Zone> zoneDescriptorOpt =
                DescriptorUtils.getZone(zoneName, infrastructureDescriptorModel);

        // Validation should have caught this, but better to be safe
        if (zoneDescriptorOpt.isEmpty()) {
            log.error("Could not find zone info in the infrastructure descriptor for zone: {}", zoneName);
            throw new CastorException(ErrorCode.INVALID_DESCRIPTOR_CONFIGURATION, zoneName);
        }

        InfrastructureDescriptorModel.Zone zoneDescriptor = zoneDescriptorOpt.get();

        InfrastructureDescriptorModel.NotaryServer notaryServerDescriptor = zoneDescriptor.getNotaryServer();
        Endpoint.Builder notaryServerBuilder = Endpoint.newBuilder();
        if (notaryServerDescriptor != null) {
            if (notaryServerDescriptor.getUrl() != null) {
                notaryServerBuilder.setAddress(notaryServerDescriptor.getUrl().toString());
            }

            if (StringUtils.hasText(notaryServerDescriptor.getTlsCertificateData())) {
                notaryServerBuilder.setTransportSecurity(TransportSecurity
                                                                 .newBuilder()
                                                                 .setType(TransportSecurity.Type.TLSv1_2)
                                                                 .setCertificateData(notaryServerDescriptor
                                                                                 .getTlsCertificateData()).build());
            } else {
                notaryServerBuilder.setTransportSecurity(
                        TransportSecurity.newBuilder().setType(TransportSecurity.Type.NONE));
            }
        }

        InfrastructureDescriptorModel.Wavefront wavefrontDescriptor = zoneDescriptor.getWavefront();
        Wavefront.Builder waveFrontBuilder = Wavefront.newBuilder();
        if (wavefrontDescriptor != null) {
            waveFrontBuilder
                    .setUrl(wavefrontDescriptor.getUrl().toString())
                    .setToken(wavefrontDescriptor.getToken())
                    .build();
        }

        InfrastructureDescriptorModel.ElasticSearch elasticSearchDescriptor = zoneDescriptor.getElasticSearch();
        ElasticSearch.Builder elasticSearchBuilder = ElasticSearch.newBuilder();
        if (elasticSearchDescriptor != null) {
            elasticSearchBuilder
                    .setUrl(elasticSearchDescriptor.getUrl().toString())
                    .setUsername(elasticSearchDescriptor.getUserName())
                    .setPassword(elasticSearchDescriptor.getPassword())
                    .build();
        }

        InfrastructureDescriptorModel.ContainerRegistry containerDescriptor =
                zoneDescriptor.getContainerRegistry();
        Endpoint.Builder containerBuilder = Endpoint.newBuilder();

        if (containerDescriptor != null) {
            Credential containerRegistryCredential = Credential.newBuilder()
                    .setType(Credential.Type.PASSWORD)
                    .setPasswordCredential(
                            PasswordCredential.newBuilder()
                                    .setUsername(containerDescriptor.getUserName())
                                    .setPassword(containerDescriptor.getPassword())
                                    .build()
                    )
                    .build();

            containerBuilder
                .setAddress(containerDescriptor.getUrl().toString())
                .setCredential(containerRegistryCredential);

            if (StringUtils.hasText(containerDescriptor.getTlsCertificateData())) {
                containerBuilder.setTransportSecurity(TransportSecurity
                                                              .newBuilder()
                                                              .setType(TransportSecurity.Type.TLSv1_2)
                                                              .setCertificateData(containerDescriptor
                                                                                  .getTlsCertificateData()));
            } else {
                containerBuilder.setTransportSecurity(
                        TransportSecurity.newBuilder().setType(TransportSecurity.Type.NONE));
            }
        }

        // Build vSphere data center info
        InfrastructureDescriptorModel.Network networkDescriptor = zoneDescriptor.getNetwork();

        List<String> nameServers = networkDescriptor.getNameServers();
        if (nameServers == null || nameServers.size() == 0) {
            nameServers = Collections.emptyList();
        }

        IPv4Network network = IPv4Network.newBuilder()
                .setName(networkDescriptor.getName())
                .setAddressAllocation(IPv4Network.AddressAllocationScheme.STATIC)
                .setGatewayIp(networkDescriptor.getGateway())
                .setSubnet(networkDescriptor.getSubnet())
                .setAllocationServer(Endpoint.newBuilder().build())
                .addAllNameServers(nameServers)
                .build();

        InfrastructureDescriptorModel.OutboundProxy outboundProxyDescriptor =
                zoneDescriptor.getOutboundProxy();

        OutboundProxyInfo.Builder outboundProxyInfoBuilder = OutboundProxyInfo.newBuilder();
        if (outboundProxyDescriptor != null) {
            String httpHost = outboundProxyDescriptor.getHttpHost();
            if (StringUtils.hasText(httpHost)) {
                outboundProxyInfoBuilder.setHttpHost(httpHost);
                outboundProxyInfoBuilder.setHttpPort(outboundProxyDescriptor.getHttpPort());
            }

            String httpsHost = outboundProxyDescriptor.getHttpsHost();
            if (StringUtils.hasText(httpsHost)) {
                outboundProxyInfoBuilder.setHttpsHost(httpsHost);
                outboundProxyInfoBuilder.setHttpsPort(outboundProxyDescriptor.getHttpsPort());
            }
        }

        InfrastructureDescriptorModel.VCenter vCenterDescriptor = zoneDescriptor.getVCenter();

        VSphereDatacenterInfo vSphereDatacenterInfo = VSphereDatacenterInfo.newBuilder()
                .setResourcePool(vCenterDescriptor.getResourcePool())
                .setDatastore(vCenterDescriptor.getStorage())
                .setFolder(vCenterDescriptor.getFolder())
                .setNetwork(network)
                .setOutboundProxy(outboundProxyInfoBuilder.build())
                .build();

        List<LogManagement> logManagements = buildLogManagements(zoneDescriptor);

        // Build orchestration site
        VSphereOrchestrationSiteInfo.Builder vSphereOrchestrationSiteInfoBuilder =
                VSphereOrchestrationSiteInfo.newBuilder();

        Credential vCenterCredential = Credential.newBuilder()
                .setType(Credential.Type.PASSWORD)
                .setPasswordCredential(
                        PasswordCredential.newBuilder()
                                .setUsername(vCenterDescriptor.getUserName())
                                .setPassword(vCenterDescriptor.getPassword())
                                .build()
                )
                .build();

        Endpoint.Builder vCenterApiEndpointBuilder = Endpoint.newBuilder()
                .setAddress(vCenterDescriptor.getUrl().toString())
                .setCredential(vCenterCredential);

        if (StringUtils.hasText(vCenterDescriptor.getTlsCertificateData())) {
            vCenterApiEndpointBuilder.setTransportSecurity(TransportSecurity.newBuilder()
                .setCertificateData(vCenterDescriptor.getTlsCertificateData()).build());
        } else {
            vCenterApiEndpointBuilder.setTransportSecurity(TransportSecurity.newBuilder().build());
        }

        Endpoint vCenterApiEndpoint = vCenterApiEndpointBuilder.build();

        vSphereOrchestrationSiteInfoBuilder
                .setApi(vCenterApiEndpoint)
                .setContainerRegistry(containerBuilder.build())
                .setNotaryServer(notaryServerBuilder.build())
                .setVsphere(vSphereDatacenterInfo)
                .setWavefront(waveFrontBuilder.build())
                .setElasticsearch(elasticSearchBuilder.build())
                .addAllLogManagements(logManagements);

        Map<String, String> labels = new HashMap<>();
        if (StringUtils.hasText(zoneDescriptor.getName())) {
            labels.put(NAME_KEY, zoneDescriptor.getName());
        }
        if (StringUtils.hasText(zoneDescriptor.getLatitude())) {
            labels.put(LAT_KEY, zoneDescriptor.getLatitude());
        }
        if (StringUtils.hasText(zoneDescriptor.getLongitude())) {
            labels.put(LONG_KEY, zoneDescriptor.getLongitude());
        }

        VSphereOrchestrationSiteInfo vSphereOrchestrationSiteInfo = vSphereOrchestrationSiteInfoBuilder.build();

        OrchestrationSiteInfo orchestrationSiteInfo = OrchestrationSiteInfo.newBuilder()
                .setType(OrchestrationSiteInfo.Type.VSPHERE)
                .putAllLabels(labels)
                .setVmc(VmcOrchestrationSiteInfo.newBuilder().build())
                .setVsphere(vSphereOrchestrationSiteInfo)
                .build();

        return orchestrationSiteInfo;
    }

    private static List<LogManagement> buildLogManagements(InfrastructureDescriptorModel.Zone zoneDescriptor) {
        List<InfrastructureDescriptorModel.LogManagement> logManagementDescriptor = zoneDescriptor.getLogManagement();
        if (logManagementDescriptor == null) {
            return Collections.emptyList();
        }

        List<LogManagement> logManagements = logManagementDescriptor.stream()
                .map(lmd -> {
                    String address = lmd.getPort() == 0 ? lmd.getAddress() : lmd.getAddress() + ":" + lmd.getPort();

                    Credential credential = Credential.newBuilder()
                            .setType(Credential.Type.PASSWORD)
                            .setPasswordCredential(
                                    PasswordCredential.newBuilder()
                                            .setUsername(lmd.getUserName())
                                            .setPassword(lmd.getPassword())
                                            .build()
                            )
                            .build();

                    Endpoint endpoint = Endpoint.newBuilder()
                            .setAddress(address)
                            .setCredential(credential)
                            .build();

                    LogManagement logManagement = LogManagement.newBuilder()
                            .setDestination(LogManagement.Type.valueOf(lmd.getType()))
                            .setEndpoint(endpoint)
                            .setLogInsightAgentId(lmd.getLogInsightAgentId())
                            .build();

                    return logManagement;
                })
                .collect(Collectors.toList());

        return logManagements;
    }
}
