/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.google.common.base.Strings;
import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.Credential;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
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
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.Sites;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.v1.TransportSecurity;
import com.vmware.blockchain.deployment.v1.VSphereDatacenterInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Wavefront;

import lombok.extern.log4j.Log4j2;

/**
 * Provisioner service implementation.
 */
@Service
@Log4j2
public class ProvisionerServiceImpl implements ProvisionerService {

    private static final String NAME_KEY = "name";
    private static final String LAT_KEY = "geo-latitude";
    private static final String LONG_KEY = "geo-longitude";
    private static final String PROVISIONING_TIMEOUT_MINUTES_KEY = "castor.deployment.timeout.minutes";

    private final Environment environment;
    private final ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient;
    private final ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningClient;

    private final Map<DeploymentDescriptorModel.BlockchainType, BlockchainType> blockchainTypeMap = new HashMap<>();

    @Autowired
    public ProvisionerServiceImpl(
            Environment environment,
            ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub blockingProvisioningClient,
            ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub asyncProvisioningClient) {
        this.environment = environment;
        this.blockingProvisioningClient = blockingProvisioningClient;
        this.asyncProvisioningClient = asyncProvisioningClient;

        blockchainTypeMap.put(DeploymentDescriptorModel.BlockchainType.DAML, BlockchainType.DAML);
        blockchainTypeMap.put(DeploymentDescriptorModel.BlockchainType.ETHEREUM, BlockchainType.ETHEREUM);
        blockchainTypeMap.put(DeploymentDescriptorModel.BlockchainType.HLF, BlockchainType.HLF);
    }

    @Override
    public void provisioningHandoff(
            PrintWriter printWriter, InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) {

        DeploymentRequest deploymentRequest = constructDeploymentRequest(
                infrastructureDescriptorModel, deploymentDescriptorModel);
        log.debug("Deployment request " + deploymentRequest);

        // Request a deployment from the provisioning service
        log.debug("Requesting a deployment with request info: {}", deploymentRequest);
        String deploymentRequestId = submitDeploymentRequest(deploymentRequest, deploymentCompletionFuture);
        printWriter.printf("Deployment Request Id: %s\n", deploymentRequestId);

        // Process callbacks from the provisioning service
        CastorDeploymentStatus status = provisionAndComplete(printWriter, deploymentRequestId);
        if (CastorDeploymentStatus.FAILURE == status) {
            deprovisionDeployment(infrastructureDescriptorModel, deploymentDescriptorModel, deploymentRequestId);
            deploymentCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        } else {
            deploymentCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
        }
    }

    private String submitDeploymentRequest(
            DeploymentRequest deploymentRequest,
            CompletableFuture<CastorDeploymentStatus> deploymentCompletionFuture) throws CastorException {
        try {
            DeploymentRequestResponse response = blockingProvisioningClient.createDeployment(deploymentRequest);
            String requestId = response.getId();
            log.info("Deployment submitted, request id {}", requestId);
            return requestId;
        } catch (Throwable e) {
            deploymentCompletionFuture.completeExceptionally(e);
            log.error("Error in submitting a deployment request", e);
            throw new CastorException(ErrorCode.DEPL_REQUEST_SUBMIT_ERROR, e);
        }
    }

    private CastorDeploymentStatus provisionAndComplete(PrintWriter printWriter, String deploymentRequestId) {

        // This future is for completion of provisioning itself, since those callbacks are asynchronous
        CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture = new CompletableFuture<>();

        // Request callbacks from Persephone to the previously issued deployment request.
        streamDeploymentSessionEvents(printWriter, deploymentRequestId, provisioningCompletionFuture);

        // Wait until the deployment session is completed.
        long provisioningTimeoutMinutes = environment.getProperty(PROVISIONING_TIMEOUT_MINUTES_KEY, Long.class, 30L);
        try {
            CastorDeploymentStatus provisioningStatus =
                    provisioningCompletionFuture.get(provisioningTimeoutMinutes, TimeUnit.MINUTES);
            return provisioningStatus;
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            log.error("Encountered error during provisioning call", e);
            return CastorDeploymentStatus.FAILURE;
        }
    }

    private void streamDeploymentSessionEvents(PrintWriter printWriter, String deploymentRequestId,
                                               CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture) {
        DeploymentExecutionEventResponseObserver deero =
                new DeploymentExecutionEventResponseObserver(
                        printWriter, deploymentRequestId, provisioningCompletionFuture);

        StreamDeploymentSessionEventRequest request =
                StreamDeploymentSessionEventRequest.newBuilder()
                        .setHeader(MessageHeader.newBuilder().build())
                        .setSessionId(deero.getRequestId())
                        .build();

        asyncProvisioningClient.streamDeploymentSessionEvents(request, deero);
    }

    private DeploymentRequest constructDeploymentRequest(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel) {

        // Build committer and client nodes
        NodeAssignment.Builder nodeAssignmentBuilder = NodeAssignment.newBuilder();
        // Build Committers
        buildCommitters(infrastructureDescriptorModel, deploymentDescriptorModel, nodeAssignmentBuilder);
        // Build Clients
        buildClients(infrastructureDescriptorModel, deploymentDescriptorModel, nodeAssignmentBuilder);

        // Build sites
        List<OrchestrationSite> orchestrationSites =
                buildSites(infrastructureDescriptorModel, deploymentDescriptorModel);

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

    private List<OrchestrationSite> buildSites(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel) {

        Set<String> allUniqueZoneNames = new HashSet<>();
        Set<String> committerZoneNames = deploymentDescriptorModel.getCommitters().stream().map(
                DeploymentDescriptorModel.Committer::getZoneName).collect(Collectors.toSet());
        allUniqueZoneNames.addAll(committerZoneNames);
        Set<String> clientZoneNames = deploymentDescriptorModel.getClients().stream()
                .map(DeploymentDescriptorModel.Client::getZoneName).collect(Collectors.toSet());
        allUniqueZoneNames.addAll(clientZoneNames);

        List<OrchestrationSite> sites = allUniqueZoneNames.stream()
                .map(zoneName -> toOrchestrationSite(zoneName, infrastructureDescriptorModel))
                .collect(Collectors.toList());
        return sites;
    }

    private void buildClients(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel,
            NodeAssignment.Builder nodeAssignmentBuilder) {
        List<DeploymentDescriptorModel.Client> clients = deploymentDescriptorModel.getClients();

        // A map to hold groupIndex to groupId (UUID) mapping.
        final Map<String, String> groupMap = new HashMap<>();

        clients.forEach(client -> {
            Properties.Builder propBuilder = Properties.newBuilder();
            if (StringUtils.hasText(client.getAuthUrlJwt())) {
                propBuilder.putValues(NodeProperty.Name.CLIENT_AUTH_JWT.name(), client.getAuthUrlJwt());
            }
            if (StringUtils.hasText(client.getProvidedIp())) {
                propBuilder.putValues(NodeProperty.Name.VM_IP.name(), client.getProvidedIp());
            }
            int clientDiskSize = infrastructureDescriptorModel.getOrganization().getClientDiskGb();
            if (clientDiskSize > 0) {
                String clientDiskSizeString = String.valueOf(clientDiskSize);
                propBuilder.putValues(DeploymentAttributes.VM_STORAGE.name(), clientDiskSizeString);
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

            if (!Strings.isNullOrEmpty(groupName)) {
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

    private void buildCommitters(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel,
            NodeAssignment.Builder nodeAssignmentBuilder) {
        List<DeploymentDescriptorModel.Committer> committers = deploymentDescriptorModel.getCommitters();
        committers.forEach(committer -> {
            Properties.Builder propBuilder = Properties.newBuilder();
            if (StringUtils.hasText(committer.getProvidedIp())) {
                propBuilder.putValues(NodeProperty.Name.VM_IP.name(), committer.getProvidedIp());
            }
            int committerDiskSize = infrastructureDescriptorModel.getOrganization().getCommitterDiskGb();
            if (committerDiskSize > 0) {
                String committerDiskSizeString = String.valueOf(committerDiskSize);
                propBuilder.putValues(DeploymentAttributes.VM_STORAGE.name(), committerDiskSizeString);
            }

            nodeAssignmentBuilder.addEntries(
                    NodeAssignment.Entry.newBuilder()
                            .setType(NodeType.REPLICA)
                            .setNodeId(UUID.randomUUID().toString())
                            .setSite(OrchestrationSiteIdentifier.newBuilder().setId(committer.getZoneName()).build())
                            .setProperties(propBuilder)
            );
        });
    }

    private Properties buildProperties(
            InfrastructureDescriptorModel infrastructureDescriptorModel) {
        Properties.Builder propertiesBuilder = Properties.newBuilder();
        // Build properties
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

        int cpuCount = infrastructureDescriptorModel.getOrganization().getCpuCount();
        if (cpuCount > 0) {
            propertiesBuilder.putValues(DeploymentAttributes.VM_CPU_COUNT.name(), String.valueOf(cpuCount));
        }

        int memory = infrastructureDescriptorModel.getOrganization().getMemoryGb();
        if (memory > 0) {
            propertiesBuilder.putValues(DeploymentAttributes.VM_MEMORY.name(), String.valueOf(memory));
        }

        boolean generatePassword = infrastructureDescriptorModel.getOrganization().isGeneratePassword();
        if (generatePassword) {
            propertiesBuilder.putValues(DeploymentAttributes.GENERATE_PASSWORD.name(), "True");
        }
        return propertiesBuilder.build();
    }


    private static int fromIpAddr(String ipAddr) {
        String[] ips = ipAddr.split("\\.");
        int result = 0;
        for (int i = 0; i < 4; i++) {
            int ip = Integer.parseInt(ips[i]);
            result <<= 8;
            result |= ip;
        }
        return result;
    }

    private OrchestrationSiteInfo toOrchestrationSiteInfo(
            String zoneName, InfrastructureDescriptorModel infrastructureDescriptorModel) {

        Optional<InfrastructureDescriptorModel.Zone> zoneDescriptorOpt =
                DescriptorUtils.getZone(zoneName, infrastructureDescriptorModel);

        // Validation should have caught this, but better to be safe
        if (zoneDescriptorOpt.isEmpty()) {
            log.error("Could not find zone info in the infrastructure descriptor for zone: {}", zoneName);
            throw new CastorException(ErrorCode.INVALID_DESCRIPTOR_CONFIGURATION, zoneName);
        }

        InfrastructureDescriptorModel.Zone zoneDescriptor = zoneDescriptorOpt.get();

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
                .setCredential(containerRegistryCredential)
                .setTransportSecurity(
                    TransportSecurity.newBuilder().setType(TransportSecurity.Type.NONE)
                );
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

        Endpoint vCenterApiEndpoint =
                Endpoint.newBuilder()
                        .setAddress(vCenterDescriptor.getUrl().toString())
                        .setCredential(vCenterCredential)
                        .setTransportSecurity(TransportSecurity.newBuilder().build())
                        .build();

        vSphereOrchestrationSiteInfoBuilder
                .setApi(vCenterApiEndpoint)
                .setContainerRegistry(containerBuilder.build())
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

    private List<LogManagement> buildLogManagements(InfrastructureDescriptorModel.Zone zoneDescriptor) {
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

    private OrchestrationSite toOrchestrationSite(
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

    private void deprovisionDeployment(
            InfrastructureDescriptorModel infrastructureDescriptorModel,
            DeploymentDescriptorModel deploymentDescriptorModel,
            String requestId) {
        log.info("Initiating deprovisioning for deployment with request id: {}", requestId);
        List<OrchestrationSite> orchestrationSites =
                buildSites(infrastructureDescriptorModel, deploymentDescriptorModel);
        Sites sites = Sites.newBuilder()
                .addAllInfoList(orchestrationSites)
                .build();

        DeprovisionDeploymentRequest deprovisionDeploymentRequest =
                DeprovisionDeploymentRequest.newBuilder()
                        .setSessionId(requestId)
                        .setSites(sites)
                        .build();

        // Use the blocking API since DeprovisionDeploymentResponse has no useful info.
        blockingProvisioningClient.deprovisionDeployment(deprovisionDeploymentRequest);
        log.info("Deprovisioning completed for deployment with request id: {}", requestId);
    }
}
