/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.deployment.v1.BlockchainType.DAML;
import static com.vmware.blockchain.deployment.v1.BlockchainType.ETHEREUM;
import static com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainGetResponse;
import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainPatch;
import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainPost;
import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainTaskResponse;
import static com.vmware.blockchain.services.blockchains.BlockchainUtils.toInfo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableMap;
import com.google.protobuf.InvalidProtocolBufferException;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.ForbiddenException;
import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentResponse;
import com.vmware.blockchain.deployment.v1.GenerateConfigurationResponse;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSite;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.Sites;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.clients.ClientService;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplate;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplateService;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.VmbcRoles;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;

/**
 * Controller to create and list blockchains.
 */
@RestController
public class BlockchainController {
    private static final Logger logger = LogManager.getLogger(BlockchainController.class);

    private static final Map<BlockchainType,
            com.vmware.blockchain.deployment.v1.BlockchainType> enumMapForBlockchainType =
            ImmutableMap.of(BlockchainType.ETHEREUM, ETHEREUM,
                    BlockchainType.DAML, DAML);

    private BlockchainService blockchainService;
    private OrganizationService organizationService;
    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private TaskService taskService;
    private ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub provisioningClient;
    private OperationContext operationContext;
    private ReplicaService replicaService;
    private ClientService clientService;
    private ZoneService zoneService;
    private ConnectionPoolManager connectionPoolManager;
    private Integer clientNumber;
    private List<Integer> replicaNumber;
    private NodeSizeTemplateService nodeSizeTemplateService;

    @Autowired
    public BlockchainController(BlockchainService blockchainService,
                                OrganizationService organizationService,
                                AuthHelper authHelper,
                                DefaultProfiles defaultProfiles,
                                TaskService taskService,
                                ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub provisioningClient,
                                OperationContext operationContext,
                                ReplicaService replicaService,
                                ClientService clientService,
                                ZoneService zoneService,
                                ConnectionPoolManager connectionPoolManager,
                                @Value("${vmbc.client.number}")
                                int clientNumber,
                                @Value("#{${vmbc.replica.number}}")
                                List<Integer> replicaNumber,
                                NodeSizeTemplateService nodeSizeTemplateService) {
        this.blockchainService = blockchainService;
        this.organizationService = organizationService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
        this.provisioningClient = provisioningClient;
        this.operationContext = operationContext;
        this.replicaService = replicaService;
        this.clientService = clientService;
        this.zoneService = zoneService;
        this.connectionPoolManager = connectionPoolManager;
        this.clientNumber = clientNumber;
        this.replicaNumber = replicaNumber;
        this.nodeSizeTemplateService = nodeSizeTemplateService;

    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUser()")
    ResponseEntity<List<BlockchainGetResponse>> list(
            @RequestParam(name = "all", required = false, defaultValue = "false") String all
    ) {
        boolean getAllBlockchains = Boolean.valueOf(all);
        List<Blockchain> chains;
        // if we are operator, we can get all blockchains.
        if (authHelper.hasAnyAuthority(VmbcRoles.systemAdmin())) {
            chains = blockchainService.list();
        } else {
            // Otherwise, we can only see our consortium.
            chains = blockchainService.listByIds(authHelper.getAccessChains());
        }

        List<BlockchainGetResponse> idList;

        if (!getAllBlockchains) {
            idList = chains.stream()
                    .filter(blockchain -> blockchain.getState() != Blockchain.BlockchainState.INACTIVE)
                    .map(BlockchainGetResponse::new)
                    .collect(Collectors.toList());
        } else {
            idList = chains.stream().map(BlockchainGetResponse::new)
                    .collect(Collectors.toList());
        }

        return new ResponseEntity<>(idList, HttpStatus.OK);
    }

    /**
     * Get the blockchain details.
     */
    @RequestMapping(path = "/api/blockchains/{bid}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.canAccessChain(#bid)")
    ResponseEntity<BlockchainGetResponse> get(@PathVariable UUID bid) throws NotFoundException {
        Blockchain blockchain = safeGetBlockchain(bid);

        // If we are to populate nodes.
        //List<NodeInterface> nodes = new ArrayList<>();
        //nodes.addAll(replicaService.getReplicas(id));
        //nodes.addAll(clientService.getClientsByParentId(id));

        BlockchainGetResponse blockchainGetResponse = new BlockchainGetResponse(blockchain);
        return new ResponseEntity<>(blockchainGetResponse, HttpStatus.OK);
    }

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     * Note that after deployment we must remove the authtoken from the cache, since the user will
     * now have access to this blockchain
     * @throws Exception any exception
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    public ResponseEntity<BlockchainTaskResponse> createBlockchain(@Valid @RequestBody BlockchainPost body)
            throws Exception {

        Organization organization = organizationService.get(authHelper.getOrganizationId());

        // Determine whether or not we can create a new blockchain
        var blockchainCount = getMaxChains(organization);
        if (authHelper.getUpdateChains().size() >= blockchainCount) {
            logger.info("Request for too many blockchains: current {}, limit {}",
                        authHelper.getUpdateChains().size(), blockchainCount);
            throw new BadRequestException(ErrorCodeType.BLOCKCHAIN_LIMIT, authHelper.getUpdateChains().size());
        }
        if (body.getBlockchainType() == null) {
            throw new BadRequestException(ErrorCodeType.BAD_REQUEST, "Invalid blockchain type.");
        }

        // Make sure we have access to given consortium.
        if (!authHelper.canUpdateConsortium(body.getConsortiumId())) {
            throw new ForbiddenException(ErrorCodeType.NOT_ALLOWED);
        }
        // Validate mandatory field
        // change to fixed list
        // logger.info("Creating new blockchain. Cluster size {}", clusterSize);

        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);

        createDeployment(body, organization, task);
        logger.info("Deployment scheduled");

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

    /**
     * Update the given blockchain.
     * @throws NotFoundException NotFoundException
     */
    @RequestMapping(path = "/api/blockchains/{bid}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainTaskResponse> updateBlockchain(@PathVariable UUID bid,
            @RequestBody BlockchainPatch body) throws NotFoundException {
        // TODO: Actual PATCH

        // Temporary: create a completed task that points to the default blockchain
        Blockchain blockchain = safeGetBlockchain(bid);

        Task task = new Task();
        task.setState(State.SUCCEEDED);
        task.setMessage("Default Blockchain");
        task.setResourceId(defaultProfiles.getBlockchain().getId());
        if (task.getResourceId() != null) {
            task.setResourceLink("/api/blockchains/".concat(defaultProfiles.getBlockchain().getId().toString()));
        }
        task = taskService.put(task);

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

    /**
     * Temp API, DO NOT publish.
     * API which generates a new config.
     */
    @RequestMapping(path = "/api/blockchains/{bid}/regenerate-configuration", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainApiObjects.GenerateConfigResponse> regenerateConfiguration(@PathVariable UUID bid)
            throws NotFoundException {

        Blockchain blockchain = safeGetBlockchain(bid);
        Organization organization = organizationService.get(authHelper.getOrganizationId());
        var replicas = replicaService.getReplicas(blockchain.getId());
        var clients = clientService.getClientsByParentId(blockchain.getId());

        Set<UUID> zoneIds = new HashSet<>();
        NodeAssignment.Builder nodeAssignment = NodeAssignment.newBuilder();

        replicas.stream().forEach(k -> {
            nodeAssignment.addEntries(NodeAssignment.Entry.newBuilder()
                    .setType(NodeType.REPLICA)
                    .setNodeId(k.getId().toString())
                    .setSite(OrchestrationSiteIdentifier.newBuilder()
                                    .setId(k.getZoneId().toString()).build())
                    .setProperties(Properties.newBuilder()
                            .putValues(DeployedResource.DeployedResourcePropertyKey.PRIVATE_IP.name(),
                                       k.getPrivateIp())
                                           .build()));

            zoneIds.add(k.getZoneId());
        });

        clients.stream().forEach(k -> {
            var properties = clientPropertyBuilder(k.getAuthJwtUrl(), k.getGroupName(),
                                                   k.getGroupId().toString());
            properties.putValues(DeployedResource.DeployedResourcePropertyKey.PRIVATE_IP.name(), k.getPrivateIp());
            nodeAssignment.addEntries(NodeAssignment.Entry.newBuilder()
                                              .setType(NodeType.CLIENT)
                                              .setNodeId(k.getId().toString())
                                              .setSite(OrchestrationSiteIdentifier.newBuilder()
                                                               .setId(k.getZoneId().toString()).build())
                                              .setProperties(properties));

            zoneIds.add(k.getZoneId());
        });

        List<OrchestrationSite> sitesInfo = getOrchestrationSites(zoneIds);


        var deploymentSpec = DeploymentSpec.newBuilder()
                .setConsortiumId(blockchain.consortium.toString())
                .setBlockchainId(blockchain.getId().toString())
                .setBlockchainType(enumMapForBlockchainType.get(blockchain.type))
                .setSites(Sites.newBuilder().addAllInfoList(sitesInfo).build())
                .setNodeAssignment(nodeAssignment)
                .setProperties(getPropertySpec(organization))
                .build();

        var request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder()
                                   .setId(operationContext.getId() != null ? operationContext.getId() : "").build())
                .setSpec(deploymentSpec)
                .build();

        var promise = new CompletableFuture<GenerateConfigurationResponse>();
        provisioningClient.generateConfiguration(request, FleetUtils.blockedResultObserver(promise));
        try {
            var configId = promise.get().getId();
            return new ResponseEntity<>(new BlockchainApiObjects.GenerateConfigResponse(UUID.fromString(configId)),
                                        HttpStatus.OK);
        } catch (Exception e) {
            logger.error("Error1 generating new configuration for blockchain {}", bid);
            throw new HelenException(ErrorCodeType.INTERNAL_ERROR,
                                     "Error generating new configuration for blockchain.");
        }
    }

    /**
     * Temp API, DO NOT publish.
     * API which generates a new config.
     */
    @RequestMapping(path = "/api/blockchains/{bid}/clone", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainApiObjects.BlockchainTaskResponse> cloneBlockchain(@PathVariable UUID bid)
            throws Exception {

        Blockchain blockchain = safeGetBlockchain(bid);
        var replicas = replicaService.getReplicas(blockchain.getId());
        var clients = clientService.getClientsByParentId(blockchain.getId());

        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);

        NodeAssignment.Builder nodeAssignment = NodeAssignment.newBuilder();
        Set<UUID> zoneIds = new HashSet<>();

        replicas.stream().forEach(k -> {
            nodeAssignment.addEntries(NodeAssignment.Entry.newBuilder()
                                              .setType(NodeType.REPLICA)
                                              .setNodeId(UUID.randomUUID().toString())
                                              .setSite(OrchestrationSiteIdentifier.newBuilder()
                                                               .setId(k.getZoneId().toString()).build())
                                              .build());

            zoneIds.add(k.getZoneId());
        });

        clients.stream().forEach(k -> {
            var properties = clientPropertyBuilder(k.getAuthJwtUrl(), k.getGroupName(),
                                                   k.getGroupId().toString());
            nodeAssignment.addEntries(NodeAssignment.Entry.newBuilder()
                                              .setType(NodeType.CLIENT)
                                              .setNodeId(UUID.randomUUID().toString())
                                              .setSite(OrchestrationSiteIdentifier.newBuilder()
                                                               .setId(k.getZoneId().toString()).build())
                                              .setProperties(properties));

            zoneIds.add(k.getZoneId());
        });

        List<OrchestrationSite> sitesInfo = getOrchestrationSites(zoneIds);

        Organization organization = organizationService.get(authHelper.getOrganizationId());
        var genericProperties = getPropertySpec(organization);
        genericProperties.putValues(DeploymentAttributes.NO_LAUNCH.name(), "True");
        var deploymentSpec = DeploymentSpec.newBuilder()
                .setConsortiumId(blockchain.consortium.toString())
                .setBlockchainType(enumMapForBlockchainType.get(blockchain.type))
                .setSites(Sites.newBuilder().addAllInfoList(sitesInfo).build())
                .setNodeAssignment(nodeAssignment)
                .setProperties(genericProperties)
                .build();

        invokeDeploymentRequest(blockchain.consortium, blockchain.type, task, nodeAssignment, deploymentSpec);
        logger.info("Deployment scheduled");

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

    /**
     * De-register the blockchain.
     */
    @RequestMapping(path = "/api/blockchains/deregister/{bid}", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainGetResponse> deRegister(@PathVariable("bid") UUID bid) throws Exception {
        Blockchain blockchain = safeGetBlockchain(bid);

        if (blockchain.getState() != Blockchain.BlockchainState.INACTIVE) {
            blockchain.setState(Blockchain.BlockchainState.INACTIVE);
            blockchain = blockchainService.put(blockchain);
            return new ResponseEntity<>(new BlockchainGetResponse(blockchain), HttpStatus.ACCEPTED);
        } else {
            throw new BadRequestException(String.format("Blockchain %s is already de-registered.", bid.toString()));
        }
    }


    /**
     * Temporary pre IA/GA to handle resources/POC. Should not be published in the API doc or provided to the customer.
     * @param bid BlockchainId
     */
    @RequestMapping(path = "/api/blockchains/{bid}", method = RequestMethod.DELETE)
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainGetResponse> deprovision(@PathVariable("bid") UUID bid) {
        Blockchain blockchain = safeGetBlockchain(bid);
        if (blockchain.getState() != Blockchain.BlockchainState.ACTIVE) {
            Set<UUID> zoneIds = new HashSet<>();
            zoneIds.addAll(replicaService.getReplicas(blockchain.getId()).stream().map(k -> k.getZoneId())
                                   .collect(Collectors.toList()));
            zoneIds.addAll(clientService.getClientsByParentId(blockchain.getId()).stream().map(k -> k.getZoneId())
                                   .collect(Collectors.toList()));
            List<OrchestrationSite> sitesInfo = getOrchestrationSites(zoneIds);

            // This will need modification when versioning kicks in.
            List<DeployedResource> resource = blockchain.getRawResourceInfo().values().stream()
                    .flatMap(Collection::stream).collect(Collectors.toList()).stream().map(k -> {
                        try {
                            DeployedResource.Builder builder = DeployedResource.newBuilder();
                            com.google.protobuf.util.JsonFormat.parser().ignoringUnknownFields().merge(k, builder);
                            return builder.build();
                        } catch (InvalidProtocolBufferException e) {
                            throw new HelenException(
                                    String.format("Error accepting request for deletion {}", bid.toString()));
                        }
                    }).collect(Collectors.toList());

            var request = DeprovisionDeploymentRequest.newBuilder()
                    .setSites(Sites.newBuilder().addAllInfoList(sitesInfo).build())
                    .addAllResource(resource)
                    .build();

            var promise = new CompletableFuture<DeprovisionDeploymentResponse>();
            provisioningClient.deprovisionDeployment(request, FleetUtils.blockedResultObserver(promise));
            // Does not wait for response.
            return new ResponseEntity<>(new BlockchainGetResponse(blockchain), HttpStatus.ACCEPTED);

        } else {
            throw new BadRequestException(String.format("Blockchain %s is in active state and cannot be deleted."
                                                        + "Deregister the blockchain first.", bid.toString()));
        }
    }
    // --------------------- Private methods -------------------- //


    /**
     * The actual call which will contact server and add the model request.
     */
    private void createDeployment(BlockchainPost body, Organization organization, Task task) throws Exception {
        // Validate number of replicas and clients.
        if (!validateNumberofReplicas(body) || !validateNumberOfClients(body)) {
            throw new BadRequestException(String.format("Expected number of replicas is %s", replicaNumber,
                                         "user input replicas %s", body.getReplicaZoneIds().size(),
                                         "Expected number of clients is <= %s", clientNumber,
                                         "user input clients is %s", body.getClientNodes().size()));
        }
        NodeAssignment.Builder nodeAssignment = NodeAssignment.newBuilder();
        Properties.Builder basePropBuilder = getPropertySpec(organization);

        // Collect a list of zones from replicas and clients.
        List<UUID> zoneIds = new ArrayList<>();
        // Process the replica nodes.
        if (body.getReplicaNodes() != null) {
            body.getReplicaNodes()
                    .forEach(k -> {
                        // Add the zoneId to zoneIds.
                        zoneIds.add(k.getZoneId());

                        Properties.Builder propBuilder = Properties.newBuilder();
                        if (k.getSizingInfo() != null && !k.getSizingInfo().isEmpty()) {
                            // Validate sizing info, if the input is invalid throw a BadRequestException.
                            validateSizingInfo(k.getSizingInfo(), "Replica");
                            // Add Sizing Info to Properties.
                            addSizingInfoToProperties(k.getSizingInfo(), propBuilder);
                        }

                        nodeAssignment.addEntries(NodeAssignment.Entry
                                                          .newBuilder()
                                                          .setType(NodeType.REPLICA)
                                                          .setNodeId(UUID.randomUUID().toString())
                                                          .setSite(
                                                                  OrchestrationSiteIdentifier
                                                                          .newBuilder()
                                                                          .setId(k.getZoneId().toString())
                                                                          .build())
                                                          .setProperties(propBuilder));
                    });
            logger.debug("zoneIds from replica nodes parameter: {}", zoneIds);
            logger.debug("nodeAssignmentEntries from replica nodes parameter {}",
                         nodeAssignment.getEntriesList());
        } else if (body.getReplicaZoneIds() != null) {
            // Not a big fan of big if-else statements but this is an exception because code will go soon.
            // Get the replica zone ids from "replica_zone_ids" parameter.
            zoneIds.addAll(body.getReplicaZoneIds());

            //remove this after replicaZoneIds is deprecated
            zoneIds
                    .forEach(k -> nodeAssignment.addEntries(NodeAssignment.Entry.newBuilder()
                                                                    .setType(NodeType.REPLICA)
                                                                    .setNodeId(UUID.randomUUID().toString())
                                                                    .setSite(
                                                                            OrchestrationSiteIdentifier.newBuilder()
                                                                                    .setId(k.toString()).build())));
            logger.debug("zoneIds from replica zone ids parameter: {}", zoneIds);
            logger.debug("nodeAssignmentEntries from replica zone ids parameter {}",
                         nodeAssignment.getEntriesList());
        }

        // Process client nodes.
        if (body.getClientNodes() != null) {
            // A map to hold groupIndex to groupId (UUID) mapping.
            final Map<String, UUID> groupMap = new HashMap<String, UUID>();
            body.getClientNodes()
                    .forEach(k -> {

                        // Add the zoneId to zoneIds.
                        zoneIds.add(k.getZoneId());

                        UUID groupId = null;
                        if (groupMap.containsKey(k.getGroupName())) {
                            groupId = groupMap.get(k.getGroupName());
                        } else {
                            groupId = UUID.randomUUID();
                            groupMap.put(k.getGroupName(), groupId);
                        }
                        Properties.Builder propBuilder = clientPropertyBuilder(k.getAuthUrlJwt(),
                                                                               k.getGroupName(), groupId.toString());

                        if (k.getSizingInfo() != null && !k.getSizingInfo().isEmpty()) {
                            // Validate sizing info, if the input is invalid throw a BadRequestException.
                            validateSizingInfo(k.getSizingInfo(), "Client");
                            // Add Sizing Info to Properties.
                            addSizingInfoToProperties(k.getSizingInfo(), propBuilder);
                        }

                        // All credentials have to be provided to enable TLS
                        boolean tlsEnabled = !Strings.isNullOrEmpty(k.getPem())
                                && !Strings.isNullOrEmpty(k.getCrt())
                                && !Strings.isNullOrEmpty(k.getCacrt());


                        if (tlsEnabled) {
                            // Propagate TLS details if and only if all details are provided
                            String pem = k.getPem();
                            String crt = k.getCrt();
                            String cacrt = k.getCacrt();
                            propBuilder.putValues(NodeProperty.Name.TLS_PEM.name(), pem);
                            propBuilder.putValues(NodeProperty.Name.TLS_CRT.name(), crt);
                            propBuilder.putValues(NodeProperty.Name.TLS_CACRT.name(), cacrt);
                        } else {
                            // If some credentials are present while others are absent, throw BAD REQUEST exception.
                            boolean incompleteDetails = !Strings.isNullOrEmpty(k.getPem())
                                    || !Strings.isNullOrEmpty(k.getCrt())
                                    || !Strings.isNullOrEmpty(k.getCacrt());

                            if (incompleteDetails) {
                                throw new BadRequestException(ErrorCodeType.BAD_REQUEST,
                                        "Missing TLS credentials. Please provide all credentials");
                            }
                        }

                        nodeAssignment.addEntries(NodeAssignment.Entry
                                                          .newBuilder()
                                                          .setType(NodeType.CLIENT)
                                                          .setNodeId(UUID.randomUUID().toString())
                                                          .setSite(
                                                                  OrchestrationSiteIdentifier
                                                                          .newBuilder()
                                                                          .setId(k.getZoneId().toString())
                                                                          .build())
                                                          .setProperties(propBuilder));
                        logger.debug("nodeAssignmentEntries from client nodes {}",
                                     nodeAssignment.getEntriesList());
                    });
            logger.debug("zoneIds collected so far, from replicas and client nodes {}", zoneIds);
        }

        var blockChainType = enumMapForBlockchainType.get(body.getBlockchainType());

        List<OrchestrationSite> sitesInfo = getOrchestrationSites(new HashSet(zoneIds));

        var deploymentSpec = DeploymentSpec.newBuilder()
                .setConsortiumId(body.getConsortiumId().toString())
                .setBlockchainType(blockChainType)
                .setSites(Sites.newBuilder().addAllInfoList(sitesInfo).build())
                .setNodeAssignment(nodeAssignment)
                .setProperties(basePropBuilder)
                .build();

        logger.debug("deployment spec {}", deploymentSpec);
        invokeDeploymentRequest(body.getConsortiumId(), body.getBlockchainType(), task, nodeAssignment, deploymentSpec);
    }

    private void invokeDeploymentRequest(UUID consortiumId, BlockchainType type,
                                         Task task, NodeAssignment.Builder nodeAssignment,
                                         DeploymentSpec deploymentSpec)
            throws InterruptedException, java.util.concurrent.ExecutionException {

        var request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder()
                                   .setId(operationContext.getId() != null ? operationContext.getId() : "").build())
                .setSpec(deploymentSpec)
                .build();

        // Check that the API can be serviced normally after service initialization.
        var promise = new CompletableFuture<DeploymentRequestResponse>();
        provisioningClient.createDeployment(request, FleetUtils.blockedResultObserver(promise));
        String dsId = promise.get().getId();
        logger.info("Deployment started, id {} for the consortium id {}", dsId, consortiumId);

        Blockchain rawBlockchain = new Blockchain();
        rawBlockchain.setType(type);
        rawBlockchain.setConsortium(consortiumId);

        BlockchainObserver bo =
                new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService, clientService,
                                       taskService,
                                       connectionPoolManager,
                                       task.getId(), nodeAssignment.build(),
                                       rawBlockchain);
        // Watch for the event stream
        StreamDeploymentSessionEventRequest streamRequest = StreamDeploymentSessionEventRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder()
                                   .setId(operationContext.getId() != null ? operationContext.getId() : "").build())
                .setSessionId(dsId)
                .build();
        provisioningClient.streamDeploymentSessionEvents(streamRequest, bo);
    }

    private Properties.Builder clientPropertyBuilder(String authJwtUrl,
                                                     String groupName,
                                                     String groupId) {
        Properties.Builder propBuilder = Properties.newBuilder();
        if (!Strings.isNullOrEmpty(authJwtUrl)) {
            propBuilder.putValues(NodeProperty.Name.CLIENT_AUTH_JWT.name(),
                                  authJwtUrl);
        }
        if (!Strings.isNullOrEmpty(groupName)) {
            propBuilder.putValues(NodeProperty.Name.CLIENT_GROUP_ID.name(), groupId);
            propBuilder.putValues(NodeProperty.Name.CLIENT_GROUP_NAME.name(), groupName);
        }
        return propBuilder;
    }

    private Properties.Builder getPropertySpec(Organization organization) {
        Properties.Builder basePropBuilder = Properties.newBuilder();
        if (organization.getOrganizationProperties() != null) {
            if (organization.getOrganizationProperties().containsKey(Constants.ORG_DOCKER_IMAGE_OVERRIDE)) {
                basePropBuilder.putValues(DeploymentAttributes.IMAGE_TAG.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_DOCKER_IMAGE_OVERRIDE));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.DAML_SDK_INFO_OVERRIDE)) {
                basePropBuilder.putValues(DeploymentAttributes.DAML_SDK_VERSION.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.DAML_SDK_INFO_OVERRIDE));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_TEMPLATE_ID_OVERRIDE)) {
                basePropBuilder.putValues(DeploymentAttributes.TEMPLATE_ID.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_TEMPLATE_ID_OVERRIDE));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_GENERATE_PASSWORD)) {
                basePropBuilder.putValues(DeploymentAttributes.GENERATE_PASSWORD.name(), "true");
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_PREEXECUTION_ENABLED)) {
                basePropBuilder.putValues(DeploymentAttributes.PREEXECUTION_ENABLED.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_PREEXECUTION_ENABLED));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_PREEXECUTION_THRESHOLD)) {
                basePropBuilder.putValues(DeploymentAttributes.PREEXECUTION_THRESHOLD.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_PREEXECUTION_THRESHOLD));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_DECENTRALIZED_KEYS)) {
                basePropBuilder.putValues(DeploymentAttributes.DECENTRALIZED_KEYS.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_DECENTRALIZED_KEYS));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_SPLIT_CONFIG)) {
                basePropBuilder.putValues(DeploymentAttributes.SPLIT_CONFIG.name(),
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_SPLIT_CONFIG));
            } else {
                basePropBuilder.putValues(DeploymentAttributes.SPLIT_CONFIG.name(), "True");
            }
        }
        return basePropBuilder;
    }

    private boolean validateNumberofReplicas(BlockchainPost body) {
        if (body.getReplicaNodes() == null && body.getReplicaZoneIds() == null) {
            logger.error("Both parameters replica_nodes and replica_zone_ids are missing.");
            return false;
        }

        // Always look for replica nodes.
        // There is no need to check emptiness condition here, because size is 0 when list is empty.
        if (body.getReplicaNodes() != null) {
            int m = body.getReplicaNodes().size();
            logger.info("Found {} Replica nodes.", m);
            return (replicaNumber.contains(m));
        }
        // Check replica_zone_ids
        int n = body.getReplicaZoneIds().size();
        logger.info("Found {} Replica zone ids ", n);
        return replicaNumber.contains(n);
    }

    private boolean validateNumberOfClients(BlockchainPost body) {
        if (body.getBlockchainType() == BlockchainType.ETHEREUM) {
            return true;
        }
        if (body.getClientNodes() == null || body.getClientNodes().isEmpty()) {
            logger.error("No client nodes available.");
            return false;
        }
        int m = body.getClientNodes().size();
        logger.info("Found {} client nodes.", m);
        return m <= clientNumber;
    }

    private Blockchain safeGetBlockchain(UUID bid) {
        Blockchain blockchain;
        try {
            blockchain = blockchainService.get(bid);
            if (blockchain == null) {
                throw new NotFoundException(ErrorCode.BLOCKCHAIN_NOT_FOUND, bid.toString());
            }
        } catch (NotFoundException e) {
            throw new NotFoundException(ErrorCode.BLOCKCHAIN_NOT_FOUND, bid.toString());
        }

        return blockchain;
    }

    private int getMaxChains(Organization organization) {
        // admins can create any number
        if (authHelper.isSystemAdmin()) {
            return Integer.MAX_VALUE;
        }

        // default to one
        int m = 1;
        if (organization.getOrganizationProperties() != null) {
            String s = organization.getOrganizationProperties().getOrDefault(Constants.ORG_MAX_CHAINS, "1");
            m = Integer.parseInt(s);
        }
        // m == 0 means no limit
        return m == 0 ? Integer.MAX_VALUE : m;
    }

    private List<OrchestrationSite> getOrchestrationSites(Set<UUID> zoneIds) {
        return zoneIds.stream()
                .distinct() // Until prov service fixes it.
                .map(zoneService::get)
                .map(z -> OrchestrationSite.newBuilder()
                        .setInfo(toInfo(z))
                        .setId(OrchestrationSiteIdentifier.newBuilder()
                                       .setId(z.getId().toString())
                                       .build())
                        .build())
                .collect(Collectors.toList());
    }

    /**
     * Validate SizingInfo for all types of nodes.
     * @param sizingInfo Sizing info object
     * @param mode A string representing the node type.
     * @return True in case of valid input, an exception otherwise.
     * @throws BadRequestException when SizingInfo parameters are invalid.
     */
    private boolean validateSizingInfo(HashMap<NodeSizeTemplate.Parameter, String> sizingInfo, String mode)
            throws BadRequestException {
        String modeVal = mode != null ? mode + ": " : "";
        if (sizingInfo == null || sizingInfo.isEmpty()) {
            logger.error(modeVal + "Sizing Info is not available. Quit validation.");
            throw new BadRequestException(modeVal + "Sizing Info is not available.");
        }
        NodeSizeTemplate nst = nodeSizeTemplateService.getTemplate();
        NodeSizeTemplate.Range range = nst.getRange();
        NodeSizeTemplate.RangeVal rangeValCpu = range.get(NodeSizeTemplate.Parameter.NO_OF_CPUS);
        NodeSizeTemplate.RangeVal rangeValStorage = range.get(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS);
        NodeSizeTemplate.RangeVal rangeValMemory = range.get(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS);
        int cpuMax = rangeValCpu.get(NodeSizeTemplate.RangeProperty.MAX);
        int cpuMin = rangeValCpu.get(NodeSizeTemplate.RangeProperty.MIN);
        int storageMax = rangeValStorage.get(NodeSizeTemplate.RangeProperty.MAX);
        int storageMin = rangeValStorage.get(NodeSizeTemplate.RangeProperty.MIN);
        int memoryMax = rangeValMemory.get(NodeSizeTemplate.RangeProperty.MAX);
        int memoryMin = rangeValMemory.get(NodeSizeTemplate.RangeProperty.MIN);

        String cpuCount = sizingInfo.get(NodeSizeTemplate.Parameter.NO_OF_CPUS);
        Integer cpuCountInt = Pattern.matches("\\d+", cpuCount) ? Integer.parseInt(cpuCount) : 0;
        if (cpuCountInt < cpuMin || cpuCountInt > cpuMax) {
            throw new BadRequestException(String.format(modeVal + "Expected value of cpu's is between"
                                                        + " %s and %s", cpuMin, cpuMax));
        }
        String vmMemory = sizingInfo.get(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS);
        Integer vmMemoryInt = Pattern.matches("\\d+", vmMemory) ? Integer.parseInt(vmMemory) : 0;
        if (vmMemoryInt < memoryMin || vmMemoryInt > memoryMax) {
            throw new BadRequestException(String.format(modeVal + "Expected value of memory in Gigs is "
                                                        + "between %s and %s", memoryMin, memoryMax));
        }
        String vmStorage = sizingInfo.get(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS);
        Integer vmStorageInt = Pattern.matches("\\d+", vmStorage) ? Integer.parseInt(vmStorage) : 0;
        if (vmStorageInt < storageMin || vmStorageInt > storageMax) {
            throw new BadRequestException(String.format(modeVal + "Expected value of storage in Gigs is"
                                                        + " between %s and %s", storageMin, storageMax));
        }
        return true;
    }

    /**
     * Add node sizing parameters to Deployment properties, if available.
     * @param sizingInfo Sizing info parameters
     * @param propBuilder Properties
     */
    private void addSizingInfoToProperties(HashMap<NodeSizeTemplate.Parameter, String> sizingInfo,
                                           Properties.Builder propBuilder) {
        if (sizingInfo == null || propBuilder == null) {
            logger.error("Either SizingInfo or Properties is null. Quit.");
            return;
        }
        String cpuCount = sizingInfo.get(NodeSizeTemplate.Parameter.NO_OF_CPUS);
        propBuilder.putValues(DeploymentAttributes.VM_CPU_COUNT.name(), cpuCount);

        String vmMemory = sizingInfo.get(NodeSizeTemplate.Parameter.MEMORY_IN_GIGS);
        propBuilder.putValues(DeploymentAttributes.VM_MEMORY.name(), vmMemory);

        String vmStorage = sizingInfo.get(NodeSizeTemplate.Parameter.STORAGE_IN_GIGS);
        propBuilder.putValues(DeploymentAttributes.VM_STORAGE.name(), vmStorage);
    }
}
