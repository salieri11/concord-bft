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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.assertj.core.util.Strings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.ForbiddenException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeploymentSpec;
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
                                ConnectionPoolManager connectionPoolManager) {
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
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.canAccessChain(#id)")
    ResponseEntity<BlockchainGetResponse> get(@PathVariable UUID id) throws NotFoundException {
        Blockchain b = blockchainService.get(id);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", id.toString()));
        }
        // If we are to populate nodes.
        //List<NodeInterface> nodes = new ArrayList<>();
        //nodes.addAll(replicaService.getReplicas(id));
        //nodes.addAll(clientService.getClientsByBlockchainId(id));
        BlockchainGetResponse br = new BlockchainGetResponse(b);
        return new ResponseEntity<>(br, HttpStatus.OK);
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
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.canUpdateChain(#id)")
    public ResponseEntity<BlockchainTaskResponse> updateBlockchain(@PathVariable UUID id,
            @RequestBody BlockchainPatch body) throws NotFoundException {
        // TODO: Actual PATCH

        // Temporary: create a completed task that points to the default blockchain
        Blockchain b = blockchainService.get(id);
        if (b == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", id.toString()));
        }

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
     * De-register the blockchain.
     */
    @RequestMapping(path = "/api/blockchains/deregister/{bid}", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.canUpdateChain(#bid)")
    public ResponseEntity<BlockchainGetResponse> deRegister(@PathVariable("bid") UUID bid) throws Exception {
        Blockchain blockchain = blockchainService.get(bid);
        if (blockchain == null) {
            throw new NotFoundException(String.format("Blockchain %s does not exist.", bid.toString()));
        }
        if (blockchain.getState() != Blockchain.BlockchainState.INACTIVE) {
            blockchain.setState(Blockchain.BlockchainState.INACTIVE);
            blockchain = blockchainService.put(blockchain);
            return new ResponseEntity<>(new BlockchainGetResponse(blockchain), HttpStatus.ACCEPTED);
        } else {
            throw new BadRequestException(String.format("Blockchain %s is already de-registered.", bid.toString()));
        }
    }

    // --------------------- Private methods -------------------- //


    /**
     * The actual call which will contact server and add the model request.
     */
    private void createDeployment(BlockchainPost body, Organization organization, Task task) throws Exception {
        var zoneIds = body.getReplicaZoneIds();

        NodeAssignment.Builder nodeAssignment = NodeAssignment.newBuilder();
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

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_VM_CPU_OVERRIDE)) {
                basePropBuilder.putValues(DeploymentAttributes.VM_CPU_COUNT.name(),
                        organization.getOrganizationProperties()
                                .get(Constants.ORG_VM_CPU_OVERRIDE));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_VM_MEMORY_OVERRIDE)) {
                basePropBuilder.putValues(DeploymentAttributes.VM_MEMORY.name(),
                        organization.getOrganizationProperties()
                                .get(Constants.ORG_VM_MEMORY_OVERRIDE));
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_GENERATE_PASSWORD)) {
                basePropBuilder.putValues(DeploymentAttributes.GENERATE_PASSWORD.name(), "true");
            }

            if (organization.getOrganizationProperties().containsKey(Constants.ORG_ENABLE_BFT_CLIENT)) {
                basePropBuilder.putValues("enable-bft",
                                          organization.getOrganizationProperties()
                                                  .get(Constants.ORG_ENABLE_BFT_CLIENT));
            }
        }

        body.getReplicaZoneIds()
                .forEach(k -> nodeAssignment.addEntries(NodeAssignment.Entry.newBuilder()
                                                                .setType(NodeType.REPLICA)
                                                                .setNodeId(UUID.randomUUID().toString())
                                                                .setSite(
                                                                        OrchestrationSiteIdentifier.newBuilder()
                                                                                .setId(k.toString()).build())));

        if (body.getClientNodes() != null) {
            // A map to hold groupIndex to groupId (UUID) mapping.
            final Map<String, UUID> groupMap = new HashMap<String, UUID>();
            body.getClientNodes()
                    .forEach(k -> {
                        Properties.Builder propBuilder = Properties.newBuilder();
                        if (!Strings.isNullOrEmpty(k.getAuthUrlJwt())) {
                            propBuilder.putValues(NodeProperty.Name.CLIENT_AUTH_JWT.name(),
                                                  k.getAuthUrlJwt());
                        }
                        if (!Strings.isNullOrEmpty(k.getGroupIndex())) {
                            // Did we see this groupIndex earlier?
                            // If yes, then get the Id from the map.
                            // Or else, generate a new one and put it in the map.
                            UUID groupId = null;
                            if (groupMap.containsKey(k.getGroupIndex())) {
                                groupId = groupMap.get(k.getGroupIndex());
                            } else {
                                groupId = UUID.randomUUID();
                                groupMap.put(k.getGroupIndex(), groupId);
                            }
                            propBuilder.putValues(NodeProperty.Name.CLIENT_GROUP_ID.name(), groupId.toString());
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
                    });

            zoneIds.addAll(body.getClientNodes().stream().map(k -> k.getZoneId()).collect(Collectors.toList()));
        }

        var blockChainType = enumMapForBlockchainType.get(body.getBlockchainType());

        var sitesInfo = zoneIds.stream()
                .distinct() // Until prov service fixes it.
                .map(zoneService::get)
                .map(z -> OrchestrationSite.newBuilder()
                        .setInfo(toInfo(z))
                        .setId(OrchestrationSiteIdentifier.newBuilder()
                                       .setId(z.getId().toString())
                                       .build())
                        .build())
                .collect(Collectors.toList());

        var deploymentSpec = DeploymentSpec.newBuilder()
                .setConsortiumId(body.getConsortiumId().toString())
                .setBlockchainType(blockChainType)
                .setSites(Sites.newBuilder().addAllInfoList(sitesInfo).build())
                .setNodeAssignment(nodeAssignment)
                //.putAllNodeProperties()
                .setProperties(basePropBuilder)
                .build();

        var request = DeploymentRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder()
                                   .setId(operationContext.getId() != null ? operationContext.getId() : "").build())
                .setSpec(deploymentSpec)
                .build();

        // Check that the API can be serviced normally after service initialization.
        var promise = new CompletableFuture<DeploymentRequestResponse>();
        provisioningClient.createDeployment(request, FleetUtils.blockedResultObserver(promise));
        String dsId = promise.get().getId();
        logger.info("Deployment started, id {} for the consortium id {}", dsId, body.getConsortiumId());

        Blockchain rawBlockchain = new Blockchain();
        rawBlockchain.setType(body.getBlockchainType());
        rawBlockchain.setConsortium(body.getConsortiumId());

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
}
