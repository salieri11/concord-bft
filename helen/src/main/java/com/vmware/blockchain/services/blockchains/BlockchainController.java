/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import static com.vmware.blockchain.services.blockchains.BlockchainController.DeploymentType.FIXED;
import static com.vmware.blockchain.services.blockchains.BlockchainController.DeploymentType.UNSPECIFIED;
import static com.vmware.blockchain.services.blockchains.BlockchainUtils.toInfo;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.CreateClusterRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentSpecification;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.PlacementSpecification;
import com.vmware.blockchain.deployment.v1.PlacementSpecification.Entry;
import com.vmware.blockchain.deployment.v1.PlacementSpecification.Type;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceGrpc.ProvisioningServiceStub;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.blockchains.replicas.Replica.ReplicaType;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.clients.Client.ClientType;
import com.vmware.blockchain.services.configuration.ConcordConfiguration;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


/**
 * Controller to create and list blockchains.
 */
@RestController
public class BlockchainController {
    private static final Logger logger = LogManager.getLogger(BlockchainController.class);

    /**
     * The type of sites we want in the deployment.
     */
    public enum DeploymentType {
        FIXED,
        UNSPECIFIED
    }

    private static final Map<DeploymentType, PlacementSpecification.Type> enumMap =
            ImmutableMap.of(FIXED, Type.FIXED,
                            UNSPECIFIED, Type.UNSPECIFIED);

    private static final Map<BlockchainType, ConcordModelSpecification.BlockchainType> enumMapForBlockchainType =
            ImmutableMap.of(BlockchainType.ETHEREUM, ConcordModelSpecification.BlockchainType.ETHEREUM,
                    BlockchainType.DAML, ConcordModelSpecification.BlockchainType.DAML,
                    BlockchainType.HLF, ConcordModelSpecification.BlockchainType.HLF);

    @Getter
    @Setter
    @Valid
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPost {
        @NotNull(message = "Consortium ID cannot be empty")
        private UUID consortiumId;
        @JsonProperty("f_count")
        private Integer fCount;
        @JsonProperty("c_count")
        private Integer cCount;
        private BlockchainType blockchainType;
        private List<UUID> zoneIds;
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPatch {
        private String ipList;
        private String rpcUrls;
        private String rpcCerts;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    // This represents both the old node entry, and the new replica entry
    static class BlockchainReplicaBase {
        private String ip;
        private String url;
        private String cert;
        private UUID zoneId;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @Deprecated
    static class BlockchainNodeEntry extends BlockchainReplicaBase {
        UUID nodeId;

        public BlockchainNodeEntry(NodeEntry n) {
            super(n.getIp(), n.getUrl(), n.getCert(), n.getZoneId());
            nodeId = n.getNodeId();
        }
    }

    @Getter
    @Setter
    @NoArgsConstructor
    static class BlockchainReplicaEntry extends BlockchainReplicaBase {
        UUID replicaId;

        public BlockchainReplicaEntry(NodeEntry n) {
            super(n.getIp(), n.getUrl(), n.getCert(), n.getZoneId());
            replicaId = n.getNodeId();
        }
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class BlockchainGetResponse {
        private UUID id;
        private UUID consortiumId;
        private BlockchainType blockchainType;
        private Blockchain.BlockchainState blockchainState;
        @Deprecated
        private List<BlockchainNodeEntry> nodeList;
        private List<BlockchainReplicaEntry> replicaList;
        private Map<String, String> metadata;

        public BlockchainGetResponse(Blockchain b) {
            this.id = b.getId();
            this.consortiumId = b.getConsortium();
            this.blockchainType = b.getType() == null ? BlockchainType.ETHEREUM : b.getType();
            this.blockchainState = b.getState() == null ? Blockchain.BlockchainState.ACTIVE : b.getState();
            // For the moment, return both node_list and replica_list
            this.nodeList = b.getNodeList().stream().map(BlockchainNodeEntry::new).collect(Collectors.toList());
            this.replicaList = b.getNodeList().stream().map(BlockchainReplicaEntry::new).collect(Collectors.toList());
            this.metadata = b.getMetadata();
        }
    }

    /**
     * Response from blockchain post, with a task id.
     */
    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    public static class BlockchainTaskResponse {
        private UUID taskId;
    }

    private BlockchainService blockchainService;
    private OrganizationService organizationService;

    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private TaskService taskService;
    private ProvisioningServiceStub client;
    private OperationContext operationContext;
    private ReplicaService replicaService;
    private ZoneService zoneService;
    private ConcordConfiguration concordConfiguration;

    @Autowired
    public BlockchainController(BlockchainService blockchainService,
                                OrganizationService organizationService,
                                AuthHelper authHelper,
                                DefaultProfiles defaultProfiles,
                                TaskService taskService,
                                ProvisioningServiceStub client,
                                OperationContext operationContext,
                                ReplicaService replicaService,
                                ZoneService zoneService,
                                ConcordConfiguration concordConfiguration) {
        this.blockchainService = blockchainService;
        this.organizationService = organizationService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
        this.client = client;
        this.operationContext = operationContext;
        this.replicaService = replicaService;
        this.zoneService = zoneService;
        this.concordConfiguration = concordConfiguration;
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
        List<Blockchain> chains = Collections.emptyList();
        // if we are operator, we can get all blockchains.
        if (authHelper.hasAnyAuthority(Roles.systemAdmin())) {
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

        // Determine whether or not we can create a new blockchain
        if (authHelper.getUpdateChains().size() >= getMaxChains(authHelper.getOrganizationId())) {
            logger.info("Request for too many blockdhains: current {}, limit {}",
                        authHelper.getUpdateChains().size(),
                        getMaxChains(authHelper.getOrganizationId()));
            throw new BadRequestException(ErrorCodeType.BLOCKCHAIN_LIMIT, authHelper.getUpdateChains().size());
        }

        final int clusterSize = body.getFCount() * 3 + body.getCCount() * 2 + 1;
        logger.info("Creating new blockchain. Cluster size {}", clusterSize);

        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);

        // Backwards compatibility: If no blockchain_type is set, use ETHEREUM
        BlockchainType blockchainType =
                body.getBlockchainType() == null ? BlockchainType.ETHEREUM : body.getBlockchainType();

        /*
        zoneIds should not be null
        Number of zoneIds should be equal to 3F + 2C + 1
         */
        if (body.getZoneIds() == null
                || body.getZoneIds().size() != clusterSize) {
            logger.info("Number of zones not equal to cluster size");
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        DeploymentSessionIdentifier dsId;

        Organization org = organizationService.get(authHelper.getOrganizationId());

        // true if we are deploying a DAML committer, else false
        // this is for DAML v2 deployment
        boolean deployDamlCommitter;

        if (blockchainType == BlockchainType.DAML) {
            if (org.getOrganizationProperties() != null
                && org.getOrganizationProperties().containsKey("DAML_V0")
                && org.getOrganizationProperties().get("DAML_V0").equals("enabled")) {
                deployDamlCommitter = false;
            } else {
                deployDamlCommitter = true;
            }
        } else {
            deployDamlCommitter = false;
        }


        dsId = createFixedSizeCluster(client, clusterSize,
                enumMap.get(FIXED),
                body.getZoneIds(),
                blockchainType,
                body.consortiumId,
                deployDamlCommitter);

        ClientType clientType = ClientType.NONE;
        ReplicaType replicaType = ReplicaType.NONE;

        logger.info("Deployment started, id {} for the consortium id {}", dsId, body.consortiumId.toString());
        BlockchainObserver bo =
                new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService, taskService,
                                       task.getId(), body.getConsortiumId(), null, blockchainType, replicaType,
                                       null);
        // Watch for the event stream
        StreamClusterDeploymentSessionEventRequest request = StreamClusterDeploymentSessionEventRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSession(dsId)
                .build();
        client.streamClusterDeploymentSessionEvents(request, bo);
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

        // Temporary: create a completed task that points to the default blockchain
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
        if (blockchain.getState() != Blockchain.BlockchainState.INACTIVE) {
            blockchain.setState(Blockchain.BlockchainState.INACTIVE);
            blockchain = blockchainService.put(blockchain);
            return new ResponseEntity<>(new BlockchainGetResponse(blockchain), HttpStatus.ACCEPTED);
        } else {
            throw new BadRequestException(String.format("Blockchain %s is already de-registered.", bid.toString()));
        }
    }

    /**
     * The actual call which will contact server and add the model request.
     */
    private DeploymentSessionIdentifier createFixedSizeCluster(ProvisioningServiceStub client,
                                                               int clusterSize,
                                                               PlacementSpecification.Type placementType,
                                                               List<UUID> zoneIds,
                                                               BlockchainType blockchainType,
                                                               UUID consortiumId,
                                                               boolean deployDamlCommitter) throws Exception {
        List<Entry> list;
        if (zoneIds.size() != clusterSize) {
            logger.info("Number of zones not equal to cluster size");
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }
        list = zoneIds.stream()
                .map(zoneService::get)
                .map(z -> Entry.newBuilder()
                        .setType(placementType)
                        .setSite(OrchestrationSiteIdentifier.newBuilder()
                                .setId(z.getId().toString())
                                .build())
                        .setSiteInfo(toInfo(z))
                        .build())
                .collect(Collectors.toList());

        var blockChainType = blockchainType == null ? ConcordModelSpecification.BlockchainType.ETHEREUM
                : enumMapForBlockchainType.get(blockchainType);

        ConcordModelSpecification spec;

        logger.info("Concord version in Blockchain Controller {}", concordConfiguration.getVersion());

        if (deployDamlCommitter) {
            var components = concordConfiguration.getComponentsByNodeType(ConcordModelSpecification
                    .NodeType.DAML_COMMITTER);

            spec = ConcordModelSpecification.newBuilder()
                    .setVersion(concordConfiguration.getVersion())
                    .setTemplate(concordConfiguration.getTemplate())
                    .addAllComponents(components)
                    .setBlockchainType(blockChainType)
                    .setNodeType(ConcordModelSpecification.NodeType.DAML_COMMITTER)
                    .build();
        } else {
            var components = concordConfiguration.getComponentsByBlockchainType(blockChainType);

            spec = ConcordModelSpecification.newBuilder()
                    .setVersion(concordConfiguration.getVersion())
                    .setTemplate(concordConfiguration.getTemplate())
                    .addAllComponents(components)
                    .setBlockchainType(blockChainType)
                    .setNodeType(ConcordModelSpecification.NodeType.NONE)
                    .build();
        }

        var genesis = ConcordConfiguration.getGenesisObject();
        var placementSpec = PlacementSpecification.newBuilder()
                .addAllEntries(list)
                .build();

        Map<String, String> properties = new HashMap<>();

        DeploymentSpecification deploySpec = DeploymentSpecification.newBuilder()
                .setModel(spec)
                .setPlacement(placementSpec)
                .setGenesis(genesis)
                .setConsortium(consortiumId.toString())
                .setProperties(
                        Properties.newBuilder()
                                .putAllValues(properties)
                                .build()
                )
                .build();

        var request = CreateClusterRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder()
                        .setId(operationContext.getId() != null ? operationContext.getId() : "").build())
                .setSpecification(deploySpec)
                .build();

        // Check that the API can be serviced normally after service initialization.
        var promise = new CompletableFuture<DeploymentSessionIdentifier>();
        client.createCluster(request, FleetUtils.blockedResultObserver(promise));
        return promise.get();
    }

    private int getMaxChains(UUID orgId) {
        // admins can create any number
        if (authHelper.isSystemAdmin()) {
            return Integer.MAX_VALUE;
        }

        Organization organization = organizationService.get(orgId);
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
