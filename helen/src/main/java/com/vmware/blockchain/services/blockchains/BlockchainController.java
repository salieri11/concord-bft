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
import java.util.stream.IntStream;

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
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.PlacementSpecification;
import com.vmware.blockchain.deployment.v1.PlacementSpecification.Entry;
import com.vmware.blockchain.deployment.v1.PlacementSpecification.Type;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceGrpc.ProvisioningServiceStub;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
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
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class BlockchainPost {
        private UUID consortiumId;
        @JsonProperty("f_count")
        private int fCount;
        @JsonProperty("c_count")
        private int cCount;
        private DeploymentType deploymentType;
        private BlockchainType blockchainType;
        private List<UUID> zoneIds;
    }

    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class ParticipantPost {
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
        @Deprecated
        private List<BlockchainNodeEntry> nodeList;
        private List<BlockchainReplicaEntry> replicaList;

        public BlockchainGetResponse(Blockchain b) {
            this.id = b.getId();
            this.consortiumId = b.getConsortium();
            this.blockchainType = b.getType() == null ? BlockchainType.ETHEREUM : b.getType();
            // For the moment, return both node_list and replica_list
            this.nodeList = b.getNodeList().stream().map(BlockchainNodeEntry::new).collect(Collectors.toList());
            this.replicaList = b.getNodeList().stream().map(BlockchainReplicaEntry::new).collect(Collectors.toList());
        }
    }


    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class ReplicaGetResponse {
        private String publicIp;
        private String privateIp;
        private String hostName;
        private String url;
        private String cert;
        private UUID zoneId;
        private Replica.ReplicaType replicaType;
        private UUID blockchainId;

        public ReplicaGetResponse(Replica r) {
            this.publicIp = r.getPublicIp();
            this.privateIp = r.getPrivateIp();
            this.hostName = r.getHostName();
            this.url = r.getUrl();
            this.cert = r.getCert();
            this.zoneId = r.getZoneId();
            this.replicaType = r.getReplicaType();
            this.blockchainId = r.getBlockchainId();
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
    ResponseEntity<List<BlockchainGetResponse>> list() {
        List<Blockchain> chains = Collections.emptyList();
        // if we are operator, we can get all blockchains.
        if (authHelper.hasAnyAuthority(Roles.systemAdmin())) {
            chains = blockchainService.list();
        } else {
            // Otherwise, we can only see our consortium.
            chains = blockchainService.listByIds(authHelper.getAccessChains());
        }
        List<BlockchainGetResponse> idList = chains.stream().map(BlockchainGetResponse::new)
                .collect(Collectors.toList());
        return new ResponseEntity<>(idList, HttpStatus.OK);
    }

    /**
     * Get the list of all participant nodes.
     */
    @RequestMapping(path = "/api/blockchains/{bid}/client", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUser()")
    ResponseEntity<List<ReplicaGetResponse>> listParticipants(@PathVariable("bid") UUID bid) {
        List<ReplicaGetResponse> replicaGetResponseList = blockchainService.getReplicas(bid)
                .stream()
                .filter(replica -> replica.getReplicaType() == Replica.ReplicaType.DAML_PARTICIPANT)
                .map(ReplicaGetResponse::new)
                .collect(Collectors.toList());

        return new ResponseEntity<>(replicaGetResponseList, HttpStatus.OK);
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
     * The actual call which will contact server and add the model request.
     */
    private DeploymentSessionIdentifier createFixedSizeCluster(ProvisioningServiceStub client,
                                                               int clusterSize,
                                                               PlacementSpecification.Type placementType,
                                                               List<UUID> zoneIds,
                                                               BlockchainType blockchainType,
                                                               UUID consortiumId,
                                                               boolean deployCommitter) throws Exception {
        List<Entry> list;
        if (placementType == Type.FIXED) {
            if (zoneIds.size() != clusterSize) {
                logger.info("Number of zones not equal to cluster size");
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }
            list = zoneIds.stream()
                    .map(zoneService::get)
                    .map(z -> Entry.newBuilder()
                            .setType(placementType)
                            .setSite(OrchestrationSiteIdentifier.newBuilder()
                                             .setLow(z.getId().getLeastSignificantBits())
                                             .setHigh(z.getId().getMostSignificantBits())
                                             .build())
                            .setSiteInfo(toInfo(z))
                            .build())
                    .collect(Collectors.toList());
        } else {
            list = IntStream.range(0, clusterSize)
                    .mapToObj(i -> Entry.newBuilder()
                            .setType(placementType)
                            .setSite(OrchestrationSiteIdentifier.newBuilder()
                                             .setLow(1)
                                             .setHigh(i)
                                             .build())
                            .setSiteInfo(OrchestrationSiteInfo.newBuilder().build())
                            .build())
                    .collect(Collectors.toList());
        }
        var placementSpec = PlacementSpecification.newBuilder()
                .addAllEntries(list)
                .build();

        var blockChainType = blockchainType == null ? ConcordModelSpecification.BlockchainType.ETHEREUM
                : enumMapForBlockchainType.get(blockchainType);

        var genesis = ConcordConfiguration.getGenesisObject();

        ConcordModelSpecification spec;

        if (deployCommitter) {
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

        DeploymentSpecification deploySpec = DeploymentSpecification.newBuilder()
                .setModel(spec)
                .setPlacement(placementSpec)
                .setGenesis(genesis)
                .setConsortium(consortiumId.toString())
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


    /**
     * The actual call which will contact server to add the participant.
     */
    private DeploymentSessionIdentifier createParticipantCluster(ProvisioningServiceStub client,
                                                                 List<UUID> zoneIds, UUID consortiumId,
                                                                 Map<String, String> properties) throws Exception {
        List<Entry> list;

        list = zoneIds.stream()
                .map(zoneService::get)
                .map(z -> Entry.newBuilder()
                        .setType(Type.FIXED)
                        .setSite(OrchestrationSiteIdentifier.newBuilder()
                                .setLow(z.getId().getLeastSignificantBits())
                                .setHigh(z.getId().getMostSignificantBits())
                                .build())
                        .setSiteInfo(toInfo(z))
                        .build())
                .collect(Collectors.toList());

        var placementSpec = PlacementSpecification.newBuilder()
                .addAllEntries(list)
                .build();

        var components = concordConfiguration
                .getComponentsByNodeType(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);

        ConcordModelSpecification spec = ConcordModelSpecification.newBuilder()
                .setVersion(concordConfiguration.getVersion())
                .setTemplate(concordConfiguration.getTemplate())
                .addAllComponents(components)
                .setBlockchainType(ConcordModelSpecification.BlockchainType.DAML)
                .setNodeType(ConcordModelSpecification.NodeType.DAML_PARTICIPANT)
                .build();

        DeploymentSpecification deploySpec = DeploymentSpecification.newBuilder()
                .setModel(spec)
                .setPlacement(placementSpec)
                .setConsortium(consortiumId.toString())
                .setProperties(
                        Properties.newBuilder()
                                .putAllValues(properties)
                                .build()
                )
                .build();

        var request = CreateClusterRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().setId("").build())
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

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     * Note that after deployment we must remove the authtoken from the cache, since the user will
     * now have access to this blockchain
     * @throws Exception any exception
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    public ResponseEntity<BlockchainTaskResponse> createBlockchain(@RequestBody BlockchainPost body) throws Exception {

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
        If the deployment type is FIXED
        zoneIds should not be null
        Number of zoneIds should be equal to 3F + 2C + 1
         */
        if (body.deploymentType == FIXED) {
            if (body.getZoneIds() == null
                    || body.getZoneIds().size() != clusterSize) {
                logger.info("Number of zones not equal to cluster size");
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }
        }

        DeploymentSessionIdentifier dsId;

        Organization org = organizationService.get(authHelper.getOrganizationId());

        // true if we are deploying a DAML committer, else false
        // this is for DAML v2 deployment
        boolean deployDamlCommitter;

        if (org.getOrganizationProperties() != null
                && org.getOrganizationProperties().containsKey("DAML_V2")
                && org.getOrganizationProperties().get("DAML_V2").equals("enabled")
                && blockchainType == BlockchainType.DAML) {
            deployDamlCommitter = true;
        } else {
            deployDamlCommitter = false;
        }

        dsId = createFixedSizeCluster(client, clusterSize,
                enumMap.get(body.deploymentType),
                body.getZoneIds(),
                blockchainType,
                body.consortiumId,
                deployDamlCommitter);

        Replica.ReplicaType replicaType = Replica.ReplicaType.NONE;

        if (blockchainType == BlockchainType.DAML && !deployDamlCommitter) {
            replicaType = Replica.ReplicaType.DAML_PARTICIPANT;
        }

        logger.info("Deployment started, id {} for the consortium id {}", dsId, body.consortiumId.toString());
        BlockchainObserver bo =
                new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService, taskService,
                                       task.getId(), body.getConsortiumId(), blockchainType, replicaType);
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
     * Deploy a DAML Participant node for given DAML blockchain.
     */
    @RequestMapping(path = "/api/blockchains/{bid}/client", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    public ResponseEntity<BlockchainTaskResponse> createParticipant(@PathVariable("bid") UUID bid,
                                                                   @RequestBody ParticipantPost body) throws Exception {
        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);

        Blockchain blockchain = blockchainService.get(bid);

        BlockchainType blockchainType = blockchain.getType();

        List<Blockchain.NodeEntry> nodeEntryList = blockchain.getNodeList();

        String ipList = String.join(",", nodeEntryList.stream()
                .map(nodeEntry -> nodeEntry.ip + ":50051")
                .collect(Collectors.toList()));

        Map<String, String> properties = new HashMap<>();
        properties.put("committers", ipList);

        if (blockchainType != BlockchainType.DAML) {
            logger.info("Participant node can be deployed only for DAML blockchains.");
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        UUID bcConsortium = blockchain.getConsortium();

        List<UUID> zoneIdList = body.getZoneIds();

        DeploymentSessionIdentifier dsId =  createParticipantCluster(client,
                zoneIdList,
                bcConsortium,
                properties);

        logger.info("Deployment for participant node started, id {} for the consortium id {}", dsId,
                blockchain.getConsortium().toString());

        BlockchainObserver bo =
                new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService, taskService,
                        task.getId(), bcConsortium, blockchainType, Replica.ReplicaType.DAML_PARTICIPANT);
        // Watch for the event stream

        StreamClusterDeploymentSessionEventRequest request = StreamClusterDeploymentSessionEventRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSession(dsId)
                .build();

        client.streamClusterDeploymentSessionEvents(request, bo);
        logger.info("Deployment scheduled");

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }
}
