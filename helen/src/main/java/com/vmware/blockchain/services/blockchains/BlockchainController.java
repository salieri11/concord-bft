/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import static com.vmware.blockchain.services.blockchains.BlockchainController.DeploymentType.FIXED;
import static com.vmware.blockchain.services.blockchains.BlockchainController.DeploymentType.UNSPECIFIED;
import static com.vmware.blockchain.services.blockchains.BlockchainUtils.toInfo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

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
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
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
import com.vmware.blockchain.deployment.v1.ProvisioningServiceStub;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.ethereum.type.Genesis;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
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
    private ConsortiumService consortiumService;
    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private TaskService taskService;
    private ProvisioningServiceStub client;
    private OperationContext operationContext;
    private boolean mockDeployment;
    private ReplicaService replicaService;
    private ZoneService zoneService;


    @Autowired
    public BlockchainController(BlockchainService blockchainService,
                                ConsortiumService consortiumService,
                                AuthHelper authHelper,
                                DefaultProfiles defaultProfiles,
                                TaskService taskService,
                                ProvisioningServiceStub client,
                                OperationContext operationContext,
                                ReplicaService replicaService,
                                ZoneService zoneService,
                                @Value("${mock.deployment:false}") boolean mockDeployment) {
        this.blockchainService = blockchainService;
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
        this.client = client;
        this.operationContext = operationContext;
        this.replicaService = replicaService;
        this.zoneService = zoneService;
        this.mockDeployment = mockDeployment;
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
     * Get the blckchain details.
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
            int clusterSize, PlacementSpecification.Type placementType, List<UUID> zoneIds,
                                                               BlockchainType blockchainType,
                                                               UUID consortiumId) throws Exception {
        List<Entry> list;
        if (placementType == Type.FIXED) {
            if (zoneIds.size() != clusterSize) {
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }
            list = zoneIds.stream()
                    .map(zoneService::get)
                    .map(z -> new Entry(placementType,
                                        FleetUtils.identifier(OrchestrationSiteIdentifier.class, z.getId()),
                                        toInfo(z)))
                    .collect(Collectors.toList());
        } else {
            list = IntStream.range(0, clusterSize)
                    .mapToObj(i -> new Entry(placementType, new OrchestrationSiteIdentifier(1, i),
                                             new OrchestrationSiteInfo()))
                    .collect(Collectors.toList());
        }
        var placementSpec = new PlacementSpecification(list);
        var blockChainType = blockchainType == null ? ConcordModelSpecification.BlockchainType.ETHEREUM
                                                    : enumMapForBlockchainType.get(blockchainType);
        var components = getComponentsByBlockchainType(blockChainType);
        var genesis = new Genesis(
                new Genesis.Config(1, 0, 0, 0),
                "0x0000000000000000",
                "0x400",
                "0x0000000000000000000000000000000000000000000000000000000000000000",
                "0x0000000000000000000000000000000000000000000000000000000000000000",
                "0xf4240",
                Map.of(
                        "262c0d7ab5ffd4ede2199f6ea793f819e1abb019", new Genesis.Wallet("12345"),
                        "5bb088f57365907b1840e45984cae028a82af934", new Genesis.Wallet("0xabcdef"),
                        "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39", new Genesis.Wallet("0x7fffffffffffffff")
                )
        );
        ConcordModelSpecification spec = new ConcordModelSpecification(
                "20190401.1",
                "8abc7fda-9576-4b13-9beb-06f867cf2c7c",
                components,
                blockChainType
        );
        DeploymentSpecification deploySpec =
                new DeploymentSpecification(clusterSize, spec, placementSpec, genesis, consortiumId.toString());
        var request = new CreateClusterRequest(new MessageHeader(operationContext.getId()), deploySpec);
        // Check that the API can be serviced normally after service initialization.
        var promise = new CompletableFuture<DeploymentSessionIdentifier>();
        client.createCluster(request, FleetUtils.blockedResultObserver(promise));
        return promise.get();
    }

    // TODO Move this to config/metadata lookup.
    List<ConcordComponent> getComponentsByBlockchainType(ConcordModelSpecification.BlockchainType type) {
        List<ConcordComponent> response = new ArrayList<>();
        response.add(new ConcordComponent(
                ConcordComponent.Type.CONTAINER_IMAGE,
                ConcordComponent.ServiceType.GENERIC,
                "vmwblockchain/agent:latest"
        ));
        switch (type) {
            case DAML:
                response.add(
                        new ConcordComponent(
                                ConcordComponent.Type.CONTAINER_IMAGE,
                                ConcordComponent.ServiceType.DAML_CONCORD,
                                "vmwblockchain/concord-core:latest"
                        ));
                response.add(new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE,
                        "vmwblockchain/daml-execution-engine:latest"
                ));
                response.add(new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.DAML_INDEX_DB,
                        "vmwblockchain/daml-index-db:latest"
                ));
                response.add(new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.DAML_LEDGER_API,
                        "vmwblockchain/daml-ledger-api:latest"
                ));
                break;
            default:
                // Default is ETHEREUM due to backward compatibility.
            case ETHEREUM:
                response.add(new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.CONCORD,
                        "vmwblockchain/concord-core:latest"
                ));
                response.add(new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.ETHEREUM_API,
                        "vmwblockchain/ethrpc:latest"
                ));
                break;
        }
        return response;
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
        // start the deployment
        final int clusterSize = body.getFCount() * 3 + body.getCCount() * 2 + 1;
        logger.info("Creating new blockchain. Cluster size {}", clusterSize);

        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);

        // Backwards compatibility: If no blockchain_type is set, use ETHEREUM
        BlockchainType blockchainType =
                body.getBlockchainType() == null ? BlockchainType.ETHEREUM : body.getBlockchainType();

        if (mockDeployment) {
            Blockchain bc = blockchainService.get(defaultProfiles.getBlockchain().getId());
            bc.setConsortium(body.getConsortiumId());
            blockchainService.put(bc);
            task.setResourceId(bc.getId());
            task.setResourceLink(String.format("/api/blockchains/%s", bc.getId()));
            task.setMessage("Operation finished");
            task.setState(Task.State.SUCCEEDED);
            taskService.put(task);
            logger.info("Deployment mocked");
        } else {
            /*
            If the deployment type is FIXED
            zoneIds should not be null
            Number of zoneIds should be equal to 3F + 2C + 1
             */
            if (body.deploymentType == FIXED) {
                if (body.getZoneIds() == null
                        || body.getZoneIds().size() != body.getFCount() * 3 + body.getCCount() * 2 + 1) {
                    throw new BadRequestException(ErrorCode.BAD_REQUEST);
                }
            }
            DeploymentSessionIdentifier dsId = createFixedSizeCluster(client, clusterSize,
                                                                      enumMap.get(body.deploymentType),
                                                                      body.getZoneIds(),
                                                                      blockchainType,
                                                                      body.consortiumId);
            logger.info("Deployment started, id {} for the consortium id {}", dsId, body.consortiumId.toString());
            BlockchainObserver bo =
                    new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService, taskService,
                                           task.getId(), body.getConsortiumId(), blockchainType);
            // Watch for the event stream
            StreamClusterDeploymentSessionEventRequest request =
                    new StreamClusterDeploymentSessionEventRequest(new MessageHeader(), dsId);
            client.streamClusterDeploymentSessionEvents(request, bo);
            logger.info("Deployment scheduled");
        }

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

    /**
     * Update the given blockchain.
     */
    @RequestMapping(path = "/api/blockchains/{id}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.canUpdateChain(#id)")
    public ResponseEntity<BlockchainTaskResponse> updateBlockchain(@PathVariable UUID id,
            @RequestBody BlockchainPatch body) throws NotFoundException {

        // Temporary: create a completed task that points to the default bockchain
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
}
