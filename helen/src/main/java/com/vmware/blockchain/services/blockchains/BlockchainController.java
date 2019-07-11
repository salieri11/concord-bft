/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.deployment.model.ConcordComponent;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.PlacementSpecification;
import com.vmware.blockchain.deployment.model.PlacementSpecification.Entry;
import com.vmware.blockchain.deployment.model.ProvisioningServiceStub;
import com.vmware.blockchain.deployment.model.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.model.ethereum.Genesis;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.CallOptions;
import io.grpc.ManagedChannel;
import io.grpc.stub.StreamObserver;
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
        private List<String> sites;
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
    static class BlockchainNodeEntry {
        private UUID nodeId;
        private String ip;
        private String url;
        private String cert;
        private String region;

        public BlockchainNodeEntry(NodeEntry n) {
            nodeId = n.getNodeId();
            ip = n.getIp();
            url = n.getUrl();
            cert = n.getCert();
            region = n.getRegion();
        }

    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class BlockchainGetResponse {
        private UUID id;
        private UUID consortiumId;
        private List<BlockchainNodeEntry> nodeList;

        public BlockchainGetResponse(Blockchain b) {
            this.id = b.getId();
            this.consortiumId = b.getConsortium();
            this.nodeList = b.getNodeList().stream().map(BlockchainNodeEntry::new).collect(Collectors.toList());
        }
    }


    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    static class BlockchainTaskResponse {
        private UUID taskId;
    }

    private BlockchainService blockchainService;
    private ConsortiumService consortiumService;
    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private TaskService taskService;
    private ManagedChannel channel;
    private boolean mockDeployment;

    /**
     * An observer to fake a blocked call.
     */
    private <T> StreamObserver<T> blockedResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value;

            @Override
            public void onNext(T value) {
                this.value = value;
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(value);
            }
        };
    }

    @Autowired
    public BlockchainController(BlockchainService blockchainService,
                                ConsortiumService consortiumService,
                                AuthHelper authHelper,
                                DefaultProfiles defaultProfiles,
                                TaskService taskService,
                                ManagedChannel channel,
                                @Value("${mock.deployment:false}") boolean mockDeployment) {
        this.blockchainService = blockchainService;
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
        this.defaultProfiles = defaultProfiles;
        this.taskService = taskService;
        this.channel = channel;
        this.mockDeployment = mockDeployment;
    }

    /**
     * Get the list of all blockchains.
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.GET)
    @PreAuthorize("hasAnyAuthority(T(com.vmware.blockchain.services.profiles.Roles).user())")
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
            int clusterSize) throws Exception {
        // Create a blocking stub with the channel
        List<Entry> list = new ArrayList<Entry>(clusterSize);
        for (int i = 0; i < clusterSize; i++) {
            list.add(
                    new Entry(PlacementSpecification.Type.UNSPECIFIED, new OrchestrationSiteIdentifier(1, 2)));
        }
        var placementSpec = new PlacementSpecification(list);
        var components = List.of(
                new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.CONCORD,
                        "vmwblockchain/concord-core:latest"
                ),
                new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.ETHEREUM_API,
                        "vmwblockchain/ethrpc:latest"
                ),
                new ConcordComponent(
                        ConcordComponent.Type.CONTAINER_IMAGE,
                        ConcordComponent.ServiceType.GENERIC,
                        "vmwblockchain/agent-testing:latest"
                )
        );
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
                components
        );
        DeploymentSpecification deploySpec =
                new DeploymentSpecification(clusterSize, spec, placementSpec, genesis);
        var request = new CreateClusterRequest(new MessageHeader(), deploySpec);
        // Check that the API can be serviced normally after service initialization.
        var promise = new CompletableFuture<DeploymentSessionIdentifier>();
        client.createCluster(request, blockedResultObserver(promise));
        return promise.get();
    }

    /**
     * Create a new blockchain in the given consortium, with the specified nodes.
     * Note that after deployment we must remove the authtoken from the cache, since the user will
     * now have access to this blockchain
     * @throws Exception any exception
     */
    @RequestMapping(path = "/api/blockchains", method = RequestMethod.POST)
    @PreAuthorize("hasAnyAuthority(T(com.vmware.blockchain.services.profiles.Roles).consortiumAdmin())")
    public ResponseEntity<BlockchainTaskResponse> createBlockchain(@RequestBody BlockchainPost body) throws Exception {
        // start the deployment
        final int clusterSize = body.getFCount() * 3 + body.getCCount() * 2 + 1;
        logger.info("Creating new blockchain. Cluster size {}", clusterSize);

        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);
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
            final ProvisioningServiceStub client = new ProvisioningServiceStub(channel, CallOptions.DEFAULT);
            DeploymentSessionIdentifier dsId = createFixedSizeCluster(client, clusterSize);
            logger.info("Deployment started, id {}", dsId);
            BlockchainObserver bo =
                    new BlockchainObserver(authHelper, blockchainService, taskService, task.getId(),
                                           body.getConsortiumId());
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
        task.setResourceLink("/api/blockchains/".concat(defaultProfiles.getBlockchain().getId().toString()));
        task = taskService.put(task);

        return new ResponseEntity<>(new BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }
}
