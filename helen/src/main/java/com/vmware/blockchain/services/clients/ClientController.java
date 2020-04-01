/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.clients;

import static com.vmware.blockchain.services.blockchains.BlockchainUtils.toInfo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.CreateClusterRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentSpecification;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.PlacementSpecification;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceGrpc;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainController;
import com.vmware.blockchain.services.blockchains.BlockchainObserver;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.clients.Client.ClientType;
import com.vmware.blockchain.services.configuration.ConcordConfiguration;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.TaskService;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Controller to create and list clients.
 */
@RestController
public class ClientController {
    private  static final Logger logger = LogManager.getLogger(ClientController.class);


    @Getter
    @Setter
    @JsonIgnoreProperties(ignoreUnknown = true)
    static class ParticipantPost {
        private List<UUID> zoneIds;
        private String clientJwt;
    }

    /**
     * Class for ReplicaGetResponse.
     */
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ReplicaGetResponse {
        private String publicIp;
        private String privateIp;
        private String hostName;
        private String url;
        private String cert;
        private UUID zoneId;
        private Replica.ReplicaType replicaType;
        private UUID blockchainId;

        /**
         * Constructor for ReplicaGetResponse Class.
         * @param r Replica.
         */
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

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    static class ClientGetResponse {
        private String nodeId;
        private String clientGroupId;
        private String blockchainId;
        private UUID zoneId;
        private ClientType clientType;
        private Replica.ReplicaType replicaType;

        public ClientGetResponse(Client r) {
            this.nodeId = r.getNodeId();
            this.clientGroupId = r.getClientGroupId();
            this.blockchainId = r.getBlockchainId();
            this.zoneId = r.getZoneId();
            this.clientType = r.getClientType();
            this.replicaType = getReplicaType();
        }
    }

    private BlockchainService blockchainService;
    private OrganizationService organizationService;

    private AuthHelper authHelper;
    private DefaultProfiles defaultProfiles;
    private TaskService taskService;
    private ProvisioningServiceGrpc.ProvisioningServiceStub client;
    private OperationContext operationContext;
    private ReplicaService replicaService;
    private ZoneService zoneService;
    private ConcordConfiguration concordConfiguration;

    @Autowired
    public ClientController(BlockchainService blockchainService,
                            OrganizationService organizationService,
                            AuthHelper authHelper,
                            DefaultProfiles defaultProfiles,
                            TaskService taskService,
                            ProvisioningServiceGrpc.ProvisioningServiceStub client,
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
     * Get the list of all participant nodes.
     */
    @RequestMapping(path = "/api/blockchains/{bid}/clients", method = RequestMethod.GET)
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
     * The actual call which will contact server to add the participant.
     */
    private DeploymentSessionIdentifier createParticipantCluster(ProvisioningServiceGrpc.ProvisioningServiceStub client,
                                                                 List<UUID> zoneIds, UUID consortiumId,
                                                                 Properties properties) throws Exception {
        List<PlacementSpecification.Entry> list;

        list = zoneIds.stream()
                .map(zoneService::get)
                .map(z -> PlacementSpecification.Entry.newBuilder()
                        .setType(PlacementSpecification.Type.FIXED)
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
                .setProperties(properties)
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

    /**
     * Deploy a DAML Participant node for given DAML blockchain.
     */
    @RequestMapping(path = "/api/blockchains/{bid}/clients", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    public ResponseEntity<BlockchainController.BlockchainTaskResponse> createParticipant(@PathVariable("bid") UUID bid,
                                          @RequestBody ParticipantPost body) throws Exception {
        Task task = new Task();
        task.setState(Task.State.RUNNING);
        task = taskService.put(task);

        Blockchain blockchain = blockchainService.get(bid);

        Blockchain.BlockchainType blockchainType = blockchain.getType();

        List<Replica> nodeEntryList = blockchainService.getReplicas(bid);

        String ipList = String.join(",", nodeEntryList.stream()
                .map(nodeEntry -> nodeEntry.getPrivateIp() + ":50051")
                .collect(Collectors.toList()));

        Map<String, String> properties = new HashMap<>();
        properties.put(NodeProperty.Name.COMMITTERS.toString(), ipList);
        if (body.clientJwt != null) {
            properties.put(NodeProperty.Name.CLIENT_AUTH_JWT.toString(), body.clientJwt);
        }
        var propertiesBuilder = Properties.newBuilder().putAllValues(properties).build();


        if (blockchainType != Blockchain.BlockchainType.DAML) {
            logger.info("Participant node can be deployed only for DAML blockchains.");
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        UUID bcConsortium = blockchain.getConsortium();

        List<UUID> zoneIdList = body.getZoneIds();

        DeploymentSessionIdentifier dsId =  createParticipantCluster(client,
                                                                     zoneIdList,
                                                                     bcConsortium,
                                                                     propertiesBuilder);

        logger.info("Deployment for participant node started, id {} for the consortium id {}", dsId,
                    blockchain.getConsortium().toString());

        BlockchainObserver bo =
                new BlockchainObserver(authHelper, operationContext, blockchainService, replicaService, taskService,
                                       task.getId(), bcConsortium, bid, blockchainType,
                                       Replica.ReplicaType.DAML_PARTICIPANT,
                                       Client.ClientType.DAML_PARTICIPANT);
        // Watch for the event stream

        StreamClusterDeploymentSessionEventRequest request = StreamClusterDeploymentSessionEventRequest.newBuilder()
                .setHeader(MessageHeader.newBuilder().build())
                .setSession(dsId)
                .build();

        client.streamClusterDeploymentSessionEvents(request, bo);
        logger.info("Deployment scheduled");

        return new ResponseEntity<>(new BlockchainController.BlockchainTaskResponse(task.getId()), HttpStatus.ACCEPTED);
    }

}
