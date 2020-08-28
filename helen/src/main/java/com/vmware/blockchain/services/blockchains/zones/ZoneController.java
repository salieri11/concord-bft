/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.fasterxml.jackson.annotation.JsonTypeInfo.As.EXISTING_PROPERTY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Action.RELOAD;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Action.TEST;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.ON_PREM;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
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

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse;
import com.vmware.blockchain.services.blockchains.BlockchainUtils;
import com.vmware.blockchain.services.blockchains.clients.ClientService;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.OnPremZone.EndPoint;
import com.vmware.blockchain.services.blockchains.zones.Zone.Action;
import com.vmware.blockchain.services.blockchains.zones.Zone.Type;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Rest controller for node information.
 */
@RestController
@RequestMapping(path = "/api/blockchains/zones")
public class ZoneController {
    private ZoneService zoneService;
    private ClientService clientService;
    private ReplicaService replicaService;
    private AuthHelper authHelper;
    private OrchestrationSiteServiceStub orchestrationClient;

    private static final Logger logger = LogManager.getLogger(ZoneController.class);

    @Autowired
    public ZoneController(ZoneService zoneService, ClientService clientService, ReplicaService replicaService,
                          AuthHelper authHelper, OrchestrationSiteServiceStub orchestrationClient) {
        this.zoneService = zoneService;
        this.clientService = clientService;
        this.replicaService = replicaService;
        this.authHelper = authHelper;
        this.orchestrationClient = orchestrationClient;
    }

    // Response for get list.  We only want a few fields, and no subtyping.
    @Data
    @NoArgsConstructor
    static class ZoneListResponse {
        private UUID id;
        private String name;
        private String latitude;
        private String longitude;
        private Type type;

        public ZoneListResponse(Zone z) {
            this.id = z.getId();
            this.name = z.getName();
            this.latitude = z.getLatitude();
            this.longitude = z.getLongitude();
            this.type = z.type;
        }
    }

    // Response for the get request.
    @Data
    @NoArgsConstructor
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = EXISTING_PROPERTY, property = "type",
            visible = true, defaultImpl = ZoneResponse.class)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = OnPremGetResponse.class, name = "ON_PREM"),
            @JsonSubTypes.Type(value = VmcAwsGetResponse.class, name = "VMC_AWS"),
        })
    static class ZoneResponse {
        private UUID id;
        private String name;
        private String latitude;
        private String longitude;
        private Type type;

        public ZoneResponse(Zone z) {
            this.id = z.getId();
            this.name = z.getName();
            this.latitude = z.getLatitude();
            this.longitude = z.getLongitude();
            this.type = z.type;
        }
    }

    // detailed response for OnPrem zone.
    @Data
    @NoArgsConstructor
    static class OnPremGetResponse extends ZoneResponse {
        UUID orgId;
        EndPoint vcenter;
        String resourcePool;
        String storage;
        String folder;
        Zone.Network network;
        Zone.OutboundProxy outboundProxy;
        EndPoint containerRepo;
        Zone.Wavefront wavefront;
        Zone.Elasticsearch elasticsearch;
        List<OnPremZone.LogManagementOnPrem> logManagements;

        public OnPremGetResponse(OnPremZone z) {
            super(z);
            this.orgId = z.getOrgId();
            this.vcenter = z.getVCenter();
            this.resourcePool = z.getResourcePool();
            this.storage = z.getStorage();
            this.folder = z.getFolder();
            this.network = z.getNetwork();
            this.outboundProxy = z.getOutboundProxy();
            this.containerRepo = z.getContainerRepo();
            this.wavefront = z.getWavefront();
            this.elasticsearch = z.getElasticsearch();
            this.logManagements = (z.getLogManagements() == null) ? Collections.emptyList() : z.getLogManagements();
        }
    }

    // detailed response for VMC on AWS zone.
    @Data
    @NoArgsConstructor
    static class VmcAwsGetResponse extends ZoneResponse {

        public VmcAwsGetResponse(VmcAwsZone z) {
            super(z);
        }
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    static class DependentNodesGetResponse {
        List<UUID> replicaList;
        List<UUID> clientList;
    }

    // Create bodies for post,
    @Data
    @NoArgsConstructor
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = EXISTING_PROPERTY, property = "type",
            visible = true, defaultImpl = ZoneRequest.class)
    @JsonSubTypes({
            @JsonSubTypes.Type(value = OnPremRequest.class, name = "ON_PREM"),
            @JsonSubTypes.Type(value = VmcAwsRequest.class, name = "VMC_AWS"),
        })
    static class ZoneRequest {
        @NotBlank(message = "Name cannot be blank")
        private String name;
        private String latitude;
        private String longitude;
        private Type type;
    }

    @Data
    @NoArgsConstructor
    static class OnPremRequest extends ZoneRequest {
        UUID orgId;
        @NotNull(message = "vCenter cannot be empty")
        @Valid
        EndPoint vcenter;
        @NotBlank(message = "Resource Pool cannot be blank")
        String resourcePool;
        @NotBlank(message = "Storage cannot be blank")
        String storage;
        @NotBlank(message = "Folder cannot be blank")
        String folder;
        @NotNull(message = "Network cannot be empty")
        @Valid
        Zone.Network network;
        Zone.OutboundProxy outboundProxy;
        EndPoint containerRepo;
        Zone.Wavefront wavefront;
        Zone.Elasticsearch elasticsearch;
        List<OnPremZone.LogManagementOnPrem> logManagements;
    }

    static class VmcAwsRequest extends ZoneRequest {
        // TODO
        // List<VmcAwsZone.LogManagementVmcAws> logManagements;
    }

    static class ZonePatchRequest {
        private String name;
        private String latitude;
        private String longitude;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    static class DeleteResponse {
        UUID id;
    }


    /**
     * Get the list of zones.
     */
    @RequestMapping(method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isUser()")
    ResponseEntity<List<ZoneListResponse>> getZoneList() {
        List<ZoneListResponse> zones =
                zoneService.getAllAuthorized().stream().map(ZoneListResponse::new).collect(Collectors.toList());
        return new ResponseEntity<>(zones, HttpStatus.OK);
    }

    /**
     * Get a detailed response.
     */
    @RequestMapping(path = "/{zone_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<ZoneResponse> getZone(@PathVariable("zone_id") UUID zoneId) throws Exception {
        Zone zone = safeGetZone(zoneId);

        ZoneResponse response = getZoneResponse(zone);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    /**
     * Get a dependencies for a given on-prem zone.
     */
    @RequestMapping(path = "/dependencies/{zone_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<DependentNodesGetResponse> getZoneDependencies(@PathVariable("zone_id") UUID zoneId)
            throws Exception {
        safeGetZone(zoneId);

        List<UUID> replicaIdList = replicaService.getReplicasByParentId(zoneId)
                .stream().map(replica -> replica.getId()).collect(Collectors.toList());
        List<UUID> clientIdList = clientService.getClientsByParentId(zoneId)
                .stream().map(client -> client.getId()).collect(Collectors.toList());


        DependentNodesGetResponse response = new DependentNodesGetResponse(replicaIdList, clientIdList);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }


    /**
     * Create a zone, if no action specified. If an action is specified, do that action. action=test: Call persephone to
     * test the info, but don't create the zone. action=reload: Reload the zone info from persephone
     */
    @RequestMapping(method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<ZoneResponse> postZone(@RequestParam(required = false) Action action,
                                                @Valid @RequestBody(required = false) ZoneRequest request)
            throws Exception {
        if (RELOAD.equals(action)) {

            return new ResponseEntity<>(new ZoneResponse(), HttpStatus.OK);
        }
        // everything from here on needs a request body.
        if (request == null) {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        Zone zone = requestToZone(request);
        // If this is an onPrem site, do the following check on org_id:
        // If the org_id is missing, or the user is not a system admin, set the org_id to the current org.
        if (zone instanceof OnPremZone) {
            OnPremZone op = (OnPremZone) zone;
            if (op.getOrgId() == null || !authHelper.isSystemAdmin()) {
                op.setOrgId(authHelper.getOrganizationId());
            }
        }
        if (TEST.equals(action)) {
            ValidateOrchestrationSiteRequest req = ValidateOrchestrationSiteRequest.newBuilder()
                    .setHeader(MessageHeader.newBuilder().build())
                    .setSite(BlockchainUtils.toInfo(zone))
                    .build();

            CompletableFuture<ValidateOrchestrationSiteResponse> future = new CompletableFuture<>();
            orchestrationClient.validateOrchestrationSite(req, FleetUtils.blockedResultObserver(future));
            // We don't really need the value.  If this call succeeds, the connection is OK
            future.get();
            return new ResponseEntity<>(getZoneResponse(zone), HttpStatus.OK);
        }
        // save the zone, get the updated info
        ZoneResponse response = getZoneResponse(zoneService.put(zone));
        return new ResponseEntity<>(response, HttpStatus.OK);
    }


    /**
     * Get a zone from the user.
     * Change properties to ones mentioned in the request body.
     */
    @RequestMapping(path = "/{zone_id}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<ZoneResponse> patchZone(@PathVariable("zone_id") UUID zoneId,
                                           @RequestBody(required = false) ZoneRequest request)
            throws Exception {
        // Only for ONPREM for now
        Zone zone = safeGetZone(zoneId);

        // everything from here on needs a request body.
        if (request == null) {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        if (zone instanceof OnPremZone) {
            OnPremZone op = (OnPremZone) zone;
            OnPremRequest onpremRequest = (OnPremRequest) request;
            if (op.getOrgId() == null || !authHelper.isSystemAdmin()) {
                op.setOrgId(authHelper.getOrganizationId());
            }

            if (onpremRequest.getName() != null) {
                op.setName(onpremRequest.getName());
            }
            if (onpremRequest.getType() != null) {
                op.setType(onpremRequest.getType());
            }
            if (onpremRequest.getOrgId() != null) {
                op.setOrgId(onpremRequest.getOrgId());
            }
            if (onpremRequest.getVcenter() != null) {
                op.setVCenter(onpremRequest.getVcenter());
            }
            if (onpremRequest.getResourcePool() != null) {
                op.setResourcePool(onpremRequest.getResourcePool());
            }
            if (onpremRequest.getStorage() != null) {
                op.setStorage(onpremRequest.getStorage());
            }
            if (onpremRequest.getFolder() != null) {
                op.setFolder(onpremRequest.getFolder());
            }
            if (onpremRequest.getNetwork() != null) {
                op.setNetwork(onpremRequest.getNetwork());
            }
            if (onpremRequest.getOutboundProxy() != null) {
                op.setOutboundProxy(onpremRequest.getOutboundProxy());
            }
            if (onpremRequest.getContainerRepo() != null) {
                op.setContainerRepo(onpremRequest.getContainerRepo());
            }
            if (onpremRequest.getWavefront() != null) {
                op.setWavefront(onpremRequest.getWavefront());
            }
            if (onpremRequest.getElasticsearch() != null) {
                op.setElasticsearch(onpremRequest.getElasticsearch());
            }
            if (onpremRequest.getLogManagements() != null) {
                op.setLogManagements(onpremRequest.getLogManagements());
            }

            ValidateOrchestrationSiteRequest req = ValidateOrchestrationSiteRequest.newBuilder()
                    .setHeader(MessageHeader.newBuilder().build())
                    .setSite(BlockchainUtils.toInfo(zone))
                    .build();

            CompletableFuture<ValidateOrchestrationSiteResponse> future = new CompletableFuture<>();

            orchestrationClient.validateOrchestrationSite(req, FleetUtils.blockedResultObserver(future));
            // We don't really need the value.  If this call succeeds, the connection is OK
            future.get();
            zone = zoneService.put(zone);
            return new ResponseEntity<>(getZoneResponse(zone), HttpStatus.OK);
        } else {
            // TODO: Add zone patch for other blockchain types
            logger.error("PATCH is only available for On-prem blockchains");
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }
    }

    //TODO: Think about this.  Do we really want to allow delete?
    @RequestMapping(path = "/{zone_id}", method = RequestMethod.DELETE)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<DeleteResponse> deleteZone(@PathVariable("zone_id") UUID zoneId) {
        safeGetZone(zoneId); // Make sure the zone is present

        List<UUID> clientIdList = clientService.getClientsByParentId(zoneId)
                .stream().map(client -> client.getId()).collect(Collectors.toList());
        List<UUID> replicaIdList = replicaService.getReplicasByParentId(zoneId)
                .stream().map(replica -> replica.getId()).collect(Collectors.toList());

        if (clientIdList.size() > 0 || replicaIdList.size() > 0) {
            throw new BadRequestException(
                    String.format("Zone %s has following dependencies. Clients: %s Replicas %s Cannot be deleted.",
                            zoneId.toString(), clientIdList, replicaIdList
                    )
            );
        } else {
            zoneService.delete(zoneId);
            return new ResponseEntity<>(new DeleteResponse(zoneId), HttpStatus.OK);
        }
    }

    // Zone and ZoneRequest have the same field names with one exception, so just use JSON to copy
    private Zone requestToZone(ZoneRequest request) throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        String s = objectMapper.writeValueAsString(request);
        Zone zone = objectMapper.readValue(s, Zone.class);
        return zone;
    }

    private Zone safeGetZone(UUID zid) {
        Zone zone;

        try {
            zone = zoneService.getAuthorized(zid);
            if (zone == null) {
                throw new NotFoundException(ErrorCode.ZONE_NOT_FOUND, zid.toString());
            }
        } catch (NotFoundException e) {
            throw new NotFoundException(ErrorCode.ZONE_NOT_FOUND, zid.toString());
        }

        return zone;
    }

    // Yuck.  Return the correct zone response, based on type.
    private ZoneResponse getZoneResponse(Zone z) {
        ZoneResponse response;
        switch (z.type) {
            case ON_PREM:
                response = new OnPremGetResponse((OnPremZone) z);
                break;

            case VMC_AWS:
                response = new VmcAwsGetResponse((VmcAwsZone) z);
                break;

            default:
                response = new ZoneResponse(z);
        }
        return response;
    }
}
