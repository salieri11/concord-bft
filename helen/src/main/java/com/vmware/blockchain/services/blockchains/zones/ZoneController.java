/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.fasterxml.jackson.annotation.JsonTypeInfo.As.EXISTING_PROPERTY;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Action.RELOAD;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Action.TEST;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

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
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse;
import com.vmware.blockchain.services.blockchains.BlockchainUtils;
import com.vmware.blockchain.services.blockchains.zones.Zone.Action;
import com.vmware.blockchain.services.blockchains.zones.Zone.EndPoint;
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
    private AuthHelper authHelper;
    private OrchestrationSiteServiceStub orchestrationClient;


    @Autowired
    public ZoneController(ZoneService zoneService, AuthHelper authHelper,
                          OrchestrationSiteServiceStub orchestrationClient) {
        this.zoneService = zoneService;
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
        private String name;
        private String latitude;
        private String longitude;
        private Type type;
    }

    @Data
    @NoArgsConstructor
    static class OnPremRequest extends ZoneRequest {
        UUID orgId;
        EndPoint vcenter;
        String resourcePool;
        String storage;
        String folder;
        Zone.Network network;
        Zone.OutboundProxy outboundProxy;
        EndPoint containerRepo;
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
    ResponseEntity<ZoneResponse> getZone(@PathVariable("zone_id") UUID zoneId) {
        Zone z = zoneService.getAuthorized(zoneId);
        ZoneResponse response = getZoneResponse(z);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }


    /**
     * Create a zone, if no action specified. If an action is specified, do that action. action=test: Call persephone to
     * test the info, but don't create the zone. action=reload: Reload the zone info from persephone
     */
    @RequestMapping(method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<ZoneResponse> postZone(@RequestParam(required = false) Action action,
                                          @RequestBody(required = false) ZoneRequest request)
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
    ResponseEntity<ZoneResponse> patchZone(@PathVariable("zone_id") String zoneId,
                                           @RequestBody(required = false) ZoneRequest request)
            throws Exception {
        // Only for ONPREM for now
        Zone zone = zoneService.getAuthorized(UUID.fromString(zoneId));

        // everything from here on needs a request body.
        if (request == null) {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }

        if (zone instanceof OnpremZone) {
            OnpremZone op = (OnpremZone) zone;
            OnpremRequest onpremRequest = (OnpremRequest) request;
            if (op.getOrgId() == null || !authHelper.isSystemAdmin()) {
                op.setOrgId(authHelper.getOrganizationId());
            }

            if (onpremRequest.getName() != null) {
                zone.setName(onpremRequest.getName());
            }
            if (onpremRequest.getType() != null) {
                zone.setType(onpremRequest.getType());
            }
            if (onpremRequest.getOrgId() != null) {
                zone.setOrgId(onpremRequest.getOrgId());
            }
            if (onpremRequest.getVcenter() != null) {
                ((OnpremZone) zone).setVCenter(onpremRequest.getVcenter());
            }
            if (onpremRequest.getResourcePool() != null) {
                ((OnpremZone) zone).setResourcePool(onpremRequest.getResourcePool());
            }
            if (onpremRequest.getStorage() != null) {
                ((OnpremZone) zone).setStorage(onpremRequest.getStorage());
            }
            if (onpremRequest.getFolder() != null) {
                ((OnpremZone) zone).setFolder(onpremRequest.getFolder());
            }
            if (onpremRequest.getNetwork() != null) {
                ((OnpremZone) zone).setNetwork(onpremRequest.getNetwork());
            }
            if (onpremRequest.getContainerRepo() != null) {
                ((OnpremZone) zone).setContainerRepo(onpremRequest.getContainerRepo());
            }
        }

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

    //TODO: Think about this.  Do we really want to allow delete?
    @RequestMapping(path = "/{zone_id}", method = RequestMethod.DELETE)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<DeleteResponse> patchZone(@PathVariable("zone_id") UUID zoneId) {
        zoneService.delete(zoneId);
        return new ResponseEntity<>(new DeleteResponse(zoneId), HttpStatus.OK);
    }

    // Zone and ZoneRequest have the same field names with one exception, so just use JSON to copy
    private Zone requestToZone(ZoneRequest request) throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        String s = objectMapper.writeValueAsString(request);
        Zone zone = objectMapper.readValue(s, Zone.class);
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
