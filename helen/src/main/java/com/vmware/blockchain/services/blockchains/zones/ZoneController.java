/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;

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

    @Autowired
    public ZoneController(ZoneService zoneService) {
        this.zoneService = zoneService;
    }

    enum ZoneAction {
        RELOAD
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    static class ZoneGetRequest {
        private UUID id;
        private String name;
        private String latitude;
        private String longitude;
    }


    /**
     * Get the list of zones.
     */
    @RequestMapping(method = RequestMethod.GET)
    @PreAuthorize("hasAnyAuthority(T(com.vmware.blockchain.services.profiles.Roles).user())")
    ResponseEntity<List<ZoneGetRequest>> getZoneList() {
        List<ZoneGetRequest> zones = zoneService.getZones().stream()
                .map(z -> new ZoneGetRequest(z.getId(), z.getLabels().get("name"),
                                             z.getLabels().get("latitude"), z.getLabels().get("longitude")))
                .collect(Collectors.toList());
        return new ResponseEntity<>(zones, HttpStatus.OK);
    }

    /**
     * Reload the list of zones, and return the refreshed list.
     */
    @RequestMapping(method = RequestMethod.POST)
    @PreAuthorize("hasAnyAuthority(T(com.vmware.blockchain.services.profiles.Roles).consortiumAdmin())")
    ResponseEntity<List<ZoneGetRequest>> reloadZoneList(@RequestParam ZoneAction action) throws Exception {
        if (action != ZoneAction.RELOAD) {
            throw new BadRequestException(ErrorCode.BAD_REQUEST);
        }
        zoneService.loadZones();
        List<ZoneGetRequest> zones = zoneService.getZones().stream()
                .map(z -> new ZoneGetRequest(z.getId(), z.getLabels().get("name"),
                                             z.getLabels().get("latitude"), z.getLabels().get("longitude")))
                .collect(Collectors.toList());
        return new ResponseEntity<>(zones, HttpStatus.OK);
    }

}
