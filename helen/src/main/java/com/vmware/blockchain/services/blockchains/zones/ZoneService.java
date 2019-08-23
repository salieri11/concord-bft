/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesRequest;
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesResponse;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteView;

/**
 * Zone service.  Read the zones from Persephone.
 */
@Service
public class ZoneService {

    private OrchestrationSiteServiceStub client;

    @Autowired
    public ZoneService(OrchestrationSiteServiceStub client) {
        this.client = client;
    }

    private List<Zone> zones;

    /**
     * (Re)load the zone list.
     */
    public void loadZones() throws Exception {
        ListOrchestrationSitesRequest request = new ListOrchestrationSitesRequest(new MessageHeader(), 0, "");
        CompletableFuture<ListOrchestrationSitesResponse> future = new CompletableFuture<>();
        client.listOrchestrationSites(request, FleetUtils.blockedResultObserver(future));
        List<OrchestrationSiteView> sites = future.get().getSites();
        zones = sites.stream().map(s -> new Zone(FleetUtils.toUuid(s.getId()), s.getType(), s.getLabels())).collect(
                Collectors.toList());
    }

    public List<Zone> getZones() {
        return zones;
    }
}
