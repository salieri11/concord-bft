/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.services.blockchains.zones.Zone.Type;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesRequest;
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesResponse;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteView;

/**
 * Zone service.  Read the zones from Persephone.
 */
@Service
public class ZoneService {

    private OrchestrationSiteServiceStub client;
    private GenericDao genericDao;

    @Autowired
    public ZoneService(OrchestrationSiteServiceStub client, GenericDao genericDao) {
        this.client = client;
        this.genericDao = genericDao;
    }

    private ImmutableMap<OrchestrationSiteInfo.Type, Type> typeMap
            = ImmutableMap.of(OrchestrationSiteInfo.Type.NONE, Type.NONE,
                              OrchestrationSiteInfo.Type.VMC, Type.VMC_AWS,
                              OrchestrationSiteInfo.Type.VSPHERE, Type.ON_PREM);

    private List<Zone> zones;

    /**
     * (Re)load the zone list.
     */
    public void loadZones()  throws Exception {
        ListOrchestrationSitesRequest request = new ListOrchestrationSitesRequest(new MessageHeader(), 0, "");
        CompletableFuture<ListOrchestrationSitesResponse> future = new CompletableFuture<>();

        client.listOrchestrationSites(request, FleetUtils.blockedResultObserver(future));
        List<OrchestrationSiteView> sites = future.get().getSites();
        zones = sites.stream()
                .map(s -> new Zone(FleetUtils.toUuid(s.getId()), typeMap.get(s.getType()), s.getLabels()))
                .collect(
                        Collectors.toList());
    }

    /**
     * Get all zones.  Currently that means we also add in the list of zones supplied by Persephone.
     * @return all zones.
     */
    public List<Zone> getZones() {
        // make a copy of the zones list
        List<Zone> allZones = new ArrayList<>(zones);
        // add in everything in the DB
        allZones.addAll(genericDao.getAllByType(Zone.class));
        return allZones;
    }

    public Zone put(Zone zone) {
        return genericDao.put(zone, null);
    }

    /**
     * Get a specific zone.  If not in database, look in default list.  Throw NotFound if no match.
     */
    public Zone get(UUID id) {
        try {
            return genericDao.get(id, Zone.class);
        } catch (NotFoundException e) {
            // Check the zones list
            Optional<Zone> oz = zones.stream().filter(z -> z.getId().equals(id)).findFirst();
            if (oz.isEmpty()) {
                throw e;
            }
            return oz.get();
        }
    }

    public List<Zone> getByType(Zone.Type type) {
        String json = JSONObject.toJSONString(Collections.singletonMap("type", type.toString()));
        return genericDao.getByJsonQuery(json, Zone.class);
    }

    public List<OnpremZone> getOnpremZones(UUID orgId) {
        String json = JSONObject.toJSONString(Collections.singletonMap("type", Type.ON_PREM.toString()));
        return genericDao.getJsonByParentQuery(orgId, json, OnpremZone.class);
    }

    public void delete(UUID zoneId) {
        genericDao.delete(zoneId, Zone.class);
    }
}
